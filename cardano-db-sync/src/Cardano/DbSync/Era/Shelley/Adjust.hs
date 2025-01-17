{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Adjust
  ( adjustEpochRewards
  ) where

import           Cardano.Prelude hiding (from, groupBy, on)

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as Db

import           Cardano.DbSync.Cache
import qualified Cardano.DbSync.Era.Shelley.Generic.Rewards as Generic
import           Cardano.DbSync.Era.Shelley.Generic.StakeCred

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Database.Esqueleto.Experimental (SqlBackend, delete, from, in_, table, val,
                   valList, where_, (==.), (^.))

-- Hlint warns about another version of this operator.
{- HLINT ignore "Redundant ^." -}

-- This is a hack/workaround for an issue related to the `Reward` table.
--
-- Reward payments (either staking rewards or MIR payments) can be made to a valid stake address
-- which is then deregistered before the payment is actually paid out (which happens at the epoch
-- boundary). To fix this, at the start of the epoch we find all the stake addresses which have
-- been de-registered and not reregistered and then delete all rewards for those addresses and that
-- epoch.

adjustEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Cache -> EpochNo -> Generic.Rewards
    -> Set StakeCred
    -> ReaderT SqlBackend m ()
adjustEpochRewards tracer cache epochNo rwds creds = do
  let eraIgnored = Map.toList $ Generic.rwdRewards rwds
  liftIO . logInfo tracer $ mconcat
    [ "Removing ", if null eraIgnored then "" else Db.textShow (length eraIgnored) <> " rewards and "
    , show (length creds), " orphaned rewards"]
  forM_ eraIgnored $ \(cred, rewards)->
    forM_ (Set.toList rewards) $ \rwd ->
      deleteReward cache epochNo (cred, rwd)
  crds <- rights <$> forM (Set.toList creds) (queryStakeAddrWithCache cache DontCacheNew)
  deleteOrphanedRewards epochNo crds

deleteReward
    :: (MonadBaseControl IO m, MonadIO m)
    => Cache -> EpochNo -> (StakeCred, Generic.Reward)
    -> ReaderT SqlBackend m ()
deleteReward cache epochNo (cred, rwd) = do
  mAddrId <- queryStakeAddrWithCache cache DontCacheNew cred
  eiPoolId <- case Generic.rewardPool rwd of
    Nothing -> pure $ Left $ Db.DbLookupMessage "deleteReward.queryPoolKeyWithCache"
    Just poolHash -> queryPoolKeyWithCache cache DontCacheNew poolHash
  case (mAddrId, eiPoolId) of
    (Right addrId, Right poolId) -> do
      delete $ do
        rwdDb <- from $ table @Db.Reward
        where_ (rwdDb ^. Db.RewardAddrId ==. val addrId)
        where_ (rwdDb ^. Db.RewardType ==. val (Generic.rewardSource rwd))
        where_ (rwdDb ^. Db.RewardSpendableEpoch ==. val (unEpochNo epochNo))
        where_ (rwdDb ^. Db.RewardPoolId ==. val (Just poolId))
    _ -> pure ()

deleteOrphanedRewards :: MonadIO m => EpochNo -> [Db.StakeAddressId] -> ReaderT SqlBackend m ()
deleteOrphanedRewards (EpochNo epochNo) xs =
  delete $ do
    rwd <- from $ table @Db.Reward
    where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
    where_ (rwd ^. Db.RewardAddrId `in_` valList xs)
