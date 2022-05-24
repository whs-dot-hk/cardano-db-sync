{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.DbSync.Era.Shelley.Generic.Rewards
  ( Reward (..)
  , Rewards (..)
  , elemCount
  , totalAda
  ) where

import           Cardano.Prelude

import           Cardano.Db (Ada, RewardSource (..), word64ToAda)

import           Cardano.DbSync.Types

import qualified Data.Strict.Maybe as Strict

import           Cardano.Ledger.Coin (Coin (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.CanHardFork ()

data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool :: !(Strict.Maybe PoolKeyHash)
  , rewardAmount :: !Coin
  } deriving (Eq, Ord, Show)

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
newtype Rewards = Rewards
  { rwdRewards :: Map StakeCred (Set Reward)
  } deriving (Eq, Show)

elemCount :: Rewards -> Int
elemCount = sum . map Set.size . Map.elems . rwdRewards

totalAda :: Rewards -> Ada
totalAda rwds =
  word64ToAda . fromIntegral . sum
    . concatMap (map (unCoin . rewardAmount) . Set.toList)
    $ Map.elems (rwdRewards rwds)
