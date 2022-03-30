{-# LANGUAGE TypeApplications #-}
module Cardano.Db.Delete
  ( deleteAfterBlockNo
  , deleteCascadeBlock
  , deleteCascadeAfter
  , deleteCascadeBlockNo
  , deleteCascadeSlotNo
  , deleteDelistedPool
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Esqueleto.Experimental (delete, deleteCount, from, just, table, val,
                   where_, (==.), (>.), (^.))

import           Database.Persist.Sql (SqlBackend)

import           Data.ByteString (ByteString)
import           Data.Int (Int64)

import           Cardano.Db.Query (isJust)
import           Cardano.Db.Run (transactionCommit)
import           Cardano.Db.Schema

import           Ouroboros.Network.Block (BlockNo (..))


-- | Delete all blocks with a block number greater than or equal to the supplied `BlockNo`.
-- Returns 'True' if any blocks were deleted and 'False' if none were found.
deleteAfterBlockNo :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteAfterBlockNo (BlockNo blkNo) = do
  count <- deleteCount $ do
            blk <- from $ table @Block
            where_ (blk ^. BlockBlockNo >. just (val blkNo))

  delete $ from (table @CollateralTxIn) >>= \cti -> where_ (cti ^. CollateralTxInBlockNo >. val blkNo)
  delete $ from (table @Delegation) >>= \ d -> where_ (d ^. DelegationBlockNo >. val blkNo)
  delete $ from (table @StakeDeregistration) >>= \ sd -> where_ (sd ^. StakeDeregistrationBlockNo >. val blkNo)
  delete $ from (table @StakeRegistration) >>= \ sr -> where_ (sr ^. StakeRegistrationBlockNo >. val blkNo)
  delete $ from (table @Tx) >>= \ tx -> where_ (tx ^. TxBlockNo >. val blkNo)
  delete $ from (table @TxIn) >>= \ txi -> where_ (txi ^. TxInBlockNo >. val blkNo)
  delete $ from (table @TxOut) >>= \ txo -> where_ (txo ^. TxOutBlockNo >. val blkNo)

  transactionCommit
  pure $ isNonZero count

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockHash ==. val (blockHash block))

-- | Delete a block after the specified 'BlockId'. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeAfter :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteCascadeAfter (BlockNo blkNo) = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (isJust $ blk ^. BlockEpochNo)
      where_ (blk ^. BlockBlockNo ==. just (val blkNo))

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlockNo :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteCascadeBlockNo (BlockNo blkNo) = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockBlockNo ==. just (val blkNo))

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeSlotNo :: MonadIO m => SlotNo -> ReaderT SqlBackend m Bool
deleteCascadeSlotNo (SlotNo slotNo) = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockSlotNo ==. just (val slotNo))

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
deleteDelistedPool poolHash = do
  isNonZero <$$>
    deleteCount $ do
      delisted <- from $ table @DelistedPool
      where_ (delisted ^. DelistedPoolHashRaw ==. val poolHash)

-- -------------------------------------------------------------------------------------------------

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

isNonZero :: Int64 -> Bool
isNonZero = (> 0)
