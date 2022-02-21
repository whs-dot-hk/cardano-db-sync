{-# LANGUAGE FlexibleContexts #-}

module Cardano.DbSync.Cache
    ( Cache
    , newEmptyCache
    , queryMAWithCache
    , queryPrevBlockWithCache
    , insertBlockAndCache
    ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Control
import qualified Data.Map.Strict as Map
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

import           Cardano.Ledger.Mary.Value (AssetName (..))

import qualified Cardano.Db as DB
import           Cardano.DbSync.Era.Util
import           Cardano.DbSync.Error

import           Database.Persist.Postgresql (SqlBackend)

data Cache = Cache
  { maIndex :: IORef (Map (ByteString, AssetName) DB.MultiAssetId)
  , prevBlock :: IORef (Maybe (DB.BlockId, ByteString))
  }


newEmptyCache :: MonadIO m => m Cache
newEmptyCache = liftIO $ Cache <$> newIORef Map.empty <*> newIORef Nothing

queryMAWithCache :: MonadIO m => Cache -> ByteString -> AssetName
                 -> ReaderT SqlBackend m (Maybe DB.MultiAssetId)
queryMAWithCache Cache {maIndex = ref} policyId a@(AssetName aName) = do
    mp <- liftIO $ readIORef ref 
    case Map.lookup (policyId, a) mp of
      Just maId -> pure $ Just maId
      Nothing -> do
        maId <- DB.queryMultiAssetId policyId aName
        case maId of
          Nothing -> pure Nothing
          Just mId -> do
            liftIO $ modifyIORef ref $ Map.insert (policyId, a) mId
            pure maId 

queryPrevBlockWithCache :: MonadIO m => Text -> Cache -> ByteString
                        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
queryPrevBlockWithCache msg Cache {prevBlock = ref} hsh = do
    mCachedPrev <- liftIO $ readIORef ref
    case mCachedPrev of
      Just (cachedBlockId, cachedHash) | cachedHash == hsh -> pure cachedBlockId
      _ -> liftLookupFail msg $ DB.queryBlockId hsh

insertBlockAndCache :: (MonadIO m, MonadBaseControl IO m) => Cache -> DB.Block -> ReaderT SqlBackend m DB.BlockId
insertBlockAndCache Cache {prevBlock = ref} block = do
    bid <- DB.insertBlock block
    liftIO $ writeIORef ref $ Just (bid, DB.blockHash block)
    pure bid