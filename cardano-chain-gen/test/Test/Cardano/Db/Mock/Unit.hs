{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Class.MonadSTM.Strict
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)

import           Ouroboros.Network.Block (blockNo, blockPoint, blockSlot)

import qualified Cardano.Db as DB

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Ledger.Alonzo.Data
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Keys
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.SafeHash
import           Cardano.Ledger.Shelley.TxBody
import           Cardano.Ledger.Slot (BlockNo (..), EpochNo)

import           Cardano.DbSync.Era.Shelley.Generic.Block (blockHash)
import           Cardano.DbSync.Era.Shelley.Generic.Util

import           Cardano.SMASH.Server.PoolDataLayer
import           Cardano.SMASH.Server.Types

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Examples
import           Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import           Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import           Cardano.Mock.Forging.Tx.Generic
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import           Cardano.Mock.Forging.Types

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import           Test.Cardano.Db.Mock.Config
import           Test.Cardano.Db.Mock.Examples
import           Test.Cardano.Db.Mock.UnifiedApi
import           Test.Cardano.Db.Mock.Validate

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
    testGroup "unit tests"
      [ testGroup "simple"
          [ test "simple forge blocks" forgeBlocks
          , test "sync one block" addSimple
          , test "restart db-sync" restartDBSync
          , test "sync small chain" addSimpleChain
          ]
      , testGroup "rollbacks"
          [ test "simple rollback" simpleRollback
          , test "sync bigger chain" bigChain
          , test "rollback while db-sync is off" restartAndRollback
          , test "rollback further" rollbackFurther
          ]
      , testGroup "different configs"
          [ test "genesis config without pool" configNoPools
          , test "genesis config without stakes" configNoStakes
          ]
      , testGroup "blocks with txs"
          [ test "simple tx" addSimpleTx
          , test "simple tx in Shelley era" addSimpleTxShelley
          , test "consume utxo same block" consumeSameBlock
          ]
      , testGroup "stake addresses"
          [ test "(de)registrations" registrationTx
          , test "(de)registrations in same block" registrationsSameBlock
          , test "(de)registrations in same tx" registrationsSameTx
          , test "stake address pointers" stakeAddressPtr
          , test "stake address pointers deregistration" stakeAddressPtrDereg
          , test "stake address pointers. Use before registering." stakeAddressPtrUseBefore
          ]
      , testGroup "rewards"
          [ test "rewards simple" simpleRewards
          , test "shelley rewards from multiple sources" rewardsShelley
          , test "rewards with deregistration" rewardsDeregistration
          , test "Mir Cert" mirReward
          , test "Mir rollback" mirRewardRollback
          , test "Mir Cert Shelley" mirRewardShelley
          , test "Mir Cert deregistration" mirRewardDereg
          , test "test rewards empty last part of epoch" rewardsEmptyChainLast
          , test "test delta rewards" rewardsDelta
          , test "rollback on epoch boundary" rollbackBoundary
          , test "single MIR Cert multiple outputs" singleMIRCertMultiOut
          ]
      , testGroup "stake distribution"
          [ test "stake distribution from genesis" stakeDistGenesis
          , test "2000 delegations" delegations2000
          , test "2001 delegations" delegations2001
          , test "8000 delegations" delegations8000
          , test "many delegations" delegationsMany
          , test "many delegations, sparse chain" delegationsManyNotDense
          ]
      , testGroup "plutus spend scripts"
          [ test "simple script lock" simpleScript
          , test "unlock script in same block" unlockScriptSameBlock
          , test "failed script" failedScript
          , test "failed script in same block" failedScriptSameBlock
          , test "multiple scripts unlocked" multipleScripts
          , test "multiple scripts unlocked same block" multipleScriptsSameBlock
          , test "multiple scripts failed" multipleScriptsFailed
          , test "multiple scripts failed same block" multipleScriptsFailedSameBlock
          ]
      , testGroup "plutus cert scripts"
          [ test "stake scripts" registrationScriptTx
          , test "stake scripts deregistration" deregistrationScriptTx
          , test "multiple stake scripts deregistration" deregistrationsScriptTxs
          , test "multiple stake scripts deregistration in same tx" deregistrationsScriptTx
          , test "multiple stake scripts deregistration in same tx missing redeemer 1" deregistrationsScriptTx'
          , test "multiple stake scripts deregistration in same tx missing redeemer 2" deregistrationsScriptTx''
          ]
      , testGroup "MultiAssets plutus scripts"
          [ test "mint simple multi asset" mintMultiAsset
          , test "mint many multi assets" mintMultiAssets
          , test "swap many multi assets" swapMultiAssets
          ]
      , testGroup "pools and smash"
          [ test "pool registration" poolReg
          , test "query pool that's not registered" nonexistantPoolQuery
          , test "pool deregistration" poolDeReg
          , test "pool multiple deregistration" poolDeRegMany
          , test "delist pool" poolDelist
          ]
      ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)

defaultConfigDir ::  FilePath
defaultConfigDir = "config"

forgeBlocks :: IOManager -> [(Text, Text)] -> Assertion
forgeBlocks = do
    withFullConfig defaultConfigDir testLabel $ \interpreter _mockServer _dbSync -> do
      _block0 <- forgeNext interpreter mockBlock0
      _block1 <- forgeNext interpreter mockBlock1
      block2 <- forgeNext interpreter mockBlock2
      let blkNo = blockNo block2
      assertBool (show blkNo <> " /= " <> "3")
        $ blkNo == BlockNo 3
  where
    testLabel = "forgeBlocks"

addSimple :: IOManager -> [(Text, Text)] -> Assertion
addSimple =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- Given a mock block, translate it into a real block and submit it to the
      -- chainsync server
      void $ forgeNextAndSubmit interpreter mockServer mockBlock0
      -- start db-sync and let it sync
      startDBSync dbSync
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "addSimple"

addSimpleChain :: IOManager -> [(Text, Text)] -> Assertion
addSimpleChain =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the blocks to real Cardano blocks.
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      -- start db-sync and let it sync
      startDBSync dbSync
      -- add more blocks
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff dbSync 3
  where
    testLabel = "addSimpleChain"

restartDBSync :: IOManager -> [(Text, Text)] -> Assertion
restartDBSync =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      void $ forgeNextAndSubmit interpreter mockServer mockBlock0
      -- start db-sync and let it sync
      startDBSync dbSync
      assertBlockNoBackoff dbSync 1

      stopDBSync dbSync
      -- The server sees a separate client here
      startDBSync dbSync
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "restartDBSync"

simpleRollback :: IOManager -> [(Text, Text)] -> Assertion
simpleRollback = do
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      startDBSync dbSync
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff dbSync 3

      atomically $ rollback mockServer (blockPoint blk1)
      assertBlockNoBackoff dbSync 2
  where
    testLabel = "simpleRollback"

bigChain :: IOManager -> [(Text, Text)] -> Assertion
bigChain =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
      startDBSync dbSync
      assertBlockNoBackoff dbSync 101

      blks' <- forM (replicate 100 mockBlock1) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 201

      forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 206

      atomically $ rollback mockServer (blockPoint $ last blks')
      assertBlockNoBackoff dbSync 201
  where
    testLabel = "bigChain"

restartAndRollback :: IOManager -> [(Text, Text)] -> Assertion
restartAndRollback =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
      startDBSync dbSync
      assertBlockNoBackoff dbSync 101

      blks <- forM (replicate 100 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 201

      forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 206

      stopDBSync dbSync
      atomically $ rollback mockServer (blockPoint $ last blks)
      startDBSync dbSync
      assertBlockNoBackoff dbSync 201
  where
    testLabel = "restartAndRollback"

-- wibble
rollbackFurther :: IOManager -> [(Text, Text)] -> Assertion
rollbackFurther =
  withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
    blks <- replicateM 80 (forgeNextFindLeaderAndSubmit interpreter mockServer [])
    startDBSync dbSync
    assertBlockNoBackoff dbSync 80

    -- We want to test that db-sync rollbacks temporarily to block 34
    -- and then syncs further. We add references to blocks 34 and 35, to
    -- validate later that one is deleted through cascade, but the other was not
    -- because a checkpoint was found.
    let blockHash1 = hfBlockHash (blks !! 33)
    Right bid1 <- queryDBSync dbSync $ DB.queryBlockId blockHash1
    cm1 <- queryDBSync dbSync $ DB.insertCostModel $ DB.CostModel "{\"1\" : 1}" bid1

    let blockHash2 = hfBlockHash (blks !! 34)
    Right bid2 <- queryDBSync dbSync $ DB.queryBlockId blockHash2
    cm2 <- queryDBSync dbSync $ DB.insertCostModel $ DB.CostModel "{\"2\" : 2}" bid2

    -- Note that there is no epoch change, which would add a new entry, since we have
    -- 80 blocks and not 100, which is the expected blocks/epoch. This also means there
    -- no epoch snapshots
    assertEqQuery dbSync DB.queryCostModel [cm1, cm2] "Unexpected CostModels"

    -- server tells db-sync to rollback to point 50. However db-sync only has
    -- a snapshot at block 34, so it will go there first. There is no proper way
    -- to test that db-sync temporarily is there, that's why we have this trick
    -- with references.
    atomically $ rollback mockServer (blockPoint $ blks !! 50)
    assertBlockNoBackoff dbSync 51

    assertEqQuery dbSync DB.queryCostModel [cm1] "Unexpected CostModel"
  where
    testLabel = "rollbackFurther"

configNoPools :: IOManager -> [(Text, Text)] -> Assertion
configNoPools =
    withFullConfig "config2" testLabel $ \_ _ dbSync -> do
      startDBSync  dbSync
      assertBlocksCount dbSync 2
      assertTxCount dbSync 6
      stopDBSync dbSync
      startDBSync  dbSync
      -- Nothing changes, so polling assertions doesn't help here
      -- We have to pause and check if anything crashed.
      threadDelay 3_000_000
      checkStillRuns dbSync
      assertBlocksCount dbSync 2 -- 2 genesis blocks
      assertTxCount dbSync 6
  where
    testLabel = "configNoPools"


configNoStakes :: IOManager -> [(Text, Text)] -> Assertion
configNoStakes =
    withFullConfig "config3" testLabel $ \interpreter _ dbSync -> do
      startDBSync  dbSync
      assertBlocksCount dbSync 2
      assertTxCount dbSync 7
      stopDBSync dbSync
      startDBSync dbSync
      -- Nothing changes, so polling assertions don't help here
      -- We have to pause and check if anything crashed.
      threadDelay 3_000_000
      checkStillRuns dbSync
      assertBlocksCount dbSync 2
      assertTxCount dbSync 7
      -- A pool with no stakes can't create a block.
      eblk <- try $ forgeNext interpreter mockBlock0
      case eblk of
        Right _ -> assertFailure "should fail"
        Left WentTooFar -> pure ()
        -- TODO add an option to disable fingerprint validation for tests like this.
        Left (EmptyFingerprint _ _) -> pure ()
        Left err -> assertFailure $ "got " <> show err <> " instead of WentTooFar"
  where
    testLabel = "configNoStakes"

addSimpleTx :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTx =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
              Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

      startDBSync  dbSync
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "addSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      void $ withShelleyFindLeaderAndSubmitTx interpreter mockServer $
            Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

      -- start db-sync and let it sync
      startDBSync  dbSync
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "addSimpleTxShelley"

registrationTx :: IOManager -> [(Text, Text)] -> Assertion
registrationTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)]

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)]

    -- We add interval or else the txs would have the same id
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer
        (fmap (Alonzo.addValidityInterval 1000)
           . Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)])

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer
        (fmap (Alonzo.addValidityInterval 2000)
           . Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)])

    assertBlockNoBackoff dbSync 4
    assertCertCounts dbSync (2,2,0,0)
  where
    testLabel = "registrationTx"

registrationsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)] st
        tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)] st
        tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)] st
        tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)] st
        Right [tx0, tx1, Alonzo.addValidityInterval 1000 tx2, Alonzo.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2,2,0,0)
  where
    testLabel = "registrationsSameBlock"

registrationsSameTx :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey)
                               , (StakeIndexNew 1, DCertDeleg . DeRegKey)
                               , (StakeIndexNew 1, DCertDeleg . RegKey)
                               , (StakeIndexNew 1, DCertDeleg . DeRegKey)]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2,2,0,0)
  where
    testLabel = "registrationsSameTx"

stakeAddressPtr :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtr =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    blk <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey)]

    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
      Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

    assertBlockNoBackoff dbSync 2
    assertCertCounts dbSync (1,0,0,0)
  where
    testLabel = "stakeAddressPtr"

stakeAddressPtrDereg :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrDereg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    blk <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 0, DCertDeleg . RegKey)]

    let ptr0 = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    blk' <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      tx1 <- Alonzo.mkSimpleDCertTx [ (StakeIndexNew 0, DCertDeleg . DeRegKey)
                                    , (StakeIndexNew 0, DCertDeleg . RegKey) ]
                                    st
      pure [tx0, tx1]

    let ptr1 = Ptr (blockSlot blk') (TxIx 1) (CertIx 1)

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithPtr 0 ptr1) 20000 20000 st
        tx1 <- Alonzo.mkPaymentTx (UTxOIndex 2) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
        pure [tx0, tx1]

    st <- getAlonzoLedgerState interpreter
    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (2,1,0,0)
    -- The 2 addresses have the same payment credentials and they reference the same
    -- stake credentials, however they have
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr0) (DB.DbLovelace 40000) st
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr1) (DB.DbLovelace 20000) st
  where
    testLabel = "stakeAddressPtrDereg"

stakeAddressPtrUseBefore :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrUseBefore =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      -- first use this stake credential
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 10000 500

        -- and then register it
      blk <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey)]

      let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

      assertBlockNoBackoff dbSync 3
      assertCertCounts dbSync (1,0,0,0)
  where
    testLabel = "stakeAddressPtrUseBefore"

consumeSameBlock :: IOManager -> [(Text, Text)] -> Assertion
consumeSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
        let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
        tx1 <- Alonzo.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
        pure [tx0, tx1]
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "consumeSameBlock"

simpleRewards :: IOManager -> [(Text, Text)] -> Assertion
simpleRewards =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      -- Pools are not registered yet, this takes 2 epochs. So fees of this tx
      -- should not create any rewards.
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      a <- fillEpochs interpreter mockServer 3
      assertBlockNoBackoff dbSync (fromIntegral $ 2 + length a)

      -- The pool leaders take leader rewards with value 0
      assertRewardCount dbSync 3

      st <- getAlonzoLedgerState interpreter
      -- False indicates that we provide the full expected list of addresses with rewards.
      assertRewardCounts dbSync st False (Just 3)
          [ (StakeIndexPoolLeader (PoolIndex 0), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndex 1), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndex 2), (1,0,0,0,0))]

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillEpochs interpreter mockServer 2

      assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 2 + length b)
      assertRewardCount dbSync 14
      assertRewardCounts dbSync st True (Just 5)
          -- 2 pool leaders also delegate to pools.
          [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (1,1,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (1,1,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 0), (0,1,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 1), (0,1,0,0,0))
          ]
  where
    testLabel = "simpleRewards"

-- This test is the same as the previous, but in Shelley era. Rewards result
-- should be different because of the old Shelley bug.
-- https://github.com/input-output-hk/cardano-db-sync/issues/959
--
-- The differenece in rewards is triggered when a reward address of a pool A
-- delegates to a pool B and is not an owner of pool B. In this case it receives
-- leader rewards from pool A and member rewards from pool B. In this test, we
-- have 2 instances of this case, one where A = B and one where A /= B.
rewardsShelley :: IOManager -> [(Text, Text)] -> Assertion
rewardsShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      void $ withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      a <- fillEpochs interpreter mockServer 3
      assertRewardCount dbSync 3

      void $ withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillEpochs interpreter mockServer 2

      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + 3)
      st <- withShelleyLedgerState interpreter Right
      -- Note we have 2 rewards less compared to Alonzo era
      assertRewardCount dbSync 12
      assertRewardCounts dbSync st True (Just 5)
          -- Here we dont' have both leader and member rewards.
          [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (1,0,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 0), (0,1,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 1), (0,1,0,0,0))
          ]

  where
    testLabel = "rewardsShelley"

rewardsDeregistration :: IOManager -> [(Text, Text)] -> Assertion
rewardsDeregistration =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDepositTxPools (UTxOIndex 1) 20000

      -- first move to treasury from reserves
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
            -- register the stake address and delegate to a pool
        let poolId = resolvePool (PoolIndex 0) st
        tx1 <- Alonzo.mkSimpleDCertTx
                    [ (StakeIndexNew 1, DCertDeleg . RegKey)
                    , (StakeIndexNew 1, \stCred -> DCertDeleg $ Delegate $ Delegation stCred poolId) ]
                    st
            -- send some funds to the address so
        tx2 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 100000 5000 st
        Right [tx1, tx2]

      a <- fillEpochs interpreter mockServer 3
      assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a)

      st <- getAlonzoLedgerState interpreter

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillEpochs interpreter mockServer 2

      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length a + length b)
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,1,0,0,0))]

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000

      c <- fillEpochs interpreter mockServer 2

      assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + length b + length c)
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,2,0,0,0))]

      d <- fillEpochs interpreter mockServer 1
      e <- fillEpochPercentage interpreter mockServer 85
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
          Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)]

      f <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f))
      -- stays at 2, since it's deregistered.
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,2,0,0,0))]

      g <- fillEpochs interpreter mockServer 2
      assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f <> g))
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,2,0,0,0))]

  where
    testLabel = "rewardsDeregistration"

mirReward :: IOManager -> [(Text, Text)] -> Assertion
mirReward =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      void $ fillEpochPercentage interpreter mockServer 50

      -- mir from treasury
      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))] st
        tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200))))] st
        tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300))))] st
        pure [tx1, tx2, tx3]

      void $ fillUntilNextEpoch interpreter mockServer

      st <- getAlonzoLedgerState interpreter
      -- 2 mir rewards from treasury are sumed
      assertRewardCounts dbSync st True Nothing [(StakeIndex 1, (0,0,1,1,0))]
  where
    testLabel = "mirReward"

mirRewardRollback :: IOManager -> [(Text, Text)] -> Assertion
mirRewardRollback =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey) ]

      a <- fillUntilNextEpoch interpreter mockServer
      b <- fillEpochPercentage interpreter mockServer 5
      -- mir from treasury
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 1000))))]
      c <- fillEpochPercentage interpreter mockServer 50
      d <- fillUntilNextEpoch interpreter mockServer

      st <- getAlonzoLedgerState interpreter
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d))
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,0,0,1,0))]

      atomically $ rollback mockServer (blockPoint $ last c)
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c))
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,0,0,1,0))]
      stopDBSync dbSync
      startDBSync dbSync
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c))
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,0,0,1,0))]

      forM_ d $ atomically . addBlock mockServer
      e <- fillEpochPercentage interpreter mockServer 5
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d <> e))
      assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0,0,0,1,0))]
  where
    testLabel = "mirRewardRollback"

mirRewardShelley :: IOManager -> [(Text, Text)] -> Assertion
mirRewardShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      -- TODO test that this has no effect. You can't send funds between reserves and
      -- treasury before protocol version 5.
      void $ withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        const $ Shelley.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))]
                         (Wdrl mempty)

      a <- fillEpochPercentage interpreter mockServer 50

      -- mir from reserves
      void $ withShelleyFindLeaderAndSubmitTx  interpreter mockServer $ Shelley.mkSimpleDCertTx
        [(StakeIndex 1, \cred -> DCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))]

      b <- fillUntilNextEpoch interpreter mockServer

      st <- withShelleyLedgerState interpreter Right
      assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a + length b)
      assertRewardCounts dbSync st False Nothing [(StakeIndex 1, (0,0,1,0,0))]
  where
    testLabel = "mirRewardShelley"


mirRewardDereg :: IOManager -> [(Text, Text)] -> Assertion
mirRewardDereg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      a <- fillUntilNextEpoch interpreter mockServer

      -- mir from treasury
      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))] st
        tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200))))] st
        tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300))))] st
        pure [tx1, tx2, tx3]

      b <- fillEpochPercentage interpreter mockServer 20
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
          Alonzo.mkSimpleDCertTx [(StakeIndex 1, DCertDeleg . DeRegKey)]

      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b))
      -- deregistration means empty rewards
      st <- getAlonzoLedgerState interpreter
      assertRewardCounts dbSync st False Nothing []
  where
    testLabel = "mirRewardDereg"

rewardsEmptyChainLast :: IOManager -> [(Text, Text)] -> Assertion
rewardsEmptyChainLast =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      a <- fillEpochs interpreter mockServer 3
      assertRewardCount dbSync 3

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillUntilNextEpoch interpreter mockServer
      assertRewardCount dbSync 6

      c <- fillEpochPercentage interpreter mockServer 68

      -- Skip a percentage of the epoch epoch
      void $ skipUntilNextEpoch interpreter mockServer []
      d <- fillUntilNextEpoch interpreter mockServer
      assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 1 + length b + length c + 1 + length d)
      assertRewardCount dbSync 17
  where
    testLabel = "rewardsEmptyChainLast"

rewardsDelta :: IOManager -> [(Text, Text)] -> Assertion
rewardsDelta =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      -- These delegation push the computation of the 3 leader
      -- rewards toward the 8k/f slot, so it can be delayed even more
      -- with the missing blocks and create the delta reward.
      -- This trick may break at some point in the future.
      a <- delegateAndSendBlocks 1000 interpreter
      forM_ a $ atomically . addBlock mockServer
      void $ registerAllStakeCreds interpreter mockServer
      b <- fillEpochs interpreter mockServer 3
      assertRewardCount dbSync 3

      c <- fillUntilNextEpoch interpreter mockServer
      assertRewardCount dbSync 6

      d <- fillEpochPercentage interpreter mockServer 68
      assertRewardCount dbSync 6

      -- Skip a percentage of the epoch epoch
      void $ skipUntilNextEpoch interpreter mockServer []
      assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + length b + length c + 1 + length d)
      -- These are delta rewards aka rewards that were added at the epoch boundary, because the reward
      -- update was not complete on time, due to missing blocks.
      assertRewardCount dbSync 9
  where
    testLabel = "rewardsDelta"

rollbackBoundary :: IOManager -> [(Text, Text)] -> Assertion
rollbackBoundary =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer
      a <- fillEpochs interpreter mockServer 2

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      blks <- forgeAndSubmitBlocks interpreter mockServer 50
      blks' <- fillUntilNextEpoch interpreter mockServer

      assertRewardCount dbSync 3
      atomically $ rollback mockServer (blockPoint $ last blks)
      assertBlockNoBackoff dbSync (2 + length a + length blks)
      forM_ blks' $ atomically . addBlock mockServer
      assertBlockNoBackoff dbSync (2 + length a + length blks + length blks')
      assertRewardCount dbSync 3
  where
    testLabel = "rollbackBoundary"

singleMIRCertMultiOut :: IOManager -> [(Text, Text)] -> Assertion
singleMIRCertMultiOut =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
            Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      a <- fillUntilNextEpoch interpreter mockServer

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \ state -> do
            stakeAddr0 <- resolveStakeCreds (StakeIndex 0) state
            stakeAddr1 <- resolveStakeCreds (StakeIndex 1) state
            let saMIR = StakeAddressesMIR (Map.fromList [(stakeAddr0, DeltaCoin 10), (stakeAddr1, DeltaCoin 20)])
            Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR saMIR, DCertMir $ MIRCert TreasuryMIR saMIR] (Wdrl mempty)

      b <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (2 + length a + length b)
      assertRewardCount dbSync 4
  where
    testLabel = "singleMIRCertMultiOut"

stakeDistGenesis :: IOManager -> [(Text, Text)] -> Assertion
stakeDistGenesis =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync
      a <- fillUntilNextEpoch interpreter mockServer
      assertBlockNoBackoff dbSync (fromIntegral $ length a)
      -- There are 5 delegations in genesis
      assertEpochStake dbSync 5
  where
    testLabel = "stakeDistGenesis"

delegations2000 :: IOManager -> [(Text, Text)] -> Assertion
delegations2000 =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync
      a <- delegateAndSendBlocks 1995 interpreter
      forM_ a $ atomically . addBlock mockServer
      b <- fillUntilNextEpoch interpreter mockServer
      c <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
      -- There are exactly 2000 entries on the second epoch, 5 from genesis and 1995 manually added
      assertEpochStakeEpoch dbSync 2 2000

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 1)
      assertEpochStakeEpoch dbSync 2 2000
  where
    testLabel = "delegations2000"

delegations2001 :: IOManager -> [(Text, Text)] -> Assertion
delegations2001 =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync
      a <- delegateAndSendBlocks 1996 interpreter
      forM_ a $ atomically . addBlock mockServer
      b <- fillUntilNextEpoch interpreter mockServer
      c <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
      -- The first block of epoch inserts 2000 out of 2001 epoch distribution.
      assertEpochStakeEpoch dbSync 2 2000
      -- The remaining entry is inserted on the next block.
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 1)
      assertEpochStakeEpoch dbSync 2 2001
  where
    testLabel = "delegations2001"

delegations8000 :: IOManager -> [(Text, Text)] -> Assertion
delegations8000 =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync
      a <- delegateAndSendBlocks 7995 interpreter
      forM_ a $ atomically . addBlock mockServer
      b <- fillEpochs interpreter mockServer 3

      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b)
      assertEpochStakeEpoch dbSync 3 2000

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertEpochStakeEpoch dbSync 3 4000

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertEpochStakeEpoch dbSync 3 6000

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertEpochStakeEpoch dbSync 3 8000

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertEpochStakeEpoch dbSync 3 8000
  where
    testLabel = "delegations8000"

delegationsMany :: IOManager -> [(Text, Text)] -> Assertion
delegationsMany =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync
      a <- delegateAndSendBlocks 40000 interpreter
      forM_ a $ atomically . addBlock mockServer
      b <- fillEpochs interpreter mockServer 5

      -- too long. We cannot use default delays
      assertBlockNoBackoffTimes (repeat 10) dbSync (fromIntegral $ length a + length b)
      -- The slice size here is
      -- 1 + div (delegationsLen * 5) expectedBlocks = 2001
      -- instead of 2000, because there are many delegations
      assertEpochStakeEpoch dbSync 7 2001

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertEpochStakeEpoch dbSync 7 4002

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertEpochStakeEpoch dbSync 7 6003
  where
    testLabel = "delegationsMany"

delegationsManyNotDense :: IOManager -> [(Text, Text)] -> Assertion
delegationsManyNotDense =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync
      a <- delegateAndSendBlocks 40000 interpreter
      forM_ a $ atomically . addBlock mockServer
      b <- fillEpochs interpreter mockServer 5

      -- too long. We cannot use default delays
      assertBlockNoBackoffTimes (repeat 10) dbSync (fromIntegral $ length a + length b)
      -- The slice size here is
      -- 1 + div (delegationsLen * 5) expectedBlocks = 2001
      -- instead of 2000, because there are many delegations
      assertEpochStakeEpoch dbSync 7 2001

      -- Blocks come on average every 5 slots. If we skip 15 slots before each block,
      -- we are expected to get only 1/4 of the expected blocks. The adjusted slices
      -- should still be long enough to cover everything.
      replicateM_ 40 $
        forgeNextSkipSlotsFindLeaderAndSubmit interpreter mockServer 15 []

      -- Even if the chain is sparse, all distributions are inserted.
      assertEpochStakeEpoch dbSync 7 40005
  where
    testLabel = "delegationsManyNotDense"

simpleScript :: IOManager -> [(Text, Text)] -> Assertion
simpleScript =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      a <- fillUntilNextEpoch interpreter mockServer

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000

      assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
      assertEqQuery dbSync (fmap getOutFields <$> DB.queryScriptOutputs) [expectedFields] "Unexpected script outputs"
  where
    testLabel = "simpleScript"
    getOutFields txOut = (DB.txOutAddress txOut, DB.txOutAddressHasScript txOut, DB.txOutValue txOut, DB.txOutDataHash txOut)
    expectedFields = ( renderAddress alwaysSucceedsScriptAddr
                     , True, DB.DbLovelace 20000
                     , Just $ Crypto.hashToBytes (extractHash $ hashData plutusDataList))

unlockScript :: IOManager -> [(Text, Text)] -> Assertion
unlockScript =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      -- We don't use withAlonzoFindLeaderAndSubmitTx here, because we want access to the tx.
      tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000
      void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

      assertBlockNoBackoff dbSync 3
      assertAlonzoCounts dbSync (1,1,1,1,1,1,0,0)
  where
    testLabel = "unlockScriptSameBlock"

unlockScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000 st
        let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
        tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 2
      assertAlonzoCounts dbSync (1,1,1,1,1,1,0,0)

  where
    testLabel = "unlockScriptSameBlock"

failedScript :: IOManager -> [(Text, Text)] -> Assertion
failedScript =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [False] 20000 20000
      void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500

      assertBlockNoBackoff dbSync 2
      assertAlonzoCounts dbSync (0,0,0,0,1,0,1,1)
  where
    testLabel = "failedScript"

failedScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
failedScriptSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ registerAllStakeCreds interpreter mockServer

      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [False] 20000 20000 st
        let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
        tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 2
      assertAlonzoCounts dbSync (0,0,0,0,1,0,1,1)
  where
    testLabel = "failedScriptSameBlock"

multipleScripts :: IOManager -> [(Text, Text)] -> Assertion
multipleScripts =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000
    let utxo = Alonzo.mkUTxOAlonzo tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <- withAlonzoLedgerState interpreter $
        Alonzo.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1,2,1,1,3,2,0,0)
  where
    testLabel = "multipleScripts"

multipleScriptsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000 st
      let utxo =  Alonzo.mkUTxOAlonzo tx0
          pair1 = head utxo
          pair2 = utxo !! 2
      tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1,2,1,1,3,2,0,0)
  where
    testLabel = "multipleScriptsSameBlock"

multipleScriptsFailed :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailed =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

    let utxos = Alonzo.mkUTxOAlonzo tx0
    tx1 <- withAlonzoLedgerState interpreter $
        Alonzo.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0,0,0,0,3,0,1,1)
  where
    testLabel = "multipleScriptsFailed"

multipleScriptsFailedSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailedSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000 st

      let utxos = tail $ Alonzo.mkUTxOAlonzo tx0
      tx1 <- Alonzo.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (0,0,0,0,3,0,1,1)
  where
    testLabel = "multipleScriptsFailedSameBlock"

registrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
registrationScriptTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)]
    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (0,0,0,1)
  where
    testLabel = "registrationScriptTx"

deregistrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationScriptTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (1,0,0,1)
  where
    testLabel = "deregistrationScriptTx"

deregistrationsScriptTxs :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTxs =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg .   RegKey)] st
      tx3 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      pure [tx0, tx1, Alonzo.addValidityInterval 1000 tx2, Alonzo.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2,0,0,1)
    assertAlonzoCounts dbSync (1,2,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTxs"

deregistrationsScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [ (StakeIndexScript True, True, DCertDeleg . DeRegKey)
                                    , (StakeIndexScript True, False, DCertDeleg .  RegKey)
                                    , (StakeIndexScript True, True, DCertDeleg . DeRegKey)
                                    ]
                                    True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2,0,0,1)
    assertAlonzoCounts dbSync (1,2,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTx"

-- Like previous but missing a redeemer. This is a known ledger issue
deregistrationsScriptTx' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx' =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [ (StakeIndexScript True, False, DCertDeleg . DeRegKey)
                               , (StakeIndexScript True, False, DCertDeleg .   RegKey)
                               , (StakeIndexScript True, True, DCertDeleg . DeRegKey)]
                               True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    -- TODO: This is a bug! The first field should be 2. However the deregistrations
    -- are missing the redeemers
    assertScriptCert dbSync (0,0,0,1)
    assertAlonzoCounts dbSync (1,1,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTx'"

-- Like previous but missing the other redeemer. This is a known ledger issue
deregistrationsScriptTx'' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx'' =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [ (StakeIndexScript True, True, DCertDeleg . DeRegKey)
                               , (StakeIndexScript True, False, DCertDeleg .   RegKey)
                               , (StakeIndexScript True, False, DCertDeleg . DeRegKey)]
                               True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2,0,0,1)
    assertAlonzoCounts dbSync (1,1,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTx''"

mintMultiAsset ::  IOManager -> [(Text, Text)] -> Assertion
mintMultiAsset =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \st -> do
        let val0 = Value 1 $ Map.singleton (PolicyID alwaysMintScriptHash) (Map.singleton (head assetNames) 1)
        Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, Value 10000 mempty)] val0 True 100 st

      assertBlockNoBackoff dbSync 1
      assertAlonzoCounts dbSync (1,1,1,1,0,0,0,0)
  where
    testLabel = "mintMultiAsset"

mintMultiAssets ::  IOManager -> [(Text, Text)] -> Assertion
mintMultiAssets =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        let assets0 = Map.fromList [(head assetNames,10), (assetNames !! 1,4)]
        let policy0 = PolicyID alwaysMintScriptHash
        let policy1 = PolicyID alwaysSucceedsScriptHash
        let val1 = Value 1 $ Map.fromList [(policy0, assets0), (policy1, assets0)]
        tx0 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, Value 10000 mempty)] val1 True 100 st
        tx1 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 2] (UTxOIndex 3) [(UTxOAddressNew 0, Value 10000 mempty)] val1 True 200 st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 1
      assertAlonzoCounts dbSync (2,4,1,2,0,0,0,0)
  where
    testLabel = "mintMultiAssets"

swapMultiAssets ::  IOManager -> [(Text, Text)] -> Assertion
swapMultiAssets =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
          let assetsMinted0 = Map.fromList [(head assetNames, 10), (assetNames !! 1, 4)]
          let policy0 = PolicyID alwaysMintScriptHash
          let policy1 = PolicyID alwaysSucceedsScriptHash
          let mintValue0 = Value 100 $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
          let assets0 = Map.fromList [(head assetNames, 5), (assetNames !! 1, 2)]
          let outValue0 = Value 20 $ Map.fromList [(policy0, assets0), (policy1, assets0)]

          tx0 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1)
                  [(UTxOAddress alwaysSucceedsScriptAddr, outValue0), (UTxOAddress alwaysMintScriptAddr, outValue0)] mintValue0 True 100 st

          let utxos = Alonzo.mkUTxOAlonzo tx0
          tx1 <- Alonzo.mkMAssetsScriptTx
            [UTxOPair (head utxos), UTxOPair (utxos !! 1), UTxOIndex 2]
            (UTxOIndex 3)
            [ (UTxOAddress alwaysSucceedsScriptAddr, outValue0), (UTxOAddress alwaysMintScriptAddr, outValue0)
            , (UTxOAddressNew 0, outValue0), (UTxOAddressNew 0, outValue0)]
            mintValue0 True 200 st
          pure [tx0, tx1]

      assertBlockNoBackoff dbSync 1
      assertAlonzoCounts dbSync (2,6,1,2,4,2,0,0)
  where
    testLabel = "swapMultiAssets"

poolReg ::  IOManager -> [(Text, Text)] -> Assertion
poolReg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 1
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
           , PoolIndexNew 0
           , Alonzo.consPoolParamsTwoOwners
           )
          ]

      assertBlockNoBackoff dbSync 2
      assertPoolCounters dbSync (addPoolCounters (1,1,1,2,0,1) initCounter)
      st <- getAlonzoLedgerState interpreter
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st
  where
    testLabel = "poolReg"

-- Issue https://github.com/input-output-hk/cardano-db-sync/issues/997
nonexistantPoolQuery ::  IOManager -> [(Text, Text)] -> Assertion
nonexistantPoolQuery =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 1

      st <- getAlonzoLedgerState interpreter
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Left RecordDoesNotExist, False, False))] st

  where
    testLabel = "nonexistantPoolQuery"

poolDeReg ::  IOManager -> [(Text, Text)] -> Assertion
poolDeReg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 1
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , ([], PoolIndexNew 0, \_ poolId -> DCertPool $ RetirePool poolId 1)
          ]

      assertBlockNoBackoff dbSync 2
      assertPoolCounters dbSync (addPoolCounters (1,1,1,2,1,1) initCounter)

      st <- getAlonzoLedgerState interpreter
      -- Not retired yet, because epoch has not changed
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st

      -- change epoch
      a <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
      -- these counters are the same
      assertPoolCounters dbSync (addPoolCounters (1,1,1,2,1,1) initCounter)

      -- the pool is now retired, since the epoch changed.
      assertPoolLayerCounters dbSync (1,0) [(PoolIndexNew 0, (Right True, False, False))] st

  where
    testLabel = "poolDeReg"

poolDeRegMany :: IOManager -> [(Text, Text)] -> Assertion
poolDeRegMany =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 1
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ -- register
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
            -- de register
          , ([], PoolIndexNew 0, mkPoolDereg 4)

            -- register
          , ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
            -- register with different owner and reward address
          , ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 0]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

      void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
              tx0 <- Alonzo.mkDCertPoolTx
                [ -- register
                 ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 2]
                  , PoolIndexNew 0
                  , Alonzo.consPoolParamsTwoOwners
                  )
                ] st

              tx1 <- Alonzo.mkDCertPoolTx
                [ -- deregister
                  ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 4)

                  -- register
                , ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
                  , PoolIndexNew 0
                  , Alonzo.consPoolParamsTwoOwners
                  )

                   -- deregister
                ,  ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 1)
                ] st
              pure [tx0, tx1]

      assertBlockNoBackoff dbSync 3
      -- TODO fix PoolOwner and PoolRelay unique key
      assertPoolCounters dbSync (addPoolCounters (1,1,5,10,3,5) initCounter)

      st <- getAlonzoLedgerState interpreter
      -- Not retired yet, because epoch has not changed
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st

      -- change epoch
      a <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ length a + 3)
      -- these counters are the same
      assertPoolCounters dbSync (addPoolCounters (1,1,5,10,3,5) initCounter)

      -- from all these certificates only the latest matters. So it will retire
      -- on epoch 0
      assertPoolLayerCounters dbSync (1,0) [(PoolIndexNew 0, (Right True, False, False))] st

  where
    testLabel = "poolDeRegMany"
    mkPoolDereg
        :: EpochNo -> [StakeCredential StandardCrypto] -> KeyHash 'StakePool StandardCrypto
        -> DCert StandardCrypto
    mkPoolDereg epochNo _creds keyHash = DCertPool $ RetirePool keyHash epochNo

poolDelist :: IOManager -> [(Text, Text)] -> Assertion
poolDelist =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 1
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 3
      st <- getAlonzoLedgerState interpreter
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st

      let poolKeyHash = resolvePool (PoolIndexNew 0) st
      let poolId = dbToServantPoolId $ unKeyHashRaw poolKeyHash
      poolLayer <- getPoolLayer dbSync
      void $ dlAddDelistedPool poolLayer poolId

      -- This is not async, so we don't need to do exponential backoff
      -- delisted not retired
      assertPoolLayerCounters dbSync (0,1) [(PoolIndexNew 0, (Right False, True, True))] st

      void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ ([], PoolIndexNew 0, \_ poolHash -> DCertPool $ RetirePool poolHash 1)]

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 5
      -- delisted and pending retirement
      assertPoolLayerCounters dbSync (0,1) [(PoolIndexNew 0, (Right False, True, True))] st

      a <- fillUntilNextEpoch interpreter mockServer

      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + 1)
      -- delisted and retired
      assertPoolLayerCounters dbSync (1,1) [(PoolIndexNew 0, (Right True, True, False))] st
  where
    testLabel = "poolDelist"


hfBlockHash :: CardanoBlock -> ByteString
hfBlockHash blk =
  case blk of
    BlockShelley sblk -> blockHash sblk
    BlockAlonzo ablk -> blockHash ablk
    _ -> error "hfBlockHash: unsupported block type"

throwLeft :: Exception err => IO (Either err a) -> IO a
throwLeft action = do
  ma <- action
  case ma of
    Left err -> throwIO err
    Right a -> pure a
