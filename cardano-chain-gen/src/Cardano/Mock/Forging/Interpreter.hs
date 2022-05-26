{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Interpreter
  ( Interpreter
  , initInterpreter
  , withInterpreter
  , forgeNextFindLeader
  , forgeNext
  , forgeNextAfter
  , getCurrentInterpreterState
  , getCurrentLedgerState
  , getCurrentEpoch
  , getCurrentSlot
  , forgeWithStakeCreds
  , withBabbageLedgerState
  , withAlonzoLedgerState
  , withShelleyLedgerState
  , mkTxId
  ) where

import           Cardano.Prelude (bimap, getField, throwIO)

import           Control.Monad (forM, when)
import           Control.Monad.Except (runExcept)
import           Control.Tracer (Tracer)

import           Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (NS (S, Z))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Word (Word64)

import           GHC.Generics (Generic)

import           Cardano.Ledger.Alonzo.Tx
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import           Cardano.Ledger.Shelley.LedgerState (NewEpochState (..))
import qualified Cardano.Ledger.TxIn as Ledger

import           Cardano.Mock.ChainDB
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import           Cardano.Mock.Forging.Types

import           NoThunks.Class (OnlyCheckWhnfNamed (..))

import           Ouroboros.Consensus.Block (BlockForging, BlockNo (..), BlockProtocol, EpochNo,
                   ForgeStateInfo, ShouldForge (..), SlotNo (..), blockNo, blockSlot,
                   checkShouldForge)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAlonzo, StandardBabbage, StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Config (TopLevelConfig, configConsensus, configLedger,
                   topLevelConfigLedger)
import           Ouroboros.Consensus.Forecast (Forecast (..))
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Mempool as Consensus
import           Ouroboros.Consensus.HeaderValidation (headerStateChainDep)
import           Ouroboros.Consensus.Ledger.Abstract (TickedLedgerState, applyChainTick)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, headerState, ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, Validated,
                   WhetherToIntervene (..), applyTx)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo, pInfoBlockForging,
                   pInfoConfig, pInfoInitLedger)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState, IsLeader, LedgerView,
                   tickChainDepState)
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, shelleyLedgerState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import qualified Ouroboros.Consensus.TypeFamilyWrappers as Consensus
import           Ouroboros.Consensus.Util.IOLike (Exception, NoThunks, StrictMVar, modifyMVar,
                   newMVar, readMVar, swapMVar)
import           Ouroboros.Consensus.Util.Orphans ()

import           System.Directory (doesPathExist)

data Interpreter = Interpreter
  { interpForging :: !(Map Int (BlockForging IO CardanoBlock))
  , interpState :: !(StrictMVar IO InterpreterState)
  , interpTracerForge :: !(Tracer IO (ForgeStateInfo CardanoBlock))
  , interpTopLeverConfig :: !(TopLevelConfig CardanoBlock)
  , interpFingerMode :: !FingerprintMode
  , interpFingerFile :: !FilePath
  }

data InterpreterState = InterpreterState
  { istChain :: !(ChainDB CardanoBlock)
  , istForecast :: !(Forecast (LedgerView (BlockProtocol CardanoBlock)))
  , istSlot :: !SlotNo
  -- ^ The first slot to try the next block
  , istNextBlockNo :: !BlockNo
  -- ^ the block number of the block to be forged
  , istFingerprint :: Fingerprint
  -- ^ newest first list of slots where blocks were succesfully produced.
  } deriving Generic
    deriving NoThunks via OnlyCheckWhnfNamed "InterpreterState" InterpreterState

-- | Vrf and leader election is pseudo-random.
-- Running a simulation multiple times, will always give the same result.
-- So after the first simulation, we can reuse the slots, stored in some file
-- to avoid costly executions.
-- 'SearchSlots' mode starts from an empty list of slots and progressively
-- fills the list. Eventually the list is dumbed to the 'FilePath'.
-- 'ValidateSlots' mode starts from the list of slots and validates that the given
-- leader can indeed forge the next slot every time.
data FingerprintMode
  = SearchSlots !FilePath
  | ValidateSlots

newtype Fingerprint
  = Fingerprint [Word64]
  deriving (Generic, FromJSON, ToJSON)

deriving instance Generic (ChainDB CardanoBlock)

deriving instance NoThunks (Forecast a)

deriving instance Generic (Forecast a)

mkFingerprint :: FilePath -> IO (FingerprintMode, Fingerprint)
mkFingerprint path = do
  fileExists <- doesPathExist path
  if fileExists
    then do
      mfingerPrint <- eitherDecodeFileStrict path
      (ValidateSlots,) <$> either (throwIO . FingerprintDecodeError) pure mfingerPrint
    else
      pure (SearchSlots path, emptyFingerprint)

isSearchingMode :: FingerprintMode -> Bool
isSearchingMode fm =
  case fm of
    SearchSlots {} -> True
    _ -> False

-- | Given the current slot, return the slot to test and the next 'Fingerprint'
getFingerTipSlot :: Interpreter -> Fingerprint -> SlotNo -> Either ForgingError SlotNo
getFingerTipSlot interpreter fingerprint currentSlotNo =
  case interpFingerMode interpreter of
    SearchSlots {} -> Right currentSlotNo
    ValidateSlots ->
      case fst <$> unconsFingerprint fingerprint of
        Nothing -> Left $ EmptyFingerprint currentSlotNo (interpFingerFile interpreter)
        Just slotNo -> Right slotNo

addSlot :: Fingerprint -> SlotNo -> Fingerprint
addSlot (Fingerprint slots) slot = Fingerprint (unSlotNo slot : slots)

unconsFingerprint :: Fingerprint -> Maybe (SlotNo, Fingerprint)
unconsFingerprint (Fingerprint slots) = bimap SlotNo Fingerprint <$> List.uncons slots

lengthSlots :: Fingerprint -> Int
lengthSlots (Fingerprint slots) = length slots

emptyFingerprint :: Fingerprint
emptyFingerprint = Fingerprint []

reverseFingerprint :: Fingerprint -> Fingerprint
reverseFingerprint (Fingerprint slots) = Fingerprint $ reverse slots

finalizeFingerprint :: Interpreter -> IO ()
finalizeFingerprint inter = do
  interState <- getCurrentInterpreterState inter
  case interpFingerMode inter of
    SearchSlots fp -> encodeFile fp $ reverseFingerprint (istFingerprint interState)
    ValidateSlots -> pure ()

initInterpreter
    :: ProtocolInfo IO CardanoBlock -> Tracer IO (ForgeStateInfo CardanoBlock) -> FilePath
    -> IO Interpreter
initInterpreter pinfo traceForge fingerprintFile = do
  forging <- pInfoBlockForging pinfo
  let topLeverCfg = pInfoConfig pinfo
  let initSt = pInfoInitLedger pinfo
  let ledgerView = mkForecast topLeverCfg initSt
  (mode, fingerprint) <- mkFingerprint fingerprintFile
  stvar <- newMVar $
            InterpreterState
              { istChain = initChainDB topLeverCfg initSt
              , istForecast = ledgerView
              , istSlot = SlotNo 0
              -- The first real Byron block (ie block that can contain txs) is number 1.
              , istNextBlockNo = BlockNo 1
              , istFingerprint = fingerprint
              }
  pure $ Interpreter
          { interpForging = Map.fromList $ zip [0..] forging
          , interpState = stvar
          , interpTracerForge = traceForge
          , interpTopLeverConfig = topLeverCfg
          , interpFingerMode = mode
          , interpFingerFile = fingerprintFile
          }

withInterpreter
    :: ProtocolInfo IO CardanoBlock -> Tracer IO (ForgeStateInfo CardanoBlock) -> FilePath
    -> (Interpreter -> IO a)
    -> IO a
withInterpreter p t f action = do
  interp <- initInterpreter p t f
  a <- action interp
  finalizeFingerprint interp
  pure a

addOrValidateSlot
    :: Interpreter -> Fingerprint -> CardanoBlock
    -> Either ForgingError Fingerprint
addOrValidateSlot interp fingerprint blk =
  case interpFingerMode interp of
    SearchSlots _ -> Right $ addSlot fingerprint (blockSlot blk)
    ValidateSlots ->
      case unconsFingerprint fingerprint of
        Nothing -> Left $ EmptyFingerprint (blockSlot blk) (interpFingerFile interp)
        Just (slotNo, fingerPrint') ->
          if slotNo == blockSlot blk
            then Right fingerPrint'
            else
              -- The validation here is unecessary, since we have used the slot to
              -- forge the block. But we do it nontheless as a sanity check.
              Left $ NotExpectedSlotNo (blockSlot blk) slotNo (lengthSlots fingerPrint')

forgeWithStakeCreds :: Interpreter -> IO CardanoBlock
forgeWithStakeCreds inter = do
  st <- getCurrentLedgerState inter
  tx <- case ledgerState st of
          LedgerStateShelley sts -> either throwIO (pure . TxShelley) $ Shelley.mkDCertTxPools sts
          LedgerStateAlonzo sta -> either throwIO (pure . TxAlonzo) $ Alonzo.mkDCertTxPools sta
          LedgerStateBabbage stb -> either throwIO (pure . TxBabbage) $ Babbage.mkDCertTxPools stb
          _ -> throwIO UnexpectedEra
  forgeNextFindLeader inter [tx]

forgeNextAfter :: Interpreter -> Word64 -> [TxEra] -> IO CardanoBlock
forgeNextAfter interpreter skipSlots txes = do
    modifyMVar (interpState interpreter) $ \st ->
      pure (st {istSlot = istSlot st + SlotNo skipSlots}, ())
    forgeNextFindLeader interpreter txes

forgeNextFindLeader :: Interpreter -> [TxEra] -> IO CardanoBlock
forgeNextFindLeader interpreter txes =
    forgeNextLeaders interpreter txes $ Map.elems (interpForging interpreter)

forgeNext :: Interpreter -> MockBlock -> IO CardanoBlock
forgeNext interpreter testBlock =
  case Map.lookup (unNodeId $ node testBlock) (interpForging interpreter) of
    Nothing -> throwIO $ NonExistantNode (node testBlock)
    Just forging -> forgeNextLeaders interpreter (txs testBlock) [forging]

forgeNextLeaders :: Interpreter -> [TxEra] -> [BlockForging IO CardanoBlock] -> IO CardanoBlock
forgeNextLeaders interpreter txes possibleLeaders = do
        interState <- getCurrentInterpreterState interpreter
        (blk, fingerprint) <- tryOrValidateSlot interState possibleLeaders
        let !chain' = extendChainDB (istChain interState) blk
        let !newSt = currentState chain'
        let newInterState =
              InterpreterState
                { istChain = chain'
                , istForecast = mkForecast cfg newSt
                , istSlot = blockSlot blk + 1
                , istNextBlockNo = blockNo blk + 1
                , istFingerprint = fingerprint
                }
        _ <- swapMVar (interpState interpreter) newInterState
        pure blk
  where
    cfg :: TopLevelConfig CardanoBlock
    cfg = interpTopLeverConfig interpreter

    tryOrValidateSlot
        :: InterpreterState -> [BlockForging IO CardanoBlock] -> IO (CardanoBlock, Fingerprint)
    tryOrValidateSlot interState blockForgings = do
      currentSlot <- throwLeftIO $
                        getFingerTipSlot interpreter (istFingerprint interState) (istSlot interState)
      trySlots interState blockForgings 0 currentSlot (isSearchingMode $ interpFingerMode interpreter)

    trySlots
        :: InterpreterState -> [BlockForging IO CardanoBlock] -> Int -> SlotNo -> Bool
        -> IO (CardanoBlock, Fingerprint)
    trySlots interState blockForgings numberOfTries currentSlot searching = do
      when (numberOfTries > 140) (throwIO WentTooFar)
      mproof <- tryAllForging interpreter interState currentSlot blockForgings
      case mproof of
        Nothing ->
          if searching
            then trySlots interState blockForgings (numberOfTries + 1) (currentSlot + 1) searching
            else throwIO $ FailedToValidateSlot currentSlot (lengthSlots $ istFingerprint interState) (interpFingerFile interpreter)
        Just (proof, blockForging) -> do
          -- Tick the ledger state for the 'SlotNo' we're producing a block for
          let tickedLedgerSt :: Ticked (LedgerState CardanoBlock)
              !tickedLedgerSt = applyChainTick (configLedger cfg) currentSlot
                                    (ledgerState . currentState $ istChain interState)
          !blk <- Block.forgeBlock blockForging cfg (istNextBlockNo interState) currentSlot
                    tickedLedgerSt (mkValidated <$> txes) proof

          (blk,) <$> throwLeftIO (addOrValidateSlot interpreter (istFingerprint interState) blk)

    -- We will probably not use it and wait for ledger to provide a way to construct
    -- Validated Tx in an unsafe way.
    _applyTxs
        :: [Consensus.GenTx CardanoBlock] -> SlotNo -> TickedLedgerState CardanoBlock
        -> Either (ApplyTxErr CardanoBlock) [Validated (GenTx CardanoBlock)]
    _applyTxs genTxs slotNo st =
      runExcept .
        forM genTxs $ \ tx ->
          snd <$> applyTx (topLevelConfigLedger cfg) Intervene slotNo tx st

tryAllForging
    :: Interpreter -> InterpreterState -> SlotNo -> [BlockForging IO CardanoBlock]
    -> IO (Maybe (IsLeader (BlockProtocol CardanoBlock), BlockForging IO CardanoBlock))
tryAllForging interpreter interState currentSlot xs = do
  case xs of
    [] -> throwIO $ userError "tryAllForging: found empty forgers"
    (forging : rest) -> do
      let cfg = interpTopLeverConfig interpreter

      -- We require the ticked ledger view in order to construct the ticked 'ChainDepState'.
      ledgerView <- case runExcept (forecastFor (istForecast interState) currentSlot) of
                      Right lv -> pure (lv :: Ticked (LedgerView (BlockProtocol CardanoBlock)))
                      -- Left can only happen if we cross an epoch boundary
                      Left err -> throwIO $ ForecastError currentSlot err

      -- Tick the 'ChainDepState' for the 'SlotNo' we're producing a block for. We only need the
      -- ticked 'ChainDepState' to check the whether we're a leader. This is much cheaper than
      -- ticking the entire 'ExtLedgerState'.
      let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol CardanoBlock))
          !tickedChainDepState =
              tickChainDepState (configConsensus cfg) ledgerView currentSlot
                  (headerStateChainDep (headerState $ currentState $ istChain interState))

      !shouldForge <- checkShouldForge forging (interpTracerForge interpreter) cfg
                        currentSlot tickedChainDepState
      case shouldForge of
        ShouldForge proof -> pure $ Just (proof, forging)
        _ | not (null rest) -> tryAllForging interpreter interState currentSlot rest
        ForgeStateUpdateError err -> do
          Text.putStrLn $ Text.unwords
            ["TraceForgeStateUpdateError", textShow currentSlot, textShow err]
          pure Nothing
        CannotForge cannotForge -> do
          Text.putStrLn $ Text.unwords
            ["TraceNodeCannotForge", textShow currentSlot, textShow cannotForge]
          pure Nothing
        NotLeader -> do
          pure Nothing

getCurrentInterpreterState :: Interpreter -> IO  InterpreterState
getCurrentInterpreterState = readMVar . interpState

getCurrentLedgerState :: Interpreter -> IO (ExtLedgerState CardanoBlock)
getCurrentLedgerState = fmap (currentState . istChain) . getCurrentInterpreterState

getCurrentEpoch :: Interpreter -> IO EpochNo
getCurrentEpoch inter = do
    est <- getCurrentLedgerState inter
    case ledgerState est of
      LedgerStateShelley st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateAllegra st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateMary st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateAlonzo st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateBabbage st -> pure $ nesEL $ shelleyLedgerState st
      _ -> throwIO UnexpectedEra

getCurrentSlot :: Interpreter -> IO SlotNo
getCurrentSlot interp = istSlot <$> readMVar (interpState interp)

withBabbageLedgerState
    :: Interpreter -> (LedgerState (ShelleyBlock PraosStandard StandardBabbage) -> Either ForgingError a)
    -> IO a
withBabbageLedgerState inter mk = do
  st <- getCurrentLedgerState inter
  case ledgerState st of
    LedgerStateBabbage sta -> case mk sta of
      Right a -> pure a
      Left err -> throwIO err
    _ -> throwIO ExpectedBabbageState

withAlonzoLedgerState
    :: Interpreter -> (LedgerState (ShelleyBlock TPraosStandard StandardAlonzo) -> Either ForgingError a)
    -> IO a
withAlonzoLedgerState inter mk = do
  st <- getCurrentLedgerState inter
  case ledgerState st of
    LedgerStateAlonzo sta -> case mk sta of
      Right a -> pure a
      Left err -> throwIO err
    _ -> throwIO ExpectedAlonzoState

withShelleyLedgerState
    :: Interpreter -> (LedgerState (ShelleyBlock TPraosStandard StandardShelley) -> Either ForgingError a)
    -> IO a
withShelleyLedgerState inter mk = do
  st <- getCurrentLedgerState inter
  case ledgerState st of
    LedgerStateShelley sts -> case mk sts of
      Right a -> pure a
      Left err -> throwIO err
    _ -> throwIO ExpectedShelleyState

mkTxId :: TxEra -> Ledger.TxId StandardCrypto
mkTxId txe =
  case txe of
    TxAlonzo tx -> Ledger.txid @StandardAlonzo (getField @"body" tx)
    TxBabbage tx -> Ledger.txid @StandardBabbage (getField @"body" tx)
    TxShelley tx -> Ledger.txid @StandardShelley (getField @"body" tx)

mkValidated :: TxEra -> Validated (Consensus.GenTx CardanoBlock)
mkValidated txe =
  case txe of
    TxAlonzo tx ->
      Consensus.HardForkValidatedGenTx
        (Consensus.OneEraValidatedGenTx
          (S (S (S (S (Z (Consensus.WrapValidatedGenTx
            (Consensus.mkShelleyValidatedTx $ Ledger.unsafeMakeValidated tx)
        )))))))
    TxShelley tx ->
      Consensus.HardForkValidatedGenTx
        (Consensus.OneEraValidatedGenTx
          (S (Z (Consensus.WrapValidatedGenTx
            (Consensus.mkShelleyValidatedTx $ Ledger.unsafeMakeValidated tx)
        ))))
    TxBabbage tx ->
      Consensus.HardForkValidatedGenTx
        (Consensus.OneEraValidatedGenTx
          (S (S (S (S (S (Z (Consensus.WrapValidatedGenTx
            (Consensus.mkShelleyValidatedTx $ Ledger.unsafeMakeValidated tx)
        ))))))))

mkForecast
    :: TopLevelConfig CardanoBlock -> ExtLedgerState CardanoBlock
    -> Forecast (LedgerView (BlockProtocol CardanoBlock))
mkForecast cfg st = ledgerViewForecastAt (configLedger cfg) (ledgerState st)

textShow :: Show a => a -> Text
textShow = Text.pack . show

throwLeftIO :: Exception e => Either e a -> IO a
throwLeftIO = either throwIO pure
