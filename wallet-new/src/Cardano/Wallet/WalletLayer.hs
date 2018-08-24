{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Errors
    , CreateWalletError(..)
    , GetWalletError(..)
    , UpdateWalletError(..)
    , UpdateWalletPasswordError(..)
    , DeleteWalletError(..)
    , NewPaymentError(..)
    , EstimateFeesError(..)
    , RedeemAdaError(..)
    , CreateAddressError(..)
    , ValidateAddressError(..)
    , CreateAccountError(..)
    , GetAccountError(..)
    , GetAccountsError(..)
    , GetTxError(..)
    , DeleteAccountError(..)
    , UpdateAccountError(..)
    ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import           Generics.SOP.TH (deriveGeneric)
import qualified Prelude
import           Servant (err400, err404)
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Pos.Chain.Block (Blund)
import           Pos.Core (Coin, Timestamp)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Txp (Tx, TxId)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.Response (SliceOf (..), WalletResponse)
import           Cardano.Wallet.API.Response.JSend (HasDiagnostic (..))
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus,
                     ToServantError (..))
import           Cardano.Wallet.API.V1.Generic (jsendErrorGenericParseJSON,
                     jsendErrorGenericToJSON)
import           Cardano.Wallet.API.V1.Types (Account, AccountBalance,
                     AccountIndex, AccountUpdate, Address, NewAccount,
                     NewAddress, NewWallet, NodeInfo, NodeSettings,
                     PasswordUpdate, Payment, Redemption, Transaction, V1 (..),
                     Wallet, WalletAddress, WalletId, WalletUpdate)
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation, InputGrouping)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as Kernel
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (TimeExecutionLimit)

------------------------------------------------------------
-- Errors when manipulating wallets
------------------------------------------------------------

data CreateWalletError =
      CreateWalletError Kernel.CreateWalletError
    | CreateWalletFirstAccountCreationFailed Kernel.CreateAccountError
    deriving (Generic, Eq)

deriveGeneric ''CreateWalletError

instance HasDiagnostic CreateWalletError where
    getDiagnosticKey = \case
        CreateWalletError -> error "TODO"
        CreateWalletFirstAccountCreationFailed -> error "TODO"

instance ToServantError CreateWalletError where
    declareServantError = \case
        CreateWalletError -> error "TODO"
        CreateWalletFirstAccountCreationFailed -> error "TODO"

instance ToHttpErrorStatus CreateWalletError

instance ToJSON CreateWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON CreateWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Arbitrary CreateWalletError where
    arbitrary = oneof [ CreateWalletError <$> arbitrary
                      , CreateWalletFirstAccountCreationFailed <$> arbitrary
                      ]

instance Buildable CreateWalletError where
    build (CreateWalletError kernelError) =
        bprint ("CreateWalletError " % build) kernelError
    build (CreateWalletFirstAccountCreationFailed kernelError) =
        bprint ("CreateWalletFirstAccountCreationFailed " % build) kernelError

data GetWalletError =
      GetWalletError (V1 Kernel.UnknownHdRoot)
    | GetWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | GetWalletWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''GetWalletError

instance HasDiagnostic GetWalletError where
    getDiagnosticKey = \case
        GetWalletError -> error "TODO"
        GetWalletErrorNotFound -> error "TODO"
        GetWalletWalletIdDecodingFailed -> error "TODO"

instance ToServantError GetWalletError where
    declareServantError = \case
        GetWalletError -> error "TODO"
        GetWalletErrorNotFound -> error "TODO"
        GetWalletWalletIdDecodingFailed -> error "TODO"

instance ToHttpErrorStatus GetWalletError

instance ToJSON GetWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetWalletError where
    build (GetWalletError (V1 kernelError)) =
        bprint ("GetWalletError " % build) kernelError
    build (GetWalletErrorNotFound walletId) =
        bprint ("GetWalletErrorNotFound " % build) walletId
    build (GetWalletWalletIdDecodingFailed txt) =
        bprint ("GetWalletWalletIdDecodingFailed " % build) txt

data UpdateWalletError =
      UpdateWalletError (V1 Kernel.UnknownHdRoot)
    | UpdateWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | UpdateWalletWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''UpdateWalletError

instance HasDiagnostic UpdateWalletError where
    getDiagnosticKey = \case
        UpdateWalletError -> error "TODO"
        UpdateWalletErrorNotFound -> error "TODO"
        UpdateWalletWalletIdDecodingFailed -> error "TODO"

instance ToServantError UpdateWalletError where
    declareServantError = \case
        UpdateWalletError -> error "TODO"
        UpdateWalletErrorNotFound -> error "TODO"
        UpdateWalletWalletIdDecodingFailed -> error "TODO"

instance ToHttpErrorStatus UpdateWalletError

instance ToJSON UpdateWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON UpdateWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable UpdateWalletError where
    build (UpdateWalletError (V1 kernelError)) =
        bprint ("UpdateWalletError " % build) kernelError
    build (UpdateWalletErrorNotFound walletId) =
        bprint ("UpdateWalletErrorNotFound " % build) walletId
    build (UpdateWalletWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletWalletIdDecodingFailed " % build) txt

data UpdateWalletPasswordError =
      UpdateWalletPasswordWalletIdDecodingFailed Text
    | UpdateWalletPasswordError Kernel.UpdateWalletPasswordError
    deriving (Generic, Eq)

deriveGeneric ''UpdateWalletPasswordError

instance HasDiagnostic UpdateWalletPasswordError where
    getDiagnosticKey = \case
        UpdateWalletPasswordWalletIdDecodingFailed -> error "TODO"
        UpdateWalletPasswordError -> error "TODO"

instance ToServantError UpdateWalletPasswordError where
    declareServantError = \case
        UpdateWalletPasswordWalletIdDecodingFailed -> error "TODO"
        UpdateWalletPasswordError -> error "TODO"

instance ToHttpErrorStatus UpdateWalletPasswordError

instance ToJSON UpdateWalletPasswordError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON UpdateWalletPasswordError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletPasswordWalletIdDecodingFailed " % build) txt
    build (UpdateWalletPasswordError kernelError) =
        bprint ("UpdateWalletPasswordError " % build) kernelError

data DeleteWalletError =
      DeleteWalletWalletIdDecodingFailed Text
    | DeleteWalletError (V1 Kernel.UnknownHdRoot)
    deriving (Generic, Eq)

deriveGeneric ''DeleteWalletError

instance HasDiagnostic DeleteWalletError where
    getDiagnosticKey = \case
        DeleteWalletWalletIdDecodingFailed _ -> error "TODO"
        DeleteWalletError _ -> error "TODO"

instance ToServantError DeleteWalletError where
    declareServantError = \case
        DeleteWalletWalletIdDecodingFailed _ -> error "TODO"
        DeleteWalletError _ -> error "TODO"

instance ToHttpErrorStatus DeleteWalletError

instance ToJSON DeleteWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON DeleteWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable DeleteWalletError where
    build (DeleteWalletWalletIdDecodingFailed txt) =
        bprint ("DeleteWalletWalletIdDecodingFailed " % build) txt
    build (DeleteWalletError kernelError) =
        bprint ("DeleteWalletError " % build) kernelError

------------------------------------------------------------
-- Errors when dealing with addresses
------------------------------------------------------------

data CreateAddressError =
      CreateAddressError Kernel.CreateAddressError
    | CreateAddressAddressDecodingFailed Text
    -- ^ Decoding the input 'Text' as an 'Address' failed.
    deriving (Generic, Eq)

deriveGeneric ''CreateAddressError

instance HasDiagnostic CreateAddressError where
    getDiagnosticKey = \case
        CreateAddressError _ -> error "TODO"
        CreateAddressAddressDecodingFailed _ -> error "TODO"

instance ToServantError CreateAddressError where
    declareServantError = \case
        CreateAddressError _ -> error "TODO"
        CreateAddressAddressDecodingFailed _ -> error "TODO"

instance ToHttpErrorStatus CreateAddressError

instance ToJSON CreateAddressError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON CreateAddressError where
    parseJSON = jsendErrorGenericParseJSON

instance Arbitrary CreateAddressError where
    arbitrary = oneof [ CreateAddressError <$> arbitrary
                      , pure (CreateAddressAddressDecodingFailed "Ae2tdPwUPEZ18ZjTLnLVr9CEvUEUX4eW1LBHbxxx")
                      ]

instance Buildable CreateAddressError where
    build (CreateAddressError kernelError) =
        bprint ("CreateAddressError " % build) kernelError
    build (CreateAddressAddressDecodingFailed txt) =
        bprint ("CreateAddressAddressDecodingFailed " % build) txt

data ValidateAddressError =
      ValidateAddressDecodingFailed Text
    -- ^ When trying to decode this raw 'Text' into a proper Cardano
    -- 'Address' the decoding failed. Unfortunately we are not able to
    -- provide a more accurate error description as 'decodeTextAddress' doesn't
    -- offer such.
    deriving (Generic, Eq)

deriveGeneric ''ValidateAddressError

instance HasDiagnostic ValidateAddressError where
    getDiagnosticKey = \case
        ValidateAddressDecodingFailed _ -> error "TODO"

instance ToServantError ValidateAddressError where
    declareServantError = \case _ -> error "TODO"

instance ToHttpErrorStatus ValidateAddressError

instance ToJSON ValidateAddressError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON ValidateAddressError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable ValidateAddressError where
    build (ValidateAddressDecodingFailed rawText) =
        bprint ("ValidateAddressDecodingFailed " % build) rawText

------------------------------------------------------------
-- Errors when dealing with Accounts
------------------------------------------------------------

data CreateAccountError =
      CreateAccountError Kernel.CreateAccountError
    | CreateAccountWalletIdDecodingFailed Text
    -- ^ Decoding the parent's 'WalletId' from a raw 'Text' failed.
    | CreateAccountFirstAddressGenerationFailed Kernel.CreateAddressError
    -- ^ When trying to create the first 'Address' to go in tandem with this
    -- 'Account', the generation failed.
    deriving (Generic, Eq)

deriveGeneric ''CreateAccountError

instance HasDiagnostic CreateAccountError where
    getDiagnosticKey = \case
        CreateAccountError _ -> error "TODO"
        CreateAccountWalletIdDecodingFailed _ -> error "TODO"
        CreateAccountFirstAddressGenerationFailed _ -> error "TODO"

instance ToServantError CreateAccountError where
    declareServantError = \case
        CreateAccountError _ -> error "TODO"
        CreateAccountWalletIdDecodingFailed _ -> error "TODO"
        CreateAccountFirstAddressGenerationFailed _ -> error "TODO"

instance ToHttpErrorStatus CreateAccountError

instance ToJSON CreateAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON CreateAccountError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable CreateAccountError where
    build (CreateAccountError kernelError) =
        bprint ("CreateAccountError " % build) kernelError
    build (CreateAccountWalletIdDecodingFailed txt) =
        bprint ("CreateAccountWalletIdDecodingFailed " % build) txt
    build (CreateAccountFirstAddressGenerationFailed kernelError) =
        bprint ("CreateAccountFirstAddressGenerationFailed " % build) kernelError

data GetAccountError =
      GetAccountError (V1 Kernel.UnknownHdAccount)
    | GetAccountWalletIdDecodingFailed Text
    deriving (Generic, Eq)



deriveGeneric ''GetAccountError

instance HasDiagnostic GetAccountError where
    getDiagnosticKey = \case
        GetAccountError _ -> error "TODO"
        GetAccountWalletIdDecodingFailed _ -> error "TODO"

instance ToServantError GetAccountError where
    declareServantError = \case
        GetAccountError _ -> error "TODO"
        GetAccountWalletIdDecodingFailed _ -> error "TODO"

instance ToHttpErrorStatus GetAccountError

instance ToJSON GetAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetAccountError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetAccountError where
    build (GetAccountError kernelError) =
        bprint ("GetAccountError " % build) kernelError
    build (GetAccountWalletIdDecodingFailed txt) =
        bprint ("GetAccountWalletIdDecodingFailed " % build) txt

data DeleteAccountError =
      DeleteAccountError (V1 Kernel.UnknownHdAccount)
    | DeleteAccountWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''DeleteAccountError

instance Buildable DeleteAccountError where
    build (DeleteAccountError kernelError) =
        bprint ("DeleteAccountError " % build) kernelError
    build (DeleteAccountWalletIdDecodingFailed txt) =
        bprint ("DeleteAccountWalletIdDecodingFailed " % build) txt

instance HasDiagnostic DeleteAccountError where
    getDiagnosticKey = \case
        DeleteAccountError _ -> "unknownAccount"
        DeleteAccountWalletIdDecodingFailed _ -> "message"

instance ToServantError DeleteAccountError where
    declareServantError = \case
        DeleteAccountError _ -> err404
        DeleteAccountWalletIdDecodingFailed _ -> err400

instance ToHttpErrorStatus DeleteAccountError

instance ToJSON DeleteAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON DeleteAccountError where
    parseJSON = jsendErrorGenericParseJSON

data GetAccountsError =
      GetAccountsError Kernel.UnknownHdRoot
    | GetAccountsWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''GetAccountsError

instance HasDiagnostic GetAccountsError where
    getDiagnosticKey = \case
        GetAccountsError _                  -> "TODO"
        GetAccountsWalletIdDecodingFailed _ -> "TODO"

instance ToServantError GetAccountsError where
    declareServantError = \case
        GetAccountsError _ -> err404
        GetAccountsWalletIdDecodingFailed _ -> err400

instance ToHttpErrorStatus GetAccountsError

instance ToJSON GetAccountsError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetAccountsError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetAccountsError where
    build (GetAccountsError kernelError) =
        bprint ("GetAccountsError " % build) kernelError
    build (GetAccountsWalletIdDecodingFailed txt) =
        bprint ("GetAccountsWalletIdDecodingFailed " % build) txt

data UpdateAccountError =
      UpdateAccountError (V1 Kernel.UnknownHdAccount)
    | UpdateAccountWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''UpdateAccountError

instance HasDiagnostic UpdateAccountError where
    getDiagnosticKey = \case
        UpdateAccountError _ -> "unknownAccount"
        UpdateAccountWalletIdDecodingFailed _ -> "message"

instance ToServantError UpdateAccountError where
    declareServantError = \case
        UpdateAccountError _ -> err404
        UpdateAccountWalletIdDecodingFailed _ -> err400

instance ToHttpErrorStatus UpdateAccountError

instance ToJSON UpdateAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON UpdateAccountError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable UpdateAccountError where
    build (UpdateAccountError kernelError) =
        bprint ("UpdateAccountError " % build) kernelError
    build (UpdateAccountWalletIdDecodingFailed txt) =
        bprint ("UpdateAccountWalletIdDecodingFailed " % build) txt

------------------------------------------------------------
-- Errors when getting Transactions
------------------------------------------------------------

data GetTxError =
      GetTxMissingWalletIdError
    | GetTxAddressDecodingFailed Text
    | GetTxInvalidSortingOperaration String
    | GetTxUnknownHdAccount Kernel.UnknownHdAccount
    deriving (Generic, Eq)

deriveGeneric ''UpdateAccountError

instance HasDiagnostic GetTxError where
    getDiagnosticKey = \case
        GetTxMissingWalletIdError _ -> error "TODO"
        GetTxAddressDecodingFailed _ -> error "TODO"
        GetTxInvalidSortingOperaration _ -> error "TODO"
        GetTxUnknownHdAccount _ -> error "TODO"

instance ToServantError GetTxError where
    declareServantError = \case
        GetTxMissingWalletIdError _ -> error "TODO"
        GetTxAddressDecodingFailed _ -> error "TODO"
        GetTxInvalidSortingOperaration _ -> error "TODO"
        GetTxUnknownHdAccount _ -> error "TODO"

instance ToHttpErrorStatus GetTxError

instance ToJSON GetTxError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetTxError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetTxError where
    build GetTxMissingWalletIdError =
        bprint "GetTxMissingWalletIdError "
    build (GetTxAddressDecodingFailed txt) =
        bprint ("GetTxAddressDecodingFailed " % build) txt
    build (GetTxInvalidSortingOperaration txt) =
        bprint ("GetTxInvalidSortingOperaration " % build) txt
    build (GetTxUnknownHdAccount err) =
        bprint ("GetTxUnknownHdAccount " % build) err


instance Arbitrary GetTxError where
    arbitrary = oneof [ pure GetTxMissingWalletIdError
                      , pure (GetTxAddressDecodingFailed "by_amount")
                      , pure (GetTxInvalidSortingOperaration "123")
                      , GetTxUnknownHdAccount <$> arbitrary
                      ]

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- wallets
      createWallet         :: NewWallet -> m (Either CreateWalletError Wallet)
    , getWallets           :: m (IxSet Wallet)
    , getWallet            :: WalletId -> m (Either GetWalletError Wallet)
    , updateWallet         :: WalletId
                           -> WalletUpdate
                           -> m (Either UpdateWalletError Wallet)
    , updateWalletPassword :: WalletId
                           -> PasswordUpdate
                           -> m (Either UpdateWalletPasswordError Wallet)
    , deleteWallet         :: WalletId -> m (Either DeleteWalletError ())
    -- accounts
    , createAccount        :: WalletId
                           -> NewAccount
                           -> m (Either CreateAccountError Account)
    , getAccounts          :: WalletId
                           -> m (Either GetAccountsError (IxSet Account))
    , getAccount           :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError Account)
    , getAccountBalance    :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError AccountBalance)
    , getAccountAddresses  :: WalletId
                           -> AccountIndex
                           -> RequestParams
                           -> FilterOperations '[V1 Address] WalletAddress
                           -> m (Either GetAccountError (WalletResponse [WalletAddress]))
    , updateAccount        :: WalletId
                           -> AccountIndex
                           -> AccountUpdate
                           -> m (Either UpdateAccountError Account)
    , deleteAccount        :: WalletId
                           -> AccountIndex
                           -> m (Either DeleteAccountError ())
    -- addresses
    , createAddress        :: NewAddress
                           -> m (Either CreateAddressError WalletAddress)
    , getAddresses         :: RequestParams -> m (SliceOf WalletAddress)
    , validateAddress      :: Text
                           -> m (Either ValidateAddressError WalletAddress)

    -- transactions
    , getTransactions      :: Maybe WalletId
                           -> Maybe AccountIndex
                           -> Maybe (V1 Address)
                           -> RequestParams
                           -> FilterOperations '[V1 TxId, V1 Timestamp] Transaction
                           -> SortOperations Transaction
                           -> m (Either GetTxError (WalletResponse [Transaction]))
    , getTxFromMeta        :: TxMeta -> m (Either Kernel.UnknownHdAccount Transaction)

    -- core API
    , applyBlocks          :: OldestFirst NE Blund -> m ()
    , rollbackBlocks       :: NewestFirst NE Blund -> m ()

    -- node settings
    , getNodeSettings      :: m NodeSettings
    }

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | Performs a payment.
    , pay :: PassPhrase
          -- The \"spending password\" to decrypt the 'EncryptedSecretKey'.
          -> InputGrouping
          -- An preference on how to group inputs during coin selection.
          -> ExpenseRegulation
          -- Who pays the fee, if the sender or the receivers.
          -> Payment
          -- The payment we need to perform.
          -> m (Either NewPaymentError (Tx, TxMeta))

      -- | Estimates the fees for a payment.
    , estimateFees :: PassPhrase
                   -- The \"spending password\" to decrypt the 'EncryptedSecretKey'.
                   -> InputGrouping
                   -- An preference on how to group inputs during coin selection
                   -> ExpenseRegulation
                   -- Who pays the fee, if the sender or the receivers.
                   -> Payment
                   -- The payment we need to perform.
                   -> m (Either EstimateFeesError Coin)

      -- | Redeem ada
    , redeemAda :: Redemption -> m (Either RedeemAdaError Tx)

      -- | Node info
      --
      -- This lives in the active wallet layer as the node info endpoint returns
      -- status information about the diffusion layer
    , getNodeInfo :: m NodeInfo
    }

------------------------------------------------------------
-- Active wallet errors
------------------------------------------------------------

data NewPaymentError =
      NewPaymentError Kernel.PaymentError
    | NewPaymentTimeLimitReached TimeExecutionLimit
    | NewPaymentWalletIdDecodingFailed Text
    | NewPaymentUnknownAccountId Kernel.UnknownHdAccount
    deriving (Generic, Eq)

deriveGeneric ''NewPaymentError

instance HasDiagnostic NewPaymentError where
    getDiagnosticKey = \case
        NewPaymentError _ -> error "TODO"
        NewPaymentTimeLimitReached _ -> error "TODO"
        NewPaymentWalletIdDecodingFailed _ -> error "TODO"
        NewPaymentUnknownAccountId _ -> error "TODO"

instance ToServantError NewPaymentError where
    declareServantError = \case
        NewPaymentError _ -> error "TODO"
        NewPaymentTimeLimitReached _ -> error "TODO"
        NewPaymentWalletIdDecodingFailed _ -> error "TODO"
        NewPaymentUnknownAccountId _ -> error "TODO"

instance ToHttpErrorStatus NewPaymentError

instance ToJSON NewPaymentError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON NewPaymentError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable NewPaymentError where
    build (NewPaymentError kernelErr) =
        bprint ("NewPaymentError " % build) kernelErr
    build (NewPaymentTimeLimitReached ter) =
        bprint ("NewPaymentTimeLimitReached " % build) ter
    build (NewPaymentWalletIdDecodingFailed txt) =
        bprint ("NewPaymentWalletIdDecodingFailed " % build) txt
    build (NewPaymentUnknownAccountId err) =
        bprint ("NewPaymentUnknownAccountId " % build) err


data EstimateFeesError =
      EstimateFeesError Kernel.EstimateFeesError
    | EstimateFeesTimeLimitReached TimeExecutionLimit
    | EstimateFeesWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''EstimateFeesError

instance HasDiagnostic EstimateFeesError where
    getDiagnosticKey = \case
        EstimateFeesError _ -> error "TODO"
        EstimateFeesTimeLimitReached _ -> error "TODO"
        EstimateFeesWalletIdDecodingFailed _ -> error "TODO"

instance ToServantError EstimateFeesError where
    declareServantError = \case
        EstimateFeesError _ -> error "TODO"
        EstimateFeesTimeLimitReached _ -> error "TODO"
        EstimateFeesWalletIdDecodingFailed _ -> error "TODO"

instance ToHttpErrorStatus EstimateFeesError

instance ToJSON EstimateFeesError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON EstimateFeesError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable EstimateFeesError where
    build (EstimateFeesError kernelErr) =
        bprint ("EstimateFeesError " % build) kernelErr
    build (EstimateFeesTimeLimitReached ter) =
        bprint ("EstimateFeesTimeLimitReached " % build) ter
    build (EstimateFeesWalletIdDecodingFailed txt) =
        bprint ("EstimateFeesWalletIdDecodingFailed " % build) txt

instance Arbitrary EstimateFeesError where
    arbitrary = oneof [ EstimateFeesError <$> arbitrary
                      , EstimateFeesTimeLimitReached <$> arbitrary
                      ]

-- | TODO: Will need to be extended
data RedeemAdaError = RedeemAdaError
    deriving (Generic, Eq)

deriveGeneric ''RedeemAdaError

instance HasDiagnostic RedeemAdaError where
    getDiagnosticKey = error "TODO"

instance ToServantError RedeemAdaError where
    declareServantError = error "TODO"

instance ToHttpErrorStatus RedeemAdaError

instance ToJSON RedeemAdaError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON RedeemAdaError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable RedeemAdaError where
    build RedeemAdaError = "RedeemAdaError"
