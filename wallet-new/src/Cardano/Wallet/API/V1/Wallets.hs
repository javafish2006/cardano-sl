module Cardano.Wallet.API.V1.Wallets where

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import           Pos.Core as Core

import           Servant

type API = Tags '["Wallets"] :>
    (    "wallets" :> Summary "Creates a new or restores an existing Wallet."
                   :> ReqBody '[ValidJSON] (New Wallet)
                   :> PostCreated '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> Summary "Returns a list of the available wallets."
                   :> WalletRequestParams
                   :> FilterBy '[ WalletId
                                , Core.Coin
                                ] Wallet
                   :> SortBy   '[ Core.Coin
                                , V1 Core.Timestamp
                                ] Wallet
                   :> Get '[ValidJSON] (WalletResponse [Wallet])
    :<|> "wallets" :> CaptureWalletId
                   :> "password"
                   :> Summary "Updates the password for the given Wallet."
                   :> ReqBody '[ValidJSON] PasswordUpdate
                   :> Put '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Deletes the given Wallet and all its accounts."
                   :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Returns the Wallet identified by the given walletId."
                   :> Get '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Update the Wallet identified by the given walletId."
                   :> ReqBody '[ValidJSON] (Update Wallet)
                   :> Put '[ValidJSON] (WalletResponse Wallet)
    :<|> "wallets" :> CaptureWalletId :> "statistics" :> "utxos"
                   :> Summary "Returns Utxo statistics for the Wallet identified by the given walletId."
                   :> Get '[ValidJSON] (WalletResponse UtxoStatistics)
    :<|> "external-wallets"
                   :> Capture "rootPK" PublicKeyAsBase58
                   :> Summary "Check if this external wallet is presented in the node."
                   :> PostCreated '[ValidJSON] (WalletResponse WalletAndTxHistory)
    :<|> "external-wallets"
                   :> Summary "Creates a new or restores an existing external wallet (mobile client or hardware wallet)."
                   :> ReqBody '[ValidJSON] (New ExternalWallet)
                   :> PostCreated '[ValidJSON] (WalletResponse Wallet)
    :<|> "external-wallets"
                   :> Capture "rootPK" PublicKeyAsBase58
                   :> Summary "Deletes the given external wallet and all its accounts."
                   :> DeleteNoContent '[ValidJSON] NoContent
    )
