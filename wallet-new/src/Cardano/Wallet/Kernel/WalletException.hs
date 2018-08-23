{-# LANGUAGE ExistentialQuantification #-}

module Cardano.Wallet.Kernel.WalletException where

import           Universum

import           Data.Aeson (ToJSON, toJSON)
import           Data.Typeable (cast)
import           Formatting.Buildable (build)
import           Prelude (show)

import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus (..),
                     ToServantError (..))

data WalletException = forall e . (ToHttpErrorStatus e, Exception e, Buildable e) => WalletException e

-- FIXME: Generics somewhow?

instance Show WalletException where
    show (WalletException e) = Prelude.show e

instance ToJSON WalletException where
    toJSON (WalletException e) = toJSON e

instance ToHttpErrorStatus WalletException where
    toHttpErrorStatus (WalletException e) = toHttpErrorStatus e

instance ToServantError WalletException where
    declareServantError (WalletException e) = declareServantError e

instance Buildable WalletException where
    build (WalletException e) = build e

instance Exception WalletException

walletExceptionToException :: (ToHttpErrorStatus e, Exception e) => e -> SomeException
walletExceptionToException = toException . WalletException

walletExceptionFromException :: Exception e => SomeException -> Maybe e
walletExceptionFromException x = do
    WalletException a <- fromException x
    cast a
