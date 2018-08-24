{-# LANGUAGE ExistentialQuantification #-}

module Cardano.Wallet.Kernel.WalletException
    ( WalletException(..)
    , throwWalletM
    ) where

import           Universum

import           Cardano.Wallet.API.Response.JSend (HasDiagnostic)
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus)

import qualified Prelude


data WalletException = forall e.
    ( Exception e
    , HasDiagnostic e
    , ToHttpErrorStatus e
    ) => WalletException { getWalletException :: e }

instance Prelude.Show WalletException where
    show (WalletException e) = Prelude.show e

instance Exception WalletException

-- | Wrap a wallet exception in a proper type that can be caught later
throwWalletM ::
    ( Exception e
    , MonadThrow m
    , HasDiagnostic e
    , ToHttpErrorStatus e
    ) => e -> m a
throwWalletM =
    throwM . WalletException
