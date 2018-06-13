{-# LANGUAGE TypeFamilies #-}

module Pos.DB.Ssc.State
       ( mkSscState
       , module Pos.DB.Ssc.State.Global
       , module Pos.DB.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           System.Wlog (WithLogger)

import           Pos.Chain.Ssc (SscState (..))
import           Pos.Core.Slotting (MonadSlots, SlotCount)
import           Pos.DB (MonadDBRead)

-- Reexports
import           Pos.DB.Ssc.State.Global
import           Pos.DB.Ssc.State.Local

mkSscState
    :: forall ctx m
     . (WithLogger m, MonadDBRead m, MonadSlots ctx m)
    => SlotCount
    -> m SscState
mkSscState epochSlots = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData epochSlots
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
