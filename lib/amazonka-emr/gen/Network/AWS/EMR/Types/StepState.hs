{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepState
  ( StepState
    ( StepState'
    , StepStatePending
    , StepStateCancelPending
    , StepStateRunning
    , StepStateCompleted
    , StepStateCancelled
    , StepStateFailed
    , StepStateInterrupted
    , fromStepState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StepState = StepState'{fromStepState :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern StepStatePending :: StepState
pattern StepStatePending = StepState' "PENDING"

pattern StepStateCancelPending :: StepState
pattern StepStateCancelPending = StepState' "CANCEL_PENDING"

pattern StepStateRunning :: StepState
pattern StepStateRunning = StepState' "RUNNING"

pattern StepStateCompleted :: StepState
pattern StepStateCompleted = StepState' "COMPLETED"

pattern StepStateCancelled :: StepState
pattern StepStateCancelled = StepState' "CANCELLED"

pattern StepStateFailed :: StepState
pattern StepStateFailed = StepState' "FAILED"

pattern StepStateInterrupted :: StepState
pattern StepStateInterrupted = StepState' "INTERRUPTED"

{-# COMPLETE 
  StepStatePending,

  StepStateCancelPending,

  StepStateRunning,

  StepStateCompleted,

  StepStateCancelled,

  StepStateFailed,

  StepStateInterrupted,
  StepState'
  #-}
