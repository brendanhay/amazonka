-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepState
  ( StepState
      ( StepState',
        SSCancelPending,
        SSCancelled,
        SSCompleted,
        SSFailed,
        SSInterrupted,
        SSPending,
        SSRunning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StepState = StepState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SSCancelPending :: StepState
pattern SSCancelPending = StepState' "CANCEL_PENDING"

pattern SSCancelled :: StepState
pattern SSCancelled = StepState' "CANCELLED"

pattern SSCompleted :: StepState
pattern SSCompleted = StepState' "COMPLETED"

pattern SSFailed :: StepState
pattern SSFailed = StepState' "FAILED"

pattern SSInterrupted :: StepState
pattern SSInterrupted = StepState' "INTERRUPTED"

pattern SSPending :: StepState
pattern SSPending = StepState' "PENDING"

pattern SSRunning :: StepState
pattern SSRunning = StepState' "RUNNING"

{-# COMPLETE
  SSCancelPending,
  SSCancelled,
  SSCompleted,
  SSFailed,
  SSInterrupted,
  SSPending,
  SSRunning,
  StepState'
  #-}
