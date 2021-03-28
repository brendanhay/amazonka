{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.ActionStatus
  ( ActionStatus
    ( ActionStatus'
    , ActionStatusStandby
    , ActionStatusPending
    , ActionStatusExecutionInProgress
    , ActionStatusExecutionSuccess
    , ActionStatusExecutionFailure
    , ActionStatusReverseInProgress
    , ActionStatusReverseSuccess
    , ActionStatusReverseFailure
    , ActionStatusResetInProgress
    , ActionStatusResetFailure
    , fromActionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ActionStatus = ActionStatus'{fromActionStatus :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ActionStatusStandby :: ActionStatus
pattern ActionStatusStandby = ActionStatus' "STANDBY"

pattern ActionStatusPending :: ActionStatus
pattern ActionStatusPending = ActionStatus' "PENDING"

pattern ActionStatusExecutionInProgress :: ActionStatus
pattern ActionStatusExecutionInProgress = ActionStatus' "EXECUTION_IN_PROGRESS"

pattern ActionStatusExecutionSuccess :: ActionStatus
pattern ActionStatusExecutionSuccess = ActionStatus' "EXECUTION_SUCCESS"

pattern ActionStatusExecutionFailure :: ActionStatus
pattern ActionStatusExecutionFailure = ActionStatus' "EXECUTION_FAILURE"

pattern ActionStatusReverseInProgress :: ActionStatus
pattern ActionStatusReverseInProgress = ActionStatus' "REVERSE_IN_PROGRESS"

pattern ActionStatusReverseSuccess :: ActionStatus
pattern ActionStatusReverseSuccess = ActionStatus' "REVERSE_SUCCESS"

pattern ActionStatusReverseFailure :: ActionStatus
pattern ActionStatusReverseFailure = ActionStatus' "REVERSE_FAILURE"

pattern ActionStatusResetInProgress :: ActionStatus
pattern ActionStatusResetInProgress = ActionStatus' "RESET_IN_PROGRESS"

pattern ActionStatusResetFailure :: ActionStatus
pattern ActionStatusResetFailure = ActionStatus' "RESET_FAILURE"

{-# COMPLETE 
  ActionStatusStandby,

  ActionStatusPending,

  ActionStatusExecutionInProgress,

  ActionStatusExecutionSuccess,

  ActionStatusExecutionFailure,

  ActionStatusReverseInProgress,

  ActionStatusReverseSuccess,

  ActionStatusReverseFailure,

  ActionStatusResetInProgress,

  ActionStatusResetFailure,
  ActionStatus'
  #-}
