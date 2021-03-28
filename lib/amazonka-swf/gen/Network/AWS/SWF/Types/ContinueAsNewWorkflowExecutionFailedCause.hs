{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
  ( ContinueAsNewWorkflowExecutionFailedCause
    ( ContinueAsNewWorkflowExecutionFailedCause'
    , ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision
    , ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated
    , ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist
    , ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined
    , ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined
    , ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined
    , ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined
    , ContinueAsNewWorkflowExecutionFailedCauseContinueAsNewWorkflowExecutionRateExceeded
    , ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted
    , fromContinueAsNewWorkflowExecutionFailedCause
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause'{fromContinueAsNewWorkflowExecutionFailedCause
                                                                                               ::
                                                                                               Core.Text}
                                                      deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                      Core.Show, Core.Generic)
                                                      deriving newtype (Core.IsString,
                                                                        Core.Hashable, Core.NFData,
                                                                        Core.ToJSONKey,
                                                                        Core.FromJSONKey,
                                                                        Core.ToJSON, Core.FromJSON,
                                                                        Core.ToXML, Core.FromXML,
                                                                        Core.ToText, Core.FromText,
                                                                        Core.ToByteString,
                                                                        Core.ToQuery, Core.ToHeader)

pattern ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision = ContinueAsNewWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

pattern ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern ContinueAsNewWorkflowExecutionFailedCauseContinueAsNewWorkflowExecutionRateExceeded :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseContinueAsNewWorkflowExecutionRateExceeded = ContinueAsNewWorkflowExecutionFailedCause' "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted :: ContinueAsNewWorkflowExecutionFailedCause
pattern ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted = ContinueAsNewWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE 
  ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision,

  ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated,

  ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist,

  ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined,

  ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined,

  ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined,

  ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined,

  ContinueAsNewWorkflowExecutionFailedCauseContinueAsNewWorkflowExecutionRateExceeded,

  ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted,
  ContinueAsNewWorkflowExecutionFailedCause'
  #-}
