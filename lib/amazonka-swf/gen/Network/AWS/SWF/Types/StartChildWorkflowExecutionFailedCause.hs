{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
  ( StartChildWorkflowExecutionFailedCause
    ( StartChildWorkflowExecutionFailedCause'
    , StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist
    , StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated
    , StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded
    , StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded
    , StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded
    , StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning
    , StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined
    , StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined
    , StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined
    , StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined
    , StartChildWorkflowExecutionFailedCauseOperationNotPermitted
    , fromStartChildWorkflowExecutionFailedCause
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StartChildWorkflowExecutionFailedCause = StartChildWorkflowExecutionFailedCause'{fromStartChildWorkflowExecutionFailedCause
                                                                                         ::
                                                                                         Core.Text}
                                                   deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                   Core.Show, Core.Generic)
                                                   deriving newtype (Core.IsString, Core.Hashable,
                                                                     Core.NFData, Core.ToJSONKey,
                                                                     Core.FromJSONKey, Core.ToJSON,
                                                                     Core.FromJSON, Core.ToXML,
                                                                     Core.FromXML, Core.ToText,
                                                                     Core.FromText,
                                                                     Core.ToByteString,
                                                                     Core.ToQuery, Core.ToHeader)

pattern StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = StartChildWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

pattern StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated = StartChildWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded = StartChildWorkflowExecutionFailedCause' "OPEN_CHILDREN_LIMIT_EXCEEDED"

pattern StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded = StartChildWorkflowExecutionFailedCause' "OPEN_WORKFLOWS_LIMIT_EXCEEDED"

pattern StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded = StartChildWorkflowExecutionFailedCause' "CHILD_CREATION_RATE_EXCEEDED"

pattern StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning = StartChildWorkflowExecutionFailedCause' "WORKFLOW_ALREADY_RUNNING"

pattern StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern StartChildWorkflowExecutionFailedCauseOperationNotPermitted :: StartChildWorkflowExecutionFailedCause
pattern StartChildWorkflowExecutionFailedCauseOperationNotPermitted = StartChildWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE 
  StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist,

  StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated,

  StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded,

  StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded,

  StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded,

  StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning,

  StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined,

  StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined,

  StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined,

  StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined,

  StartChildWorkflowExecutionFailedCauseOperationNotPermitted,
  StartChildWorkflowExecutionFailedCause'
  #-}
