{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
  ( StartChildWorkflowExecutionFailedCause
      ( StartChildWorkflowExecutionFailedCause',
        SCWEFCWorkflowTypeDoesNotExist,
        SCWEFCWorkflowTypeDeprecated,
        SCWEFCOpenChildrenLimitExceeded,
        SCWEFCOpenWorkflowsLimitExceeded,
        SCWEFCChildCreationRateExceeded,
        SCWEFCWorkflowAlreadyRunning,
        SCWEFCDefaultExecutionStartToCloseTimeoutUndefined,
        SCWEFCDefaultTaskListUndefined,
        SCWEFCDefaultTaskStartToCloseTimeoutUndefined,
        SCWEFCDefaultChildPolicyUndefined,
        SCWEFCOperationNotPermitted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StartChildWorkflowExecutionFailedCause = StartChildWorkflowExecutionFailedCause' Lude.Text
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

pattern SCWEFCWorkflowTypeDoesNotExist :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCWorkflowTypeDoesNotExist = StartChildWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

pattern SCWEFCWorkflowTypeDeprecated :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCWorkflowTypeDeprecated = StartChildWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern SCWEFCOpenChildrenLimitExceeded :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCOpenChildrenLimitExceeded = StartChildWorkflowExecutionFailedCause' "OPEN_CHILDREN_LIMIT_EXCEEDED"

pattern SCWEFCOpenWorkflowsLimitExceeded :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCOpenWorkflowsLimitExceeded = StartChildWorkflowExecutionFailedCause' "OPEN_WORKFLOWS_LIMIT_EXCEEDED"

pattern SCWEFCChildCreationRateExceeded :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCChildCreationRateExceeded = StartChildWorkflowExecutionFailedCause' "CHILD_CREATION_RATE_EXCEEDED"

pattern SCWEFCWorkflowAlreadyRunning :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCWorkflowAlreadyRunning = StartChildWorkflowExecutionFailedCause' "WORKFLOW_ALREADY_RUNNING"

pattern SCWEFCDefaultExecutionStartToCloseTimeoutUndefined :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCDefaultExecutionStartToCloseTimeoutUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern SCWEFCDefaultTaskListUndefined :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCDefaultTaskListUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern SCWEFCDefaultTaskStartToCloseTimeoutUndefined :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCDefaultTaskStartToCloseTimeoutUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern SCWEFCDefaultChildPolicyUndefined :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCDefaultChildPolicyUndefined = StartChildWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern SCWEFCOperationNotPermitted :: StartChildWorkflowExecutionFailedCause
pattern SCWEFCOperationNotPermitted = StartChildWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  SCWEFCWorkflowTypeDoesNotExist,
  SCWEFCWorkflowTypeDeprecated,
  SCWEFCOpenChildrenLimitExceeded,
  SCWEFCOpenWorkflowsLimitExceeded,
  SCWEFCChildCreationRateExceeded,
  SCWEFCWorkflowAlreadyRunning,
  SCWEFCDefaultExecutionStartToCloseTimeoutUndefined,
  SCWEFCDefaultTaskListUndefined,
  SCWEFCDefaultTaskStartToCloseTimeoutUndefined,
  SCWEFCDefaultChildPolicyUndefined,
  SCWEFCOperationNotPermitted,
  StartChildWorkflowExecutionFailedCause'
  #-}
