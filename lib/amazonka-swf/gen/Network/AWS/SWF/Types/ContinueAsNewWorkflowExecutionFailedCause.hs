{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
  ( ContinueAsNewWorkflowExecutionFailedCause
      ( ContinueAsNewWorkflowExecutionFailedCause',
        CANWEFCContinueAsNewWorkflowExecutionRateExceeded,
        CANWEFCDefaultChildPolicyUndefined,
        CANWEFCDefaultExecutionStartToCloseTimeoutUndefined,
        CANWEFCDefaultTaskListUndefined,
        CANWEFCDefaultTaskStartToCloseTimeoutUndefined,
        CANWEFCOperationNotPermitted,
        CANWEFCUnhandledDecision,
        CANWEFCWorkflowTypeDeprecated,
        CANWEFCWorkflowTypeDoesNotExist
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause' Lude.Text
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

pattern CANWEFCContinueAsNewWorkflowExecutionRateExceeded :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCContinueAsNewWorkflowExecutionRateExceeded = ContinueAsNewWorkflowExecutionFailedCause' "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern CANWEFCDefaultChildPolicyUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultChildPolicyUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern CANWEFCDefaultExecutionStartToCloseTimeoutUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultExecutionStartToCloseTimeoutUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern CANWEFCDefaultTaskListUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultTaskListUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern CANWEFCDefaultTaskStartToCloseTimeoutUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultTaskStartToCloseTimeoutUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern CANWEFCOperationNotPermitted :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCOperationNotPermitted = ContinueAsNewWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CANWEFCUnhandledDecision :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCUnhandledDecision = ContinueAsNewWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

pattern CANWEFCWorkflowTypeDeprecated :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCWorkflowTypeDeprecated = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern CANWEFCWorkflowTypeDoesNotExist :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCWorkflowTypeDoesNotExist = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

{-# COMPLETE
  CANWEFCContinueAsNewWorkflowExecutionRateExceeded,
  CANWEFCDefaultChildPolicyUndefined,
  CANWEFCDefaultExecutionStartToCloseTimeoutUndefined,
  CANWEFCDefaultTaskListUndefined,
  CANWEFCDefaultTaskStartToCloseTimeoutUndefined,
  CANWEFCOperationNotPermitted,
  CANWEFCUnhandledDecision,
  CANWEFCWorkflowTypeDeprecated,
  CANWEFCWorkflowTypeDoesNotExist,
  ContinueAsNewWorkflowExecutionFailedCause'
  #-}
