{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data ContinueAsNewWorkflowExecutionFailedCause
  = CANWEFCContinueAsNewWorkflowExecutionRateExceeded
  | CANWEFCDefaultChildPolicyUndefined
  | CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
  | CANWEFCDefaultTaskListUndefined
  | CANWEFCDefaultTaskStartToCloseTimeoutUndefined
  | CANWEFCOperationNotPermitted
  | CANWEFCUnhandledDecision
  | CANWEFCWorkflowTypeDeprecated
  | CANWEFCWorkflowTypeDoesNotExist
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "continue_as_new_workflow_execution_rate_exceeded" -> pure CANWEFCContinueAsNewWorkflowExecutionRateExceeded
      "default_child_policy_undefined" -> pure CANWEFCDefaultChildPolicyUndefined
      "default_execution_start_to_close_timeout_undefined" -> pure CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
      "default_task_list_undefined" -> pure CANWEFCDefaultTaskListUndefined
      "default_task_start_to_close_timeout_undefined" -> pure CANWEFCDefaultTaskStartToCloseTimeoutUndefined
      "operation_not_permitted" -> pure CANWEFCOperationNotPermitted
      "unhandled_decision" -> pure CANWEFCUnhandledDecision
      "workflow_type_deprecated" -> pure CANWEFCWorkflowTypeDeprecated
      "workflow_type_does_not_exist" -> pure CANWEFCWorkflowTypeDoesNotExist
      e ->
        fromTextError $
          "Failure parsing ContinueAsNewWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: continue_as_new_workflow_execution_rate_exceeded, default_child_policy_undefined, default_execution_start_to_close_timeout_undefined, default_task_list_undefined, default_task_start_to_close_timeout_undefined, operation_not_permitted, unhandled_decision, workflow_type_deprecated, workflow_type_does_not_exist"

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
  toText = \case
    CANWEFCContinueAsNewWorkflowExecutionRateExceeded -> "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    CANWEFCDefaultChildPolicyUndefined -> "DEFAULT_CHILD_POLICY_UNDEFINED"
    CANWEFCDefaultExecutionStartToCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    CANWEFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
    CANWEFCDefaultTaskStartToCloseTimeoutUndefined -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    CANWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    CANWEFCUnhandledDecision -> "UNHANDLED_DECISION"
    CANWEFCWorkflowTypeDeprecated -> "WORKFLOW_TYPE_DEPRECATED"
    CANWEFCWorkflowTypeDoesNotExist -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Hashable ContinueAsNewWorkflowExecutionFailedCause

instance NFData ContinueAsNewWorkflowExecutionFailedCause

instance ToByteString ContinueAsNewWorkflowExecutionFailedCause

instance ToQuery ContinueAsNewWorkflowExecutionFailedCause

instance ToHeader ContinueAsNewWorkflowExecutionFailedCause

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "ContinueAsNewWorkflowExecutionFailedCause"
