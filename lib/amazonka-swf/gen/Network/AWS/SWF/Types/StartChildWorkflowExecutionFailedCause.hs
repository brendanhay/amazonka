{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data StartChildWorkflowExecutionFailedCause
  = SCWEFCChildCreationRateExceeded
  | SCWEFCDefaultChildPolicyUndefined
  | SCWEFCDefaultExecutionStartToCloseTimeoutUndefined
  | SCWEFCDefaultTaskListUndefined
  | SCWEFCDefaultTaskStartToCloseTimeoutUndefined
  | SCWEFCOpenChildrenLimitExceeded
  | SCWEFCOpenWorkflowsLimitExceeded
  | SCWEFCOperationNotPermitted
  | SCWEFCWorkflowAlreadyRunning
  | SCWEFCWorkflowTypeDeprecated
  | SCWEFCWorkflowTypeDoesNotExist
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

instance FromText StartChildWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "child_creation_rate_exceeded" -> pure SCWEFCChildCreationRateExceeded
      "default_child_policy_undefined" -> pure SCWEFCDefaultChildPolicyUndefined
      "default_execution_start_to_close_timeout_undefined" -> pure SCWEFCDefaultExecutionStartToCloseTimeoutUndefined
      "default_task_list_undefined" -> pure SCWEFCDefaultTaskListUndefined
      "default_task_start_to_close_timeout_undefined" -> pure SCWEFCDefaultTaskStartToCloseTimeoutUndefined
      "open_children_limit_exceeded" -> pure SCWEFCOpenChildrenLimitExceeded
      "open_workflows_limit_exceeded" -> pure SCWEFCOpenWorkflowsLimitExceeded
      "operation_not_permitted" -> pure SCWEFCOperationNotPermitted
      "workflow_already_running" -> pure SCWEFCWorkflowAlreadyRunning
      "workflow_type_deprecated" -> pure SCWEFCWorkflowTypeDeprecated
      "workflow_type_does_not_exist" -> pure SCWEFCWorkflowTypeDoesNotExist
      e ->
        fromTextError $
          "Failure parsing StartChildWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: child_creation_rate_exceeded, default_child_policy_undefined, default_execution_start_to_close_timeout_undefined, default_task_list_undefined, default_task_start_to_close_timeout_undefined, open_children_limit_exceeded, open_workflows_limit_exceeded, operation_not_permitted, workflow_already_running, workflow_type_deprecated, workflow_type_does_not_exist"

instance ToText StartChildWorkflowExecutionFailedCause where
  toText = \case
    SCWEFCChildCreationRateExceeded -> "CHILD_CREATION_RATE_EXCEEDED"
    SCWEFCDefaultChildPolicyUndefined -> "DEFAULT_CHILD_POLICY_UNDEFINED"
    SCWEFCDefaultExecutionStartToCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    SCWEFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
    SCWEFCDefaultTaskStartToCloseTimeoutUndefined -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    SCWEFCOpenChildrenLimitExceeded -> "OPEN_CHILDREN_LIMIT_EXCEEDED"
    SCWEFCOpenWorkflowsLimitExceeded -> "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    SCWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    SCWEFCWorkflowAlreadyRunning -> "WORKFLOW_ALREADY_RUNNING"
    SCWEFCWorkflowTypeDeprecated -> "WORKFLOW_TYPE_DEPRECATED"
    SCWEFCWorkflowTypeDoesNotExist -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Hashable StartChildWorkflowExecutionFailedCause

instance NFData StartChildWorkflowExecutionFailedCause

instance ToByteString StartChildWorkflowExecutionFailedCause

instance ToQuery StartChildWorkflowExecutionFailedCause

instance ToHeader StartChildWorkflowExecutionFailedCause

instance FromJSON StartChildWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "StartChildWorkflowExecutionFailedCause"
