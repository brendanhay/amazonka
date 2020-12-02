{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | The configuration settings of a workflow type.
--
--
--
-- /See:/ 'workflowTypeConfiguration' smart constructor.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration'
  { _wtcDefaultLambdaRole ::
      !(Maybe Text),
    _wtcDefaultChildPolicy ::
      !(Maybe ChildPolicy),
    _wtcDefaultTaskList ::
      !(Maybe TaskList),
    _wtcDefaultTaskPriority ::
      !(Maybe Text),
    _wtcDefaultExecutionStartToCloseTimeout ::
      !(Maybe Text),
    _wtcDefaultTaskStartToCloseTimeout ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowTypeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtcDefaultLambdaRole' - The default IAM role attached to this workflow type.
--
-- * 'wtcDefaultChildPolicy' - The default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
--
-- * 'wtcDefaultTaskList' - The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- * 'wtcDefaultTaskPriority' - The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'wtcDefaultExecutionStartToCloseTimeout' - The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'wtcDefaultTaskStartToCloseTimeout' - The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
workflowTypeConfiguration ::
  WorkflowTypeConfiguration
workflowTypeConfiguration =
  WorkflowTypeConfiguration'
    { _wtcDefaultLambdaRole = Nothing,
      _wtcDefaultChildPolicy = Nothing,
      _wtcDefaultTaskList = Nothing,
      _wtcDefaultTaskPriority = Nothing,
      _wtcDefaultExecutionStartToCloseTimeout = Nothing,
      _wtcDefaultTaskStartToCloseTimeout = Nothing
    }

-- | The default IAM role attached to this workflow type.
wtcDefaultLambdaRole :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultLambdaRole = lens _wtcDefaultLambdaRole (\s a -> s {_wtcDefaultLambdaRole = a})

-- | The default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
wtcDefaultChildPolicy :: Lens' WorkflowTypeConfiguration (Maybe ChildPolicy)
wtcDefaultChildPolicy = lens _wtcDefaultChildPolicy (\s a -> s {_wtcDefaultChildPolicy = a})

-- | The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
wtcDefaultTaskList :: Lens' WorkflowTypeConfiguration (Maybe TaskList)
wtcDefaultTaskList = lens _wtcDefaultTaskList (\s a -> s {_wtcDefaultTaskList = a})

-- | The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
wtcDefaultTaskPriority :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskPriority = lens _wtcDefaultTaskPriority (\s a -> s {_wtcDefaultTaskPriority = a})

-- | The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
wtcDefaultExecutionStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultExecutionStartToCloseTimeout = lens _wtcDefaultExecutionStartToCloseTimeout (\s a -> s {_wtcDefaultExecutionStartToCloseTimeout = a})

-- | The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
wtcDefaultTaskStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskStartToCloseTimeout = lens _wtcDefaultTaskStartToCloseTimeout (\s a -> s {_wtcDefaultTaskStartToCloseTimeout = a})

instance FromJSON WorkflowTypeConfiguration where
  parseJSON =
    withObject
      "WorkflowTypeConfiguration"
      ( \x ->
          WorkflowTypeConfiguration'
            <$> (x .:? "defaultLambdaRole")
            <*> (x .:? "defaultChildPolicy")
            <*> (x .:? "defaultTaskList")
            <*> (x .:? "defaultTaskPriority")
            <*> (x .:? "defaultExecutionStartToCloseTimeout")
            <*> (x .:? "defaultTaskStartToCloseTimeout")
      )

instance Hashable WorkflowTypeConfiguration

instance NFData WorkflowTypeConfiguration
