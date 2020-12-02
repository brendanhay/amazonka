{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | The configuration settings for a workflow execution including timeout values, tasklist etc. These configuration settings are determined from the defaults specified when registering the workflow type and those specified when starting the workflow execution.
--
--
--
-- /See:/ 'workflowExecutionConfiguration' smart constructor.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration'
  { _wecLambdaRole ::
      !(Maybe Text),
    _wecTaskPriority ::
      !(Maybe Text),
    _wecTaskStartToCloseTimeout ::
      !Text,
    _wecExecutionStartToCloseTimeout ::
      !Text,
    _wecTaskList :: !TaskList,
    _wecChildPolicy ::
      !ChildPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wecLambdaRole' - The IAM role attached to the child workflow execution.
--
-- * 'wecTaskPriority' - The priority assigned to decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'wecTaskStartToCloseTimeout' - The maximum duration allowed for decision tasks for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'wecExecutionStartToCloseTimeout' - The total duration for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'wecTaskList' - The task list used for the decision tasks generated for this workflow execution.
--
-- * 'wecChildPolicy' - The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
workflowExecutionConfiguration ::
  -- | 'wecTaskStartToCloseTimeout'
  Text ->
  -- | 'wecExecutionStartToCloseTimeout'
  Text ->
  -- | 'wecTaskList'
  TaskList ->
  -- | 'wecChildPolicy'
  ChildPolicy ->
  WorkflowExecutionConfiguration
workflowExecutionConfiguration
  pTaskStartToCloseTimeout_
  pExecutionStartToCloseTimeout_
  pTaskList_
  pChildPolicy_ =
    WorkflowExecutionConfiguration'
      { _wecLambdaRole = Nothing,
        _wecTaskPriority = Nothing,
        _wecTaskStartToCloseTimeout = pTaskStartToCloseTimeout_,
        _wecExecutionStartToCloseTimeout =
          pExecutionStartToCloseTimeout_,
        _wecTaskList = pTaskList_,
        _wecChildPolicy = pChildPolicy_
      }

-- | The IAM role attached to the child workflow execution.
wecLambdaRole :: Lens' WorkflowExecutionConfiguration (Maybe Text)
wecLambdaRole = lens _wecLambdaRole (\s a -> s {_wecLambdaRole = a})

-- | The priority assigned to decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
wecTaskPriority :: Lens' WorkflowExecutionConfiguration (Maybe Text)
wecTaskPriority = lens _wecTaskPriority (\s a -> s {_wecTaskPriority = a})

-- | The maximum duration allowed for decision tasks for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
wecTaskStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration Text
wecTaskStartToCloseTimeout = lens _wecTaskStartToCloseTimeout (\s a -> s {_wecTaskStartToCloseTimeout = a})

-- | The total duration for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
wecExecutionStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration Text
wecExecutionStartToCloseTimeout = lens _wecExecutionStartToCloseTimeout (\s a -> s {_wecExecutionStartToCloseTimeout = a})

-- | The task list used for the decision tasks generated for this workflow execution.
wecTaskList :: Lens' WorkflowExecutionConfiguration TaskList
wecTaskList = lens _wecTaskList (\s a -> s {_wecTaskList = a})

-- | The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
wecChildPolicy :: Lens' WorkflowExecutionConfiguration ChildPolicy
wecChildPolicy = lens _wecChildPolicy (\s a -> s {_wecChildPolicy = a})

instance FromJSON WorkflowExecutionConfiguration where
  parseJSON =
    withObject
      "WorkflowExecutionConfiguration"
      ( \x ->
          WorkflowExecutionConfiguration'
            <$> (x .:? "lambdaRole")
            <*> (x .:? "taskPriority")
            <*> (x .: "taskStartToCloseTimeout")
            <*> (x .: "executionStartToCloseTimeout")
            <*> (x .: "taskList")
            <*> (x .: "childPolicy")
      )

instance Hashable WorkflowExecutionConfiguration

instance NFData WorkflowExecutionConfiguration
