{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides details of @WorkflowExecutionStarted@ event.
--
--
--
-- /See:/ 'workflowExecutionStartedEventAttributes' smart constructor.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes'
  { _weseaParentInitiatedEventId ::
      !( Maybe
           Integer
       ),
    _weseaTagList ::
      !( Maybe
           [Text]
       ),
    _weseaTaskStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _weseaLambdaRole ::
      !( Maybe
           Text
       ),
    _weseaInput ::
      !( Maybe
           Text
       ),
    _weseaExecutionStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _weseaTaskPriority ::
      !( Maybe
           Text
       ),
    _weseaParentWorkflowExecution ::
      !( Maybe
           WorkflowExecution
       ),
    _weseaContinuedExecutionRunId ::
      !( Maybe
           Text
       ),
    _weseaChildPolicy ::
      !ChildPolicy,
    _weseaTaskList ::
      !TaskList,
    _weseaWorkflowType ::
      !WorkflowType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weseaParentInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this workflow execution. The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'weseaTagList' - The list of tags associated with this workflow execution. An execution can have up to 5 tags.
--
-- * 'weseaTaskStartToCloseTimeout' - The maximum duration of decision tasks for this workflow type. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'weseaLambdaRole' - The IAM role attached to the workflow execution.
--
-- * 'weseaInput' - The input provided to the workflow execution.
--
-- * 'weseaExecutionStartToCloseTimeout' - The maximum duration for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'weseaTaskPriority' - The priority of the decision tasks in the workflow execution.
--
-- * 'weseaParentWorkflowExecution' - The source workflow execution that started this workflow execution. The member isn't set if the workflow execution was not started by a workflow.
--
-- * 'weseaContinuedExecutionRunId' - If this workflow execution was started due to a @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@ of the previous workflow execution that was closed and continued as this execution.
--
-- * 'weseaChildPolicy' - The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
--
-- * 'weseaTaskList' - The name of the task list for scheduling the decision tasks for this workflow execution.
--
-- * 'weseaWorkflowType' - The workflow type of this execution.
workflowExecutionStartedEventAttributes ::
  -- | 'weseaChildPolicy'
  ChildPolicy ->
  -- | 'weseaTaskList'
  TaskList ->
  -- | 'weseaWorkflowType'
  WorkflowType ->
  WorkflowExecutionStartedEventAttributes
workflowExecutionStartedEventAttributes
  pChildPolicy_
  pTaskList_
  pWorkflowType_ =
    WorkflowExecutionStartedEventAttributes'
      { _weseaParentInitiatedEventId =
          Nothing,
        _weseaTagList = Nothing,
        _weseaTaskStartToCloseTimeout = Nothing,
        _weseaLambdaRole = Nothing,
        _weseaInput = Nothing,
        _weseaExecutionStartToCloseTimeout = Nothing,
        _weseaTaskPriority = Nothing,
        _weseaParentWorkflowExecution = Nothing,
        _weseaContinuedExecutionRunId = Nothing,
        _weseaChildPolicy = pChildPolicy_,
        _weseaTaskList = pTaskList_,
        _weseaWorkflowType = pWorkflowType_
      }

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this workflow execution. The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
weseaParentInitiatedEventId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Integer)
weseaParentInitiatedEventId = lens _weseaParentInitiatedEventId (\s a -> s {_weseaParentInitiatedEventId = a})

-- | The list of tags associated with this workflow execution. An execution can have up to 5 tags.
weseaTagList :: Lens' WorkflowExecutionStartedEventAttributes [Text]
weseaTagList = lens _weseaTagList (\s a -> s {_weseaTagList = a}) . _Default . _Coerce

-- | The maximum duration of decision tasks for this workflow type. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
weseaTaskStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskStartToCloseTimeout = lens _weseaTaskStartToCloseTimeout (\s a -> s {_weseaTaskStartToCloseTimeout = a})

-- | The IAM role attached to the workflow execution.
weseaLambdaRole :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaLambdaRole = lens _weseaLambdaRole (\s a -> s {_weseaLambdaRole = a})

-- | The input provided to the workflow execution.
weseaInput :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaInput = lens _weseaInput (\s a -> s {_weseaInput = a})

-- | The maximum duration for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
weseaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaExecutionStartToCloseTimeout = lens _weseaExecutionStartToCloseTimeout (\s a -> s {_weseaExecutionStartToCloseTimeout = a})

-- | The priority of the decision tasks in the workflow execution.
weseaTaskPriority :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskPriority = lens _weseaTaskPriority (\s a -> s {_weseaTaskPriority = a})

-- | The source workflow execution that started this workflow execution. The member isn't set if the workflow execution was not started by a workflow.
weseaParentWorkflowExecution :: Lens' WorkflowExecutionStartedEventAttributes (Maybe WorkflowExecution)
weseaParentWorkflowExecution = lens _weseaParentWorkflowExecution (\s a -> s {_weseaParentWorkflowExecution = a})

-- | If this workflow execution was started due to a @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@ of the previous workflow execution that was closed and continued as this execution.
weseaContinuedExecutionRunId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaContinuedExecutionRunId = lens _weseaContinuedExecutionRunId (\s a -> s {_weseaContinuedExecutionRunId = a})

-- | The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
weseaChildPolicy :: Lens' WorkflowExecutionStartedEventAttributes ChildPolicy
weseaChildPolicy = lens _weseaChildPolicy (\s a -> s {_weseaChildPolicy = a})

-- | The name of the task list for scheduling the decision tasks for this workflow execution.
weseaTaskList :: Lens' WorkflowExecutionStartedEventAttributes TaskList
weseaTaskList = lens _weseaTaskList (\s a -> s {_weseaTaskList = a})

-- | The workflow type of this execution.
weseaWorkflowType :: Lens' WorkflowExecutionStartedEventAttributes WorkflowType
weseaWorkflowType = lens _weseaWorkflowType (\s a -> s {_weseaWorkflowType = a})

instance FromJSON WorkflowExecutionStartedEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionStartedEventAttributes"
      ( \x ->
          WorkflowExecutionStartedEventAttributes'
            <$> (x .:? "parentInitiatedEventId")
            <*> (x .:? "tagList" .!= mempty)
            <*> (x .:? "taskStartToCloseTimeout")
            <*> (x .:? "lambdaRole")
            <*> (x .:? "input")
            <*> (x .:? "executionStartToCloseTimeout")
            <*> (x .:? "taskPriority")
            <*> (x .:? "parentWorkflowExecution")
            <*> (x .:? "continuedExecutionRunId")
            <*> (x .: "childPolicy")
            <*> (x .: "taskList")
            <*> (x .: "workflowType")
      )

instance Hashable WorkflowExecutionStartedEventAttributes

instance NFData WorkflowExecutionStartedEventAttributes
