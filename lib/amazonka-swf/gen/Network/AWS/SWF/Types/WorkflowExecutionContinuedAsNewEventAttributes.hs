{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @WorkflowExecutionContinuedAsNew@ event.
--
--
--
-- /See:/ 'workflowExecutionContinuedAsNewEventAttributes' smart constructor.
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes'
  { _wecaneaTagList ::
      !( Maybe
           [Text]
       ),
    _wecaneaTaskStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _wecaneaLambdaRole ::
      !( Maybe
           Text
       ),
    _wecaneaInput ::
      !( Maybe
           Text
       ),
    _wecaneaExecutionStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _wecaneaTaskPriority ::
      !( Maybe
           Text
       ),
    _wecaneaDecisionTaskCompletedEventId ::
      !Integer,
    _wecaneaNewExecutionRunId ::
      !Text,
    _wecaneaTaskList ::
      !TaskList,
    _wecaneaChildPolicy ::
      !ChildPolicy,
    _wecaneaWorkflowType ::
      !WorkflowType
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'WorkflowExecutionContinuedAsNewEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wecaneaTagList' - The list of tags associated with the new workflow execution.
--
-- * 'wecaneaTaskStartToCloseTimeout' - The maximum duration of decision tasks for the new workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'wecaneaLambdaRole' - The IAM role to attach to the new (continued) workflow execution.
--
-- * 'wecaneaInput' - The input provided to the new workflow execution.
--
-- * 'wecaneaExecutionStartToCloseTimeout' - The total duration allowed for the new workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'wecaneaTaskPriority' - The priority of the task to use for the decisions of the new (continued) workflow execution.
--
-- * 'wecaneaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @ContinueAsNewWorkflowExecution@ decision that started this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'wecaneaNewExecutionRunId' - The @runId@ of the new workflow execution.
--
-- * 'wecaneaTaskList' - The task list to use for the decisions of the new (continued) workflow execution.
--
-- * 'wecaneaChildPolicy' - The policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
--
-- * 'wecaneaWorkflowType' - The workflow type of this execution.
workflowExecutionContinuedAsNewEventAttributes ::
  -- | 'wecaneaDecisionTaskCompletedEventId'
  Integer ->
  -- | 'wecaneaNewExecutionRunId'
  Text ->
  -- | 'wecaneaTaskList'
  TaskList ->
  -- | 'wecaneaChildPolicy'
  ChildPolicy ->
  -- | 'wecaneaWorkflowType'
  WorkflowType ->
  WorkflowExecutionContinuedAsNewEventAttributes
workflowExecutionContinuedAsNewEventAttributes
  pDecisionTaskCompletedEventId_
  pNewExecutionRunId_
  pTaskList_
  pChildPolicy_
  pWorkflowType_ =
    WorkflowExecutionContinuedAsNewEventAttributes'
      { _wecaneaTagList =
          Nothing,
        _wecaneaTaskStartToCloseTimeout = Nothing,
        _wecaneaLambdaRole = Nothing,
        _wecaneaInput = Nothing,
        _wecaneaExecutionStartToCloseTimeout = Nothing,
        _wecaneaTaskPriority = Nothing,
        _wecaneaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        _wecaneaNewExecutionRunId = pNewExecutionRunId_,
        _wecaneaTaskList = pTaskList_,
        _wecaneaChildPolicy = pChildPolicy_,
        _wecaneaWorkflowType = pWorkflowType_
      }

-- | The list of tags associated with the new workflow execution.
wecaneaTagList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes [Text]
wecaneaTagList = lens _wecaneaTagList (\s a -> s {_wecaneaTagList = a}) . _Default . _Coerce

-- | The maximum duration of decision tasks for the new workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
wecaneaTaskStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskStartToCloseTimeout = lens _wecaneaTaskStartToCloseTimeout (\s a -> s {_wecaneaTaskStartToCloseTimeout = a})

-- | The IAM role to attach to the new (continued) workflow execution.
wecaneaLambdaRole :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaLambdaRole = lens _wecaneaLambdaRole (\s a -> s {_wecaneaLambdaRole = a})

-- | The input provided to the new workflow execution.
wecaneaInput :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaInput = lens _wecaneaInput (\s a -> s {_wecaneaInput = a})

-- | The total duration allowed for the new workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
wecaneaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaExecutionStartToCloseTimeout = lens _wecaneaExecutionStartToCloseTimeout (\s a -> s {_wecaneaExecutionStartToCloseTimeout = a})

-- | The priority of the task to use for the decisions of the new (continued) workflow execution.
wecaneaTaskPriority :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskPriority = lens _wecaneaTaskPriority (\s a -> s {_wecaneaTaskPriority = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @ContinueAsNewWorkflowExecution@ decision that started this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
wecaneaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes Integer
wecaneaDecisionTaskCompletedEventId = lens _wecaneaDecisionTaskCompletedEventId (\s a -> s {_wecaneaDecisionTaskCompletedEventId = a})

-- | The @runId@ of the new workflow execution.
wecaneaNewExecutionRunId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes Text
wecaneaNewExecutionRunId = lens _wecaneaNewExecutionRunId (\s a -> s {_wecaneaNewExecutionRunId = a})

-- | The task list to use for the decisions of the new (continued) workflow execution.
wecaneaTaskList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes TaskList
wecaneaTaskList = lens _wecaneaTaskList (\s a -> s {_wecaneaTaskList = a})

-- | The policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
wecaneaChildPolicy :: Lens' WorkflowExecutionContinuedAsNewEventAttributes ChildPolicy
wecaneaChildPolicy = lens _wecaneaChildPolicy (\s a -> s {_wecaneaChildPolicy = a})

-- | The workflow type of this execution.
wecaneaWorkflowType :: Lens' WorkflowExecutionContinuedAsNewEventAttributes WorkflowType
wecaneaWorkflowType = lens _wecaneaWorkflowType (\s a -> s {_wecaneaWorkflowType = a})

instance FromJSON WorkflowExecutionContinuedAsNewEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionContinuedAsNewEventAttributes"
      ( \x ->
          WorkflowExecutionContinuedAsNewEventAttributes'
            <$> (x .:? "tagList" .!= mempty)
            <*> (x .:? "taskStartToCloseTimeout")
            <*> (x .:? "lambdaRole")
            <*> (x .:? "input")
            <*> (x .:? "executionStartToCloseTimeout")
            <*> (x .:? "taskPriority")
            <*> (x .: "decisionTaskCompletedEventId")
            <*> (x .: "newExecutionRunId")
            <*> (x .: "taskList")
            <*> (x .: "childPolicy")
            <*> (x .: "workflowType")
      )

instance Hashable WorkflowExecutionContinuedAsNewEventAttributes

instance NFData WorkflowExecutionContinuedAsNewEventAttributes
