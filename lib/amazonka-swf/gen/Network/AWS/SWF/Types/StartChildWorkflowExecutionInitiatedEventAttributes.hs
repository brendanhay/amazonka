{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @StartChildWorkflowExecutionInitiated@ event.
--
--
--
-- /See:/ 'startChildWorkflowExecutionInitiatedEventAttributes' smart constructor.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes'
  { _scweieaControl ::
      !( Maybe
           Text
       ),
    _scweieaTagList ::
      !( Maybe
           [Text]
       ),
    _scweieaTaskStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _scweieaLambdaRole ::
      !( Maybe
           Text
       ),
    _scweieaInput ::
      !( Maybe
           Text
       ),
    _scweieaExecutionStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _scweieaTaskPriority ::
      !( Maybe
           Text
       ),
    _scweieaWorkflowId ::
      !Text,
    _scweieaWorkflowType ::
      !WorkflowType,
    _scweieaTaskList ::
      !TaskList,
    _scweieaDecisionTaskCompletedEventId ::
      !Integer,
    _scweieaChildPolicy ::
      !ChildPolicy
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartChildWorkflowExecutionInitiatedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scweieaControl' - Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
--
-- * 'scweieaTagList' - The list of tags to associated with the child workflow execution.
--
-- * 'scweieaTaskStartToCloseTimeout' - The maximum duration allowed for the decision tasks for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'scweieaLambdaRole' - The IAM role to attach to the child workflow execution.
--
-- * 'scweieaInput' - The inputs provided to the child workflow execution.
--
-- * 'scweieaExecutionStartToCloseTimeout' - The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'scweieaTaskPriority' - The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'scweieaWorkflowId' - The @workflowId@ of the child workflow execution.
--
-- * 'scweieaWorkflowType' - The type of the child workflow execution.
--
-- * 'scweieaTaskList' - The name of the task list used for the decision tasks of the child workflow execution.
--
-- * 'scweieaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
--
-- * 'scweieaChildPolicy' - The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
startChildWorkflowExecutionInitiatedEventAttributes ::
  -- | 'scweieaWorkflowId'
  Text ->
  -- | 'scweieaWorkflowType'
  WorkflowType ->
  -- | 'scweieaTaskList'
  TaskList ->
  -- | 'scweieaDecisionTaskCompletedEventId'
  Integer ->
  -- | 'scweieaChildPolicy'
  ChildPolicy ->
  StartChildWorkflowExecutionInitiatedEventAttributes
startChildWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pWorkflowType_
  pTaskList_
  pDecisionTaskCompletedEventId_
  pChildPolicy_ =
    StartChildWorkflowExecutionInitiatedEventAttributes'
      { _scweieaControl =
          Nothing,
        _scweieaTagList = Nothing,
        _scweieaTaskStartToCloseTimeout = Nothing,
        _scweieaLambdaRole = Nothing,
        _scweieaInput = Nothing,
        _scweieaExecutionStartToCloseTimeout =
          Nothing,
        _scweieaTaskPriority = Nothing,
        _scweieaWorkflowId = pWorkflowId_,
        _scweieaWorkflowType = pWorkflowType_,
        _scweieaTaskList = pTaskList_,
        _scweieaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        _scweieaChildPolicy = pChildPolicy_
      }

-- | Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
scweieaControl :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaControl = lens _scweieaControl (\s a -> s {_scweieaControl = a})

-- | The list of tags to associated with the child workflow execution.
scweieaTagList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes [Text]
scweieaTagList = lens _scweieaTagList (\s a -> s {_scweieaTagList = a}) . _Default . _Coerce

-- | The maximum duration allowed for the decision tasks for this workflow execution. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
scweieaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskStartToCloseTimeout = lens _scweieaTaskStartToCloseTimeout (\s a -> s {_scweieaTaskStartToCloseTimeout = a})

-- | The IAM role to attach to the child workflow execution.
scweieaLambdaRole :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaLambdaRole = lens _scweieaLambdaRole (\s a -> s {_scweieaLambdaRole = a})

-- | The inputs provided to the child workflow execution.
scweieaInput :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaInput = lens _scweieaInput (\s a -> s {_scweieaInput = a})

-- | The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
scweieaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaExecutionStartToCloseTimeout = lens _scweieaExecutionStartToCloseTimeout (\s a -> s {_scweieaExecutionStartToCloseTimeout = a})

-- | The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
scweieaTaskPriority :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskPriority = lens _scweieaTaskPriority (\s a -> s {_scweieaTaskPriority = a})

-- | The @workflowId@ of the child workflow execution.
scweieaWorkflowId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes Text
scweieaWorkflowId = lens _scweieaWorkflowId (\s a -> s {_scweieaWorkflowId = a})

-- | The type of the child workflow execution.
scweieaWorkflowType :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes WorkflowType
scweieaWorkflowType = lens _scweieaWorkflowType (\s a -> s {_scweieaWorkflowType = a})

-- | The name of the task list used for the decision tasks of the child workflow execution.
scweieaTaskList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes TaskList
scweieaTaskList = lens _scweieaTaskList (\s a -> s {_scweieaTaskList = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
scweieaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes Integer
scweieaDecisionTaskCompletedEventId = lens _scweieaDecisionTaskCompletedEventId (\s a -> s {_scweieaDecisionTaskCompletedEventId = a})

-- | The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
scweieaChildPolicy :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes ChildPolicy
scweieaChildPolicy = lens _scweieaChildPolicy (\s a -> s {_scweieaChildPolicy = a})

instance
  FromJSON
    StartChildWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    withObject
      "StartChildWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          StartChildWorkflowExecutionInitiatedEventAttributes'
            <$> (x .:? "control")
            <*> (x .:? "tagList" .!= mempty)
            <*> (x .:? "taskStartToCloseTimeout")
            <*> (x .:? "lambdaRole")
            <*> (x .:? "input")
            <*> (x .:? "executionStartToCloseTimeout")
            <*> (x .:? "taskPriority")
            <*> (x .: "workflowId")
            <*> (x .: "workflowType")
            <*> (x .: "taskList")
            <*> (x .: "decisionTaskCompletedEventId")
            <*> (x .: "childPolicy")
      )

instance
  Hashable
    StartChildWorkflowExecutionInitiatedEventAttributes

instance NFData StartChildWorkflowExecutionInitiatedEventAttributes
