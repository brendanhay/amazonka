{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @StartChildWorkflowExecution@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagList.member.N@ – The key is "swf:tagList.N" where N is the tag number from 0 to 4, inclusive.
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--     * @workflowType.name@ – String constraint. The key is @swf:workflowType.name@ .
--
--     * @workflowType.version@ – String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- /See:/ 'startChildWorkflowExecutionDecisionAttributes' smart constructor.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes'
  { _scwedaControl ::
      !( Maybe
           Text
       ),
    _scwedaTagList ::
      !( Maybe
           [Text]
       ),
    _scwedaTaskStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _scwedaLambdaRole ::
      !( Maybe
           Text
       ),
    _scwedaInput ::
      !( Maybe
           Text
       ),
    _scwedaExecutionStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _scwedaTaskList ::
      !( Maybe
           TaskList
       ),
    _scwedaTaskPriority ::
      !( Maybe
           Text
       ),
    _scwedaChildPolicy ::
      !( Maybe
           ChildPolicy
       ),
    _scwedaWorkflowType ::
      !WorkflowType,
    _scwedaWorkflowId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartChildWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scwedaControl' - The data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the child workflow execution.
--
-- * 'scwedaTagList' - The list of tags to associate with the child workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- * 'scwedaTaskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'scwedaLambdaRole' - The IAM role attached to the child workflow execution.
--
-- * 'scwedaInput' - The input to be provided to the workflow execution.
--
-- * 'scwedaExecutionStartToCloseTimeout' - The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'scwedaTaskList' - The name of the task list to be used for decision tasks of the child workflow execution. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'scwedaTaskPriority' - A task priority that, if set, specifies the priority for a decision task of this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'scwedaChildPolicy' - If set, specifies the policy to use for the child workflow executions if the workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
--
-- * 'scwedaWorkflowType' - The type of the workflow execution to be started.
--
-- * 'scwedaWorkflowId' - The @workflowId@ of the workflow execution. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
startChildWorkflowExecutionDecisionAttributes ::
  -- | 'scwedaWorkflowType'
  WorkflowType ->
  -- | 'scwedaWorkflowId'
  Text ->
  StartChildWorkflowExecutionDecisionAttributes
startChildWorkflowExecutionDecisionAttributes
  pWorkflowType_
  pWorkflowId_ =
    StartChildWorkflowExecutionDecisionAttributes'
      { _scwedaControl =
          Nothing,
        _scwedaTagList = Nothing,
        _scwedaTaskStartToCloseTimeout = Nothing,
        _scwedaLambdaRole = Nothing,
        _scwedaInput = Nothing,
        _scwedaExecutionStartToCloseTimeout = Nothing,
        _scwedaTaskList = Nothing,
        _scwedaTaskPriority = Nothing,
        _scwedaChildPolicy = Nothing,
        _scwedaWorkflowType = pWorkflowType_,
        _scwedaWorkflowId = pWorkflowId_
      }

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the child workflow execution.
scwedaControl :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaControl = lens _scwedaControl (\s a -> s {_scwedaControl = a})

-- | The list of tags to associate with the child workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
scwedaTagList :: Lens' StartChildWorkflowExecutionDecisionAttributes [Text]
scwedaTagList = lens _scwedaTagList (\s a -> s {_scwedaTagList = a}) . _Default . _Coerce

-- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
scwedaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaTaskStartToCloseTimeout = lens _scwedaTaskStartToCloseTimeout (\s a -> s {_scwedaTaskStartToCloseTimeout = a})

-- | The IAM role attached to the child workflow execution.
scwedaLambdaRole :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaLambdaRole = lens _scwedaLambdaRole (\s a -> s {_scwedaLambdaRole = a})

-- | The input to be provided to the workflow execution.
scwedaInput :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaInput = lens _scwedaInput (\s a -> s {_scwedaInput = a})

-- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
scwedaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaExecutionStartToCloseTimeout = lens _scwedaExecutionStartToCloseTimeout (\s a -> s {_scwedaExecutionStartToCloseTimeout = a})

-- | The name of the task list to be used for decision tasks of the child workflow execution. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
scwedaTaskList :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe TaskList)
scwedaTaskList = lens _scwedaTaskList (\s a -> s {_scwedaTaskList = a})

-- | A task priority that, if set, specifies the priority for a decision task of this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
scwedaTaskPriority :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaTaskPriority = lens _scwedaTaskPriority (\s a -> s {_scwedaTaskPriority = a})

-- | If set, specifies the policy to use for the child workflow executions if the workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
scwedaChildPolicy :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
scwedaChildPolicy = lens _scwedaChildPolicy (\s a -> s {_scwedaChildPolicy = a})

-- | The type of the workflow execution to be started.
scwedaWorkflowType :: Lens' StartChildWorkflowExecutionDecisionAttributes WorkflowType
scwedaWorkflowType = lens _scwedaWorkflowType (\s a -> s {_scwedaWorkflowType = a})

-- | The @workflowId@ of the workflow execution. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
scwedaWorkflowId :: Lens' StartChildWorkflowExecutionDecisionAttributes Text
scwedaWorkflowId = lens _scwedaWorkflowId (\s a -> s {_scwedaWorkflowId = a})

instance Hashable StartChildWorkflowExecutionDecisionAttributes

instance NFData StartChildWorkflowExecutionDecisionAttributes

instance ToJSON StartChildWorkflowExecutionDecisionAttributes where
  toJSON StartChildWorkflowExecutionDecisionAttributes' {..} =
    object
      ( catMaybes
          [ ("control" .=) <$> _scwedaControl,
            ("tagList" .=) <$> _scwedaTagList,
            ("taskStartToCloseTimeout" .=) <$> _scwedaTaskStartToCloseTimeout,
            ("lambdaRole" .=) <$> _scwedaLambdaRole,
            ("input" .=) <$> _scwedaInput,
            ("executionStartToCloseTimeout" .=)
              <$> _scwedaExecutionStartToCloseTimeout,
            ("taskList" .=) <$> _scwedaTaskList,
            ("taskPriority" .=) <$> _scwedaTaskPriority,
            ("childPolicy" .=) <$> _scwedaChildPolicy,
            Just ("workflowType" .= _scwedaWorkflowType),
            Just ("workflowId" .= _scwedaWorkflowId)
          ]
      )
