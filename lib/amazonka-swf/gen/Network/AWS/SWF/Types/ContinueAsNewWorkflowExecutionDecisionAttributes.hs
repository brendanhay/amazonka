{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
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
--     * @tag@ – A tag used to identify the workflow execution
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
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
-- /See:/ 'continueAsNewWorkflowExecutionDecisionAttributes' smart constructor.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes'
  { _canwedaTagList ::
      !( Maybe
           [Text]
       ),
    _canwedaTaskStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _canwedaLambdaRole ::
      !( Maybe
           Text
       ),
    _canwedaInput ::
      !( Maybe
           Text
       ),
    _canwedaWorkflowTypeVersion ::
      !( Maybe
           Text
       ),
    _canwedaExecutionStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _canwedaTaskList ::
      !( Maybe
           TaskList
       ),
    _canwedaTaskPriority ::
      !( Maybe
           Text
       ),
    _canwedaChildPolicy ::
      !( Maybe
           ChildPolicy
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ContinueAsNewWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'canwedaTagList' - The list of tags to associate with the new workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- * 'canwedaTaskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for the new workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'canwedaLambdaRole' - The IAM role to attach to the new (continued) execution.
--
-- * 'canwedaInput' - The input provided to the new workflow execution.
--
-- * 'canwedaWorkflowTypeVersion' - The version of the workflow to start.
--
-- * 'canwedaExecutionStartToCloseTimeout' - If set, specifies the total duration for this workflow execution. This overrides the @defaultExecutionStartToCloseTimeout@ specified when registering the workflow type. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'canwedaTaskList' - The task list to use for the decisions of the new (continued) workflow execution.
--
-- * 'canwedaTaskPriority' - The task priority that, if set, specifies the priority for the decision tasks for this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'canwedaChildPolicy' - If set, specifies the policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
continueAsNewWorkflowExecutionDecisionAttributes ::
  ContinueAsNewWorkflowExecutionDecisionAttributes
continueAsNewWorkflowExecutionDecisionAttributes =
  ContinueAsNewWorkflowExecutionDecisionAttributes'
    { _canwedaTagList =
        Nothing,
      _canwedaTaskStartToCloseTimeout = Nothing,
      _canwedaLambdaRole = Nothing,
      _canwedaInput = Nothing,
      _canwedaWorkflowTypeVersion = Nothing,
      _canwedaExecutionStartToCloseTimeout =
        Nothing,
      _canwedaTaskList = Nothing,
      _canwedaTaskPriority = Nothing,
      _canwedaChildPolicy = Nothing
    }

-- | The list of tags to associate with the new workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
canwedaTagList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes [Text]
canwedaTagList = lens _canwedaTagList (\s a -> s {_canwedaTagList = a}) . _Default . _Coerce

-- | Specifies the maximum duration of decision tasks for the new workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
canwedaTaskStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaTaskStartToCloseTimeout = lens _canwedaTaskStartToCloseTimeout (\s a -> s {_canwedaTaskStartToCloseTimeout = a})

-- | The IAM role to attach to the new (continued) execution.
canwedaLambdaRole :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaLambdaRole = lens _canwedaLambdaRole (\s a -> s {_canwedaLambdaRole = a})

-- | The input provided to the new workflow execution.
canwedaInput :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaInput = lens _canwedaInput (\s a -> s {_canwedaInput = a})

-- | The version of the workflow to start.
canwedaWorkflowTypeVersion :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaWorkflowTypeVersion = lens _canwedaWorkflowTypeVersion (\s a -> s {_canwedaWorkflowTypeVersion = a})

-- | If set, specifies the total duration for this workflow execution. This overrides the @defaultExecutionStartToCloseTimeout@ specified when registering the workflow type. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
canwedaExecutionStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaExecutionStartToCloseTimeout = lens _canwedaExecutionStartToCloseTimeout (\s a -> s {_canwedaExecutionStartToCloseTimeout = a})

-- | The task list to use for the decisions of the new (continued) workflow execution.
canwedaTaskList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe TaskList)
canwedaTaskList = lens _canwedaTaskList (\s a -> s {_canwedaTaskList = a})

-- | The task priority that, if set, specifies the priority for the decision tasks for this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
canwedaTaskPriority :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaTaskPriority = lens _canwedaTaskPriority (\s a -> s {_canwedaTaskPriority = a})

-- | If set, specifies the policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
canwedaChildPolicy :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
canwedaChildPolicy = lens _canwedaChildPolicy (\s a -> s {_canwedaChildPolicy = a})

instance Hashable ContinueAsNewWorkflowExecutionDecisionAttributes

instance NFData ContinueAsNewWorkflowExecutionDecisionAttributes

instance ToJSON ContinueAsNewWorkflowExecutionDecisionAttributes where
  toJSON ContinueAsNewWorkflowExecutionDecisionAttributes' {..} =
    object
      ( catMaybes
          [ ("tagList" .=) <$> _canwedaTagList,
            ("taskStartToCloseTimeout" .=) <$> _canwedaTaskStartToCloseTimeout,
            ("lambdaRole" .=) <$> _canwedaLambdaRole,
            ("input" .=) <$> _canwedaInput,
            ("workflowTypeVersion" .=) <$> _canwedaWorkflowTypeVersion,
            ("executionStartToCloseTimeout" .=)
              <$> _canwedaExecutionStartToCloseTimeout,
            ("taskList" .=) <$> _canwedaTaskList,
            ("taskPriority" .=) <$> _canwedaTaskPriority,
            ("childPolicy" .=) <$> _canwedaChildPolicy
          ]
      )
