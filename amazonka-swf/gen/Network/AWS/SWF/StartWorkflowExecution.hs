{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.StartWorkflowExecution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an execution of the workflow type in the specified domain using
-- the provided 'workflowId' and input data.
--
-- This action returns the newly started workflow execution.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a 'Resource' element with the domain name to limit the action to
--     only specified domains.
-- -   Use an 'Action' element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a 'Condition' element
--     with the appropriate keys.
--     -   'tagList.member.0': The key is 'swf:tagList.member.0'.
--     -   'tagList.member.1': The key is 'swf:tagList.member.1'.
--     -   'tagList.member.2': The key is 'swf:tagList.member.2'.
--     -   'tagList.member.3': The key is 'swf:tagList.member.3'.
--     -   'tagList.member.4': The key is 'swf:tagList.member.4'.
--     -   'taskList': String constraint. The key is 'swf:taskList.name'.
--     -   'workflowType.name': String constraint. The key is
--         'swf:workflowType.name'.
--     -   'workflowType.version': String constraint. The key is
--         'swf:workflowType.version'.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_StartWorkflowExecution.html AWS API Reference> for StartWorkflowExecution.
module Network.AWS.SWF.StartWorkflowExecution
    (
    -- * Creating a Request
      startWorkflowExecution
    , StartWorkflowExecution
    -- * Request Lenses
    , sTagList
    , sTaskStartToCloseTimeout
    , sLambdaRole
    , sInput
    , sExecutionStartToCloseTimeout
    , sTaskList
    , sTaskPriority
    , sChildPolicy
    , sDomain
    , sWorkflowId
    , sWorkflowType

    -- * Destructuring the Response
    , startWorkflowExecutionResponse
    , StartWorkflowExecutionResponse
    -- * Response Lenses
    , swersRunId
    , swersStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types
import           Network.AWS.SWF.Types.Product

-- | /See:/ 'startWorkflowExecution' smart constructor.
data StartWorkflowExecution = StartWorkflowExecution'
    { _sTagList                      :: !(Maybe [Text])
    , _sTaskStartToCloseTimeout      :: !(Maybe Text)
    , _sLambdaRole                   :: !(Maybe Text)
    , _sInput                        :: !(Maybe Text)
    , _sExecutionStartToCloseTimeout :: !(Maybe Text)
    , _sTaskList                     :: !(Maybe TaskList)
    , _sTaskPriority                 :: !(Maybe Text)
    , _sChildPolicy                  :: !(Maybe ChildPolicy)
    , _sDomain                       :: !Text
    , _sWorkflowId                   :: !Text
    , _sWorkflowType                 :: !WorkflowType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartWorkflowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTagList'
--
-- * 'sTaskStartToCloseTimeout'
--
-- * 'sLambdaRole'
--
-- * 'sInput'
--
-- * 'sExecutionStartToCloseTimeout'
--
-- * 'sTaskList'
--
-- * 'sTaskPriority'
--
-- * 'sChildPolicy'
--
-- * 'sDomain'
--
-- * 'sWorkflowId'
--
-- * 'sWorkflowType'
startWorkflowExecution
    :: Text -- ^ 'sDomain'
    -> Text -- ^ 'sWorkflowId'
    -> WorkflowType -- ^ 'sWorkflowType'
    -> StartWorkflowExecution
startWorkflowExecution pDomain_ pWorkflowId_ pWorkflowType_ =
    StartWorkflowExecution'
    { _sTagList = Nothing
    , _sTaskStartToCloseTimeout = Nothing
    , _sLambdaRole = Nothing
    , _sInput = Nothing
    , _sExecutionStartToCloseTimeout = Nothing
    , _sTaskList = Nothing
    , _sTaskPriority = Nothing
    , _sChildPolicy = Nothing
    , _sDomain = pDomain_
    , _sWorkflowId = pWorkflowId_
    , _sWorkflowType = pWorkflowType_
    }

-- | The list of tags to associate with the workflow execution. You can
-- specify a maximum of 5 tags. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
sTagList :: Lens' StartWorkflowExecution [Text]
sTagList = lens _sTagList (\ s a -> s{_sTagList = a}) . _Default . _Coerce;

-- | Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the 'defaultTaskStartToCloseTimout'
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- A task start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- will be returned.
sTaskStartToCloseTimeout :: Lens' StartWorkflowExecution (Maybe Text)
sTaskStartToCloseTimeout = lens _sTaskStartToCloseTimeout (\ s a -> s{_sTaskStartToCloseTimeout = a});

-- | The ARN of an IAM role that authorizes Amazon SWF to invoke AWS Lambda
-- functions.
--
-- In order for this workflow execution to invoke AWS Lambda functions, an
-- appropriate IAM role must be specified either as a default for the
-- workflow type or through this field.
sLambdaRole :: Lens' StartWorkflowExecution (Maybe Text)
sLambdaRole = lens _sLambdaRole (\ s a -> s{_sLambdaRole = a});

-- | The input for the workflow execution. This is a free form string which
-- should be meaningful to the workflow you are starting. This 'input' is
-- made available to the new workflow execution in the
-- 'WorkflowExecutionStarted' history event.
sInput :: Lens' StartWorkflowExecution (Maybe Text)
sInput = lens _sInput (\ s a -> s{_sInput = a});

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the
-- workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. Exceeding this limit will cause the workflow execution to time
-- out. Unlike some of the other timeout parameters in Amazon SWF, you
-- cannot specify a value of \"NONE\" for this timeout; there is a one-year
-- max limit on the time that a workflow execution can run.
--
-- An execution start-to-close timeout must be specified either through
-- this parameter or as a default when the workflow type is registered. If
-- neither this parameter nor a default execution start-to-close timeout is
-- specified, a fault is returned.
sExecutionStartToCloseTimeout :: Lens' StartWorkflowExecution (Maybe Text)
sExecutionStartToCloseTimeout = lens _sExecutionStartToCloseTimeout (\ s a -> s{_sExecutionStartToCloseTimeout = a});

-- | The task list to use for the decision tasks generated for this workflow
-- execution. This overrides the 'defaultTaskList' specified when
-- registering the workflow type.
--
-- A task list for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default task list was specified at registration
-- time then a fault will be returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '\/' (slash), '|' (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
sTaskList :: Lens' StartWorkflowExecution (Maybe TaskList)
sTaskList = lens _sTaskList (\ s a -> s{_sTaskList = a});

-- | The task priority to use for this workflow execution. This will override
-- any default priority that was assigned when the workflow type was
-- registered. If not set, then the default task priority for the workflow
-- type will be used. Valid values are integers that range from Java\'s
-- 'Integer.MIN_VALUE' (-2147483648) to 'Integer.MAX_VALUE' (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
sTaskPriority :: Lens' StartWorkflowExecution (Maybe Text)
sTaskPriority = lens _sTaskPriority (\ s a -> s{_sTaskPriority = a});

-- | If set, specifies the policy to use for the child workflow executions of
-- this workflow execution if it is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This policy overrides the default child policy specified when
-- registering the workflow type using RegisterWorkflowType.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a 'WorkflowExecutionCancelRequested'
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault will be returned.
sChildPolicy :: Lens' StartWorkflowExecution (Maybe ChildPolicy)
sChildPolicy = lens _sChildPolicy (\ s a -> s{_sChildPolicy = a});

-- | The name of the domain in which the workflow execution is created.
sDomain :: Lens' StartWorkflowExecution Text
sDomain = lens _sDomain (\ s a -> s{_sDomain = a});

-- | The user defined identifier associated with the workflow execution. You
-- can use this to associate a custom identifier with the workflow
-- execution. You may specify the same identifier if a workflow execution
-- is logically a /restart/ of a previous execution. You cannot have two
-- open workflow executions with the same 'workflowId' at the same time.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '\/' (slash), '|' (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
sWorkflowId :: Lens' StartWorkflowExecution Text
sWorkflowId = lens _sWorkflowId (\ s a -> s{_sWorkflowId = a});

-- | The type of the workflow to start.
sWorkflowType :: Lens' StartWorkflowExecution WorkflowType
sWorkflowType = lens _sWorkflowType (\ s a -> s{_sWorkflowType = a});

instance AWSRequest StartWorkflowExecution where
        type Rs StartWorkflowExecution =
             StartWorkflowExecutionResponse
        request = postJSON sWF
        response
          = receiveJSON
              (\ s h x ->
                 StartWorkflowExecutionResponse' <$>
                   (x .?> "runId") <*> (pure (fromEnum s)))

instance ToHeaders StartWorkflowExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.StartWorkflowExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON StartWorkflowExecution where
        toJSON StartWorkflowExecution'{..}
          = object
              (catMaybes
                 [("tagList" .=) <$> _sTagList,
                  ("taskStartToCloseTimeout" .=) <$>
                    _sTaskStartToCloseTimeout,
                  ("lambdaRole" .=) <$> _sLambdaRole,
                  ("input" .=) <$> _sInput,
                  ("executionStartToCloseTimeout" .=) <$>
                    _sExecutionStartToCloseTimeout,
                  ("taskList" .=) <$> _sTaskList,
                  ("taskPriority" .=) <$> _sTaskPriority,
                  ("childPolicy" .=) <$> _sChildPolicy,
                  Just ("domain" .= _sDomain),
                  Just ("workflowId" .= _sWorkflowId),
                  Just ("workflowType" .= _sWorkflowType)])

instance ToPath StartWorkflowExecution where
        toPath = const "/"

instance ToQuery StartWorkflowExecution where
        toQuery = const mempty

-- | Specifies the 'runId' of a workflow execution.
--
-- /See:/ 'startWorkflowExecutionResponse' smart constructor.
data StartWorkflowExecutionResponse = StartWorkflowExecutionResponse'
    { _swersRunId  :: !(Maybe Text)
    , _swersStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartWorkflowExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swersRunId'
--
-- * 'swersStatus'
startWorkflowExecutionResponse
    :: Int -- ^ 'swersStatus'
    -> StartWorkflowExecutionResponse
startWorkflowExecutionResponse pStatus_ =
    StartWorkflowExecutionResponse'
    { _swersRunId = Nothing
    , _swersStatus = pStatus_
    }

-- | The 'runId' of a workflow execution. This ID is generated by the service
-- and can be used to uniquely identify the workflow execution within a
-- domain.
swersRunId :: Lens' StartWorkflowExecutionResponse (Maybe Text)
swersRunId = lens _swersRunId (\ s a -> s{_swersRunId = a});

-- | The response status code.
swersStatus :: Lens' StartWorkflowExecutionResponse Int
swersStatus = lens _swersStatus (\ s a -> s{_swersStatus = a});
