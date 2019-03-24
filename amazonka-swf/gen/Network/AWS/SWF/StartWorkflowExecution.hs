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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an execution of the workflow type in the specified domain using the provided @workflowId@ and input data.
--
--
-- This action returns the newly started workflow execution.
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagList.member.0@ : The key is @swf:tagList.member.0@ .
--
--     * @tagList.member.1@ : The key is @swf:tagList.member.1@ .
--
--     * @tagList.member.2@ : The key is @swf:tagList.member.2@ .
--
--     * @tagList.member.3@ : The key is @swf:tagList.member.3@ .
--
--     * @tagList.member.4@ : The key is @swf:tagList.member.4@ .
--
--     * @taskList@ : String constraint. The key is @swf:taskList.name@ .
--
--     * @workflowType.name@ : String constraint. The key is @swf:workflowType.name@ .
--
--     * @workflowType.version@ : String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
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
    , swersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartWorkflowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTagList' - The list of tags to associate with the workflow execution. You can specify a maximum of 5 tags. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- * 'sTaskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'sLambdaRole' - The IAM role to attach to this workflow execution.
--
-- * 'sInput' - The input for the workflow execution. This is a free form string which should be meaningful to the workflow you are starting. This @input@ is made available to the new workflow execution in the @WorkflowExecutionStarted@ history event.
--
-- * 'sExecutionStartToCloseTimeout' - The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type. The duration is specified in seconds; an integer greater than or equal to @0@ . Exceeding this limit causes the workflow execution to time out. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is a one-year max limit on the time that a workflow execution can run.
--
-- * 'sTaskList' - The task list to use for the decision tasks generated for this workflow execution. This overrides the @defaultTaskList@ specified when registering the workflow type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'sTaskPriority' - The task priority to use for this workflow execution. This overrides any default priority that was assigned when the workflow type was registered. If not set, then the default task priority for the workflow type is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'sChildPolicy' - If set, specifies the policy to use for the child workflow executions of this workflow execution if it is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' . The supported child policies are:     * @TERMINATE@
