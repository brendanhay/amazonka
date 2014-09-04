{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.StartWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts an execution of the workflow type in the specified domain using the
-- provided workflowId and input data. This action returns the newly started
-- workflow execution. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the following parameters by using a Condition element with the appropriate
-- keys. tagList.member.0: The key is swf:tagList.member.0. tagList.member.1:
-- The key is swf:tagList.member.1. tagList.member.2: The key is
-- swf:tagList.member.2. tagList.member.3: The key is swf:tagList.member.3.
-- tagList.member.4: The key is swf:tagList.member.4. taskList: String
-- constraint. The key is swf:taskList.name. name: String constraint. The key
-- is swf:workflowType.name. version: String constraint. The key is
-- swf:workflowType.version. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. StartWorkflowExecution Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sat, 14 Jan 2012 22:45:13 GMT X-Amz-Target:
-- SimpleWorkflowService.StartWorkflowExecution Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=aYxuqLX+TO91kPVg+jh+aA8PWxQazQRN2+SZUGdOgU0=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 417 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"}, "taskList": {"name":
-- "specialTaskList"}, "input":
-- "arbitrary-string-that-is-meaningful-to-the-workflow",
-- "executionStartToCloseTimeout": "1800", "tagList": ["music purchase",
-- "digital", "ricoh-the-dog"], "taskStartToCloseTimeout": "600",
-- "childPolicy": "TERMINATE"} HTTP/1.1 200 OK Content-Length: 48
-- Content-Type: application/json x-amzn-RequestId:
-- 6c25f6e6-3f01-11e1-9a27-0760db01a4a8
-- {"runId":"1e536162-f1ea-48b0-85f3-aade88eef2f7"}.
module Network.AWS.SWF.V2012_01_25.StartWorkflowExecution
    (
    -- * Request
      StartWorkflowExecution
    -- ** Request constructor
    , mkStartWorkflowExecutionInput
    -- ** Request lenses
    , swejDomain
    , swejWorkflowId
    , swejWorkflowType
    , swejTaskList
    , swejInput
    , swejExecutionStartToCloseTimeout
    , swejTagList
    , swejTaskStartToCloseTimeout
    , swejChildPolicy

    -- * Response
    , StartWorkflowExecutionResponse
    -- ** Response lenses
    , rrrnRunId
    ) where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartWorkflowExecution' request.
mkStartWorkflowExecutionInput :: Text -- ^ 'swejDomain'
                              -> Text -- ^ 'swejWorkflowId'
                              -> WorkflowType -- ^ 'swejWorkflowType'
                              -> StartWorkflowExecution
mkStartWorkflowExecutionInput p1 p2 p3 = StartWorkflowExecution
    { _swejDomain = p1
    , _swejWorkflowId = p2
    , _swejWorkflowType = p3
    , _swejTaskList = Nothing
    , _swejInput = Nothing
    , _swejExecutionStartToCloseTimeout = Nothing
    , _swejTagList = mempty
    , _swejTaskStartToCloseTimeout = Nothing
    , _swejChildPolicy = Nothing
    }
{-# INLINE mkStartWorkflowExecutionInput #-}

data StartWorkflowExecution = StartWorkflowExecution
    { _swejDomain :: Text
      -- ^ The name of the domain in which the workflow execution is
      -- created.
    , _swejWorkflowId :: Text
      -- ^ The user defined identifier associated with the workflow
      -- execution. You can use this to associate a custom identifier with
      -- the workflow execution. You may specify the same identifier if a
      -- workflow execution is logically a restart of a previous
      -- execution. You cannot have two open workflow executions with the
      -- same workflowId at the same time. The specified string must not
      -- start or end with whitespace. It must not contain a : (colon), /
      -- (slash), | (vertical bar), or any control characters
      -- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the
      -- literal string &quot;arn&quot;.
    , _swejWorkflowType :: WorkflowType
      -- ^ The type of the workflow to start.
    , _swejTaskList :: Maybe TaskList
      -- ^ The task list to use for the decision tasks generated for this
      -- workflow execution. This overrides the defaultTaskList specified
      -- when registering the workflow type. A task list for this workflow
      -- execution must be specified either as a default for the workflow
      -- type or through this parameter. If neither this parameter is set
      -- nor a default task list was specified at registration time then a
      -- fault will be returned. The specified string must not start or
      -- end with whitespace. It must not contain a : (colon), / (slash),
      -- | (vertical bar), or any control characters (\u0000-\u001f |
      -- \u007f - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    , _swejInput :: Maybe Text
      -- ^ The input for the workflow execution. This is a free form string
      -- which should be meaningful to the workflow you are starting. This
      -- input is made available to the new workflow execution in the
      -- WorkflowExecutionStarted history event.
    , _swejExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration for this workflow execution. This overrides
      -- the defaultExecutionStartToCloseTimeout specified when
      -- registering the workflow type. The duration is specified in
      -- seconds. The valid values are integers greater than or equal to
      -- 0. Exceeding this limit will cause the workflow execution to time
      -- out. Unlike some of the other timeout parameters in Amazon SWF,
      -- you cannot specify a value of "NONE" for this timeout; there is a
      -- one-year max limit on the time that a workflow execution can run.
      -- An execution start-to-close timeout must be specified either
      -- through this parameter or as a default when the workflow type is
      -- registered. If neither this parameter nor a default execution
      -- start-to-close timeout is specified, a fault is returned.
    , _swejTagList :: [Text]
      -- ^ The list of tags to associate with the workflow execution. You
      -- can specify a maximum of 5 tags. You can list workflow executions
      -- with a specific tag by calling ListOpenWorkflowExecutions or
      -- ListClosedWorkflowExecutions and specifying a TagFilter.
    , _swejTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for this
      -- workflow execution. This parameter overrides the
      -- defaultTaskStartToCloseTimout specified when registering the
      -- workflow type using RegisterWorkflowType. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for
      -- this workflow execution must be specified either as a default for
      -- the workflow type or through this parameter. If neither this
      -- parameter is set nor a default task start-to-close timeout was
      -- specified at registration time then a fault will be returned.
    , _swejChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow
      -- executions of this workflow execution if it is terminated, by
      -- calling the TerminateWorkflowExecution action explicitly or due
      -- to an expired timeout. This policy overrides the default child
      -- policy specified when registering the workflow type using
      -- RegisterWorkflowType. The supported child policies are:
      -- TERMINATE: the child executions will be terminated.
      -- REQUEST_CANCEL: a request to cancel will be attempted for each
      -- child execution by recording a WorkflowExecutionCancelRequested
      -- event in its history. It is up to the decider to take appropriate
      -- actions when it receives an execution history with this event.
      -- ABANDON: no action will be taken. The child executions will
      -- continue to run. A child policy for this workflow execution must
      -- be specified either as a default for the workflow type or through
      -- this parameter. If neither this parameter is set nor a default
      -- child policy was specified at registration time then a fault will
      -- be returned.
    } deriving (Show, Generic)

-- | The name of the domain in which the workflow execution is created.
swejDomain :: Lens' StartWorkflowExecution (Text)
swejDomain = lens _swejDomain (\s a -> s { _swejDomain = a })
{-# INLINE swejDomain #-}

-- | The user defined identifier associated with the workflow execution. You can
-- use this to associate a custom identifier with the workflow execution. You
-- may specify the same identifier if a workflow execution is logically a
-- restart of a previous execution. You cannot have two open workflow
-- executions with the same workflowId at the same time. The specified string
-- must not start or end with whitespace. It must not contain a : (colon), /
-- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
-- \u007f - \u009f). Also, it must not contain the literal string
-- &quot;arn&quot;.
swejWorkflowId :: Lens' StartWorkflowExecution (Text)
swejWorkflowId = lens _swejWorkflowId (\s a -> s { _swejWorkflowId = a })
{-# INLINE swejWorkflowId #-}

-- | The type of the workflow to start.
swejWorkflowType :: Lens' StartWorkflowExecution (WorkflowType)
swejWorkflowType = lens _swejWorkflowType (\s a -> s { _swejWorkflowType = a })
{-# INLINE swejWorkflowType #-}

-- | The task list to use for the decision tasks generated for this workflow
-- execution. This overrides the defaultTaskList specified when registering
-- the workflow type. A task list for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task list was
-- specified at registration time then a fault will be returned. The specified
-- string must not start or end with whitespace. It must not contain a :
-- (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
swejTaskList :: Lens' StartWorkflowExecution (Maybe TaskList)
swejTaskList = lens _swejTaskList (\s a -> s { _swejTaskList = a })
{-# INLINE swejTaskList #-}

-- | The input for the workflow execution. This is a free form string which
-- should be meaningful to the workflow you are starting. This input is made
-- available to the new workflow execution in the WorkflowExecutionStarted
-- history event.
swejInput :: Lens' StartWorkflowExecution (Maybe Text)
swejInput = lens _swejInput (\s a -> s { _swejInput = a })
{-# INLINE swejInput #-}

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the workflow
-- type. The duration is specified in seconds. The valid values are integers
-- greater than or equal to 0. Exceeding this limit will cause the workflow
-- execution to time out. Unlike some of the other timeout parameters in
-- Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is
-- a one-year max limit on the time that a workflow execution can run. An
-- execution start-to-close timeout must be specified either through this
-- parameter or as a default when the workflow type is registered. If neither
-- this parameter nor a default execution start-to-close timeout is specified,
-- a fault is returned.
swejExecutionStartToCloseTimeout :: Lens' StartWorkflowExecution (Maybe Text)
swejExecutionStartToCloseTimeout = lens _swejExecutionStartToCloseTimeout (\s a -> s { _swejExecutionStartToCloseTimeout = a })
{-# INLINE swejExecutionStartToCloseTimeout #-}

-- | The list of tags to associate with the workflow execution. You can specify
-- a maximum of 5 tags. You can list workflow executions with a specific tag
-- by calling ListOpenWorkflowExecutions or ListClosedWorkflowExecutions and
-- specifying a TagFilter.
swejTagList :: Lens' StartWorkflowExecution ([Text])
swejTagList = lens _swejTagList (\s a -> s { _swejTagList = a })
{-# INLINE swejTagList #-}

-- | Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the defaultTaskStartToCloseTimout
-- specified when registering the workflow type using RegisterWorkflowType.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration. A task start-to-close timeout for this workflow
-- execution must be specified either as a default for the workflow type or
-- through this parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault will
-- be returned.
swejTaskStartToCloseTimeout :: Lens' StartWorkflowExecution (Maybe Text)
swejTaskStartToCloseTimeout = lens _swejTaskStartToCloseTimeout (\s a -> s { _swejTaskStartToCloseTimeout = a })
{-# INLINE swejTaskStartToCloseTimeout #-}

-- | If set, specifies the policy to use for the child workflow executions of
-- this workflow execution if it is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired timeout.
-- This policy overrides the default child policy specified when registering
-- the workflow type using RegisterWorkflowType. The supported child policies
-- are: TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run. A child policy for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default child policy was
-- specified at registration time then a fault will be returned.
swejChildPolicy :: Lens' StartWorkflowExecution (Maybe ChildPolicy)
swejChildPolicy = lens _swejChildPolicy (\s a -> s { _swejChildPolicy = a })
{-# INLINE swejChildPolicy #-}

instance ToPath StartWorkflowExecution

instance ToQuery StartWorkflowExecution

instance ToHeaders StartWorkflowExecution

instance ToJSON StartWorkflowExecution

newtype StartWorkflowExecutionResponse = StartWorkflowExecutionResponse
    { _rrrnRunId :: Maybe Text
      -- ^ The runId of a workflow execution. This Id is generated by the
      -- service and can be used to uniquely identify the workflow
      -- execution within a domain.
    } deriving (Show, Generic)

-- | The runId of a workflow execution. This Id is generated by the service and
-- can be used to uniquely identify the workflow execution within a domain.
rrrnRunId :: Lens' StartWorkflowExecutionResponse (Maybe Text)
rrrnRunId = lens _rrrnRunId (\s a -> s { _rrrnRunId = a })
{-# INLINE rrrnRunId #-}

instance FromJSON StartWorkflowExecutionResponse

instance AWSRequest StartWorkflowExecution where
    type Sv StartWorkflowExecution = SWF
    type Rs StartWorkflowExecution = StartWorkflowExecutionResponse

    request = get
    response _ = jsonResponse
