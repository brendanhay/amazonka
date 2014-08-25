{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified workflow execution including its
-- type and some statistics. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeWorkflowExecution Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:05:18 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=ufQVcSkfUyGPLiS8xbkEBqEc2PmEEE/3Lb9Kr8yozs8=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 127 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "execution": {"workflowId": "20110927-T-1", "runId":
-- "06b8f87a-24b3-40b6-9ceb-9676f28e9493"} } HTTP/1.1 200 OK Content-Length:
-- 577 Content-Type: application/json x-amzn-RequestId:
-- 5f85ef79-3f1d-11e1-9e8f-57bb03e21482 {"executionConfiguration":
-- {"childPolicy": "TERMINATE", "executionStartToCloseTimeout": "3600",
-- "taskList": {"name": "specialTaskList"}, "taskStartToCloseTimeout": "600"},
-- "executionInfo": {"cancelRequested": false, "execution": {"runId":
-- "06b8f87a-24b3-40b6-9ceb-9676f28e9493", "workflowId": "20110927-T-1"},
-- "executionStatus": "OPEN", "startTimestamp": 1326592619.474, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }, "openCounts":
-- {"openActivityTasks": 0, "openChildWorkflowExecutions": 0,
-- "openDecisionTasks": 1, "openTimers": 0} }.
module Network.AWS.SWF.V2012_01_25.DescribeWorkflowExecution where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeWorkflowExecution = DescribeWorkflowExecution
    { _dweiDomain :: Text
      -- ^ The name of the domain containing the workflow execution.
    , _dweiExecution :: WorkflowExecution
      -- ^ The workflow execution to describe.
    } deriving (Show, Generic)

makeLenses ''DescribeWorkflowExecution

instance ToPath DescribeWorkflowExecution

instance ToQuery DescribeWorkflowExecution

instance ToHeaders DescribeWorkflowExecution

instance ToJSON DescribeWorkflowExecution

data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse
    { _wedExecutionConfiguration :: WorkflowExecutionConfiguration
      -- ^ The configuration settings for this workflow execution including
      -- timeout values, tasklist etc.
    , _wedExecutionInfo :: WorkflowExecutionInfo
      -- ^ Information about the workflow execution.
    , _wedOpenCounts :: WorkflowExecutionOpenCounts
      -- ^ The number of tasks for this workflow execution. This includes
      -- open and closed tasks of all types.
    , _wedLatestExecutionContext :: Maybe Text
      -- ^ The latest executionContext provided by the decider for this
      -- workflow execution. A decider can provide an executionContext,
      -- which is a free form string, when closing a decision task using
      -- RespondDecisionTaskCompleted.
    , _wedLatestActivityTaskTimestamp :: Maybe POSIX
      -- ^ The time when the last activity task was scheduled for this
      -- workflow execution. You can use this information to determine if
      -- the workflow has not made progress for an unusually long period
      -- of time and might require a corrective action.
    } deriving (Show, Generic)

makeLenses ''DescribeWorkflowExecutionResponse

instance FromJSON DescribeWorkflowExecutionResponse

instance AWSRequest DescribeWorkflowExecution where
    type Sv DescribeWorkflowExecution = SWF
    type Rs DescribeWorkflowExecution = DescribeWorkflowExecutionResponse

    request = get
    response _ = jsonResponse
