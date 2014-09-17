{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
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
module Network.AWS.SWF.DescribeWorkflowExecution
    (
    -- * Request
      DescribeWorkflowExecution
    -- ** Request constructor
    , mkDescribeWorkflowExecution
    -- ** Request lenses
    , dweDomain
    , dweExecution

    -- * Response
    , DescribeWorkflowExecutionResponse
    -- ** Response constructor
    , mkDescribeWorkflowExecutionResponse
    -- ** Response lenses
    , dwerExecutionInfo
    , dwerExecutionConfiguration
    , dwerOpenCounts
    , dwerLatestActivityTaskTimestamp
    , dwerLatestExecutionContext
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeWorkflowExecution = DescribeWorkflowExecution
    { _dweDomain :: Text
    , _dweExecution :: WorkflowExecution
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeWorkflowExecution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @Execution ::@ @WorkflowExecution@
--
mkDescribeWorkflowExecution :: Text -- ^ 'dweDomain'
                            -> WorkflowExecution -- ^ 'dweExecution'
                            -> DescribeWorkflowExecution
mkDescribeWorkflowExecution p1 p2 = DescribeWorkflowExecution
    { _dweDomain = p1
    , _dweExecution = p2
    }

-- | The name of the domain containing the workflow execution.
dweDomain :: Lens' DescribeWorkflowExecution Text
dweDomain = lens _dweDomain (\s a -> s { _dweDomain = a })

-- | The workflow execution to describe.
dweExecution :: Lens' DescribeWorkflowExecution WorkflowExecution
dweExecution = lens _dweExecution (\s a -> s { _dweExecution = a })

instance ToPath DescribeWorkflowExecution

instance ToQuery DescribeWorkflowExecution

instance ToHeaders DescribeWorkflowExecution

instance ToJSON DescribeWorkflowExecution

-- | Contains details about a workflow execution.
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse
    { _dwerExecutionInfo :: WorkflowExecutionInfo
    , _dwerExecutionConfiguration :: WorkflowExecutionConfiguration
    , _dwerOpenCounts :: WorkflowExecutionOpenCounts
    , _dwerLatestActivityTaskTimestamp :: Maybe POSIX
    , _dwerLatestExecutionContext :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeWorkflowExecutionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExecutionInfo ::@ @WorkflowExecutionInfo@
--
-- * @ExecutionConfiguration ::@ @WorkflowExecutionConfiguration@
--
-- * @OpenCounts ::@ @WorkflowExecutionOpenCounts@
--
-- * @LatestActivityTaskTimestamp ::@ @Maybe POSIX@
--
-- * @LatestExecutionContext ::@ @Maybe Text@
--
mkDescribeWorkflowExecutionResponse :: WorkflowExecutionInfo -- ^ 'dwerExecutionInfo'
                                    -> WorkflowExecutionConfiguration -- ^ 'dwerExecutionConfiguration'
                                    -> WorkflowExecutionOpenCounts -- ^ 'dwerOpenCounts'
                                    -> DescribeWorkflowExecutionResponse
mkDescribeWorkflowExecutionResponse p1 p2 p3 = DescribeWorkflowExecutionResponse
    { _dwerExecutionInfo = p1
    , _dwerExecutionConfiguration = p2
    , _dwerOpenCounts = p3
    , _dwerLatestActivityTaskTimestamp = Nothing
    , _dwerLatestExecutionContext = Nothing
    }

-- | Information about the workflow execution.
dwerExecutionInfo :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionInfo
dwerExecutionInfo =
    lens _dwerExecutionInfo (\s a -> s { _dwerExecutionInfo = a })

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
dwerExecutionConfiguration :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionConfiguration
dwerExecutionConfiguration =
    lens _dwerExecutionConfiguration
         (\s a -> s { _dwerExecutionConfiguration = a })

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
dwerOpenCounts :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionOpenCounts
dwerOpenCounts = lens _dwerOpenCounts (\s a -> s { _dwerOpenCounts = a })

-- | The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require a
-- corrective action.
dwerLatestActivityTaskTimestamp :: Lens' DescribeWorkflowExecutionResponse (Maybe POSIX)
dwerLatestActivityTaskTimestamp =
    lens _dwerLatestActivityTaskTimestamp
         (\s a -> s { _dwerLatestActivityTaskTimestamp = a })

-- | The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext, which is a free form
-- string, when closing a decision task using RespondDecisionTaskCompleted.
dwerLatestExecutionContext :: Lens' DescribeWorkflowExecutionResponse (Maybe Text)
dwerLatestExecutionContext =
    lens _dwerLatestExecutionContext
         (\s a -> s { _dwerLatestExecutionContext = a })

instance FromJSON DescribeWorkflowExecutionResponse

instance AWSRequest DescribeWorkflowExecution where
    type Sv DescribeWorkflowExecution = SWF
    type Rs DescribeWorkflowExecution = DescribeWorkflowExecutionResponse

    request = get
    response _ = jsonResponse
