{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a WorkflowExecutionCancelRequested event in the currently running
-- workflow execution identified by the given domain, workflowId, and runId.
-- This logically requests the cancellation of the workflow execution as a
-- whole. It is up to the decider to take appropriate actions when it receives
-- an execution history with this event. If the runId is not specified, the
-- WorkflowExecutionCancelRequested event is recorded in the history of the
-- current open workflow execution with the specified workflowId in the
-- domain. Because this action allows the workflow to properly clean up and
-- gracefully close, it should be used instead of TerminateWorkflowExecution
-- when possible. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. You cannot
-- use an IAM policy to constrain this action's parameters. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RequestCancelWorkflowExecution
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:49:06 GMT
-- X-Amz-Target: SimpleWorkflowService.RequestCancelWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=xODwV3kbpJbWVa6bQiV2zQAw9euGI3uXI82urc+bVeo=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 106 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "runId":
-- "94861fda-a714-4126-95d7-55ba847da8ab"} HTTP/1.1 200 OK Content-Length: 0
-- Content-Type: application/json x-amzn-RequestId:
-- 6bd0627e-3ffd-11e1-9b11-7182192d0b57.
module Network.AWS.SWF
    (
    -- * Request
      RequestCancelWorkflowExecution
    -- ** Request constructor
    , mkRequestCancelWorkflowExecution
    -- ** Request lenses
    , rcweDomain
    , rcweWorkflowId
    , rcweRunId

    -- * Response
    , RequestCancelWorkflowExecutionResponse
    -- ** Response constructor
    , mkRequestCancelWorkflowExecutionResponse
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution
    { _rcweDomain :: Text
    , _rcweWorkflowId :: Text
    , _rcweRunId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestCancelWorkflowExecution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @WorkflowId ::@ @Text@
--
-- * @RunId ::@ @Maybe Text@
--
mkRequestCancelWorkflowExecution :: Text -- ^ 'rcweDomain'
                                 -> Text -- ^ 'rcweWorkflowId'
                                 -> RequestCancelWorkflowExecution
mkRequestCancelWorkflowExecution p1 p2 = RequestCancelWorkflowExecution
    { _rcweDomain = p1
    , _rcweWorkflowId = p2
    , _rcweRunId = Nothing
    }

-- | The name of the domain containing the workflow execution to cancel.
rcweDomain :: Lens' RequestCancelWorkflowExecution Text
rcweDomain = lens _rcweDomain (\s a -> s { _rcweDomain = a })

-- | The workflowId of the workflow execution to cancel.
rcweWorkflowId :: Lens' RequestCancelWorkflowExecution Text
rcweWorkflowId = lens _rcweWorkflowId (\s a -> s { _rcweWorkflowId = a })

-- | The runId of the workflow execution to cancel.
rcweRunId :: Lens' RequestCancelWorkflowExecution (Maybe Text)
rcweRunId = lens _rcweRunId (\s a -> s { _rcweRunId = a })

instance ToPath RequestCancelWorkflowExecution

instance ToQuery RequestCancelWorkflowExecution

instance ToHeaders RequestCancelWorkflowExecution

instance ToJSON RequestCancelWorkflowExecution

data RequestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestCancelWorkflowExecutionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRequestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse
mkRequestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse

instance AWSRequest RequestCancelWorkflowExecution where
    type Sv RequestCancelWorkflowExecution = SWF
    type Rs RequestCancelWorkflowExecution = RequestCancelWorkflowExecutionResponse

    request = get
    response _ = nullaryResponse RequestCancelWorkflowExecutionResponse
