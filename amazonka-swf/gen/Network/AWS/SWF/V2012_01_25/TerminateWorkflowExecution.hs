{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a WorkflowExecutionTerminated event and forces closure of the
-- workflow execution identified by the given domain, runId, and workflowId.
-- The child policy, registered with the workflow type or specified when
-- starting this execution, is applied to any open child workflow executions
-- of this workflow execution. If the identified workflow execution was in
-- progress, it is terminated immediately. If a runId is not specified, then
-- the WorkflowExecutionTerminated event is recorded in the history of the
-- current open workflow with the matching workflowId in the domain. You
-- should consider using RequestCancelWorkflowExecution action instead because
-- it allows the workflow to gracefully close while TerminateWorkflowExecution
-- does not. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. TerminateWorkflowExecution
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:56:34 GMT
-- X-Amz-Target: SimpleWorkflowService.TerminateWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=JHMRAjN6JGPawEuhiANHfiCil9KOGfDF/cuXYmuu9S4=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 218 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "runId":
-- "94861fda-a714-4126-95d7-55ba847da8ab", "reason": "transaction canceled",
-- "details": "customer credit card declined", "childPolicy": "TERMINATE"}
-- HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: 76d68a47-3ffe-11e1-b118-3bfa5e8e7fc3.
module Network.AWS.SWF.V2012_01_25.TerminateWorkflowExecution
    (
    -- * Request
      TerminateWorkflowExecution
    -- ** Request constructor
    , mkTerminateWorkflowExecution
    -- ** Request lenses
    , tweDomain
    , tweWorkflowId
    , tweRunId
    , tweReason
    , tweDetails
    , tweChildPolicy

    -- * Response
    , TerminateWorkflowExecutionResponse
    -- ** Response constructor
    , mkTerminateWorkflowExecutionResponse
    ) where

import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data TerminateWorkflowExecution = TerminateWorkflowExecution
    { _tweDomain :: Text
    , _tweWorkflowId :: Text
    , _tweRunId :: Maybe Text
    , _tweReason :: Maybe Text
    , _tweDetails :: Maybe Text
    , _tweChildPolicy :: Maybe ChildPolicy
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateWorkflowExecution' request.
mkTerminateWorkflowExecution :: Text -- ^ 'tweDomain'
                             -> Text -- ^ 'tweWorkflowId'
                             -> TerminateWorkflowExecution
mkTerminateWorkflowExecution p1 p2 = TerminateWorkflowExecution
    { _tweDomain = p1
    , _tweWorkflowId = p2
    , _tweRunId = Nothing
    , _tweReason = Nothing
    , _tweDetails = Nothing
    , _tweChildPolicy = Nothing
    }

-- | The domain of the workflow execution to terminate.
tweDomain :: Lens' TerminateWorkflowExecution Text
tweDomain = lens _tweDomain (\s a -> s { _tweDomain = a })

-- | The workflowId of the workflow execution to terminate.
tweWorkflowId :: Lens' TerminateWorkflowExecution Text
tweWorkflowId = lens _tweWorkflowId (\s a -> s { _tweWorkflowId = a })

-- | The runId of the workflow execution to terminate.
tweRunId :: Lens' TerminateWorkflowExecution (Maybe Text)
tweRunId = lens _tweRunId (\s a -> s { _tweRunId = a })

-- | An optional descriptive reason for terminating the workflow execution.
tweReason :: Lens' TerminateWorkflowExecution (Maybe Text)
tweReason = lens _tweReason (\s a -> s { _tweReason = a })

-- | Optional details for terminating the workflow execution.
tweDetails :: Lens' TerminateWorkflowExecution (Maybe Text)
tweDetails = lens _tweDetails (\s a -> s { _tweDetails = a })

-- | If set, specifies the policy to use for the child workflow executions of
-- the workflow execution being terminated. This policy overrides the child
-- policy specified for the workflow execution at registration time or when
-- starting the execution. The supported child policies are: TERMINATE: the
-- child executions will be terminated. REQUEST_CANCEL: a request to cancel
-- will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run. A child policy for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default child policy was
-- specified at registration time, a fault will be returned.
tweChildPolicy :: Lens' TerminateWorkflowExecution (Maybe ChildPolicy)
tweChildPolicy = lens _tweChildPolicy (\s a -> s { _tweChildPolicy = a })

instance ToPath TerminateWorkflowExecution

instance ToQuery TerminateWorkflowExecution

instance ToHeaders TerminateWorkflowExecution

instance ToJSON TerminateWorkflowExecution

data TerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateWorkflowExecutionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkTerminateWorkflowExecutionResponse :: TerminateWorkflowExecutionResponse
mkTerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse

instance AWSRequest TerminateWorkflowExecution where
    type Sv TerminateWorkflowExecution = SWF
    type Rs TerminateWorkflowExecution = TerminateWorkflowExecutionResponse

    request = get
    response _ = nullaryResponse TerminateWorkflowExecutionResponse
