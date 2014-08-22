{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.PollForDecisionTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Used by deciders to get a DecisionTask from the specified decision
-- taskList. A decision task may be returned for any open workflow execution
-- that is using the specified task list. The task includes a paginated view
-- of the history of the workflow execution. The decider should use the
-- workflow type and the history to determine how to properly handle the task.
-- This action initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon a task becomes available. If no
-- decision task is available in the specified task list before the timeout of
-- 60 seconds expires, an empty result is returned. An empty result, in this
-- context, means that a DecisionTask is returned, but that the value of
-- taskToken is an empty string. Deciders should set their client side socket
-- timeout to at least 70 seconds (10 seconds higher than the timeout).
-- Because the number of workflow history events for a single workflow
-- execution might be very large, the result returned might be split up across
-- a number of pages. To retrieve subsequent pages, make additional calls to
-- PollForDecisionTask using the nextPageToken returned by the initial call.
-- Note that you do not call GetWorkflowExecutionHistory with this
-- nextPageToken. Instead, call PollForDecisionTask again. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. Constrain the taskList.name parameter
-- by using a Condition element with the swf:taskList.name key to allow the
-- action to access only certain task lists. If the caller does not have
-- sufficient permissions to invoke the action, or the parameter values fall
-- outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. PollForDecisionTask Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:09:54 GMT
-- X-Amz-Target: SimpleWorkflowService.PollForDecisionTask Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=R3CJ2HMLSVpc2p6eafeztZCZWcgza+h61gSUuWx15gw=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 171 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "specialTaskList"}, "identity":
-- "Decider01", "maximumPageSize": 50, "reverseOrder": true} HTTP/1.1 200 OK
-- Content-Length: 1639 Content-Type: application/json x-amzn-RequestId:
-- 03db54cf-3f1e-11e1-b118-3bfa5e8e7fc3 {"events": [
-- {"decisionTaskStartedEventAttributes": {"identity": "Decider01",
-- "scheduledEventId": 2}, "eventId": 3, "eventTimestamp": 1326593394.566,
-- "eventType": "DecisionTaskStarted"},
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 2, "eventTimestamp":
-- 1326592619.474, "eventType": "DecisionTaskScheduled"}, {"eventId": 1,
-- "eventTimestamp": 1326592619.474, "eventType": "WorkflowExecutionStarted",
-- "workflowExecutionStartedEventAttributes": {"childPolicy": "TERMINATE",
-- "executionStartToCloseTimeout": "3600", "input":
-- "arbitrary-string-that-is-meaningful-to-the-workflow",
-- "parentInitiatedEventId": 0, "tagList": ["music purchase", "digital",
-- "ricoh-the-dog"], "taskList": {"name": "specialTaskList"},
-- "taskStartToCloseTimeout": "600", "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } } ], "previousStartedEventId":
-- 0, "startedEventId": 3, "taskToken":
-- "AAAAKgAAAAEAAAAAAAAAATZDvCYwk/hP/X1ZGdJhb+T6OWzcBx2DPhsIi5HF4aGQI4OXrDE7Ny3uM+aiAhGrmeNyVAa4yNIBQuoZuJA5G+BoaB0JuHFBOynHDTnm7ayNH43KhMkfdrDG4elfHSz3m/EtbLnFGueAr7+3NKDG6x4sTKg3cZpOtSguSx05yI1X3AtscS8ATcLB2Y3Aub1YonN/i/k67voca/GFsSiwSz3AAnJj1IPvrujgIj9KUvckwRPC5ET7d33XJcRp+gHYzZsBLVBaRmV3gEYAnp2ICslFn4YSjGy+dFXCNpOa4G1O8pczCbFUGbQ3+5wf0RSaa/xMq2pfdBKnuFp0wp8kw1k+5ZsbtDZeZn8g5GyKCLiLms/xD0OxugGGUe5ZlAoHEkTWGxZj/G32P7cMoCgrcACfFPdx1LNYYEre7YiGiyjGnfW2t5mW7VK9Np28vcXVbdpH4JNEB9OuB1xqL8N8ifPVtc72uxB1i9XEdq/8rkXasSEw4TubB2FwgqnuJstmfEhpOdb5HfhR6OwmnHuk9eszO/fUkGucTUXQP2hhB+Gz",
-- "workflowExecution": {"runId": "06b8f87a-24b3-40b6-9ceb-9676f28e9493",
-- "workflowId": "20110927-T-1"}, "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }.
module Network.AWS.SWF.V2012_01_25.PollForDecisionTask where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'PollForDecisionTask' request.
pollForDecisionTask :: Text -- ^ '_pfdtiDomain'
                    -> TaskList -- ^ '_pfdtiTaskList'
                    -> PollForDecisionTask
pollForDecisionTask p1 p2 = PollForDecisionTask
    { _pfdtiDomain = p1
    , _pfdtiTaskList = p2
    , _pfdtiIdentity = Nothing
    , _pfdtiMaximumPageSize = Nothing
    , _pfdtiNextPageToken = Nothing
    , _pfdtiReverseOrder = Nothing
    }

data PollForDecisionTask = PollForDecisionTask
    { _pfdtiDomain :: Text
      -- ^ The name of the domain containing the task lists to poll.
    , _pfdtiTaskList :: TaskList
      -- ^ Specifies the task list to poll for decision tasks. The specified
      -- string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control
      -- characters (\u0000-\u001f | \u007f - \u009f). Also, it must not
      -- contain the literal string &quot;arn&quot;.
    , _pfdtiIdentity :: Maybe Text
      -- ^ Identity of the decider making the request, which is recorded in
      -- the DecisionTaskStarted event in the workflow history. This
      -- enables diagnostic tracing when problems arise. The form of this
      -- identity is user defined.
    , _pfdtiMaximumPageSize :: Maybe Integer
      -- ^ The maximum number of history events returned in each page. The
      -- default is 100, but the caller can override this value to a page
      -- size smaller than the default. You cannot specify a page size
      -- greater than 100. Note that the number of events may be less than
      -- the maxiumum page size, in which case, the returned page will
      -- have fewer results than the maximumPageSize specified.
    , _pfdtiNextPageToken :: Maybe Text
      -- ^ If on a previous call to this method a NextPageToken was
      -- returned, the results are being paginated. To get the next page
      -- of results, repeat the call with the returned token and all other
      -- arguments unchanged. The nextPageToken returned by this action
      -- cannot be used with GetWorkflowExecutionHistory to get the next
      -- page. You must call PollForDecisionTask again (with the
      -- nextPageToken) to retrieve the next page of history records.
      -- Calling PollForDecisionTask with a nextPageToken will not return
      -- a new decision task..
    , _pfdtiReverseOrder :: Maybe Bool
      -- ^ When set to true, returns the events in reverse order. By default
      -- the results are returned in ascending order of the eventTimestamp
      -- of the events.
    } deriving (Show, Generic)

makeLenses ''PollForDecisionTask

instance ToPath PollForDecisionTask

instance ToQuery PollForDecisionTask

instance ToHeaders PollForDecisionTask

instance ToJSON PollForDecisionTask

data PollForDecisionTaskResponse = PollForDecisionTaskResponse
    { _ddddkStartedEventId :: Integer
      -- ^ The id of the DecisionTaskStarted event recorded in the history.
    , _ddddkEvents :: [HistoryEvent]
      -- ^ A paginated list of history events of the workflow execution. The
      -- decider uses this during the processing of the decision task.
    , _ddddkTaskToken :: Text
      -- ^ The opaque string used as a handle on the task. This token is
      -- used by workers to communicate progress and response information
      -- back to the system about the task.
    , _ddddkWorkflowExecution :: WorkflowExecution
      -- ^ The workflow execution for which this decision task was created.
    , _ddddkWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution for which this decision task
      -- was created.
    , _ddddkPreviousStartedEventId :: Maybe Integer
      -- ^ The id of the DecisionTaskStarted event of the previous decision
      -- task of this workflow execution that was processed by the
      -- decider. This can be used to determine the events in the history
      -- new since the last decision task received by the decider.
    , _ddddkNextPageToken :: Maybe Text
      -- ^ Returns a value if the results are paginated. To get the next
      -- page of results, repeat the request specifying this token and all
      -- other arguments unchanged.
    } deriving (Show, Generic)

makeLenses ''PollForDecisionTaskResponse

instance FromJSON PollForDecisionTaskResponse

instance AWSRequest PollForDecisionTask where
    type Sv PollForDecisionTask = SWF
    type Rs PollForDecisionTask = PollForDecisionTaskResponse

    request = get
    response _ = jsonResponse

instance AWSPager PollForDecisionTask where
    next rq rs = (\x -> rq { _pfdtiNextPageToken = Just x })
        <$> (_ddddkNextPageToken rs)
