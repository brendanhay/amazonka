{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.PollForActivityTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Used by workers to get an ActivityTask from the specified activity
-- taskList. This initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon as a task becomes available. The
-- maximum time the service holds on to the request before responding is 60
-- seconds. If no task is available within 60 seconds, the poll will return an
-- empty result. An empty result, in this context, means that an ActivityTask
-- is returned, but that the value of taskToken is an empty string. If a task
-- is returned, the worker should use its type to identify and process it
-- correctly. Workers should set their client side socket timeout to at least
-- 70 seconds (10 seconds higher than the maximum time service may hold the
-- poll request). Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the taskList.name parameter by using a Condition element with the
-- swf:taskList.name key to allow the action to access only certain task
-- lists. If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- PollForActivityTask Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:53:52 GMT X-Amz-Target:
-- SimpleWorkflowService.PollForActivityTask Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=dv0H1RPYucoIcRckspWO0f8xG120MWZRKmj3O5/A4rY=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 108 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "mainTaskList"}, "identity":
-- "VerifyCreditCardWorker01"} HTTP/1.1 200 OK Content-Length: 837
-- Content-Type: application/json x-amzn-RequestId:
-- b48fb6b5-3ff5-11e1-a23a-99d60383ae71 {"activityId": "verification-27",
-- "activityType": {"name": "activityVerify", "version": "1.0"}, "input":
-- "5634-0056-4367-0923,12/12,437", "startedEventId": 11, "taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAX9p3pcp3857oLXFUuwdxRU5/zmn9f40XaMF7VohAH4jOtjXpZu7GdOzEi0b3cWYHbG5b5dpdcTXHUDPVMHXiUxCgr+Nc/wUW9016W4YxJGs/jmxzPln8qLftU+SW135Q0UuKp5XRGoRTJp3tbHn2pY1vC8gDB/K69J6q668U1pd4Cd9o43//lGgOIjN0/Ihg+DO+83HNcOuVEQMM28kNMXf7yePh31M4dMKJwQaQZG13huJXDwzJOoZQz+XFuqFly+lPnCE4XvsnhfAvTsh50EtNDEtQzPCFJoUeld9g64V/FS/39PHL3M93PBUuroPyHuCwHsNC6fZ7gM/XOKmW4kKnXPoQweEUkFV/J6E6+M1reBO7nJADTrLSnajg6MY/viWsEYmMw/DS5FlquFaDIhFkLhWUWN+V2KqiKS23GYwpzgZ7fgcWHQF2NLEY3zrjam4LW/UW5VLCyM3FpVD3erCTi9IvUgslPzyVGuWNAoTmgJEWvimgwiHxJMxxc9JBDR390iMmImxVl3eeSDUWx8reQltiviadPDjyRmVhYP8",
-- "workflowExecution": {"runId": "cfa2bd33-31b0-4b75-b131-255bb0d97b3f",
-- "workflowId": "20110927-T-1"} }.
module Network.AWS.SWF.PollForActivityTask
    (
    -- * Request
      PollForActivityTask
    -- ** Request constructor
    , pollForActivityTask
    -- ** Request lenses
    , pfatDomain
    , pfatTaskList
    , pfatIdentity

    -- * Response
    , PollForActivityTaskResponse
    -- ** Response constructor
    , pollForActivityTaskResponse
    -- ** Response lenses
    , pfatrTaskToken
    , pfatrActivityId
    , pfatrStartedEventId
    , pfatrWorkflowExecution
    , pfatrActivityType
    , pfatrInput
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data PollForActivityTask = PollForActivityTask
    { _pfatDomain :: Text
    , _pfatTaskList :: TaskList
    , _pfatIdentity :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PollForActivityTask' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @TaskList ::@ @TaskList@
--
-- * @Identity ::@ @Maybe Text@
--
pollForActivityTask :: Text -- ^ 'pfatDomain'
                      -> TaskList -- ^ 'pfatTaskList'
                      -> PollForActivityTask
pollForActivityTask p1 p2 = PollForActivityTask
    { _pfatDomain = p1
    , _pfatTaskList = p2
    , _pfatIdentity = Nothing
    }

-- | The name of the domain that contains the task lists being polled.
pfatDomain :: Lens' PollForActivityTask Text
pfatDomain = lens _pfatDomain (\s a -> s { _pfatDomain = a })

-- | Specifies the task list to poll for activity tasks. The specified string
-- must not start or end with whitespace. It must not contain a : (colon), /
-- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
-- \u007f - \u009f). Also, it must not contain the literal string
-- &quot;arn&quot;.
pfatTaskList :: Lens' PollForActivityTask TaskList
pfatTaskList = lens _pfatTaskList (\s a -> s { _pfatTaskList = a })

-- | Identity of the worker making the request, which is recorded in the
-- ActivityTaskStarted event in the workflow history. This enables diagnostic
-- tracing when problems arise. The form of this identity is user defined.
pfatIdentity :: Lens' PollForActivityTask (Maybe Text)
pfatIdentity = lens _pfatIdentity (\s a -> s { _pfatIdentity = a })

instance ToPath PollForActivityTask

instance ToQuery PollForActivityTask

instance ToHeaders PollForActivityTask

instance ToJSON PollForActivityTask

-- | Unit of work sent to an activity worker.
data PollForActivityTaskResponse = PollForActivityTaskResponse
    { _pfatrTaskToken :: Text
    , _pfatrActivityId :: Text
    , _pfatrStartedEventId :: !Integer
    , _pfatrWorkflowExecution :: WorkflowExecution
    , _pfatrActivityType :: ActivityType
    , _pfatrInput :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PollForActivityTaskResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskToken ::@ @Text@
--
-- * @ActivityId ::@ @Text@
--
-- * @StartedEventId ::@ @Integer@
--
-- * @WorkflowExecution ::@ @WorkflowExecution@
--
-- * @ActivityType ::@ @ActivityType@
--
-- * @Input ::@ @Maybe Text@
--
pollForActivityTaskResponse :: Text -- ^ 'pfatrTaskToken'
                              -> Text -- ^ 'pfatrActivityId'
                              -> Integer -- ^ 'pfatrStartedEventId'
                              -> WorkflowExecution -- ^ 'pfatrWorkflowExecution'
                              -> ActivityType -- ^ 'pfatrActivityType'
                              -> PollForActivityTaskResponse
pollForActivityTaskResponse p1 p2 p3 p4 p5 = PollForActivityTaskResponse
    { _pfatrTaskToken = p1
    , _pfatrActivityId = p2
    , _pfatrStartedEventId = p3
    , _pfatrWorkflowExecution = p4
    , _pfatrActivityType = p5
    , _pfatrInput = Nothing
    }

-- | The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the system
-- about the task.
pfatrTaskToken :: Lens' PollForActivityTaskResponse Text
pfatrTaskToken = lens _pfatrTaskToken (\s a -> s { _pfatrTaskToken = a })

-- | The unique ID of the task.
pfatrActivityId :: Lens' PollForActivityTaskResponse Text
pfatrActivityId = lens _pfatrActivityId (\s a -> s { _pfatrActivityId = a })

-- | The id of the ActivityTaskStarted event recorded in the history.
pfatrStartedEventId :: Lens' PollForActivityTaskResponse Integer
pfatrStartedEventId =
    lens _pfatrStartedEventId (\s a -> s { _pfatrStartedEventId = a })

-- | The workflow execution that started this activity task.
pfatrWorkflowExecution :: Lens' PollForActivityTaskResponse WorkflowExecution
pfatrWorkflowExecution =
    lens _pfatrWorkflowExecution (\s a -> s { _pfatrWorkflowExecution = a })

-- | The type of this activity task.
pfatrActivityType :: Lens' PollForActivityTaskResponse ActivityType
pfatrActivityType =
    lens _pfatrActivityType (\s a -> s { _pfatrActivityType = a })

-- | The inputs provided when the activity task was scheduled. The form of the
-- input is user defined and should be meaningful to the activity
-- implementation.
pfatrInput :: Lens' PollForActivityTaskResponse (Maybe Text)
pfatrInput = lens _pfatrInput (\s a -> s { _pfatrInput = a })

instance FromJSON PollForActivityTaskResponse

instance AWSRequest PollForActivityTask where
    type Sv PollForActivityTask = SWF
    type Rs PollForActivityTask = PollForActivityTaskResponse

    request = get
    response _ = jsonResponse
