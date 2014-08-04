{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.V2012_01_25.PollForActivityTask
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
module Network.AWS.SWF.V2012_01_25.PollForActivityTask where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PollForActivityTask' request.
pollForActivityTask :: Text -- ^ '_pfatiDomain'
                    -> TaskList -- ^ '_pfatiTaskList'
                    -> PollForActivityTask
pollForActivityTask p1 p2 = PollForActivityTask
    { _pfatiDomain = p1
    , _pfatiTaskList = p2
    , _pfatiIdentity = Nothing
    }

data PollForActivityTask = PollForActivityTask
    { _pfatiDomain :: Text
      -- ^ The name of the domain that contains the task lists being polled.
    , _pfatiTaskList :: TaskList
      -- ^ Specifies the task list to poll for activity tasks. The specified
      -- string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control
      -- characters (\u0000-\u001f | \u007f - \u009f). Also, it must not
      -- contain the literal string &quot;arn&quot;.
    , _pfatiIdentity :: Maybe Text
      -- ^ Identity of the worker making the request, which is recorded in
      -- the ActivityTaskStarted event in the workflow history. This
      -- enables diagnostic tracing when problems arise. The form of this
      -- identity is user defined.
    } deriving (Generic)

makeLenses ''PollForActivityTask

instance ToPath PollForActivityTask

instance ToQuery PollForActivityTask

instance ToHeaders PollForActivityTask

instance ToJSON PollForActivityTask

data PollForActivityTaskResponse = PollForActivityTaskResponse
    { _avActivityId :: Text
      -- ^ The unique ID of the task.
    , _avActivityType :: ActivityType
      -- ^ The type of this activity task.
    , _avStartedEventId :: Integer
      -- ^ The id of the ActivityTaskStarted event recorded in the history.
    , _avTaskToken :: Text
      -- ^ The opaque string used as a handle on the task. This token is
      -- used by workers to communicate progress and response information
      -- back to the system about the task.
    , _avWorkflowExecution :: WorkflowExecution
      -- ^ The workflow execution that started this activity task.
    , _avInput :: Maybe Text
      -- ^ The inputs provided when the activity task was scheduled. The
      -- form of the input is user defined and should be meaningful to the
      -- activity implementation.
    } deriving (Generic)

makeLenses ''PollForActivityTaskResponse

instance FromJSON PollForActivityTaskResponse

instance AWSRequest PollForActivityTask where
    type Sv PollForActivityTask = SWF
    type Rs PollForActivityTask = PollForActivityTaskResponse

    request = get
    response _ = jsonResponse
