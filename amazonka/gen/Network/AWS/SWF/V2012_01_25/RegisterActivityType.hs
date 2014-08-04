{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.V2012_01_25.RegisterActivityType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers a new activity type along with its configuration settings in the
-- specified domain. A TypeAlreadyExists fault is returned if the type already
-- exists in the domain. You cannot change any configuration settings of the
-- type after its registration, and it must be registered as a new version.
-- Access Control You can use IAM policies to control this action's access to
-- Amazon SWF resources as follows: Use a Resource element with the domain
-- name to limit the action to only specified domains. Use an Action element
-- to allow or deny permission to call this action. Constrain the following
-- parameters by using a Condition element with the appropriate keys.
-- defaultTaskList.name: String constraint. The key is
-- swf:defaultTaskList.name. name: String constraint. The key is swf:name.
-- version: String constraint. The key is swf:version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RegisterActivityType Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 00:14:06 GMT
-- X-Amz-Target: SimpleWorkflowService.RegisterActivityType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=F9cptqaGWa2H7LW3dpctF9J5svsB6FRZ4krghCRnml0=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 343 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "name": "activityVerify", "version": "1.0", "description":
-- "Verify the customer credit card", "defaultTaskStartToCloseTimeout": "600",
-- "defaultTaskHeartbeatTimeout": "120", "defaultTaskList": {"name":
-- "mainTaskList"}, "defaultTaskScheduleToStartTimeout": "300",
-- "defaultTaskScheduleToCloseTimeout": "900"} HTTP/1.1 200 OK Content-Length:
-- 0 Content-Type: application/json x-amzn-RequestId:
-- d68969c7-3f0d-11e1-9b11-7182192d0b57.
module Network.AWS.SWF.V2012_01_25.RegisterActivityType where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RegisterActivityType' request.
registerActivityType :: Text -- ^ '_ratiDomain'
                     -> Text -- ^ '_ratiName'
                     -> Text -- ^ '_ratiVersion'
                     -> RegisterActivityType
registerActivityType p1 p2 p3 = RegisterActivityType
    { _ratiDomain = p1
    , _ratiName = p2
    , _ratiVersion = p3
    , _ratiDescription = Nothing
    , _ratiDefaultTaskScheduleToStartTimeout = Nothing
    , _ratiDefaultTaskHeartbeatTimeout = Nothing
    , _ratiDefaultTaskScheduleToCloseTimeout = Nothing
    , _ratiDefaultTaskStartToCloseTimeout = Nothing
    , _ratiDefaultTaskList = Nothing
    }

data RegisterActivityType = RegisterActivityType
    { _ratiDomain :: Text
      -- ^ The name of the domain in which this activity is to be
      -- registered.
    , _ratiName :: Text
      -- ^ The name of the activity type within the domain. The specified
      -- string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control
      -- characters (\u0000-\u001f | \u007f - \u009f). Also, it must not
      -- contain the literal string &quot;arn&quot;.
    , _ratiVersion :: Text
      -- ^ The version of the activity type. The activity type consists of
      -- the name and version, the combination of which must be unique
      -- within the domain. The specified string must not start or end
      -- with whitespace. It must not contain a : (colon), / (slash), |
      -- (vertical bar), or any control characters (\u0000-\u001f | \u007f
      -- - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    , _ratiDescription :: Maybe Text
      -- ^ A textual description of the activity type.
    , _ratiDefaultTaskScheduleToStartTimeout :: Maybe Text
      -- ^ If set, specifies the default maximum duration that a task of
      -- this activity type can wait before being assigned to a worker.
      -- This default can be overridden when scheduling an activity task
      -- using the ScheduleActivityTask Decision. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , _ratiDefaultTaskHeartbeatTimeout :: Maybe Text
      -- ^ If set, specifies the default maximum time before which a worker
      -- processing a task of this type must report progress by calling
      -- RecordActivityTaskHeartbeat. If the timeout is exceeded, the
      -- activity task is automatically timed out. This default can be
      -- overridden when scheduling an activity task using the
      -- ScheduleActivityTask Decision. If the activity worker
      -- subsequently attempts to record a heartbeat or returns a result,
      -- the activity worker receives an UnknownResource fault. In this
      -- case, Amazon SWF no longer considers the activity task to be
      -- valid; the activity worker should clean up the activity task. The
      -- valid values are integers greater than or equal to 0. An integer
      -- value can be used to specify the duration in seconds while NONE
      -- can be used to specify unlimited duration.
    , _ratiDefaultTaskScheduleToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the default maximum duration for a task of this
      -- activity type. This default can be overridden when scheduling an
      -- activity task using the ScheduleActivityTask Decision. The valid
      -- values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be
      -- used to specify unlimited duration.
    , _ratiDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the default maximum duration that a worker can
      -- take to process tasks of this activity type. This default can be
      -- overridden when scheduling an activity task using the
      -- ScheduleActivityTask Decision. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    , _ratiDefaultTaskList :: Maybe TaskList
      -- ^ If set, specifies the default task list to use for scheduling
      -- tasks of this activity type. This default task list is used if a
      -- task list is not provided when a task is scheduled through the
      -- ScheduleActivityTask Decision.
    } deriving (Generic)

makeLenses ''RegisterActivityType

instance ToPath RegisterActivityType

instance ToQuery RegisterActivityType

instance ToHeaders RegisterActivityType

instance ToJSON RegisterActivityType

data RegisterActivityTypeResponse = RegisterActivityTypeResponse
    deriving (Eq, Show, Generic)

makeLenses ''RegisterActivityTypeResponse

instance AWSRequest RegisterActivityType where
    type Sv RegisterActivityType = SWF
    type Rs RegisterActivityType = RegisterActivityTypeResponse

    request = get
    response _ _ = return (Right RegisterActivityTypeResponse)
