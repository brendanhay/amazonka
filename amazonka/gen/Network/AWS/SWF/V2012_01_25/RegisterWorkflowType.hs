{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.V2012_01_25.RegisterWorkflowType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers a new workflow type and its configuration settings in the
-- specified domain. The retention period for the workflow history is set by
-- the RegisterDomain action. If the type already exists, then a
-- TypeAlreadyExists fault is returned. You cannot change the configuration
-- settings of a workflow type once it is registered and it must be registered
-- as a new version. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the following parameters by using a Condition element with the appropriate
-- keys. defaultTaskList.name: String constraint. The key is
-- swf:defaultTaskList.name. name: String constraint. The key is swf:name.
-- version: String constraint. The key is swf:version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RegisterWorkflowType Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Fri, 13 Jan 2012 18:59:33 GMT
-- X-Amz-Target: SimpleWorkflowService.RegisterWorkflowType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=p5FUOoV3QXAafb7aK5z79Ztu5v0w9NeEqLu0ei+P9FA=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 300 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "name": "customerOrderWorkflow", "version": "1.0",
-- "description": "Handle customer orders", "defaultTaskStartToCloseTimeout":
-- "600", "defaultExecutionStartToCloseTimeout": "3600", "defaultTaskList":
-- {"name": "mainTaskList"}, "defaultChildPolicy": "TERMINATE"} HTTP/1.1 200
-- OK Content-Length: 0 Content-Type: application/json x-amzn-RequestId:
-- bb469e67-3e18-11e1-9914-a356b6ea8bdf.
module Network.AWS.SWF.V2012_01_25.RegisterWorkflowType where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RegisterWorkflowType' request.
registerWorkflowType :: Text -- ^ '_rwtiDomain'
                     -> Text -- ^ '_rwtiName'
                     -> Text -- ^ '_rwtiVersion'
                     -> RegisterWorkflowType
registerWorkflowType p1 p2 p3 = RegisterWorkflowType
    { _rwtiDomain = p1
    , _rwtiName = p2
    , _rwtiVersion = p3
    , _rwtiDefaultChildPolicy = Nothing
    , _rwtiDescription = Nothing
    , _rwtiDefaultExecutionStartToCloseTimeout = Nothing
    , _rwtiDefaultTaskStartToCloseTimeout = Nothing
    , _rwtiDefaultTaskList = Nothing
    }

data RegisterWorkflowType = RegisterWorkflowType
    { _rwtiDomain :: Text
      -- ^ The name of the domain in which to register the workflow type.
    , _rwtiName :: Text
      -- ^ The name of the workflow type. The specified string must not
      -- start or end with whitespace. It must not contain a : (colon), /
      -- (slash), | (vertical bar), or any control characters
      -- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the
      -- literal string &quot;arn&quot;.
    , _rwtiVersion :: Text
      -- ^ The version of the workflow type. The workflow type consists of
      -- the name and version, the combination of which must be unique
      -- within the domain. To get a list of all currently registered
      -- workflow types, use the ListWorkflowTypes action. The specified
      -- string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control
      -- characters (\u0000-\u001f | \u007f - \u009f). Also, it must not
      -- contain the literal string &quot;arn&quot;.
    , _rwtiDefaultChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the default policy to use for the child
      -- workflow executions when a workflow execution of this type is
      -- terminated, by calling the TerminateWorkflowExecution action
      -- explicitly or due to an expired timeout. This default can be
      -- overridden when starting a workflow execution using the
      -- StartWorkflowExecution action or the StartChildWorkflowExecution
      -- Decision. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _rwtiDescription :: Maybe Text
      -- ^ Textual description of the workflow type.
    , _rwtiDefaultExecutionStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the default maximum duration for executions of
      -- this workflow type. You can override this default when starting
      -- an execution through the StartWorkflowExecution Action or
      -- StartChildWorkflowExecution Decision. The duration is specified
      -- in seconds. The valid values are integers greater than or equal
      -- to 0. Unlike some of the other timeout parameters in Amazon SWF,
      -- you cannot specify a value of "NONE" for
      -- defaultExecutionStartToCloseTimeout; there is a one-year max
      -- limit on the time that a workflow execution can run. Exceeding
      -- this limit will always cause the workflow execution to time out.
    , _rwtiDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the default maximum duration of decision tasks
      -- for this workflow type. This default can be overridden when
      -- starting a workflow execution using the StartWorkflowExecution
      -- action or the StartChildWorkflowExecution Decision. The valid
      -- values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be
      -- used to specify unlimited duration.
    , _rwtiDefaultTaskList :: Maybe TaskList
      -- ^ If set, specifies the default task list to use for scheduling
      -- decision tasks for executions of this workflow type. This default
      -- is used only if a task list is not provided when starting the
      -- execution through the StartWorkflowExecution Action or
      -- StartChildWorkflowExecution Decision.
    } deriving (Generic)

makeLenses ''RegisterWorkflowType

instance ToPath RegisterWorkflowType

instance ToQuery RegisterWorkflowType

instance ToHeaders RegisterWorkflowType

instance ToJSON RegisterWorkflowType

data RegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse
    deriving (Eq, Show, Generic)

makeLenses ''RegisterWorkflowTypeResponse

instance AWSRequest RegisterWorkflowType where
    type Sv RegisterWorkflowType = SWF
    type Rs RegisterWorkflowType = RegisterWorkflowTypeResponse

    request = get
    response _ _ = return (Right RegisterWorkflowTypeResponse)
