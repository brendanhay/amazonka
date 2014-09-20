{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.RegisterWorkflowType
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
module Network.AWS.SWF.RegisterWorkflowType
    (
    -- * Request
      RegisterWorkflowType
    -- ** Request constructor
    , registerWorkflowType
    -- ** Request lenses
    , rwtDomain
    , rwtName
    , rwtVersion
    , rwtDescription
    , rwtDefaultTaskStartToCloseTimeout
    , rwtDefaultExecutionStartToCloseTimeout
    , rwtDefaultTaskList
    , rwtDefaultChildPolicy

    -- * Response
    , RegisterWorkflowTypeResponse
    -- ** Response constructor
    , registerWorkflowTypeResponse
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data RegisterWorkflowType = RegisterWorkflowType
    { _rwtDomain :: Text
    , _rwtName :: Text
    , _rwtVersion :: Text
    , _rwtDescription :: Maybe Text
    , _rwtDefaultTaskStartToCloseTimeout :: Maybe Text
    , _rwtDefaultExecutionStartToCloseTimeout :: Maybe Text
    , _rwtDefaultTaskList :: Maybe TaskList
    , _rwtDefaultChildPolicy :: Maybe ChildPolicy
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterWorkflowType' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @Name ::@ @Text@
--
-- * @Version ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @DefaultTaskStartToCloseTimeout ::@ @Maybe Text@
--
-- * @DefaultExecutionStartToCloseTimeout ::@ @Maybe Text@
--
-- * @DefaultTaskList ::@ @Maybe TaskList@
--
-- * @DefaultChildPolicy ::@ @Maybe ChildPolicy@
--
registerWorkflowType :: Text -- ^ 'rwtDomain'
                     -> Text -- ^ 'rwtName'
                     -> Text -- ^ 'rwtVersion'
                     -> RegisterWorkflowType
registerWorkflowType p1 p2 p3 = RegisterWorkflowType
    { _rwtDomain = p1
    , _rwtName = p2
    , _rwtVersion = p3
    , _rwtDescription = Nothing
    , _rwtDefaultTaskStartToCloseTimeout = Nothing
    , _rwtDefaultExecutionStartToCloseTimeout = Nothing
    , _rwtDefaultTaskList = Nothing
    , _rwtDefaultChildPolicy = Nothing
    }

-- | The name of the domain in which to register the workflow type.
rwtDomain :: Lens' RegisterWorkflowType Text
rwtDomain = lens _rwtDomain (\s a -> s { _rwtDomain = a })

-- | The name of the workflow type. The specified string must not start or end
-- with whitespace. It must not contain a : (colon), / (slash), | (vertical
-- bar), or any control characters (\u0000-\u001f | \u007f - \u009f). Also, it
-- must not contain the literal string &quot;arn&quot;.
rwtName :: Lens' RegisterWorkflowType Text
rwtName = lens _rwtName (\s a -> s { _rwtName = a })

-- | The version of the workflow type. The workflow type consists of the name
-- and version, the combination of which must be unique within the domain. To
-- get a list of all currently registered workflow types, use the
-- ListWorkflowTypes action. The specified string must not start or end with
-- whitespace. It must not contain a : (colon), / (slash), | (vertical bar),
-- or any control characters (\u0000-\u001f | \u007f - \u009f). Also, it must
-- not contain the literal string &quot;arn&quot;.
rwtVersion :: Lens' RegisterWorkflowType Text
rwtVersion = lens _rwtVersion (\s a -> s { _rwtVersion = a })

-- | Textual description of the workflow type.
rwtDescription :: Lens' RegisterWorkflowType (Maybe Text)
rwtDescription = lens _rwtDescription (\s a -> s { _rwtDescription = a })

-- | If set, specifies the default maximum duration of decision tasks for this
-- workflow type. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- StartChildWorkflowExecution Decision. The valid values are integers greater
-- than or equal to 0. An integer value can be used to specify the duration in
-- seconds while NONE can be used to specify unlimited duration.
rwtDefaultTaskStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultTaskStartToCloseTimeout =
    lens _rwtDefaultTaskStartToCloseTimeout
         (\s a -> s { _rwtDefaultTaskStartToCloseTimeout = a })

-- | If set, specifies the default maximum duration for executions of this
-- workflow type. You can override this default when starting an execution
-- through the StartWorkflowExecution Action or StartChildWorkflowExecution
-- Decision. The duration is specified in seconds. The valid values are
-- integers greater than or equal to 0. Unlike some of the other timeout
-- parameters in Amazon SWF, you cannot specify a value of "NONE" for
-- defaultExecutionStartToCloseTimeout; there is a one-year max limit on the
-- time that a workflow execution can run. Exceeding this limit will always
-- cause the workflow execution to time out.
rwtDefaultExecutionStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultExecutionStartToCloseTimeout =
    lens _rwtDefaultExecutionStartToCloseTimeout
         (\s a -> s { _rwtDefaultExecutionStartToCloseTimeout = a })

-- | If set, specifies the default task list to use for scheduling decision
-- tasks for executions of this workflow type. This default is used only if a
-- task list is not provided when starting the execution through the
-- StartWorkflowExecution Action or StartChildWorkflowExecution Decision.
rwtDefaultTaskList :: Lens' RegisterWorkflowType (Maybe TaskList)
rwtDefaultTaskList =
    lens _rwtDefaultTaskList (\s a -> s { _rwtDefaultTaskList = a })

-- | If set, specifies the default policy to use for the child workflow
-- executions when a workflow execution of this type is terminated, by calling
-- the TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This default can be overridden when starting a workflow execution
-- using the StartWorkflowExecution action or the StartChildWorkflowExecution
-- Decision. The supported child policies are: TERMINATE: the child executions
-- will be terminated. REQUEST_CANCEL: a request to cancel will be attempted
-- for each child execution by recording a WorkflowExecutionCancelRequested
-- event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event. ABANDON: no action
-- will be taken. The child executions will continue to run.
rwtDefaultChildPolicy :: Lens' RegisterWorkflowType (Maybe ChildPolicy)
rwtDefaultChildPolicy =
    lens _rwtDefaultChildPolicy (\s a -> s { _rwtDefaultChildPolicy = a })

instance ToPath RegisterWorkflowType

instance ToQuery RegisterWorkflowType

instance ToHeaders RegisterWorkflowType

instance ToJSON RegisterWorkflowType

data RegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterWorkflowTypeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
registerWorkflowTypeResponse :: RegisterWorkflowTypeResponse
registerWorkflowTypeResponse = RegisterWorkflowTypeResponse

instance AWSRequest RegisterWorkflowType where
    type Sv RegisterWorkflowType = SWF
    type Rs RegisterWorkflowType = RegisterWorkflowTypeResponse

    request = get
    response _ = nullaryResponse RegisterWorkflowTypeResponse
