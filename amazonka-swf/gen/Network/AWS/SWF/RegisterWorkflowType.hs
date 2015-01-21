{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Registers a new /workflow type/ and its configuration settings in the specified
-- domain.
--
-- The retention period for the workflow history is set by the 'RegisterDomain'
-- action.
--
-- If the type already exists, then a 'TypeAlreadyExists' fault is returned. You
-- cannot change the configuration settings of a workflow type once it is
-- registered and it must be registered as a new version. Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. Constrain the following parameters by using a 'Condition' element
-- with the appropriate keys.   'defaultTaskList.name': String constraint. The key
-- is 'swf:defaultTaskList.name'.  'name': String constraint. The key is 'swf:name'.  'version': String constraint. The key is 'swf:version'.    If the caller does not have
-- sufficient permissions to invoke the action, or the parameter values fall
-- outside the specified constraints, the action fails. The associated event
-- attribute's cause parameter will be set to OPERATION_NOT_PERMITTED. For
-- details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to AmazonSWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RegisterWorkflowType.html>
module Network.AWS.SWF.RegisterWorkflowType
    (
    -- * Request
      RegisterWorkflowType
    -- ** Request constructor
    , registerWorkflowType
    -- ** Request lenses
    , rwtDefaultChildPolicy
    , rwtDefaultExecutionStartToCloseTimeout
    , rwtDefaultTaskList
    , rwtDefaultTaskPriority
    , rwtDefaultTaskStartToCloseTimeout
    , rwtDescription
    , rwtDomain
    , rwtName
    , rwtVersion

    -- * Response
    , RegisterWorkflowTypeResponse
    -- ** Response constructor
    , registerWorkflowTypeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data RegisterWorkflowType = RegisterWorkflowType
    { _rwtDefaultChildPolicy                  :: Maybe ChildPolicy
    , _rwtDefaultExecutionStartToCloseTimeout :: Maybe Text
    , _rwtDefaultTaskList                     :: Maybe TaskList
    , _rwtDefaultTaskPriority                 :: Maybe Text
    , _rwtDefaultTaskStartToCloseTimeout      :: Maybe Text
    , _rwtDescription                         :: Maybe Text
    , _rwtDomain                              :: Text
    , _rwtName                                :: Text
    , _rwtVersion                             :: Text
    } deriving (Eq, Read, Show)

-- | 'RegisterWorkflowType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwtDefaultChildPolicy' @::@ 'Maybe' 'ChildPolicy'
--
-- * 'rwtDefaultExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'rwtDefaultTaskList' @::@ 'Maybe' 'TaskList'
--
-- * 'rwtDefaultTaskPriority' @::@ 'Maybe' 'Text'
--
-- * 'rwtDefaultTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'rwtDescription' @::@ 'Maybe' 'Text'
--
-- * 'rwtDomain' @::@ 'Text'
--
-- * 'rwtName' @::@ 'Text'
--
-- * 'rwtVersion' @::@ 'Text'
--
registerWorkflowType :: Text -- ^ 'rwtDomain'
                     -> Text -- ^ 'rwtName'
                     -> Text -- ^ 'rwtVersion'
                     -> RegisterWorkflowType
registerWorkflowType p1 p2 p3 = RegisterWorkflowType
    { _rwtDomain                              = p1
    , _rwtName                                = p2
    , _rwtVersion                             = p3
    , _rwtDescription                         = Nothing
    , _rwtDefaultTaskStartToCloseTimeout      = Nothing
    , _rwtDefaultExecutionStartToCloseTimeout = Nothing
    , _rwtDefaultTaskList                     = Nothing
    , _rwtDefaultTaskPriority                 = Nothing
    , _rwtDefaultChildPolicy                  = Nothing
    }

-- | If set, specifies the default policy to use for the child workflow executions
-- when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be
-- overridden when starting a workflow execution using the 'StartWorkflowExecution'
-- action or the 'StartChildWorkflowExecution' 'Decision'.
--
-- The supported child policies are:
--
-- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event. ABANDON: no action
-- will be taken. The child executions will continue to run.
rwtDefaultChildPolicy :: Lens' RegisterWorkflowType (Maybe ChildPolicy)
rwtDefaultChildPolicy =
    lens _rwtDefaultChildPolicy (\s a -> s { _rwtDefaultChildPolicy = a })

-- | If set, specifies the default maximum duration for executions of this
-- workflow type. You can override this default when starting an execution
-- through the 'StartWorkflowExecution' Action or 'StartChildWorkflowExecution' 'Decision'.
--
-- The duration is specified in seconds; an integer greater than or equal to 0.
-- Unlike some of the other timeout parameters in Amazon SWF, you cannot specify
-- a value of "NONE" for 'defaultExecutionStartToCloseTimeout'; there is a
-- one-year max limit on the time that a workflow execution can run. Exceeding
-- this limit will always cause the workflow execution to time out.
rwtDefaultExecutionStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultExecutionStartToCloseTimeout =
    lens _rwtDefaultExecutionStartToCloseTimeout
        (\s a -> s { _rwtDefaultExecutionStartToCloseTimeout = a })

-- | If set, specifies the default task list to use for scheduling decision tasks
-- for executions of this workflow type. This default is used only if a task
-- list is not provided when starting the execution through the 'StartWorkflowExecution' Action or 'StartChildWorkflowExecution' 'Decision'.
rwtDefaultTaskList :: Lens' RegisterWorkflowType (Maybe TaskList)
rwtDefaultTaskList =
    lens _rwtDefaultTaskList (\s a -> s { _rwtDefaultTaskList = a })

-- | The default task priority to assign to the workflow type. If not assigned,
-- then "0" will be used. Valid values are integers that range from Java's 'Integer.MIN_VALUE' (-2147483648) to 'Integer.MAX_VALUE' (2147483647). Higher numbers indicate
-- higher priority.
--
-- For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
rwtDefaultTaskPriority :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultTaskPriority =
    lens _rwtDefaultTaskPriority (\s a -> s { _rwtDefaultTaskPriority = a })

-- | If set, specifies the default maximum duration of decision tasks for this
-- workflow type. This default can be overridden when starting a workflow
-- execution using the 'StartWorkflowExecution' action or the 'StartChildWorkflowExecution' 'Decision'.
--
-- The duration is specified in seconds; an integer greater than or equal to 0.
-- The value "NONE" can be used to specify unlimited duration.
rwtDefaultTaskStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultTaskStartToCloseTimeout =
    lens _rwtDefaultTaskStartToCloseTimeout
        (\s a -> s { _rwtDefaultTaskStartToCloseTimeout = a })

-- | Textual description of the workflow type.
rwtDescription :: Lens' RegisterWorkflowType (Maybe Text)
rwtDescription = lens _rwtDescription (\s a -> s { _rwtDescription = a })

-- | The name of the domain in which to register the workflow type.
rwtDomain :: Lens' RegisterWorkflowType Text
rwtDomain = lens _rwtDomain (\s a -> s { _rwtDomain = a })

-- | The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string quotarnquot.
rwtName :: Lens' RegisterWorkflowType Text
rwtName = lens _rwtName (\s a -> s { _rwtName = a })

-- | The version of the workflow type.
--
-- The workflow type consists of the name and version, the combination of which
-- must be unique within the domain. To get a list of all currently registered
-- workflow types, use the 'ListWorkflowTypes' action. The specified string must
-- not start or end with whitespace. It must not contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters (\u0000-\u001f | \u007f -
-- \u009f). Also, it must not contain the literal string quotarnquot.
rwtVersion :: Lens' RegisterWorkflowType Text
rwtVersion = lens _rwtVersion (\s a -> s { _rwtVersion = a })

data RegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RegisterWorkflowTypeResponse' constructor.
registerWorkflowTypeResponse :: RegisterWorkflowTypeResponse
registerWorkflowTypeResponse = RegisterWorkflowTypeResponse

instance ToPath RegisterWorkflowType where
    toPath = const "/"

instance ToQuery RegisterWorkflowType where
    toQuery = const mempty

instance ToHeaders RegisterWorkflowType

instance ToJSON RegisterWorkflowType where
    toJSON RegisterWorkflowType{..} = object
        [ "domain"                              .= _rwtDomain
        , "name"                                .= _rwtName
        , "version"                             .= _rwtVersion
        , "description"                         .= _rwtDescription
        , "defaultTaskStartToCloseTimeout"      .= _rwtDefaultTaskStartToCloseTimeout
        , "defaultExecutionStartToCloseTimeout" .= _rwtDefaultExecutionStartToCloseTimeout
        , "defaultTaskList"                     .= _rwtDefaultTaskList
        , "defaultTaskPriority"                 .= _rwtDefaultTaskPriority
        , "defaultChildPolicy"                  .= _rwtDefaultChildPolicy
        ]

instance AWSRequest RegisterWorkflowType where
    type Sv RegisterWorkflowType = SWF
    type Rs RegisterWorkflowType = RegisterWorkflowTypeResponse

    request  = post "RegisterWorkflowType"
    response = nullResponse RegisterWorkflowTypeResponse
