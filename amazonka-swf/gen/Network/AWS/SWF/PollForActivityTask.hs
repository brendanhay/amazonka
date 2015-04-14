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

-- Module      : Network.AWS.SWF.PollForActivityTask
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

-- | Used by workers to get an 'ActivityTask' from the specified activity 'taskList'.
-- This initiates a long poll, where the service holds the HTTP connection open
-- and responds as soon as a task becomes available. The maximum time the
-- service holds on to the request before responding is 60 seconds. If no task
-- is available within 60 seconds, the poll will return an empty result. An
-- empty result, in this context, means that an ActivityTask is returned, but
-- that the value of taskToken is an empty string. If a task is returned, the
-- worker should use its type to identify and process it correctly.
--
-- Workers should set their client side socket timeout to at least 70 seconds
-- (10 seconds higher than the maximum time service may hold the poll request). Access Control
--
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. Constrain the 'taskList.name' parameter by using a Condition
-- element with the 'swf:taskList.name' key to allow the action to access only
-- certain task lists.  If the caller does not have sufficient permissions to
-- invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails. The associated event attribute's cause
-- parameter will be set to OPERATION_NOT_PERMITTED. For details and example IAM
-- policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_PollForActivityTask.html>
module Network.AWS.SWF.PollForActivityTask
    (
    -- * Request
      PollForActivityTask
    -- ** Request constructor
    , pollForActivityTask
    -- ** Request lenses
    , pfatDomain
    , pfatIdentity
    , pfatTaskList

    -- * Response
    , PollForActivityTaskResponse
    -- ** Response constructor
    , pollForActivityTaskResponse
    -- ** Response lenses
    , pfatrActivityId
    , pfatrActivityType
    , pfatrInput
    , pfatrStartedEventId
    , pfatrTaskToken
    , pfatrWorkflowExecution
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data PollForActivityTask = PollForActivityTask
    { _pfatDomain   :: Text
    , _pfatIdentity :: Maybe Text
    , _pfatTaskList :: TaskList
    } deriving (Eq, Read, Show)

-- | 'PollForActivityTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfatDomain' @::@ 'Text'
--
-- * 'pfatIdentity' @::@ 'Maybe' 'Text'
--
-- * 'pfatTaskList' @::@ 'TaskList'
--
pollForActivityTask :: Text -- ^ 'pfatDomain'
                    -> TaskList -- ^ 'pfatTaskList'
                    -> PollForActivityTask
pollForActivityTask p1 p2 = PollForActivityTask
    { _pfatDomain   = p1
    , _pfatTaskList = p2
    , _pfatIdentity = Nothing
    }

-- | The name of the domain that contains the task lists being polled.
pfatDomain :: Lens' PollForActivityTask Text
pfatDomain = lens _pfatDomain (\s a -> s { _pfatDomain = a })

-- | Identity of the worker making the request, recorded in the 'ActivityTaskStarted'
-- event in the workflow history. This enables diagnostic tracing when problems
-- arise. The form of this identity is user defined.
pfatIdentity :: Lens' PollForActivityTask (Maybe Text)
pfatIdentity = lens _pfatIdentity (\s a -> s { _pfatIdentity = a })

-- | Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string quotarnquot.
pfatTaskList :: Lens' PollForActivityTask TaskList
pfatTaskList = lens _pfatTaskList (\s a -> s { _pfatTaskList = a })

data PollForActivityTaskResponse = PollForActivityTaskResponse
    { _pfatrActivityId        :: Text
    , _pfatrActivityType      :: ActivityType
    , _pfatrInput             :: Maybe Text
    , _pfatrStartedEventId    :: Integer
    , _pfatrTaskToken         :: Text
    , _pfatrWorkflowExecution :: WorkflowExecution
    } deriving (Eq, Read, Show)

-- | 'PollForActivityTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfatrActivityId' @::@ 'Text'
--
-- * 'pfatrActivityType' @::@ 'ActivityType'
--
-- * 'pfatrInput' @::@ 'Maybe' 'Text'
--
-- * 'pfatrStartedEventId' @::@ 'Integer'
--
-- * 'pfatrTaskToken' @::@ 'Text'
--
-- * 'pfatrWorkflowExecution' @::@ 'WorkflowExecution'
--
pollForActivityTaskResponse :: Text -- ^ 'pfatrTaskToken'
                            -> Text -- ^ 'pfatrActivityId'
                            -> Integer -- ^ 'pfatrStartedEventId'
                            -> WorkflowExecution -- ^ 'pfatrWorkflowExecution'
                            -> ActivityType -- ^ 'pfatrActivityType'
                            -> PollForActivityTaskResponse
pollForActivityTaskResponse p1 p2 p3 p4 p5 = PollForActivityTaskResponse
    { _pfatrTaskToken         = p1
    , _pfatrActivityId        = p2
    , _pfatrStartedEventId    = p3
    , _pfatrWorkflowExecution = p4
    , _pfatrActivityType      = p5
    , _pfatrInput             = Nothing
    }

-- | The unique ID of the task.
pfatrActivityId :: Lens' PollForActivityTaskResponse Text
pfatrActivityId = lens _pfatrActivityId (\s a -> s { _pfatrActivityId = a })

-- | The type of this activity task.
pfatrActivityType :: Lens' PollForActivityTaskResponse ActivityType
pfatrActivityType =
    lens _pfatrActivityType (\s a -> s { _pfatrActivityType = a })

-- | The inputs provided when the activity task was scheduled. The form of the
-- input is user defined and should be meaningful to the activity implementation.
pfatrInput :: Lens' PollForActivityTaskResponse (Maybe Text)
pfatrInput = lens _pfatrInput (\s a -> s { _pfatrInput = a })

-- | The id of the 'ActivityTaskStarted' event recorded in the history.
pfatrStartedEventId :: Lens' PollForActivityTaskResponse Integer
pfatrStartedEventId =
    lens _pfatrStartedEventId (\s a -> s { _pfatrStartedEventId = a })

-- | The opaque string used as a handle on the task. This token is used by workers
-- to communicate progress and response information back to the system about the
-- task.
pfatrTaskToken :: Lens' PollForActivityTaskResponse Text
pfatrTaskToken = lens _pfatrTaskToken (\s a -> s { _pfatrTaskToken = a })

-- | The workflow execution that started this activity task.
pfatrWorkflowExecution :: Lens' PollForActivityTaskResponse WorkflowExecution
pfatrWorkflowExecution =
    lens _pfatrWorkflowExecution (\s a -> s { _pfatrWorkflowExecution = a })

instance ToPath PollForActivityTask where
    toPath = const "/"

instance ToQuery PollForActivityTask where
    toQuery = const mempty

instance ToHeaders PollForActivityTask

instance ToJSON PollForActivityTask where
    toJSON PollForActivityTask{..} = object
        [ "domain"   .= _pfatDomain
        , "taskList" .= _pfatTaskList
        , "identity" .= _pfatIdentity
        ]

instance AWSRequest PollForActivityTask where
    type Sv PollForActivityTask = SWF
    type Rs PollForActivityTask = PollForActivityTaskResponse

    request  = post "PollForActivityTask"
    response = jsonResponse

instance FromJSON PollForActivityTaskResponse where
    parseJSON = withObject "PollForActivityTaskResponse" $ \o -> PollForActivityTaskResponse
        <$> o .:  "activityId"
        <*> o .:  "activityType"
        <*> o .:? "input"
        <*> o .:  "startedEventId"
        <*> o .:  "taskToken"
        <*> o .:  "workflowExecution"
