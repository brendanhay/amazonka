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

-- Module      : Network.AWS.SWF.PollForDecisionTask
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

-- | Used by deciders to get a 'DecisionTask' from the specified decision 'taskList'.
-- A decision task may be returned for any open workflow execution that is using
-- the specified task list. The task includes a paginated view of the history of
-- the workflow execution. The decider should use the workflow type and the
-- history to determine how to properly handle the task.
--
-- This action initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon a task becomes available. If no decision
-- task is available in the specified task list before the timeout of 60 seconds
-- expires, an empty result is returned. An empty result, in this context, means
-- that a DecisionTask is returned, but that the value of taskToken is an empty
-- string.
--
-- Deciders should set their client side socket timeout to at least 70 seconds
-- (10 seconds higher than the timeout). Because the number of workflow history
-- events for a single workflow execution might be very large, the result
-- returned might be split up across a number of pages. To retrieve subsequent
-- pages, make additional calls to 'PollForDecisionTask' using the 'nextPageToken'
-- returned by the initial call. Note that you do not call 'GetWorkflowExecutionHistory' with this 'nextPageToken'. Instead, call 'PollForDecisionTask' again. Access
-- Control
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_PollForDecisionTask.html>
module Network.AWS.SWF.PollForDecisionTask
    (
    -- * Request
      PollForDecisionTask
    -- ** Request constructor
    , pollForDecisionTask
    -- ** Request lenses
    , pfdtDomain
    , pfdtIdentity
    , pfdtMaximumPageSize
    , pfdtNextPageToken
    , pfdtReverseOrder
    , pfdtTaskList

    -- * Response
    , PollForDecisionTaskResponse
    -- ** Response constructor
    , pollForDecisionTaskResponse
    -- ** Response lenses
    , pfdtrEvents
    , pfdtrNextPageToken
    , pfdtrPreviousStartedEventId
    , pfdtrStartedEventId
    , pfdtrTaskToken
    , pfdtrWorkflowExecution
    , pfdtrWorkflowType
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data PollForDecisionTask = PollForDecisionTask
    { _pfdtDomain          :: Text
    , _pfdtIdentity        :: Maybe Text
    , _pfdtMaximumPageSize :: Maybe Nat
    , _pfdtNextPageToken   :: Maybe Text
    , _pfdtReverseOrder    :: Maybe Bool
    , _pfdtTaskList        :: TaskList
    } deriving (Eq, Read, Show)

-- | 'PollForDecisionTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfdtDomain' @::@ 'Text'
--
-- * 'pfdtIdentity' @::@ 'Maybe' 'Text'
--
-- * 'pfdtMaximumPageSize' @::@ 'Maybe' 'Natural'
--
-- * 'pfdtNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'pfdtReverseOrder' @::@ 'Maybe' 'Bool'
--
-- * 'pfdtTaskList' @::@ 'TaskList'
--
pollForDecisionTask :: Text -- ^ 'pfdtDomain'
                    -> TaskList -- ^ 'pfdtTaskList'
                    -> PollForDecisionTask
pollForDecisionTask p1 p2 = PollForDecisionTask
    { _pfdtDomain          = p1
    , _pfdtTaskList        = p2
    , _pfdtIdentity        = Nothing
    , _pfdtNextPageToken   = Nothing
    , _pfdtMaximumPageSize = Nothing
    , _pfdtReverseOrder    = Nothing
    }

-- | The name of the domain containing the task lists to poll.
pfdtDomain :: Lens' PollForDecisionTask Text
pfdtDomain = lens _pfdtDomain (\s a -> s { _pfdtDomain = a })

-- | Identity of the decider making the request, which is recorded in the
-- DecisionTaskStarted event in the workflow history. This enables diagnostic
-- tracing when problems arise. The form of this identity is user defined.
pfdtIdentity :: Lens' PollForDecisionTask (Maybe Text)
pfdtIdentity = lens _pfdtIdentity (\s a -> s { _pfdtIdentity = a })

-- | The maximum number of results that will be returned per call. 'nextPageToken'
-- can be used to obtain futher pages of results. The default is 100, which is
-- the maximum allowed page size. You can, however, specify a page size /smaller/
-- than 100.
--
-- This is an upper limit only; the actual number of results returned per call
-- may be fewer than the specified maximum.
pfdtMaximumPageSize :: Lens' PollForDecisionTask (Maybe Natural)
pfdtMaximumPageSize =
    lens _pfdtMaximumPageSize (\s a -> s { _pfdtMaximumPageSize = a })
        . mapping _Nat

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
--
-- The 'nextPageToken' returned by this action cannot be used with 'GetWorkflowExecutionHistory' to get the next page. You must call 'PollForDecisionTask' again (with the 'nextPageToken') to retrieve the next page of history records. Calling 'PollForDecisionTask'
-- with a 'nextPageToken' will not return a new decision task..
pfdtNextPageToken :: Lens' PollForDecisionTask (Maybe Text)
pfdtNextPageToken =
    lens _pfdtNextPageToken (\s a -> s { _pfdtNextPageToken = a })

-- | When set to 'true', returns the events in reverse order. By default the results
-- are returned in ascending order of the 'eventTimestamp' of the events.
pfdtReverseOrder :: Lens' PollForDecisionTask (Maybe Bool)
pfdtReverseOrder = lens _pfdtReverseOrder (\s a -> s { _pfdtReverseOrder = a })

-- | Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string quotarnquot.
pfdtTaskList :: Lens' PollForDecisionTask TaskList
pfdtTaskList = lens _pfdtTaskList (\s a -> s { _pfdtTaskList = a })

data PollForDecisionTaskResponse = PollForDecisionTaskResponse
    { _pfdtrEvents                 :: List "events" HistoryEvent
    , _pfdtrNextPageToken          :: Maybe Text
    , _pfdtrPreviousStartedEventId :: Maybe Integer
    , _pfdtrStartedEventId         :: Integer
    , _pfdtrTaskToken              :: Text
    , _pfdtrWorkflowExecution      :: WorkflowExecution
    , _pfdtrWorkflowType           :: WorkflowType
    } deriving (Eq, Read, Show)

-- | 'PollForDecisionTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfdtrEvents' @::@ ['HistoryEvent']
--
-- * 'pfdtrNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'pfdtrPreviousStartedEventId' @::@ 'Maybe' 'Integer'
--
-- * 'pfdtrStartedEventId' @::@ 'Integer'
--
-- * 'pfdtrTaskToken' @::@ 'Text'
--
-- * 'pfdtrWorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'pfdtrWorkflowType' @::@ 'WorkflowType'
--
pollForDecisionTaskResponse :: Text -- ^ 'pfdtrTaskToken'
                            -> Integer -- ^ 'pfdtrStartedEventId'
                            -> WorkflowExecution -- ^ 'pfdtrWorkflowExecution'
                            -> WorkflowType -- ^ 'pfdtrWorkflowType'
                            -> PollForDecisionTaskResponse
pollForDecisionTaskResponse p1 p2 p3 p4 = PollForDecisionTaskResponse
    { _pfdtrTaskToken              = p1
    , _pfdtrStartedEventId         = p2
    , _pfdtrWorkflowExecution      = p3
    , _pfdtrWorkflowType           = p4
    , _pfdtrEvents                 = mempty
    , _pfdtrNextPageToken          = Nothing
    , _pfdtrPreviousStartedEventId = Nothing
    }

-- | A paginated list of history events of the workflow execution. The decider
-- uses this during the processing of the decision task.
pfdtrEvents :: Lens' PollForDecisionTaskResponse [HistoryEvent]
pfdtrEvents = lens _pfdtrEvents (\s a -> s { _pfdtrEvents = a }) . _List

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
pfdtrNextPageToken :: Lens' PollForDecisionTaskResponse (Maybe Text)
pfdtrNextPageToken =
    lens _pfdtrNextPageToken (\s a -> s { _pfdtrNextPageToken = a })

-- | The id of the DecisionTaskStarted event of the previous decision task of this
-- workflow execution that was processed by the decider. This can be used to
-- determine the events in the history new since the last decision task received
-- by the decider.
pfdtrPreviousStartedEventId :: Lens' PollForDecisionTaskResponse (Maybe Integer)
pfdtrPreviousStartedEventId =
    lens _pfdtrPreviousStartedEventId
        (\s a -> s { _pfdtrPreviousStartedEventId = a })

-- | The id of the 'DecisionTaskStarted' event recorded in the history.
pfdtrStartedEventId :: Lens' PollForDecisionTaskResponse Integer
pfdtrStartedEventId =
    lens _pfdtrStartedEventId (\s a -> s { _pfdtrStartedEventId = a })

-- | The opaque string used as a handle on the task. This token is used by workers
-- to communicate progress and response information back to the system about the
-- task.
pfdtrTaskToken :: Lens' PollForDecisionTaskResponse Text
pfdtrTaskToken = lens _pfdtrTaskToken (\s a -> s { _pfdtrTaskToken = a })

-- | The workflow execution for which this decision task was created.
pfdtrWorkflowExecution :: Lens' PollForDecisionTaskResponse WorkflowExecution
pfdtrWorkflowExecution =
    lens _pfdtrWorkflowExecution (\s a -> s { _pfdtrWorkflowExecution = a })

-- | The type of the workflow execution for which this decision task was created.
pfdtrWorkflowType :: Lens' PollForDecisionTaskResponse WorkflowType
pfdtrWorkflowType =
    lens _pfdtrWorkflowType (\s a -> s { _pfdtrWorkflowType = a })

instance ToPath PollForDecisionTask where
    toPath = const "/"

instance ToQuery PollForDecisionTask where
    toQuery = const mempty

instance ToHeaders PollForDecisionTask

instance ToJSON PollForDecisionTask where
    toJSON PollForDecisionTask{..} = object
        [ "domain"          .= _pfdtDomain
        , "taskList"        .= _pfdtTaskList
        , "identity"        .= _pfdtIdentity
        , "nextPageToken"   .= _pfdtNextPageToken
        , "maximumPageSize" .= _pfdtMaximumPageSize
        , "reverseOrder"    .= _pfdtReverseOrder
        ]

instance AWSRequest PollForDecisionTask where
    type Sv PollForDecisionTask = SWF
    type Rs PollForDecisionTask = PollForDecisionTaskResponse

    request  = post "PollForDecisionTask"
    response = jsonResponse

instance FromJSON PollForDecisionTaskResponse where
    parseJSON = withObject "PollForDecisionTaskResponse" $ \o -> PollForDecisionTaskResponse
        <$> o .:? "events" .!= mempty
        <*> o .:? "nextPageToken"
        <*> o .:? "previousStartedEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "taskToken"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance AWSPager PollForDecisionTask where
    page rq rs
        | stop (rs ^. pfdtrNextPageToken) = Nothing
        | otherwise = (\x -> rq & pfdtNextPageToken ?~ x)
            <$> (rs ^. pfdtrNextPageToken)
