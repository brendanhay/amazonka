{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForActivityTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to get an ActivityTask from the specified activity
-- @taskList@. This initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon as a task becomes available. The
-- maximum time the service holds on to the request before responding is 60
-- seconds. If no task is available within 60 seconds, the poll will return
-- an empty result. An empty result, in this context, means that an
-- ActivityTask is returned, but that the value of taskToken is an empty
-- string. If a task is returned, the worker should use its type to
-- identify and process it correctly.
--
-- Workers should set their client side socket timeout to at least 70
-- seconds (10 seconds higher than the maximum time service may hold the
-- poll request).
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the @taskList.name@ parameter by using a __Condition__
--     element with the @swf:taskList.name@ key to allow the action to
--     access only certain task lists.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_PollForActivityTask.html>
module Network.AWS.SWF.PollForActivityTask
    (
    -- * Request
      PollForActivityTask
    -- ** Request constructor
    , pollForActivityTask
    -- ** Request lenses
    , pfatIdentity
    , pfatDomain
    , pfatTaskList

    -- * Response
    , PollForActivityTaskResponse
    -- ** Response constructor
    , pollForActivityTaskResponse
    -- ** Response lenses
    , pfatrsInput
    , pfatrsStatus
    , pfatrsTaskToken
    , pfatrsActivityId
    , pfatrsStartedEventId
    , pfatrsWorkflowExecution
    , pfatrsActivityType
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'pollForActivityTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfatIdentity'
--
-- * 'pfatDomain'
--
-- * 'pfatTaskList'
data PollForActivityTask = PollForActivityTask'
    { _pfatIdentity :: !(Maybe Text)
    , _pfatDomain   :: !Text
    , _pfatTaskList :: !TaskList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PollForActivityTask' smart constructor.
pollForActivityTask :: Text -> TaskList -> PollForActivityTask
pollForActivityTask pDomain_ pTaskList_ =
    PollForActivityTask'
    { _pfatIdentity = Nothing
    , _pfatDomain = pDomain_
    , _pfatTaskList = pTaskList_
    }

-- | Identity of the worker making the request, recorded in the
-- @ActivityTaskStarted@ event in the workflow history. This enables
-- diagnostic tracing when problems arise. The form of this identity is
-- user defined.
pfatIdentity :: Lens' PollForActivityTask (Maybe Text)
pfatIdentity = lens _pfatIdentity (\ s a -> s{_pfatIdentity = a});

-- | The name of the domain that contains the task lists being polled.
pfatDomain :: Lens' PollForActivityTask Text
pfatDomain = lens _pfatDomain (\ s a -> s{_pfatDomain = a});

-- | Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
pfatTaskList :: Lens' PollForActivityTask TaskList
pfatTaskList = lens _pfatTaskList (\ s a -> s{_pfatTaskList = a});

instance AWSRequest PollForActivityTask where
        type Sv PollForActivityTask = SWF
        type Rs PollForActivityTask =
             PollForActivityTaskResponse
        request = postJSON "PollForActivityTask"
        response
          = receiveJSON
              (\ s h x ->
                 PollForActivityTaskResponse' <$>
                   (x .?> "input") <*> (pure (fromEnum s)) <*>
                     (x .:> "taskToken")
                     <*> (x .:> "activityId")
                     <*> (x .:> "startedEventId")
                     <*> (x .:> "workflowExecution")
                     <*> (x .:> "activityType"))

instance ToHeaders PollForActivityTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.PollForActivityTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON PollForActivityTask where
        toJSON PollForActivityTask'{..}
          = object
              ["identity" .= _pfatIdentity,
               "domain" .= _pfatDomain, "taskList" .= _pfatTaskList]

instance ToPath PollForActivityTask where
        toPath = const "/"

instance ToQuery PollForActivityTask where
        toQuery = const mempty

-- | Unit of work sent to an activity worker.
--
-- /See:/ 'pollForActivityTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfatrsInput'
--
-- * 'pfatrsStatus'
--
-- * 'pfatrsTaskToken'
--
-- * 'pfatrsActivityId'
--
-- * 'pfatrsStartedEventId'
--
-- * 'pfatrsWorkflowExecution'
--
-- * 'pfatrsActivityType'
data PollForActivityTaskResponse = PollForActivityTaskResponse'
    { _pfatrsInput             :: !(Maybe Text)
    , _pfatrsStatus            :: !Int
    , _pfatrsTaskToken         :: !Text
    , _pfatrsActivityId        :: !Text
    , _pfatrsStartedEventId    :: !Integer
    , _pfatrsWorkflowExecution :: !WorkflowExecution
    , _pfatrsActivityType      :: !ActivityType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PollForActivityTaskResponse' smart constructor.
pollForActivityTaskResponse :: Int -> Text -> Text -> Integer -> WorkflowExecution -> ActivityType -> PollForActivityTaskResponse
pollForActivityTaskResponse pStatus_ pTaskToken_ pActivityId_ pStartedEventId_ pWorkflowExecution_ pActivityType_ =
    PollForActivityTaskResponse'
    { _pfatrsInput = Nothing
    , _pfatrsStatus = pStatus_
    , _pfatrsTaskToken = pTaskToken_
    , _pfatrsActivityId = pActivityId_
    , _pfatrsStartedEventId = pStartedEventId_
    , _pfatrsWorkflowExecution = pWorkflowExecution_
    , _pfatrsActivityType = pActivityType_
    }

-- | The inputs provided when the activity task was scheduled. The form of
-- the input is user defined and should be meaningful to the activity
-- implementation.
pfatrsInput :: Lens' PollForActivityTaskResponse (Maybe Text)
pfatrsInput = lens _pfatrsInput (\ s a -> s{_pfatrsInput = a});

-- | FIXME: Undocumented member.
pfatrsStatus :: Lens' PollForActivityTaskResponse Int
pfatrsStatus = lens _pfatrsStatus (\ s a -> s{_pfatrsStatus = a});

-- | The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the
-- system about the task.
pfatrsTaskToken :: Lens' PollForActivityTaskResponse Text
pfatrsTaskToken = lens _pfatrsTaskToken (\ s a -> s{_pfatrsTaskToken = a});

-- | The unique ID of the task.
pfatrsActivityId :: Lens' PollForActivityTaskResponse Text
pfatrsActivityId = lens _pfatrsActivityId (\ s a -> s{_pfatrsActivityId = a});

-- | The id of the @ActivityTaskStarted@ event recorded in the history.
pfatrsStartedEventId :: Lens' PollForActivityTaskResponse Integer
pfatrsStartedEventId = lens _pfatrsStartedEventId (\ s a -> s{_pfatrsStartedEventId = a});

-- | The workflow execution that started this activity task.
pfatrsWorkflowExecution :: Lens' PollForActivityTaskResponse WorkflowExecution
pfatrsWorkflowExecution = lens _pfatrsWorkflowExecution (\ s a -> s{_pfatrsWorkflowExecution = a});

-- | The type of this activity task.
pfatrsActivityType :: Lens' PollForActivityTaskResponse ActivityType
pfatrsActivityType = lens _pfatrsActivityType (\ s a -> s{_pfatrsActivityType = a});
