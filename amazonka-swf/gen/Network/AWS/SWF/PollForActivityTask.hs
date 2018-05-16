{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForActivityTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to get an 'ActivityTask' from the specified activity @taskList@ . This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available. The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns an empty result. An empty result, in this context, means that an ActivityTask is returned, but that the value of taskToken is an empty string. If a task is returned, the worker should use its type to identify and process it correctly.
--
--
-- /Important:/ Workers should set their client side socket timeout to at least 70 seconds (10 seconds higher than the maximum time service may hold the poll request).
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.PollForActivityTask
    (
    -- * Creating a Request
      pollForActivityTask
    , PollForActivityTask
    -- * Request Lenses
    , pfatIdentity
    , pfatDomain
    , pfatTaskList

    -- * Destructuring the Response
    , pollForActivityTaskResponse
    , PollForActivityTaskResponse
    -- * Response Lenses
    , pfatrsActivityType
    , pfatrsActivityId
    , pfatrsInput
    , pfatrsTaskToken
    , pfatrsWorkflowExecution
    , pfatrsResponseStatus
    , pfatrsStartedEventId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'pollForActivityTask' smart constructor.
data PollForActivityTask = PollForActivityTask'
  { _pfatIdentity :: !(Maybe Text)
  , _pfatDomain   :: !Text
  , _pfatTaskList :: !TaskList
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PollForActivityTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfatIdentity' - Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- * 'pfatDomain' - The name of the domain that contains the task lists being polled.
--
-- * 'pfatTaskList' - Specifies the task list to poll for activity tasks. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
pollForActivityTask
    :: Text -- ^ 'pfatDomain'
    -> TaskList -- ^ 'pfatTaskList'
    -> PollForActivityTask
pollForActivityTask pDomain_ pTaskList_ =
  PollForActivityTask'
    { _pfatIdentity = Nothing
    , _pfatDomain = pDomain_
    , _pfatTaskList = pTaskList_
    }


-- | Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
pfatIdentity :: Lens' PollForActivityTask (Maybe Text)
pfatIdentity = lens _pfatIdentity (\ s a -> s{_pfatIdentity = a})

-- | The name of the domain that contains the task lists being polled.
pfatDomain :: Lens' PollForActivityTask Text
pfatDomain = lens _pfatDomain (\ s a -> s{_pfatDomain = a})

-- | Specifies the task list to poll for activity tasks. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
pfatTaskList :: Lens' PollForActivityTask TaskList
pfatTaskList = lens _pfatTaskList (\ s a -> s{_pfatTaskList = a})

instance AWSRequest PollForActivityTask where
        type Rs PollForActivityTask =
             PollForActivityTaskResponse
        request = postJSON swf
        response
          = receiveJSON
              (\ s h x ->
                 PollForActivityTaskResponse' <$>
                   (x .?> "activityType") <*> (x .?> "activityId") <*>
                     (x .?> "input")
                     <*> (x .?> "taskToken")
                     <*> (x .?> "workflowExecution")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "startedEventId"))

instance Hashable PollForActivityTask where

instance NFData PollForActivityTask where

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
              (catMaybes
                 [("identity" .=) <$> _pfatIdentity,
                  Just ("domain" .= _pfatDomain),
                  Just ("taskList" .= _pfatTaskList)])

instance ToPath PollForActivityTask where
        toPath = const "/"

instance ToQuery PollForActivityTask where
        toQuery = const mempty

-- | Unit of work sent to an activity worker.
--
--
--
-- /See:/ 'pollForActivityTaskResponse' smart constructor.
data PollForActivityTaskResponse = PollForActivityTaskResponse'
  { _pfatrsActivityType      :: !(Maybe ActivityType)
  , _pfatrsActivityId        :: !(Maybe Text)
  , _pfatrsInput             :: !(Maybe Text)
  , _pfatrsTaskToken         :: !(Maybe Text)
  , _pfatrsWorkflowExecution :: !(Maybe WorkflowExecution)
  , _pfatrsResponseStatus    :: !Int
  , _pfatrsStartedEventId    :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PollForActivityTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfatrsActivityType' - The type of this activity task.
--
-- * 'pfatrsActivityId' - The unique ID of the task.
--
-- * 'pfatrsInput' - The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
--
-- * 'pfatrsTaskToken' - The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
--
-- * 'pfatrsWorkflowExecution' - The workflow execution that started this activity task.
--
-- * 'pfatrsResponseStatus' - -- | The response status code.
--
-- * 'pfatrsStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded in the history.
pollForActivityTaskResponse
    :: Int -- ^ 'pfatrsResponseStatus'
    -> Integer -- ^ 'pfatrsStartedEventId'
    -> PollForActivityTaskResponse
pollForActivityTaskResponse pResponseStatus_ pStartedEventId_ =
  PollForActivityTaskResponse'
    { _pfatrsActivityType = Nothing
    , _pfatrsActivityId = Nothing
    , _pfatrsInput = Nothing
    , _pfatrsTaskToken = Nothing
    , _pfatrsWorkflowExecution = Nothing
    , _pfatrsResponseStatus = pResponseStatus_
    , _pfatrsStartedEventId = pStartedEventId_
    }


-- | The type of this activity task.
pfatrsActivityType :: Lens' PollForActivityTaskResponse (Maybe ActivityType)
pfatrsActivityType = lens _pfatrsActivityType (\ s a -> s{_pfatrsActivityType = a})

-- | The unique ID of the task.
pfatrsActivityId :: Lens' PollForActivityTaskResponse (Maybe Text)
pfatrsActivityId = lens _pfatrsActivityId (\ s a -> s{_pfatrsActivityId = a})

-- | The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
pfatrsInput :: Lens' PollForActivityTaskResponse (Maybe Text)
pfatrsInput = lens _pfatrsInput (\ s a -> s{_pfatrsInput = a})

-- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
pfatrsTaskToken :: Lens' PollForActivityTaskResponse (Maybe Text)
pfatrsTaskToken = lens _pfatrsTaskToken (\ s a -> s{_pfatrsTaskToken = a})

-- | The workflow execution that started this activity task.
pfatrsWorkflowExecution :: Lens' PollForActivityTaskResponse (Maybe WorkflowExecution)
pfatrsWorkflowExecution = lens _pfatrsWorkflowExecution (\ s a -> s{_pfatrsWorkflowExecution = a})

-- | -- | The response status code.
pfatrsResponseStatus :: Lens' PollForActivityTaskResponse Int
pfatrsResponseStatus = lens _pfatrsResponseStatus (\ s a -> s{_pfatrsResponseStatus = a})

-- | The ID of the @ActivityTaskStarted@ event recorded in the history.
pfatrsStartedEventId :: Lens' PollForActivityTaskResponse Integer
pfatrsStartedEventId = lens _pfatrsStartedEventId (\ s a -> s{_pfatrsStartedEventId = a})

instance NFData PollForActivityTaskResponse where
