{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForDecisionTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Used by deciders to get a DecisionTask from the specified decision
-- @taskList@. A decision task may be returned for any open workflow
-- execution that is using the specified task list. The task includes a
-- paginated view of the history of the workflow execution. The decider
-- should use the workflow type and the history to determine how to
-- properly handle the task.
--
-- This action initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon a task becomes available. If no
-- decision task is available in the specified task list before the timeout
-- of 60 seconds expires, an empty result is returned. An empty result, in
-- this context, means that a DecisionTask is returned, but that the value
-- of taskToken is an empty string.
--
-- Deciders should set their client side socket timeout to at least 70
-- seconds (10 seconds higher than the timeout).
--
-- Because the number of workflow history events for a single workflow
-- execution might be very large, the result returned might be split up
-- across a number of pages. To retrieve subsequent pages, make additional
-- calls to @PollForDecisionTask@ using the @nextPageToken@ returned by the
-- initial call. Note that you do __not__ call
-- @GetWorkflowExecutionHistory@ with this @nextPageToken@. Instead, call
-- @PollForDecisionTask@ again.
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_PollForDecisionTask.html>
module Network.AWS.SWF.PollForDecisionTask
    (
    -- * Request
      PollForDecisionTask
    -- ** Request constructor
    , pollForDecisionTask
    -- ** Request lenses
    , pfdtrqNextPageToken
    , pfdtrqReverseOrder
    , pfdtrqIdentity
    , pfdtrqMaximumPageSize
    , pfdtrqDomain
    , pfdtrqTaskList

    -- * Response
    , PollForDecisionTaskResponse
    -- ** Response constructor
    , pollForDecisionTaskResponse
    -- ** Response lenses
    , pfdtrsNextPageToken
    , pfdtrsPreviousStartedEventId
    , pfdtrsStatus
    , pfdtrsTaskToken
    , pfdtrsStartedEventId
    , pfdtrsWorkflowExecution
    , pfdtrsWorkflowType
    , pfdtrsEvents
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'pollForDecisionTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfdtrqNextPageToken'
--
-- * 'pfdtrqReverseOrder'
--
-- * 'pfdtrqIdentity'
--
-- * 'pfdtrqMaximumPageSize'
--
-- * 'pfdtrqDomain'
--
-- * 'pfdtrqTaskList'
data PollForDecisionTask = PollForDecisionTask'
    { _pfdtrqNextPageToken   :: !(Maybe Text)
    , _pfdtrqReverseOrder    :: !(Maybe Bool)
    , _pfdtrqIdentity        :: !(Maybe Text)
    , _pfdtrqMaximumPageSize :: !(Maybe Nat)
    , _pfdtrqDomain          :: !Text
    , _pfdtrqTaskList        :: !TaskList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PollForDecisionTask' smart constructor.
pollForDecisionTask :: Text -> TaskList -> PollForDecisionTask
pollForDecisionTask pDomain_ pTaskList_ =
    PollForDecisionTask'
    { _pfdtrqNextPageToken = Nothing
    , _pfdtrqReverseOrder = Nothing
    , _pfdtrqIdentity = Nothing
    , _pfdtrqMaximumPageSize = Nothing
    , _pfdtrqDomain = pDomain_
    , _pfdtrqTaskList = pTaskList_
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- The @nextPageToken@ returned by this action cannot be used with
-- GetWorkflowExecutionHistory to get the next page. You must call
-- PollForDecisionTask again (with the @nextPageToken@) to retrieve the
-- next page of history records. Calling PollForDecisionTask with a
-- @nextPageToken@ will not return a new decision task.
--
-- .
pfdtrqNextPageToken :: Lens' PollForDecisionTask (Maybe Text)
pfdtrqNextPageToken = lens _pfdtrqNextPageToken (\ s a -> s{_pfdtrqNextPageToken = a});

-- | When set to @true@, returns the events in reverse order. By default the
-- results are returned in ascending order of the @eventTimestamp@ of the
-- events.
pfdtrqReverseOrder :: Lens' PollForDecisionTask (Maybe Bool)
pfdtrqReverseOrder = lens _pfdtrqReverseOrder (\ s a -> s{_pfdtrqReverseOrder = a});

-- | Identity of the decider making the request, which is recorded in the
-- DecisionTaskStarted event in the workflow history. This enables
-- diagnostic tracing when problems arise. The form of this identity is
-- user defined.
pfdtrqIdentity :: Lens' PollForDecisionTask (Maybe Text)
pfdtrqIdentity = lens _pfdtrqIdentity (\ s a -> s{_pfdtrqIdentity = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 100, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than 100.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
pfdtrqMaximumPageSize :: Lens' PollForDecisionTask (Maybe Natural)
pfdtrqMaximumPageSize = lens _pfdtrqMaximumPageSize (\ s a -> s{_pfdtrqMaximumPageSize = a}) . mapping _Nat;

-- | The name of the domain containing the task lists to poll.
pfdtrqDomain :: Lens' PollForDecisionTask Text
pfdtrqDomain = lens _pfdtrqDomain (\ s a -> s{_pfdtrqDomain = a});

-- | Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
pfdtrqTaskList :: Lens' PollForDecisionTask TaskList
pfdtrqTaskList = lens _pfdtrqTaskList (\ s a -> s{_pfdtrqTaskList = a});

instance AWSPager PollForDecisionTask where
        page rq rs
          | stop (rs ^. pfdtrsNextPageToken) = Nothing
          | stop (rs ^. pfdtrsEvents) = Nothing
          | otherwise =
            Just $ rq &
              pfdtrqNextPageToken .~ rs ^. pfdtrsNextPageToken

instance AWSRequest PollForDecisionTask where
        type Sv PollForDecisionTask = SWF
        type Rs PollForDecisionTask =
             PollForDecisionTaskResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PollForDecisionTaskResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "previousStartedEventId")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "taskToken")
                     <*> (x .:> "startedEventId")
                     <*> (x .:> "workflowExecution")
                     <*> (x .:> "workflowType")
                     <*> (x .?> "events" .!@ mempty))

instance ToHeaders PollForDecisionTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.PollForDecisionTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON PollForDecisionTask where
        toJSON PollForDecisionTask'{..}
          = object
              ["nextPageToken" .= _pfdtrqNextPageToken,
               "reverseOrder" .= _pfdtrqReverseOrder,
               "identity" .= _pfdtrqIdentity,
               "maximumPageSize" .= _pfdtrqMaximumPageSize,
               "domain" .= _pfdtrqDomain,
               "taskList" .= _pfdtrqTaskList]

instance ToPath PollForDecisionTask where
        toPath = const "/"

instance ToQuery PollForDecisionTask where
        toQuery = const mempty

-- | A structure that represents a decision task. Decision tasks are sent to
-- deciders in order for them to make decisions.
--
-- /See:/ 'pollForDecisionTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pfdtrsNextPageToken'
--
-- * 'pfdtrsPreviousStartedEventId'
--
-- * 'pfdtrsStatus'
--
-- * 'pfdtrsTaskToken'
--
-- * 'pfdtrsStartedEventId'
--
-- * 'pfdtrsWorkflowExecution'
--
-- * 'pfdtrsWorkflowType'
--
-- * 'pfdtrsEvents'
data PollForDecisionTaskResponse = PollForDecisionTaskResponse'
    { _pfdtrsNextPageToken          :: !(Maybe Text)
    , _pfdtrsPreviousStartedEventId :: !(Maybe Integer)
    , _pfdtrsStatus                 :: !Int
    , _pfdtrsTaskToken              :: !Text
    , _pfdtrsStartedEventId         :: !Integer
    , _pfdtrsWorkflowExecution      :: !WorkflowExecution
    , _pfdtrsWorkflowType           :: !WorkflowType
    , _pfdtrsEvents                 :: ![HistoryEvent]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PollForDecisionTaskResponse' smart constructor.
pollForDecisionTaskResponse :: Int -> Text -> Integer -> WorkflowExecution -> WorkflowType -> PollForDecisionTaskResponse
pollForDecisionTaskResponse pStatus_ pTaskToken_ pStartedEventId_ pWorkflowExecution_ pWorkflowType_ =
    PollForDecisionTaskResponse'
    { _pfdtrsNextPageToken = Nothing
    , _pfdtrsPreviousStartedEventId = Nothing
    , _pfdtrsStatus = pStatus_
    , _pfdtrsTaskToken = pTaskToken_
    , _pfdtrsStartedEventId = pStartedEventId_
    , _pfdtrsWorkflowExecution = pWorkflowExecution_
    , _pfdtrsWorkflowType = pWorkflowType_
    , _pfdtrsEvents = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
pfdtrsNextPageToken :: Lens' PollForDecisionTaskResponse (Maybe Text)
pfdtrsNextPageToken = lens _pfdtrsNextPageToken (\ s a -> s{_pfdtrsNextPageToken = a});

-- | The id of the DecisionTaskStarted event of the previous decision task of
-- this workflow execution that was processed by the decider. This can be
-- used to determine the events in the history new since the last decision
-- task received by the decider.
pfdtrsPreviousStartedEventId :: Lens' PollForDecisionTaskResponse (Maybe Integer)
pfdtrsPreviousStartedEventId = lens _pfdtrsPreviousStartedEventId (\ s a -> s{_pfdtrsPreviousStartedEventId = a});

-- | FIXME: Undocumented member.
pfdtrsStatus :: Lens' PollForDecisionTaskResponse Int
pfdtrsStatus = lens _pfdtrsStatus (\ s a -> s{_pfdtrsStatus = a});

-- | The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the
-- system about the task.
pfdtrsTaskToken :: Lens' PollForDecisionTaskResponse Text
pfdtrsTaskToken = lens _pfdtrsTaskToken (\ s a -> s{_pfdtrsTaskToken = a});

-- | The id of the @DecisionTaskStarted@ event recorded in the history.
pfdtrsStartedEventId :: Lens' PollForDecisionTaskResponse Integer
pfdtrsStartedEventId = lens _pfdtrsStartedEventId (\ s a -> s{_pfdtrsStartedEventId = a});

-- | The workflow execution for which this decision task was created.
pfdtrsWorkflowExecution :: Lens' PollForDecisionTaskResponse WorkflowExecution
pfdtrsWorkflowExecution = lens _pfdtrsWorkflowExecution (\ s a -> s{_pfdtrsWorkflowExecution = a});

-- | The type of the workflow execution for which this decision task was
-- created.
pfdtrsWorkflowType :: Lens' PollForDecisionTaskResponse WorkflowType
pfdtrsWorkflowType = lens _pfdtrsWorkflowType (\ s a -> s{_pfdtrsWorkflowType = a});

-- | A paginated list of history events of the workflow execution. The
-- decider uses this during the processing of the decision task.
pfdtrsEvents :: Lens' PollForDecisionTaskResponse [HistoryEvent]
pfdtrsEvents = lens _pfdtrsEvents (\ s a -> s{_pfdtrsEvents = a});
