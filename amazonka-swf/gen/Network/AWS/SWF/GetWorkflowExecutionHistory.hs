{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.GetWorkflowExecutionHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified workflow execution. The results may
-- be split into multiple pages. To retrieve subsequent pages, make the
-- call again using the @nextPageToken@ returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_GetWorkflowExecutionHistory.html AWS API Reference> for GetWorkflowExecutionHistory.
module Network.AWS.SWF.GetWorkflowExecutionHistory
    (
    -- * Creating a Request
      GetWorkflowExecutionHistory
    , getWorkflowExecutionHistory
    -- * Request Lenses
    , gwehNextPageToken
    , gwehReverseOrder
    , gwehMaximumPageSize
    , gwehDomain
    , gwehExecution

    -- * Destructuring the Response
    , GetWorkflowExecutionHistoryResponse
    , getWorkflowExecutionHistoryResponse
    -- * Response Lenses
    , gwehrsNextPageToken
    , gwehrsStatus
    , gwehrsEvents
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'getWorkflowExecutionHistory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gwehNextPageToken'
--
-- * 'gwehReverseOrder'
--
-- * 'gwehMaximumPageSize'
--
-- * 'gwehDomain'
--
-- * 'gwehExecution'
data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory'
    { _gwehNextPageToken   :: !(Maybe Text)
    , _gwehReverseOrder    :: !(Maybe Bool)
    , _gwehMaximumPageSize :: !(Maybe Nat)
    , _gwehDomain          :: !Text
    , _gwehExecution       :: !WorkflowExecution
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetWorkflowExecutionHistory' smart constructor.
getWorkflowExecutionHistory :: Text -> WorkflowExecution -> GetWorkflowExecutionHistory
getWorkflowExecutionHistory pDomain_ pExecution_ =
    GetWorkflowExecutionHistory'
    { _gwehNextPageToken = Nothing
    , _gwehReverseOrder = Nothing
    , _gwehMaximumPageSize = Nothing
    , _gwehDomain = pDomain_
    , _gwehExecution = pExecution_
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
gwehNextPageToken :: Lens' GetWorkflowExecutionHistory (Maybe Text)
gwehNextPageToken = lens _gwehNextPageToken (\ s a -> s{_gwehNextPageToken = a});

-- | When set to @true@, returns the events in reverse order. By default the
-- results are returned in ascending order of the @eventTimeStamp@ of the
-- events.
gwehReverseOrder :: Lens' GetWorkflowExecutionHistory (Maybe Bool)
gwehReverseOrder = lens _gwehReverseOrder (\ s a -> s{_gwehReverseOrder = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 1000, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than the maximum.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
gwehMaximumPageSize :: Lens' GetWorkflowExecutionHistory (Maybe Natural)
gwehMaximumPageSize = lens _gwehMaximumPageSize (\ s a -> s{_gwehMaximumPageSize = a}) . mapping _Nat;

-- | The name of the domain containing the workflow execution.
gwehDomain :: Lens' GetWorkflowExecutionHistory Text
gwehDomain = lens _gwehDomain (\ s a -> s{_gwehDomain = a});

-- | Specifies the workflow execution for which to return the history.
gwehExecution :: Lens' GetWorkflowExecutionHistory WorkflowExecution
gwehExecution = lens _gwehExecution (\ s a -> s{_gwehExecution = a});

instance AWSPager GetWorkflowExecutionHistory where
        page rq rs
          | stop (rs ^. gwehrsNextPageToken) = Nothing
          | stop (rs ^. gwehrsEvents) = Nothing
          | otherwise =
            Just $ rq &
              gwehNextPageToken .~ rs ^. gwehrsNextPageToken

instance AWSRequest GetWorkflowExecutionHistory where
        type Sv GetWorkflowExecutionHistory = SWF
        type Rs GetWorkflowExecutionHistory =
             GetWorkflowExecutionHistoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetWorkflowExecutionHistoryResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "events" .!@ mempty))

instance ToHeaders GetWorkflowExecutionHistory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.GetWorkflowExecutionHistory"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetWorkflowExecutionHistory where
        toJSON GetWorkflowExecutionHistory'{..}
          = object
              ["nextPageToken" .= _gwehNextPageToken,
               "reverseOrder" .= _gwehReverseOrder,
               "maximumPageSize" .= _gwehMaximumPageSize,
               "domain" .= _gwehDomain,
               "execution" .= _gwehExecution]

instance ToPath GetWorkflowExecutionHistory where
        toPath = const "/"

instance ToQuery GetWorkflowExecutionHistory where
        toQuery = const mempty

-- | Paginated representation of a workflow history for a workflow execution.
-- This is the up to date, complete and authoritative record of the events
-- related to all tasks and events in the life of the workflow execution.
--
-- /See:/ 'getWorkflowExecutionHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gwehrsNextPageToken'
--
-- * 'gwehrsStatus'
--
-- * 'gwehrsEvents'
data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse'
    { _gwehrsNextPageToken :: !(Maybe Text)
    , _gwehrsStatus        :: !Int
    , _gwehrsEvents        :: ![HistoryEvent]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetWorkflowExecutionHistoryResponse' smart constructor.
getWorkflowExecutionHistoryResponse :: Int -> GetWorkflowExecutionHistoryResponse
getWorkflowExecutionHistoryResponse pStatus_ =
    GetWorkflowExecutionHistoryResponse'
    { _gwehrsNextPageToken = Nothing
    , _gwehrsStatus = pStatus_
    , _gwehrsEvents = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
gwehrsNextPageToken :: Lens' GetWorkflowExecutionHistoryResponse (Maybe Text)
gwehrsNextPageToken = lens _gwehrsNextPageToken (\ s a -> s{_gwehrsNextPageToken = a});

-- | Undocumented member.
gwehrsStatus :: Lens' GetWorkflowExecutionHistoryResponse Int
gwehrsStatus = lens _gwehrsStatus (\ s a -> s{_gwehrsStatus = a});

-- | The list of history events.
gwehrsEvents :: Lens' GetWorkflowExecutionHistoryResponse [HistoryEvent]
gwehrsEvents = lens _gwehrsEvents (\ s a -> s{_gwehrsEvents = a}) . _Coerce;
