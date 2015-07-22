{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListClosedWorkflowExecutions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of closed workflow executions in the specified domain
-- that meet the filtering criteria. The results may be split into multiple
-- pages. To retrieve subsequent pages, make the call again using the
-- nextPageToken returned by the initial call.
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
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @tagFilter.tag@: String constraint. The key is
--         @swf:tagFilter.tag@.
--     -   @typeFilter.name@: String constraint. The key is
--         @swf:typeFilter.name@.
--     -   @typeFilter.version@: String constraint. The key is
--         @swf:typeFilter.version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListClosedWorkflowExecutions.html>
module Network.AWS.SWF.ListClosedWorkflowExecutions
    (
    -- * Request
      ListClosedWorkflowExecutions
    -- ** Request constructor
    , listClosedWorkflowExecutions
    -- ** Request lenses
    , lcwerqNextPageToken
    , lcwerqCloseStatusFilter
    , lcwerqExecutionFilter
    , lcwerqTypeFilter
    , lcwerqCloseTimeFilter
    , lcwerqReverseOrder
    , lcwerqTagFilter
    , lcwerqStartTimeFilter
    , lcwerqMaximumPageSize
    , lcwerqDomain

    -- * Response
    , WorkflowExecutionInfos
    -- ** Response constructor
    , workflowExecutionInfos
    -- ** Response lenses
    , lcwersNextPageToken
    , lcwersExecutionInfos
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'listClosedWorkflowExecutions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcwerqNextPageToken'
--
-- * 'lcwerqCloseStatusFilter'
--
-- * 'lcwerqExecutionFilter'
--
-- * 'lcwerqTypeFilter'
--
-- * 'lcwerqCloseTimeFilter'
--
-- * 'lcwerqReverseOrder'
--
-- * 'lcwerqTagFilter'
--
-- * 'lcwerqStartTimeFilter'
--
-- * 'lcwerqMaximumPageSize'
--
-- * 'lcwerqDomain'
data ListClosedWorkflowExecutions = ListClosedWorkflowExecutions'
    { _lcwerqNextPageToken     :: !(Maybe Text)
    , _lcwerqCloseStatusFilter :: !(Maybe CloseStatusFilter)
    , _lcwerqExecutionFilter   :: !(Maybe WorkflowExecutionFilter)
    , _lcwerqTypeFilter        :: !(Maybe WorkflowTypeFilter)
    , _lcwerqCloseTimeFilter   :: !(Maybe ExecutionTimeFilter)
    , _lcwerqReverseOrder      :: !(Maybe Bool)
    , _lcwerqTagFilter         :: !(Maybe TagFilter)
    , _lcwerqStartTimeFilter   :: !(Maybe ExecutionTimeFilter)
    , _lcwerqMaximumPageSize   :: !(Maybe Nat)
    , _lcwerqDomain            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListClosedWorkflowExecutions' smart constructor.
listClosedWorkflowExecutions :: Text -> ListClosedWorkflowExecutions
listClosedWorkflowExecutions pDomain =
    ListClosedWorkflowExecutions'
    { _lcwerqNextPageToken = Nothing
    , _lcwerqCloseStatusFilter = Nothing
    , _lcwerqExecutionFilter = Nothing
    , _lcwerqTypeFilter = Nothing
    , _lcwerqCloseTimeFilter = Nothing
    , _lcwerqReverseOrder = Nothing
    , _lcwerqTagFilter = Nothing
    , _lcwerqStartTimeFilter = Nothing
    , _lcwerqMaximumPageSize = Nothing
    , _lcwerqDomain = pDomain
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
lcwerqNextPageToken :: Lens' ListClosedWorkflowExecutions (Maybe Text)
lcwerqNextPageToken = lens _lcwerqNextPageToken (\ s a -> s{_lcwerqNextPageToken = a});

-- | If specified, only workflow executions that match this /close status/
-- are listed. For example, if TERMINATED is specified, then only
-- TERMINATED workflow executions are listed.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
lcwerqCloseStatusFilter :: Lens' ListClosedWorkflowExecutions (Maybe CloseStatusFilter)
lcwerqCloseStatusFilter = lens _lcwerqCloseStatusFilter (\ s a -> s{_lcwerqCloseStatusFilter = a});

-- | If specified, only workflow executions matching the workflow id
-- specified in the filter are returned.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
lcwerqExecutionFilter :: Lens' ListClosedWorkflowExecutions (Maybe WorkflowExecutionFilter)
lcwerqExecutionFilter = lens _lcwerqExecutionFilter (\ s a -> s{_lcwerqExecutionFilter = a});

-- | If specified, only executions of the type specified in the filter are
-- returned.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
lcwerqTypeFilter :: Lens' ListClosedWorkflowExecutions (Maybe WorkflowTypeFilter)
lcwerqTypeFilter = lens _lcwerqTypeFilter (\ s a -> s{_lcwerqTypeFilter = a});

-- | If specified, the workflow executions are included in the returned
-- results based on whether their close times are within the range
-- specified by this filter. Also, if this parameter is specified, the
-- returned results are ordered by their close times.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
lcwerqCloseTimeFilter :: Lens' ListClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
lcwerqCloseTimeFilter = lens _lcwerqCloseTimeFilter (\ s a -> s{_lcwerqCloseTimeFilter = a});

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start or the close time
-- of the executions.
lcwerqReverseOrder :: Lens' ListClosedWorkflowExecutions (Maybe Bool)
lcwerqReverseOrder = lens _lcwerqReverseOrder (\ s a -> s{_lcwerqReverseOrder = a});

-- | If specified, only executions that have the matching tag are listed.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
lcwerqTagFilter :: Lens' ListClosedWorkflowExecutions (Maybe TagFilter)
lcwerqTagFilter = lens _lcwerqTagFilter (\ s a -> s{_lcwerqTagFilter = a});

-- | If specified, the workflow executions are included in the returned
-- results based on whether their start times are within the range
-- specified by this filter. Also, if this parameter is specified, the
-- returned results are ordered by their start times.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
lcwerqStartTimeFilter :: Lens' ListClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
lcwerqStartTimeFilter = lens _lcwerqStartTimeFilter (\ s a -> s{_lcwerqStartTimeFilter = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 100, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than 100.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
lcwerqMaximumPageSize :: Lens' ListClosedWorkflowExecutions (Maybe Natural)
lcwerqMaximumPageSize = lens _lcwerqMaximumPageSize (\ s a -> s{_lcwerqMaximumPageSize = a}) . mapping _Nat;

-- | The name of the domain that contains the workflow executions to list.
lcwerqDomain :: Lens' ListClosedWorkflowExecutions Text
lcwerqDomain = lens _lcwerqDomain (\ s a -> s{_lcwerqDomain = a});

instance AWSPager ListClosedWorkflowExecutions where
        page rq rs
          | stop (rs ^. lcwersNextPageToken) = Nothing
          | stop (rs ^. lcwersExecutionInfos) = Nothing
          | otherwise =
            Just $ rq &
              lcwerqNextPageToken .~ rs ^. lcwersNextPageToken

instance AWSRequest ListClosedWorkflowExecutions
         where
        type Sv ListClosedWorkflowExecutions = SWF
        type Rs ListClosedWorkflowExecutions =
             WorkflowExecutionInfos
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders ListClosedWorkflowExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.ListClosedWorkflowExecutions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListClosedWorkflowExecutions where
        toJSON ListClosedWorkflowExecutions'{..}
          = object
              ["nextPageToken" .= _lcwerqNextPageToken,
               "closeStatusFilter" .= _lcwerqCloseStatusFilter,
               "executionFilter" .= _lcwerqExecutionFilter,
               "typeFilter" .= _lcwerqTypeFilter,
               "closeTimeFilter" .= _lcwerqCloseTimeFilter,
               "reverseOrder" .= _lcwerqReverseOrder,
               "tagFilter" .= _lcwerqTagFilter,
               "startTimeFilter" .= _lcwerqStartTimeFilter,
               "maximumPageSize" .= _lcwerqMaximumPageSize,
               "domain" .= _lcwerqDomain]

instance ToPath ListClosedWorkflowExecutions where
        toPath = const "/"

instance ToQuery ListClosedWorkflowExecutions where
        toQuery = const mempty
