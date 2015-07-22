{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of open workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListOpenWorkflowExecutions.html>
module Network.AWS.SWF.ListOpenWorkflowExecutions
    (
    -- * Request
      ListOpenWorkflowExecutions
    -- ** Request constructor
    , listOpenWorkflowExecutions
    -- ** Request lenses
    , lowerqNextPageToken
    , lowerqExecutionFilter
    , lowerqTypeFilter
    , lowerqReverseOrder
    , lowerqTagFilter
    , lowerqMaximumPageSize
    , lowerqDomain
    , lowerqStartTimeFilter

    -- * Response
    , WorkflowExecutionInfos
    -- ** Response constructor
    , workflowExecutionInfos
    -- ** Response lenses
    , lowersNextPageToken
    , lowersExecutionInfos
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'listOpenWorkflowExecutions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lowerqNextPageToken'
--
-- * 'lowerqExecutionFilter'
--
-- * 'lowerqTypeFilter'
--
-- * 'lowerqReverseOrder'
--
-- * 'lowerqTagFilter'
--
-- * 'lowerqMaximumPageSize'
--
-- * 'lowerqDomain'
--
-- * 'lowerqStartTimeFilter'
data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions'
    { _lowerqNextPageToken   :: !(Maybe Text)
    , _lowerqExecutionFilter :: !(Maybe WorkflowExecutionFilter)
    , _lowerqTypeFilter      :: !(Maybe WorkflowTypeFilter)
    , _lowerqReverseOrder    :: !(Maybe Bool)
    , _lowerqTagFilter       :: !(Maybe TagFilter)
    , _lowerqMaximumPageSize :: !(Maybe Nat)
    , _lowerqDomain          :: !Text
    , _lowerqStartTimeFilter :: !ExecutionTimeFilter
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListOpenWorkflowExecutions' smart constructor.
listOpenWorkflowExecutions :: Text -> ExecutionTimeFilter -> ListOpenWorkflowExecutions
listOpenWorkflowExecutions pDomain pStartTimeFilter =
    ListOpenWorkflowExecutions'
    { _lowerqNextPageToken = Nothing
    , _lowerqExecutionFilter = Nothing
    , _lowerqTypeFilter = Nothing
    , _lowerqReverseOrder = Nothing
    , _lowerqTagFilter = Nothing
    , _lowerqMaximumPageSize = Nothing
    , _lowerqDomain = pDomain
    , _lowerqStartTimeFilter = pStartTimeFilter
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
lowerqNextPageToken :: Lens' ListOpenWorkflowExecutions (Maybe Text)
lowerqNextPageToken = lens _lowerqNextPageToken (\ s a -> s{_lowerqNextPageToken = a});

-- | If specified, only workflow executions matching the workflow id
-- specified in the filter are returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
lowerqExecutionFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowExecutionFilter)
lowerqExecutionFilter = lens _lowerqExecutionFilter (\ s a -> s{_lowerqExecutionFilter = a});

-- | If specified, only executions of the type specified in the filter are
-- returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
lowerqTypeFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowTypeFilter)
lowerqTypeFilter = lens _lowerqTypeFilter (\ s a -> s{_lowerqTypeFilter = a});

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the
-- executions.
lowerqReverseOrder :: Lens' ListOpenWorkflowExecutions (Maybe Bool)
lowerqReverseOrder = lens _lowerqReverseOrder (\ s a -> s{_lowerqReverseOrder = a});

-- | If specified, only executions that have the matching tag are listed.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
lowerqTagFilter :: Lens' ListOpenWorkflowExecutions (Maybe TagFilter)
lowerqTagFilter = lens _lowerqTagFilter (\ s a -> s{_lowerqTagFilter = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 100, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than 100.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
lowerqMaximumPageSize :: Lens' ListOpenWorkflowExecutions (Maybe Natural)
lowerqMaximumPageSize = lens _lowerqMaximumPageSize (\ s a -> s{_lowerqMaximumPageSize = a}) . mapping _Nat;

-- | The name of the domain that contains the workflow executions to list.
lowerqDomain :: Lens' ListOpenWorkflowExecutions Text
lowerqDomain = lens _lowerqDomain (\ s a -> s{_lowerqDomain = a});

-- | Workflow executions are included in the returned results based on
-- whether their start times are within the range specified by this filter.
lowerqStartTimeFilter :: Lens' ListOpenWorkflowExecutions ExecutionTimeFilter
lowerqStartTimeFilter = lens _lowerqStartTimeFilter (\ s a -> s{_lowerqStartTimeFilter = a});

instance AWSPager ListOpenWorkflowExecutions where
        page rq rs
          | stop (rs ^. lowersNextPageToken) = Nothing
          | stop (rs ^. lowersExecutionInfos) = Nothing
          | otherwise =
            Just $ rq &
              lowerqNextPageToken .~ rs ^. lowersNextPageToken

instance AWSRequest ListOpenWorkflowExecutions where
        type Sv ListOpenWorkflowExecutions = SWF
        type Rs ListOpenWorkflowExecutions =
             WorkflowExecutionInfos
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders ListOpenWorkflowExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.ListOpenWorkflowExecutions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListOpenWorkflowExecutions where
        toJSON ListOpenWorkflowExecutions'{..}
          = object
              ["nextPageToken" .= _lowerqNextPageToken,
               "executionFilter" .= _lowerqExecutionFilter,
               "typeFilter" .= _lowerqTypeFilter,
               "reverseOrder" .= _lowerqReverseOrder,
               "tagFilter" .= _lowerqTagFilter,
               "maximumPageSize" .= _lowerqMaximumPageSize,
               "domain" .= _lowerqDomain,
               "startTimeFilter" .= _lowerqStartTimeFilter]

instance ToPath ListOpenWorkflowExecutions where
        toPath = const "/"

instance ToQuery ListOpenWorkflowExecutions where
        toQuery = const mempty
