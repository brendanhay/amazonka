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
-- Module      : Network.AWS.SWF.ListClosedWorkflowExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of closed workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagFilter.tag@ : String constraint. The key is @swf:tagFilter.tag@ .
--
--     * @typeFilter.name@ : String constraint. The key is @swf:typeFilter.name@ .
--
--     * @typeFilter.version@ : String constraint. The key is @swf:typeFilter.version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListClosedWorkflowExecutions
    (
    -- * Creating a Request
      listClosedWorkflowExecutions
    , ListClosedWorkflowExecutions
    -- * Request Lenses
    , lcweNextPageToken
    , lcweExecutionFilter
    , lcweCloseStatusFilter
    , lcweTypeFilter
    , lcweCloseTimeFilter
    , lcweReverseOrder
    , lcweTagFilter
    , lcweStartTimeFilter
    , lcweMaximumPageSize
    , lcweDomain

    -- * Destructuring the Response
    , workflowExecutionInfos
    , WorkflowExecutionInfos
    -- * Response Lenses
    , weiNextPageToken
    , weiExecutionInfos
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'listClosedWorkflowExecutions' smart constructor.
data ListClosedWorkflowExecutions = ListClosedWorkflowExecutions'
  { _lcweNextPageToken     :: !(Maybe Text)
  , _lcweExecutionFilter   :: !(Maybe WorkflowExecutionFilter)
  , _lcweCloseStatusFilter :: !(Maybe CloseStatusFilter)
  , _lcweTypeFilter        :: !(Maybe WorkflowTypeFilter)
  , _lcweCloseTimeFilter   :: !(Maybe ExecutionTimeFilter)
  , _lcweReverseOrder      :: !(Maybe Bool)
  , _lcweTagFilter         :: !(Maybe TagFilter)
  , _lcweStartTimeFilter   :: !(Maybe ExecutionTimeFilter)
  , _lcweMaximumPageSize   :: !(Maybe Nat)
  , _lcweDomain            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListClosedWorkflowExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcweNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'lcweExecutionFilter' - If specified, only workflow executions matching the workflow ID specified in the filter are returned.
--
-- * 'lcweCloseStatusFilter' - If specified, only workflow executions that match this /close status/ are listed. For example, if TERMINATED is specified, then only TERMINATED workflow executions are listed.
--
-- * 'lcweTypeFilter' - If specified, only executions of the type specified in the filter are returned.
--
-- * 'lcweCloseTimeFilter' - If specified, the workflow executions are included in the returned results based on whether their close times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their close times.
--
-- * 'lcweReverseOrder' - When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start or the close time of the executions.
--
-- * 'lcweTagFilter' - If specified, only executions that have the matching tag are listed.
--
-- * 'lcweStartTimeFilter' - If specified, the workflow executions are included in the returned results based on whether their start times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their start times.
--
-- * 'lcweMaximumPageSize' - The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- * 'lcweDomain' - The name of the domain that contains the workflow executions to list.
listClosedWorkflowExecutions
    :: Text -- ^ 'lcweDomain'
    -> ListClosedWorkflowExecutions
listClosedWorkflowExecutions pDomain_ =
  ListClosedWorkflowExecutions'
    { _lcweNextPageToken = Nothing
    , _lcweExecutionFilter = Nothing
    , _lcweCloseStatusFilter = Nothing
    , _lcweTypeFilter = Nothing
    , _lcweCloseTimeFilter = Nothing
    , _lcweReverseOrder = Nothing
    , _lcweTagFilter = Nothing
    , _lcweStartTimeFilter = Nothing
    , _lcweMaximumPageSize = Nothing
    , _lcweDomain = pDomain_
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
lcweNextPageToken :: Lens' ListClosedWorkflowExecutions (Maybe Text)
lcweNextPageToken = lens _lcweNextPageToken (\ s a -> s{_lcweNextPageToken = a})

-- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
lcweExecutionFilter :: Lens' ListClosedWorkflowExecutions (Maybe WorkflowExecutionFilter)
lcweExecutionFilter = lens _lcweExecutionFilter (\ s a -> s{_lcweExecutionFilter = a})

-- | If specified, only workflow executions that match this /close status/ are listed. For example, if TERMINATED is specified, then only TERMINATED workflow executions are listed.
lcweCloseStatusFilter :: Lens' ListClosedWorkflowExecutions (Maybe CloseStatusFilter)
lcweCloseStatusFilter = lens _lcweCloseStatusFilter (\ s a -> s{_lcweCloseStatusFilter = a})

-- | If specified, only executions of the type specified in the filter are returned.
lcweTypeFilter :: Lens' ListClosedWorkflowExecutions (Maybe WorkflowTypeFilter)
lcweTypeFilter = lens _lcweTypeFilter (\ s a -> s{_lcweTypeFilter = a})

-- | If specified, the workflow executions are included in the returned results based on whether their close times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their close times.
lcweCloseTimeFilter :: Lens' ListClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
lcweCloseTimeFilter = lens _lcweCloseTimeFilter (\ s a -> s{_lcweCloseTimeFilter = a})

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start or the close time of the executions.
lcweReverseOrder :: Lens' ListClosedWorkflowExecutions (Maybe Bool)
lcweReverseOrder = lens _lcweReverseOrder (\ s a -> s{_lcweReverseOrder = a})

-- | If specified, only executions that have the matching tag are listed.
lcweTagFilter :: Lens' ListClosedWorkflowExecutions (Maybe TagFilter)
lcweTagFilter = lens _lcweTagFilter (\ s a -> s{_lcweTagFilter = a})

-- | If specified, the workflow executions are included in the returned results based on whether their start times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their start times.
lcweStartTimeFilter :: Lens' ListClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
lcweStartTimeFilter = lens _lcweStartTimeFilter (\ s a -> s{_lcweStartTimeFilter = a})

-- | The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
lcweMaximumPageSize :: Lens' ListClosedWorkflowExecutions (Maybe Natural)
lcweMaximumPageSize = lens _lcweMaximumPageSize (\ s a -> s{_lcweMaximumPageSize = a}) . mapping _Nat

-- | The name of the domain that contains the workflow executions to list.
lcweDomain :: Lens' ListClosedWorkflowExecutions Text
lcweDomain = lens _lcweDomain (\ s a -> s{_lcweDomain = a})

instance AWSPager ListClosedWorkflowExecutions where
        page rq rs
          | stop (rs ^. weiNextPageToken) = Nothing
          | stop (rs ^. weiExecutionInfos) = Nothing
          | otherwise =
            Just $ rq &
              lcweNextPageToken .~ rs ^. weiNextPageToken

instance AWSRequest ListClosedWorkflowExecutions
         where
        type Rs ListClosedWorkflowExecutions =
             WorkflowExecutionInfos
        request = postJSON swf
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable ListClosedWorkflowExecutions where

instance NFData ListClosedWorkflowExecutions where

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
              (catMaybes
                 [("nextPageToken" .=) <$> _lcweNextPageToken,
                  ("executionFilter" .=) <$> _lcweExecutionFilter,
                  ("closeStatusFilter" .=) <$> _lcweCloseStatusFilter,
                  ("typeFilter" .=) <$> _lcweTypeFilter,
                  ("closeTimeFilter" .=) <$> _lcweCloseTimeFilter,
                  ("reverseOrder" .=) <$> _lcweReverseOrder,
                  ("tagFilter" .=) <$> _lcweTagFilter,
                  ("startTimeFilter" .=) <$> _lcweStartTimeFilter,
                  ("maximumPageSize" .=) <$> _lcweMaximumPageSize,
                  Just ("domain" .= _lcweDomain)])

instance ToPath ListClosedWorkflowExecutions where
        toPath = const "/"

instance ToQuery ListClosedWorkflowExecutions where
        toQuery = const mempty
