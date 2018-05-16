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
-- Module      : Network.AWS.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of open workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.
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
module Network.AWS.SWF.ListOpenWorkflowExecutions
    (
    -- * Creating a Request
      listOpenWorkflowExecutions
    , ListOpenWorkflowExecutions
    -- * Request Lenses
    , loweNextPageToken
    , loweExecutionFilter
    , loweTypeFilter
    , loweReverseOrder
    , loweTagFilter
    , loweMaximumPageSize
    , loweDomain
    , loweStartTimeFilter

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

-- | /See:/ 'listOpenWorkflowExecutions' smart constructor.
data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions'
  { _loweNextPageToken   :: !(Maybe Text)
  , _loweExecutionFilter :: !(Maybe WorkflowExecutionFilter)
  , _loweTypeFilter      :: !(Maybe WorkflowTypeFilter)
  , _loweReverseOrder    :: !(Maybe Bool)
  , _loweTagFilter       :: !(Maybe TagFilter)
  , _loweMaximumPageSize :: !(Maybe Nat)
  , _loweDomain          :: !Text
  , _loweStartTimeFilter :: !ExecutionTimeFilter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOpenWorkflowExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loweNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'loweExecutionFilter' - If specified, only workflow executions matching the workflow ID specified in the filter are returned.
--
-- * 'loweTypeFilter' - If specified, only executions of the type specified in the filter are returned.
--
-- * 'loweReverseOrder' - When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
--
-- * 'loweTagFilter' - If specified, only executions that have the matching tag are listed.
--
-- * 'loweMaximumPageSize' - The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- * 'loweDomain' - The name of the domain that contains the workflow executions to list.
--
-- * 'loweStartTimeFilter' - Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
listOpenWorkflowExecutions
    :: Text -- ^ 'loweDomain'
    -> ExecutionTimeFilter -- ^ 'loweStartTimeFilter'
    -> ListOpenWorkflowExecutions
listOpenWorkflowExecutions pDomain_ pStartTimeFilter_ =
  ListOpenWorkflowExecutions'
    { _loweNextPageToken = Nothing
    , _loweExecutionFilter = Nothing
    , _loweTypeFilter = Nothing
    , _loweReverseOrder = Nothing
    , _loweTagFilter = Nothing
    , _loweMaximumPageSize = Nothing
    , _loweDomain = pDomain_
    , _loweStartTimeFilter = pStartTimeFilter_
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
loweNextPageToken :: Lens' ListOpenWorkflowExecutions (Maybe Text)
loweNextPageToken = lens _loweNextPageToken (\ s a -> s{_loweNextPageToken = a})

-- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
loweExecutionFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowExecutionFilter)
loweExecutionFilter = lens _loweExecutionFilter (\ s a -> s{_loweExecutionFilter = a})

-- | If specified, only executions of the type specified in the filter are returned.
loweTypeFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowTypeFilter)
loweTypeFilter = lens _loweTypeFilter (\ s a -> s{_loweTypeFilter = a})

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
loweReverseOrder :: Lens' ListOpenWorkflowExecutions (Maybe Bool)
loweReverseOrder = lens _loweReverseOrder (\ s a -> s{_loweReverseOrder = a})

-- | If specified, only executions that have the matching tag are listed.
loweTagFilter :: Lens' ListOpenWorkflowExecutions (Maybe TagFilter)
loweTagFilter = lens _loweTagFilter (\ s a -> s{_loweTagFilter = a})

-- | The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
loweMaximumPageSize :: Lens' ListOpenWorkflowExecutions (Maybe Natural)
loweMaximumPageSize = lens _loweMaximumPageSize (\ s a -> s{_loweMaximumPageSize = a}) . mapping _Nat

-- | The name of the domain that contains the workflow executions to list.
loweDomain :: Lens' ListOpenWorkflowExecutions Text
loweDomain = lens _loweDomain (\ s a -> s{_loweDomain = a})

-- | Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
loweStartTimeFilter :: Lens' ListOpenWorkflowExecutions ExecutionTimeFilter
loweStartTimeFilter = lens _loweStartTimeFilter (\ s a -> s{_loweStartTimeFilter = a})

instance AWSPager ListOpenWorkflowExecutions where
        page rq rs
          | stop (rs ^. weiNextPageToken) = Nothing
          | stop (rs ^. weiExecutionInfos) = Nothing
          | otherwise =
            Just $ rq &
              loweNextPageToken .~ rs ^. weiNextPageToken

instance AWSRequest ListOpenWorkflowExecutions where
        type Rs ListOpenWorkflowExecutions =
             WorkflowExecutionInfos
        request = postJSON swf
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable ListOpenWorkflowExecutions where

instance NFData ListOpenWorkflowExecutions where

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
              (catMaybes
                 [("nextPageToken" .=) <$> _loweNextPageToken,
                  ("executionFilter" .=) <$> _loweExecutionFilter,
                  ("typeFilter" .=) <$> _loweTypeFilter,
                  ("reverseOrder" .=) <$> _loweReverseOrder,
                  ("tagFilter" .=) <$> _loweTagFilter,
                  ("maximumPageSize" .=) <$> _loweMaximumPageSize,
                  Just ("domain" .= _loweDomain),
                  Just ("startTimeFilter" .= _loweStartTimeFilter)])

instance ToPath ListOpenWorkflowExecutions where
        toPath = const "/"

instance ToQuery ListOpenWorkflowExecutions where
        toQuery = const mempty
