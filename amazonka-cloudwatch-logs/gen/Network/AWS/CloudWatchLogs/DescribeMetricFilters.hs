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
-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the metrics filters associated with the specified log group.
-- The list returned in the response is ASCII-sorted by filter name.
--
-- By default, this operation returns up to 50 metric filters. If there are
-- more metric filters to list, the response would contain a 'nextToken'
-- value in the response body. You can also limit the number of metric
-- filters returned in the response by specifying the 'limit' parameter in
-- the request.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
    (
    -- * Creating a Request
      describeMetricFilters
    , DescribeMetricFilters
    -- * Request Lenses
    , dmfFilterNamePrefix
    , dmfNextToken
    , dmfLimit
    , dmfLogGroupName

    -- * Destructuring the Response
    , describeMetricFiltersResponse
    , DescribeMetricFiltersResponse
    -- * Response Lenses
    , dmfrsNextToken
    , dmfrsMetricFilters
    , dmfrsResponseStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMetricFilters' smart constructor.
data DescribeMetricFilters = DescribeMetricFilters'
    { _dmfFilterNamePrefix :: !(Maybe Text)
    , _dmfNextToken        :: !(Maybe Text)
    , _dmfLimit            :: !(Maybe Nat)
    , _dmfLogGroupName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMetricFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmfFilterNamePrefix'
--
-- * 'dmfNextToken'
--
-- * 'dmfLimit'
--
-- * 'dmfLogGroupName'
describeMetricFilters
    :: Text -- ^ 'dmfLogGroupName'
    -> DescribeMetricFilters
describeMetricFilters pLogGroupName_ =
    DescribeMetricFilters'
    { _dmfFilterNamePrefix = Nothing
    , _dmfNextToken = Nothing
    , _dmfLimit = Nothing
    , _dmfLogGroupName = pLogGroupName_
    }

-- | Will only return metric filters that match the provided
-- filterNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dmfFilterNamePrefix :: Lens' DescribeMetricFilters (Maybe Text)
dmfFilterNamePrefix = lens _dmfFilterNamePrefix (\ s a -> s{_dmfFilterNamePrefix = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- 'DescribeMetricFilters' request.
dmfNextToken :: Lens' DescribeMetricFilters (Maybe Text)
dmfNextToken = lens _dmfNextToken (\ s a -> s{_dmfNextToken = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
dmfLimit :: Lens' DescribeMetricFilters (Maybe Natural)
dmfLimit = lens _dmfLimit (\ s a -> s{_dmfLimit = a}) . mapping _Nat;

-- | The log group name for which metric filters are to be listed.
dmfLogGroupName :: Lens' DescribeMetricFilters Text
dmfLogGroupName = lens _dmfLogGroupName (\ s a -> s{_dmfLogGroupName = a});

instance AWSPager DescribeMetricFilters where
        page rq rs
          | stop (rs ^. dmfrsNextToken) = Nothing
          | stop (rs ^. dmfrsMetricFilters) = Nothing
          | otherwise =
            Just $ rq & dmfNextToken .~ rs ^. dmfrsNextToken

instance AWSRequest DescribeMetricFilters where
        type Rs DescribeMetricFilters =
             DescribeMetricFiltersResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMetricFiltersResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "metricFilters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMetricFilters

instance NFData DescribeMetricFilters

instance ToHeaders DescribeMetricFilters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeMetricFilters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMetricFilters where
        toJSON DescribeMetricFilters'{..}
          = object
              (catMaybes
                 [("filterNamePrefix" .=) <$> _dmfFilterNamePrefix,
                  ("nextToken" .=) <$> _dmfNextToken,
                  ("limit" .=) <$> _dmfLimit,
                  Just ("logGroupName" .= _dmfLogGroupName)])

instance ToPath DescribeMetricFilters where
        toPath = const "/"

instance ToQuery DescribeMetricFilters where
        toQuery = const mempty

-- | /See:/ 'describeMetricFiltersResponse' smart constructor.
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
    { _dmfrsNextToken      :: !(Maybe Text)
    , _dmfrsMetricFilters  :: !(Maybe [MetricFilter])
    , _dmfrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMetricFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmfrsNextToken'
--
-- * 'dmfrsMetricFilters'
--
-- * 'dmfrsResponseStatus'
describeMetricFiltersResponse
    :: Int -- ^ 'dmfrsResponseStatus'
    -> DescribeMetricFiltersResponse
describeMetricFiltersResponse pResponseStatus_ =
    DescribeMetricFiltersResponse'
    { _dmfrsNextToken = Nothing
    , _dmfrsMetricFilters = Nothing
    , _dmfrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dmfrsNextToken :: Lens' DescribeMetricFiltersResponse (Maybe Text)
dmfrsNextToken = lens _dmfrsNextToken (\ s a -> s{_dmfrsNextToken = a});

-- | Undocumented member.
dmfrsMetricFilters :: Lens' DescribeMetricFiltersResponse [MetricFilter]
dmfrsMetricFilters = lens _dmfrsMetricFilters (\ s a -> s{_dmfrsMetricFilters = a}) . _Default . _Coerce;

-- | The response status code.
dmfrsResponseStatus :: Lens' DescribeMetricFiltersResponse Int
dmfrsResponseStatus = lens _dmfrsResponseStatus (\ s a -> s{_dmfrsResponseStatus = a});

instance NFData DescribeMetricFiltersResponse
