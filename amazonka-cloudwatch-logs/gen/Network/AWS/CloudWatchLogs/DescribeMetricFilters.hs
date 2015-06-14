{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns all the metrics filters associated with the specified log group.
-- The list returned in the response is ASCII-sorted by filter name.
--
-- By default, this operation returns up to 50 metric filters. If there are
-- more metric filters to list, the response would contain a @nextToken@
-- value in the response body. You can also limit the number of metric
-- filters returned in the response by specifying the @limit@ parameter in
-- the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeMetricFilters.html>
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
    (
    -- * Request
      DescribeMetricFilters
    -- ** Request constructor
    , describeMetricFilters
    -- ** Request lenses
    , dmfLogGroupName
    , dmfFilterNamePrefix
    , dmfNextToken
    , dmfLimit

    -- * Response
    , DescribeMetricFiltersResponse
    -- ** Response constructor
    , describeMetricFiltersResponse
    -- ** Response lenses
    , dmfrMetricFilters
    , dmfrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatchLogs.Types

-- | /See:/ 'describeMetricFilters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfLogGroupName'
--
-- * 'dmfFilterNamePrefix'
--
-- * 'dmfNextToken'
--
-- * 'dmfLimit'
data DescribeMetricFilters = DescribeMetricFilters'{_dmfLogGroupName :: Text, _dmfFilterNamePrefix :: Text, _dmfNextToken :: Text, _dmfLimit :: Nat} deriving (Eq, Read, Show)

-- | 'DescribeMetricFilters' smart constructor.
describeMetricFilters :: Text -> Text -> Text -> Natural -> DescribeMetricFilters
describeMetricFilters pLogGroupName pFilterNamePrefix pNextToken pLimit = DescribeMetricFilters'{_dmfLogGroupName = pLogGroupName, _dmfFilterNamePrefix = pFilterNamePrefix, _dmfNextToken = pNextToken, _dmfLimit = _Nat # pLimit};

-- | The log group name for which metric filters are to be listed.
dmfLogGroupName :: Lens' DescribeMetricFilters Text
dmfLogGroupName = lens _dmfLogGroupName (\ s a -> s{_dmfLogGroupName = a});

-- | Will only return metric filters that match the provided
-- filterNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dmfFilterNamePrefix :: Lens' DescribeMetricFilters Text
dmfFilterNamePrefix = lens _dmfFilterNamePrefix (\ s a -> s{_dmfFilterNamePrefix = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- @DescribeMetricFilters@ request.
dmfNextToken :: Lens' DescribeMetricFilters Text
dmfNextToken = lens _dmfNextToken (\ s a -> s{_dmfNextToken = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
dmfLimit :: Lens' DescribeMetricFilters Natural
dmfLimit = lens _dmfLimit (\ s a -> s{_dmfLimit = a}) . _Nat;

instance AWSRequest DescribeMetricFilters where
        type Sv DescribeMetricFilters = CloudWatchLogs
        type Rs DescribeMetricFilters =
             DescribeMetricFiltersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMetricFiltersResponse' <$>
                   x .?> "metricFilters" .!@ mempty <*>
                     x .:> "nextToken")

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
              ["logGroupName" .= _dmfLogGroupName,
               "filterNamePrefix" .= _dmfFilterNamePrefix,
               "nextToken" .= _dmfNextToken, "limit" .= _dmfLimit]

instance ToPath DescribeMetricFilters where
        toPath = const "/"

instance ToQuery DescribeMetricFilters where
        toQuery = const mempty

-- | /See:/ 'describeMetricFiltersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfrMetricFilters'
--
-- * 'dmfrNextToken'
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'{_dmfrMetricFilters :: [MetricFilter], _dmfrNextToken :: Text} deriving (Eq, Read, Show)

-- | 'DescribeMetricFiltersResponse' smart constructor.
describeMetricFiltersResponse :: Text -> DescribeMetricFiltersResponse
describeMetricFiltersResponse pNextToken = DescribeMetricFiltersResponse'{_dmfrMetricFilters = mempty, _dmfrNextToken = pNextToken};

-- | FIXME: Undocumented member.
dmfrMetricFilters :: Lens' DescribeMetricFiltersResponse [MetricFilter]
dmfrMetricFilters = lens _dmfrMetricFilters (\ s a -> s{_dmfrMetricFilters = a});

-- | FIXME: Undocumented member.
dmfrNextToken :: Lens' DescribeMetricFiltersResponse Text
dmfrNextToken = lens _dmfrNextToken (\ s a -> s{_dmfrNextToken = a});
