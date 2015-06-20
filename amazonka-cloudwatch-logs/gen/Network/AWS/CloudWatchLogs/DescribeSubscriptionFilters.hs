{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
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

-- | Returns all the subscription filters associated with the specified log
-- group. The list returned in the response is ASCII-sorted by filter name.
--
-- By default, this operation returns up to 50 subscription filters. If
-- there are more subscription filters to list, the response would contain
-- a @nextToken@ value in the response body. You can also limit the number
-- of subscription filters returned in the response by specifying the
-- @limit@ parameter in the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html>
module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
    (
    -- * Request
      DescribeSubscriptionFilters
    -- ** Request constructor
    , describeSubscriptionFilters
    -- ** Request lenses
    , dsfFilterNamePrefix
    , dsfNextToken
    , dsfLimit
    , dsfLogGroupName

    -- * Response
    , DescribeSubscriptionFiltersResponse
    -- ** Response constructor
    , describeSubscriptionFiltersResponse
    -- ** Response lenses
    , dsfrSubscriptionFilters
    , dsfrNextToken
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSubscriptionFilters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfFilterNamePrefix'
--
-- * 'dsfNextToken'
--
-- * 'dsfLimit'
--
-- * 'dsfLogGroupName'
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'{_dsfFilterNamePrefix :: Maybe Text, _dsfNextToken :: Maybe Text, _dsfLimit :: Maybe Nat, _dsfLogGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeSubscriptionFilters' smart constructor.
describeSubscriptionFilters :: Text -> DescribeSubscriptionFilters
describeSubscriptionFilters pLogGroupName = DescribeSubscriptionFilters'{_dsfFilterNamePrefix = Nothing, _dsfNextToken = Nothing, _dsfLimit = Nothing, _dsfLogGroupName = pLogGroupName};

-- | Will only return subscription filters that match the provided
-- filterNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dsfFilterNamePrefix :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfFilterNamePrefix = lens _dsfFilterNamePrefix (\ s a -> s{_dsfFilterNamePrefix = a});

-- | FIXME: Undocumented member.
dsfNextToken :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfNextToken = lens _dsfNextToken (\ s a -> s{_dsfNextToken = a});

-- | FIXME: Undocumented member.
dsfLimit :: Lens' DescribeSubscriptionFilters (Maybe Natural)
dsfLimit = lens _dsfLimit (\ s a -> s{_dsfLimit = a}) . mapping _Nat;

-- | The log group name for which subscription filters are to be listed.
dsfLogGroupName :: Lens' DescribeSubscriptionFilters Text
dsfLogGroupName = lens _dsfLogGroupName (\ s a -> s{_dsfLogGroupName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeSubscriptionFilters where
        type Sv DescribeSubscriptionFilters = CloudWatchLogs
        type Rs DescribeSubscriptionFilters =
             DescribeSubscriptionFiltersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSubscriptionFiltersResponse' <$>
                   (x .?> "subscriptionFilters" .!@ mempty) <*>
                     (x .?> "nextToken"))

instance ToHeaders DescribeSubscriptionFilters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeSubscriptionFilters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSubscriptionFilters where
        toJSON DescribeSubscriptionFilters'{..}
          = object
              ["filterNamePrefix" .= _dsfFilterNamePrefix,
               "nextToken" .= _dsfNextToken, "limit" .= _dsfLimit,
               "logGroupName" .= _dsfLogGroupName]

instance ToPath DescribeSubscriptionFilters where
        toPath = const "/"

instance ToQuery DescribeSubscriptionFilters where
        toQuery = const mempty

-- | /See:/ 'describeSubscriptionFiltersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrSubscriptionFilters'
--
-- * 'dsfrNextToken'
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'{_dsfrSubscriptionFilters :: Maybe [SubscriptionFilter], _dsfrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeSubscriptionFiltersResponse' smart constructor.
describeSubscriptionFiltersResponse :: DescribeSubscriptionFiltersResponse
describeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'{_dsfrSubscriptionFilters = Nothing, _dsfrNextToken = Nothing};

-- | FIXME: Undocumented member.
dsfrSubscriptionFilters :: Lens' DescribeSubscriptionFiltersResponse [SubscriptionFilter]
dsfrSubscriptionFilters = lens _dsfrSubscriptionFilters (\ s a -> s{_dsfrSubscriptionFilters = a}) . _Default;

-- | FIXME: Undocumented member.
dsfrNextToken :: Lens' DescribeSubscriptionFiltersResponse (Maybe Text)
dsfrNextToken = lens _dsfrNextToken (\ s a -> s{_dsfrNextToken = a});
