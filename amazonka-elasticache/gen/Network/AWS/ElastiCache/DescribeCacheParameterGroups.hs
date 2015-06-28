{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
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

-- | The /DescribeCacheParameterGroups/ action returns a list of cache
-- parameter group descriptions. If a cache parameter group name is
-- specified, the list will contain only the descriptions for that group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheParameterGroups.html>
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    (
    -- * Request
      DescribeCacheParameterGroups
    -- ** Request constructor
    , describeCacheParameterGroups
    -- ** Request lenses
    , dcpgCacheParameterGroupName
    , dcpgMaxRecords
    , dcpgMarker

    -- * Response
    , DescribeCacheParameterGroupsResponse
    -- ** Response constructor
    , describeCacheParameterGroupsResponse
    -- ** Response lenses
    , dcpgrCacheParameterGroups
    , dcpgrMarker
    , dcpgrStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheParameterGroups/ action.
--
-- /See:/ 'describeCacheParameterGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgCacheParameterGroupName'
--
-- * 'dcpgMaxRecords'
--
-- * 'dcpgMarker'
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
    { _dcpgCacheParameterGroupName :: !(Maybe Text)
    , _dcpgMaxRecords              :: !(Maybe Int)
    , _dcpgMarker                  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeCacheParameterGroups' smart constructor.
describeCacheParameterGroups :: DescribeCacheParameterGroups
describeCacheParameterGroups =
    DescribeCacheParameterGroups'
    { _dcpgCacheParameterGroupName = Nothing
    , _dcpgMaxRecords = Nothing
    , _dcpgMarker = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpgCacheParameterGroupName :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgCacheParameterGroupName = lens _dcpgCacheParameterGroupName (\ s a -> s{_dcpgCacheParameterGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcpgMaxRecords :: Lens' DescribeCacheParameterGroups (Maybe Int)
dcpgMaxRecords = lens _dcpgMaxRecords (\ s a -> s{_dcpgMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcpgMarker :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgMarker = lens _dcpgMarker (\ s a -> s{_dcpgMarker = a});

instance AWSPager DescribeCacheParameterGroups where
        page rq rs
          | stop (rs ^. dcpgrMarker) = Nothing
          | stop (rs ^. dcpgrCacheParameterGroups) = Nothing
          | otherwise =
            Just $ rq & dcpgMarker .~ rs ^. dcpgrMarker

instance AWSRequest DescribeCacheParameterGroups
         where
        type Sv DescribeCacheParameterGroups = ElastiCache
        type Rs DescribeCacheParameterGroups =
             DescribeCacheParameterGroupsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeCacheParameterGroupsResult"
              (\ s h x ->
                 DescribeCacheParameterGroupsResponse' <$>
                   (x .@? "CacheParameterGroups" .!@ mempty >>=
                      may (parseXMLList "CacheParameterGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure s))

instance ToHeaders DescribeCacheParameterGroups where
        toHeaders = const mempty

instance ToPath DescribeCacheParameterGroups where
        toPath = const "/"

instance ToQuery DescribeCacheParameterGroups where
        toQuery DescribeCacheParameterGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheParameterGroups" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _dcpgCacheParameterGroupName,
               "MaxRecords" =: _dcpgMaxRecords,
               "Marker" =: _dcpgMarker]

-- | Represents the output of a /DescribeCacheParameterGroups/ action.
--
-- /See:/ 'describeCacheParameterGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgrCacheParameterGroups'
--
-- * 'dcpgrMarker'
--
-- * 'dcpgrStatus'
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
    { _dcpgrCacheParameterGroups :: !(Maybe [CacheParameterGroup])
    , _dcpgrMarker               :: !(Maybe Text)
    , _dcpgrStatus               :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeCacheParameterGroupsResponse' smart constructor.
describeCacheParameterGroupsResponse :: Status -> DescribeCacheParameterGroupsResponse
describeCacheParameterGroupsResponse pStatus =
    DescribeCacheParameterGroupsResponse'
    { _dcpgrCacheParameterGroups = Nothing
    , _dcpgrMarker = Nothing
    , _dcpgrStatus = pStatus
    }

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
dcpgrCacheParameterGroups :: Lens' DescribeCacheParameterGroupsResponse [CacheParameterGroup]
dcpgrCacheParameterGroups = lens _dcpgrCacheParameterGroups (\ s a -> s{_dcpgrCacheParameterGroups = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dcpgrMarker :: Lens' DescribeCacheParameterGroupsResponse (Maybe Text)
dcpgrMarker = lens _dcpgrMarker (\ s a -> s{_dcpgrMarker = a});

-- | FIXME: Undocumented member.
dcpgrStatus :: Lens' DescribeCacheParameterGroupsResponse Status
dcpgrStatus = lens _dcpgrStatus (\ s a -> s{_dcpgrStatus = a});
