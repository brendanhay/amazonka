{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheParameterGroups/ action returns a list of cache
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
    , dcpgrqCacheParameterGroupName
    , dcpgrqMaxRecords
    , dcpgrqMarker

    -- * Response
    , DescribeCacheParameterGroupsResponse
    -- ** Response constructor
    , describeCacheParameterGroupsResponse
    -- ** Response lenses
    , dcpgrsCacheParameterGroups
    , dcpgrsMarker
    , dcpgrsStatus
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
-- * 'dcpgrqCacheParameterGroupName'
--
-- * 'dcpgrqMaxRecords'
--
-- * 'dcpgrqMarker'
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
    { _dcpgrqCacheParameterGroupName :: !(Maybe Text)
    , _dcpgrqMaxRecords              :: !(Maybe Int)
    , _dcpgrqMarker                  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheParameterGroups' smart constructor.
describeCacheParameterGroups :: DescribeCacheParameterGroups
describeCacheParameterGroups =
    DescribeCacheParameterGroups'
    { _dcpgrqCacheParameterGroupName = Nothing
    , _dcpgrqMaxRecords = Nothing
    , _dcpgrqMarker = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpgrqCacheParameterGroupName :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgrqCacheParameterGroupName = lens _dcpgrqCacheParameterGroupName (\ s a -> s{_dcpgrqCacheParameterGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcpgrqMaxRecords :: Lens' DescribeCacheParameterGroups (Maybe Int)
dcpgrqMaxRecords = lens _dcpgrqMaxRecords (\ s a -> s{_dcpgrqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcpgrqMarker :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgrqMarker = lens _dcpgrqMarker (\ s a -> s{_dcpgrqMarker = a});

instance AWSPager DescribeCacheParameterGroups where
        page rq rs
          | stop (rs ^. dcpgrsMarker) = Nothing
          | stop (rs ^. dcpgrsCacheParameterGroups) = Nothing
          | otherwise =
            Just $ rq & dcpgrqMarker .~ rs ^. dcpgrsMarker

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
                     <*> (pure (fromEnum s)))

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
                 _dcpgrqCacheParameterGroupName,
               "MaxRecords" =: _dcpgrqMaxRecords,
               "Marker" =: _dcpgrqMarker]

-- | Represents the output of a /DescribeCacheParameterGroups/ action.
--
-- /See:/ 'describeCacheParameterGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgrsCacheParameterGroups'
--
-- * 'dcpgrsMarker'
--
-- * 'dcpgrsStatus'
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
    { _dcpgrsCacheParameterGroups :: !(Maybe [CacheParameterGroup])
    , _dcpgrsMarker               :: !(Maybe Text)
    , _dcpgrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheParameterGroupsResponse' smart constructor.
describeCacheParameterGroupsResponse :: Int -> DescribeCacheParameterGroupsResponse
describeCacheParameterGroupsResponse pStatus_ =
    DescribeCacheParameterGroupsResponse'
    { _dcpgrsCacheParameterGroups = Nothing
    , _dcpgrsMarker = Nothing
    , _dcpgrsStatus = pStatus_
    }

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
dcpgrsCacheParameterGroups :: Lens' DescribeCacheParameterGroupsResponse [CacheParameterGroup]
dcpgrsCacheParameterGroups = lens _dcpgrsCacheParameterGroups (\ s a -> s{_dcpgrsCacheParameterGroups = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dcpgrsMarker :: Lens' DescribeCacheParameterGroupsResponse (Maybe Text)
dcpgrsMarker = lens _dcpgrsMarker (\ s a -> s{_dcpgrsMarker = a});

-- | FIXME: Undocumented member.
dcpgrsStatus :: Lens' DescribeCacheParameterGroupsResponse Int
dcpgrsStatus = lens _dcpgrsStatus (\ s a -> s{_dcpgrsStatus = a});
