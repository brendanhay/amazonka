{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheSubnetGroups/ action returns a list of cache subnet
-- group descriptions. If a subnet group name is specified, the list will
-- contain only the description of that group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheSubnetGroups.html>
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    (
    -- * Request
      DescribeCacheSubnetGroups
    -- ** Request constructor
    , describeCacheSubnetGroups
    -- ** Request lenses
    , dcsgCacheSubnetGroupName
    , dcsgMaxRecords
    , dcsgMarker

    -- * Response
    , DescribeCacheSubnetGroupsResponse
    -- ** Response constructor
    , describeCacheSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrsMarker
    , dcsgrsCacheSubnetGroups
    , dcsgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheSubnetGroups/ action.
--
-- /See:/ 'describeCacheSubnetGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgCacheSubnetGroupName'
--
-- * 'dcsgMaxRecords'
--
-- * 'dcsgMarker'
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups'
    { _dcsgCacheSubnetGroupName :: !(Maybe Text)
    , _dcsgMaxRecords           :: !(Maybe Int)
    , _dcsgMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheSubnetGroups' smart constructor.
describeCacheSubnetGroups :: DescribeCacheSubnetGroups
describeCacheSubnetGroups =
    DescribeCacheSubnetGroups'
    { _dcsgCacheSubnetGroupName = Nothing
    , _dcsgMaxRecords = Nothing
    , _dcsgMarker = Nothing
    }

-- | The name of the cache subnet group to return details for.
dcsgCacheSubnetGroupName :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgCacheSubnetGroupName = lens _dcsgCacheSubnetGroupName (\ s a -> s{_dcsgCacheSubnetGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcsgMaxRecords :: Lens' DescribeCacheSubnetGroups (Maybe Int)
dcsgMaxRecords = lens _dcsgMaxRecords (\ s a -> s{_dcsgMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcsgMarker :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgMarker = lens _dcsgMarker (\ s a -> s{_dcsgMarker = a});

instance AWSPager DescribeCacheSubnetGroups where
        page rq rs
          | stop (rs ^. dcsgrsMarker) = Nothing
          | stop (rs ^. dcsgrsCacheSubnetGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgMarker .~ rs ^. dcsgrsMarker

instance AWSRequest DescribeCacheSubnetGroups where
        type Sv DescribeCacheSubnetGroups = ElastiCache
        type Rs DescribeCacheSubnetGroups =
             DescribeCacheSubnetGroupsResponse
        request = post "DescribeCacheSubnetGroups"
        response
          = receiveXMLWrapper "DescribeCacheSubnetGroupsResult"
              (\ s h x ->
                 DescribeCacheSubnetGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "CacheSubnetGroups" .!@ mempty >>=
                        may (parseXMLList "CacheSubnetGroup"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCacheSubnetGroups where
        toHeaders = const mempty

instance ToPath DescribeCacheSubnetGroups where
        toPath = const "/"

instance ToQuery DescribeCacheSubnetGroups where
        toQuery DescribeCacheSubnetGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheSubnetGroups" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSubnetGroupName" =: _dcsgCacheSubnetGroupName,
               "MaxRecords" =: _dcsgMaxRecords,
               "Marker" =: _dcsgMarker]

-- | Represents the output of a /DescribeCacheSubnetGroups/ action.
--
-- /See:/ 'describeCacheSubnetGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrsMarker'
--
-- * 'dcsgrsCacheSubnetGroups'
--
-- * 'dcsgrsStatus'
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
    { _dcsgrsMarker            :: !(Maybe Text)
    , _dcsgrsCacheSubnetGroups :: !(Maybe [CacheSubnetGroup])
    , _dcsgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheSubnetGroupsResponse' smart constructor.
describeCacheSubnetGroupsResponse :: Int -> DescribeCacheSubnetGroupsResponse
describeCacheSubnetGroupsResponse pStatus_ =
    DescribeCacheSubnetGroupsResponse'
    { _dcsgrsMarker = Nothing
    , _dcsgrsCacheSubnetGroups = Nothing
    , _dcsgrsStatus = pStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
dcsgrsMarker :: Lens' DescribeCacheSubnetGroupsResponse (Maybe Text)
dcsgrsMarker = lens _dcsgrsMarker (\ s a -> s{_dcsgrsMarker = a});

-- | A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
dcsgrsCacheSubnetGroups :: Lens' DescribeCacheSubnetGroupsResponse [CacheSubnetGroup]
dcsgrsCacheSubnetGroups = lens _dcsgrsCacheSubnetGroups (\ s a -> s{_dcsgrsCacheSubnetGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dcsgrsStatus :: Lens' DescribeCacheSubnetGroupsResponse Int
dcsgrsStatus = lens _dcsgrsStatus (\ s a -> s{_dcsgrsStatus = a});
