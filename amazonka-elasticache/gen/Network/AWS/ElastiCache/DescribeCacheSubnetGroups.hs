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
-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheSubnetGroups/ action returns a list of cache subnet
-- group descriptions. If a subnet group name is specified, the list will
-- contain only the description of that group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheSubnetGroups.html AWS API Reference> for DescribeCacheSubnetGroups.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    (
    -- * Creating a Request
      describeCacheSubnetGroups
    , DescribeCacheSubnetGroups
    -- * Request Lenses
    , dcsgCacheSubnetGroupName
    , dcsgMarker
    , dcsgMaxRecords

    -- * Destructuring the Response
    , describeCacheSubnetGroupsResponse
    , DescribeCacheSubnetGroupsResponse
    -- * Response Lenses
    , dcsgrsMarker
    , dcsgrsCacheSubnetGroups
    , dcsgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheSubnetGroups/ action.
--
-- /See:/ 'describeCacheSubnetGroups' smart constructor.
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups'
    { _dcsgCacheSubnetGroupName :: !(Maybe Text)
    , _dcsgMarker               :: !(Maybe Text)
    , _dcsgMaxRecords           :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCacheSubnetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgCacheSubnetGroupName'
--
-- * 'dcsgMarker'
--
-- * 'dcsgMaxRecords'
describeCacheSubnetGroups
    :: DescribeCacheSubnetGroups
describeCacheSubnetGroups =
    DescribeCacheSubnetGroups'
    { _dcsgCacheSubnetGroupName = Nothing
    , _dcsgMarker = Nothing
    , _dcsgMaxRecords = Nothing
    }

-- | The name of the cache subnet group to return details for.
dcsgCacheSubnetGroupName :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgCacheSubnetGroupName = lens _dcsgCacheSubnetGroupName (\ s a -> s{_dcsgCacheSubnetGroupName = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcsgMarker :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgMarker = lens _dcsgMarker (\ s a -> s{_dcsgMarker = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified 'MaxRecords' value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcsgMaxRecords :: Lens' DescribeCacheSubnetGroups (Maybe Int)
dcsgMaxRecords = lens _dcsgMaxRecords (\ s a -> s{_dcsgMaxRecords = a});

instance AWSPager DescribeCacheSubnetGroups where
        page rq rs
          | stop (rs ^. dcsgrsMarker) = Nothing
          | stop (rs ^. dcsgrsCacheSubnetGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgMarker .~ rs ^. dcsgrsMarker

instance AWSRequest DescribeCacheSubnetGroups where
        type Rs DescribeCacheSubnetGroups =
             DescribeCacheSubnetGroupsResponse
        request = postQuery elastiCache
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
               "Marker" =: _dcsgMarker,
               "MaxRecords" =: _dcsgMaxRecords]

-- | Represents the output of a /DescribeCacheSubnetGroups/ action.
--
-- /See:/ 'describeCacheSubnetGroupsResponse' smart constructor.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
    { _dcsgrsMarker            :: !(Maybe Text)
    , _dcsgrsCacheSubnetGroups :: !(Maybe [CacheSubnetGroup])
    , _dcsgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCacheSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgrsMarker'
--
-- * 'dcsgrsCacheSubnetGroups'
--
-- * 'dcsgrsStatus'
describeCacheSubnetGroupsResponse
    :: Int -- ^ 'dcsgrsStatus'
    -> DescribeCacheSubnetGroupsResponse
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
dcsgrsCacheSubnetGroups = lens _dcsgrsCacheSubnetGroups (\ s a -> s{_dcsgrsCacheSubnetGroups = a}) . _Default . _Coerce;

-- | The response status code.
dcsgrsStatus :: Lens' DescribeCacheSubnetGroupsResponse Int
dcsgrsStatus = lens _dcsgrsStatus (\ s a -> s{_dcsgrsStatus = a});
