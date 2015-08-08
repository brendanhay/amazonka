{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheSecurityGroups/ action returns a list of cache
-- security group descriptions. If a cache security group name is
-- specified, the list will contain only the description of that group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheSecurityGroups.html AWS API Reference> for DescribeCacheSecurityGroups.
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    (
    -- * Creating a Request
      DescribeCacheSecurityGroups
    , describeCacheSecurityGroups
    -- * Request Lenses
    , dcsgsCacheSecurityGroupName
    , dcsgsMaxRecords
    , dcsgsMarker

    -- * Destructuring the Response
    , DescribeCacheSecurityGroupsResponse
    , describeCacheSecurityGroupsResponse
    -- * Response Lenses
    , dcsgsrsCacheSecurityGroups
    , dcsgsrsMarker
    , dcsgsrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheSecurityGroups/ action.
--
-- /See:/ 'describeCacheSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgsCacheSecurityGroupName'
--
-- * 'dcsgsMaxRecords'
--
-- * 'dcsgsMarker'
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups'
    { _dcsgsCacheSecurityGroupName :: !(Maybe Text)
    , _dcsgsMaxRecords             :: !(Maybe Int)
    , _dcsgsMarker                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheSecurityGroups' smart constructor.
describeCacheSecurityGroups :: DescribeCacheSecurityGroups
describeCacheSecurityGroups =
    DescribeCacheSecurityGroups'
    { _dcsgsCacheSecurityGroupName = Nothing
    , _dcsgsMaxRecords = Nothing
    , _dcsgsMarker = Nothing
    }

-- | The name of the cache security group to return details for.
dcsgsCacheSecurityGroupName :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsgsCacheSecurityGroupName = lens _dcsgsCacheSecurityGroupName (\ s a -> s{_dcsgsCacheSecurityGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcsgsMaxRecords :: Lens' DescribeCacheSecurityGroups (Maybe Int)
dcsgsMaxRecords = lens _dcsgsMaxRecords (\ s a -> s{_dcsgsMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcsgsMarker :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsgsMarker = lens _dcsgsMarker (\ s a -> s{_dcsgsMarker = a});

instance AWSPager DescribeCacheSecurityGroups where
        page rq rs
          | stop (rs ^. dcsgsrsMarker) = Nothing
          | stop (rs ^. dcsgsrsCacheSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgsMarker .~ rs ^. dcsgsrsMarker

instance AWSRequest DescribeCacheSecurityGroups where
        type Sv DescribeCacheSecurityGroups = ElastiCache
        type Rs DescribeCacheSecurityGroups =
             DescribeCacheSecurityGroupsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeCacheSecurityGroupsResult"
              (\ s h x ->
                 DescribeCacheSecurityGroupsResponse' <$>
                   (x .@? "CacheSecurityGroups" .!@ mempty >>=
                      may (parseXMLList "CacheSecurityGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCacheSecurityGroups where
        toHeaders = const mempty

instance ToPath DescribeCacheSecurityGroups where
        toPath = const "/"

instance ToQuery DescribeCacheSecurityGroups where
        toQuery DescribeCacheSecurityGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheSecurityGroups" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _dcsgsCacheSecurityGroupName,
               "MaxRecords" =: _dcsgsMaxRecords,
               "Marker" =: _dcsgsMarker]

-- | Represents the output of a /DescribeCacheSecurityGroups/ action.
--
-- /See:/ 'describeCacheSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgsrsCacheSecurityGroups'
--
-- * 'dcsgsrsMarker'
--
-- * 'dcsgsrsStatus'
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse'
    { _dcsgsrsCacheSecurityGroups :: !(Maybe [CacheSecurityGroup])
    , _dcsgsrsMarker              :: !(Maybe Text)
    , _dcsgsrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheSecurityGroupsResponse' smart constructor.
describeCacheSecurityGroupsResponse :: Int -> DescribeCacheSecurityGroupsResponse
describeCacheSecurityGroupsResponse pStatus_ =
    DescribeCacheSecurityGroupsResponse'
    { _dcsgsrsCacheSecurityGroups = Nothing
    , _dcsgsrsMarker = Nothing
    , _dcsgsrsStatus = pStatus_
    }

-- | A list of cache security groups. Each element in the list contains
-- detailed information about one group.
dcsgsrsCacheSecurityGroups :: Lens' DescribeCacheSecurityGroupsResponse [CacheSecurityGroup]
dcsgsrsCacheSecurityGroups = lens _dcsgsrsCacheSecurityGroups (\ s a -> s{_dcsgsrsCacheSecurityGroups = a}) . _Default . _Coerce;

-- | Provides an identifier to allow retrieval of paginated results.
dcsgsrsMarker :: Lens' DescribeCacheSecurityGroupsResponse (Maybe Text)
dcsgsrsMarker = lens _dcsgsrsMarker (\ s a -> s{_dcsgsrsMarker = a});

-- | Undocumented member.
dcsgsrsStatus :: Lens' DescribeCacheSecurityGroupsResponse Int
dcsgsrsStatus = lens _dcsgsrsStatus (\ s a -> s{_dcsgsrsStatus = a});
