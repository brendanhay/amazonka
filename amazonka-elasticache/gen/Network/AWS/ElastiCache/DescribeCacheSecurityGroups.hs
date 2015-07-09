{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | The /DescribeCacheSecurityGroups/ action returns a list of cache
-- security group descriptions. If a cache security group name is
-- specified, the list will contain only the description of that group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheSecurityGroups.html>
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    (
    -- * Request
      DescribeCacheSecurityGroups
    -- ** Request constructor
    , describeCacheSecurityGroups
    -- ** Request lenses
    , dcsg1CacheSecurityGroupName
    , dcsg1MaxRecords
    , dcsg1Marker

    -- * Response
    , DescribeCacheSecurityGroupsResponse
    -- ** Response constructor
    , describeCacheSecurityGroupsResponse
    -- ** Response lenses
    , dcsgr1CacheSecurityGroups
    , dcsgr1Marker
    , dcsgr1Status
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
-- * 'dcsg1CacheSecurityGroupName'
--
-- * 'dcsg1MaxRecords'
--
-- * 'dcsg1Marker'
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups'
    { _dcsg1CacheSecurityGroupName :: !(Maybe Text)
    , _dcsg1MaxRecords             :: !(Maybe Int)
    , _dcsg1Marker                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheSecurityGroups' smart constructor.
describeCacheSecurityGroups :: DescribeCacheSecurityGroups
describeCacheSecurityGroups =
    DescribeCacheSecurityGroups'
    { _dcsg1CacheSecurityGroupName = Nothing
    , _dcsg1MaxRecords = Nothing
    , _dcsg1Marker = Nothing
    }

-- | The name of the cache security group to return details for.
dcsg1CacheSecurityGroupName :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsg1CacheSecurityGroupName = lens _dcsg1CacheSecurityGroupName (\ s a -> s{_dcsg1CacheSecurityGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcsg1MaxRecords :: Lens' DescribeCacheSecurityGroups (Maybe Int)
dcsg1MaxRecords = lens _dcsg1MaxRecords (\ s a -> s{_dcsg1MaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcsg1Marker :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsg1Marker = lens _dcsg1Marker (\ s a -> s{_dcsg1Marker = a});

instance AWSPager DescribeCacheSecurityGroups where
        page rq rs
          | stop (rs ^. dcsgr1Marker) = Nothing
          | stop (rs ^. dcsgr1CacheSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & dcsg1Marker .~ rs ^. dcsgr1Marker

instance AWSRequest DescribeCacheSecurityGroups where
        type Sv DescribeCacheSecurityGroups = ElastiCache
        type Rs DescribeCacheSecurityGroups =
             DescribeCacheSecurityGroupsResponse
        request = post
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
                 _dcsg1CacheSecurityGroupName,
               "MaxRecords" =: _dcsg1MaxRecords,
               "Marker" =: _dcsg1Marker]

-- | Represents the output of a /DescribeCacheSecurityGroups/ action.
--
-- /See:/ 'describeCacheSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgr1CacheSecurityGroups'
--
-- * 'dcsgr1Marker'
--
-- * 'dcsgr1Status'
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse'
    { _dcsgr1CacheSecurityGroups :: !(Maybe [CacheSecurityGroup])
    , _dcsgr1Marker              :: !(Maybe Text)
    , _dcsgr1Status              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheSecurityGroupsResponse' smart constructor.
describeCacheSecurityGroupsResponse :: Int -> DescribeCacheSecurityGroupsResponse
describeCacheSecurityGroupsResponse pStatus =
    DescribeCacheSecurityGroupsResponse'
    { _dcsgr1CacheSecurityGroups = Nothing
    , _dcsgr1Marker = Nothing
    , _dcsgr1Status = pStatus
    }

-- | A list of cache security groups. Each element in the list contains
-- detailed information about one group.
dcsgr1CacheSecurityGroups :: Lens' DescribeCacheSecurityGroupsResponse [CacheSecurityGroup]
dcsgr1CacheSecurityGroups = lens _dcsgr1CacheSecurityGroups (\ s a -> s{_dcsgr1CacheSecurityGroups = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dcsgr1Marker :: Lens' DescribeCacheSecurityGroupsResponse (Maybe Text)
dcsgr1Marker = lens _dcsgr1Marker (\ s a -> s{_dcsgr1Marker = a});

-- | FIXME: Undocumented member.
dcsgr1Status :: Lens' DescribeCacheSecurityGroupsResponse Int
dcsgr1Status = lens _dcsgr1Status (\ s a -> s{_dcsgr1Status = a});
