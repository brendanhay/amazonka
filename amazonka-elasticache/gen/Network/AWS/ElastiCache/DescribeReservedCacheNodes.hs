{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeReservedCacheNodes/ action returns information about
-- reserved cache nodes for this account, or about a specified reserved
-- cache node.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeReservedCacheNodes.html>
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
    (
    -- * Request
      DescribeReservedCacheNodes
    -- ** Request constructor
    , describeReservedCacheNodes
    -- ** Request lenses
    , drcnrqCacheNodeType
    , drcnrqProductDescription
    , drcnrqMaxRecords
    , drcnrqMarker
    , drcnrqReservedCacheNodeId
    , drcnrqOfferingType
    , drcnrqDuration
    , drcnrqReservedCacheNodesOfferingId

    -- * Response
    , DescribeReservedCacheNodesResponse
    -- ** Response constructor
    , describeReservedCacheNodesResponse
    -- ** Response lenses
    , drcnrsMarker
    , drcnrsReservedCacheNodes
    , drcnrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeReservedCacheNodes/ action.
--
-- /See:/ 'describeReservedCacheNodes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnrqCacheNodeType'
--
-- * 'drcnrqProductDescription'
--
-- * 'drcnrqMaxRecords'
--
-- * 'drcnrqMarker'
--
-- * 'drcnrqReservedCacheNodeId'
--
-- * 'drcnrqOfferingType'
--
-- * 'drcnrqDuration'
--
-- * 'drcnrqReservedCacheNodesOfferingId'
data DescribeReservedCacheNodes = DescribeReservedCacheNodes'
    { _drcnrqCacheNodeType                :: !(Maybe Text)
    , _drcnrqProductDescription           :: !(Maybe Text)
    , _drcnrqMaxRecords                   :: !(Maybe Int)
    , _drcnrqMarker                       :: !(Maybe Text)
    , _drcnrqReservedCacheNodeId          :: !(Maybe Text)
    , _drcnrqOfferingType                 :: !(Maybe Text)
    , _drcnrqDuration                     :: !(Maybe Text)
    , _drcnrqReservedCacheNodesOfferingId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedCacheNodes' smart constructor.
describeReservedCacheNodes :: DescribeReservedCacheNodes
describeReservedCacheNodes =
    DescribeReservedCacheNodes'
    { _drcnrqCacheNodeType = Nothing
    , _drcnrqProductDescription = Nothing
    , _drcnrqMaxRecords = Nothing
    , _drcnrqMarker = Nothing
    , _drcnrqReservedCacheNodeId = Nothing
    , _drcnrqOfferingType = Nothing
    , _drcnrqDuration = Nothing
    , _drcnrqReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@, @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--     -   Previous generation: @cache.t1.micro@, @cache.m1.small@,
--         @cache.m1.medium@, @cache.m1.large@, @cache.m1.xlarge@
-- -   Compute optimized: @cache.c1.xlarge@
-- -   Memory optimized
--     -   Current generation: @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--     -   Previous generation: @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
drcnrqCacheNodeType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqCacheNodeType = lens _drcnrqCacheNodeType (\ s a -> s{_drcnrqCacheNodeType = a});

-- | The product description filter value. Use this parameter to show only
-- those reservations matching the specified product description.
drcnrqProductDescription :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqProductDescription = lens _drcnrqProductDescription (\ s a -> s{_drcnrqProductDescription = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
drcnrqMaxRecords :: Lens' DescribeReservedCacheNodes (Maybe Int)
drcnrqMaxRecords = lens _drcnrqMaxRecords (\ s a -> s{_drcnrqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
drcnrqMarker :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqMarker = lens _drcnrqMarker (\ s a -> s{_drcnrqMarker = a});

-- | The reserved cache node identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reservation ID.
drcnrqReservedCacheNodeId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqReservedCacheNodeId = lens _drcnrqReservedCacheNodeId (\ s a -> s{_drcnrqReservedCacheNodeId = a});

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid values:
-- @\"Light Utilization\"|\"Medium Utilization\"|\"Heavy Utilization\"@
drcnrqOfferingType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqOfferingType = lens _drcnrqOfferingType (\ s a -> s{_drcnrqOfferingType = a});

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
drcnrqDuration :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqDuration = lens _drcnrqDuration (\ s a -> s{_drcnrqDuration = a});

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
drcnrqReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnrqReservedCacheNodesOfferingId = lens _drcnrqReservedCacheNodesOfferingId (\ s a -> s{_drcnrqReservedCacheNodesOfferingId = a});

instance AWSPager DescribeReservedCacheNodes where
        page rq rs
          | stop (rs ^. drcnrsMarker) = Nothing
          | stop (rs ^. drcnrsReservedCacheNodes) = Nothing
          | otherwise =
            Just $ rq & drcnrqMarker .~ rs ^. drcnrsMarker

instance AWSRequest DescribeReservedCacheNodes where
        type Sv DescribeReservedCacheNodes = ElastiCache
        type Rs DescribeReservedCacheNodes =
             DescribeReservedCacheNodesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeReservedCacheNodesResult"
              (\ s h x ->
                 DescribeReservedCacheNodesResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ReservedCacheNodes" .!@ mempty >>=
                        may (parseXMLList "ReservedCacheNode"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeReservedCacheNodes where
        toHeaders = const mempty

instance ToPath DescribeReservedCacheNodes where
        toPath = const "/"

instance ToQuery DescribeReservedCacheNodes where
        toQuery DescribeReservedCacheNodes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedCacheNodes" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheNodeType" =: _drcnrqCacheNodeType,
               "ProductDescription" =: _drcnrqProductDescription,
               "MaxRecords" =: _drcnrqMaxRecords,
               "Marker" =: _drcnrqMarker,
               "ReservedCacheNodeId" =: _drcnrqReservedCacheNodeId,
               "OfferingType" =: _drcnrqOfferingType,
               "Duration" =: _drcnrqDuration,
               "ReservedCacheNodesOfferingId" =:
                 _drcnrqReservedCacheNodesOfferingId]

-- | Represents the output of a /DescribeReservedCacheNodes/ action.
--
-- /See:/ 'describeReservedCacheNodesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnrsMarker'
--
-- * 'drcnrsReservedCacheNodes'
--
-- * 'drcnrsStatus'
data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse'
    { _drcnrsMarker             :: !(Maybe Text)
    , _drcnrsReservedCacheNodes :: !(Maybe [ReservedCacheNode])
    , _drcnrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedCacheNodesResponse' smart constructor.
describeReservedCacheNodesResponse :: Int -> DescribeReservedCacheNodesResponse
describeReservedCacheNodesResponse pStatus_ =
    DescribeReservedCacheNodesResponse'
    { _drcnrsMarker = Nothing
    , _drcnrsReservedCacheNodes = Nothing
    , _drcnrsStatus = pStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnrsMarker :: Lens' DescribeReservedCacheNodesResponse (Maybe Text)
drcnrsMarker = lens _drcnrsMarker (\ s a -> s{_drcnrsMarker = a});

-- | A list of reserved cache nodes. Each element in the list contains
-- detailed information about one node.
drcnrsReservedCacheNodes :: Lens' DescribeReservedCacheNodesResponse [ReservedCacheNode]
drcnrsReservedCacheNodes = lens _drcnrsReservedCacheNodes (\ s a -> s{_drcnrsReservedCacheNodes = a}) . _Default;

-- | FIXME: Undocumented member.
drcnrsStatus :: Lens' DescribeReservedCacheNodesResponse Int
drcnrsStatus = lens _drcnrsStatus (\ s a -> s{_drcnrsStatus = a});
