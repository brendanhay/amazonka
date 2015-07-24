{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeReservedCacheNodesOfferings/ action lists available
-- reserved cache node offerings.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeReservedCacheNodesOfferings.html>
module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
    (
    -- * Request
      DescribeReservedCacheNodesOfferings
    -- ** Request constructor
    , describeReservedCacheNodesOfferings
    -- ** Request lenses
    , drcnoCacheNodeType
    , drcnoProductDescription
    , drcnoMaxRecords
    , drcnoMarker
    , drcnoOfferingType
    , drcnoDuration
    , drcnoReservedCacheNodesOfferingId

    -- * Response
    , DescribeReservedCacheNodesOfferingsResponse
    -- ** Response constructor
    , describeReservedCacheNodesOfferingsResponse
    -- ** Response lenses
    , drcnorsMarker
    , drcnorsReservedCacheNodesOfferings
    , drcnorsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeReservedCacheNodesOfferings/ action.
--
-- /See:/ 'describeReservedCacheNodesOfferings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnoCacheNodeType'
--
-- * 'drcnoProductDescription'
--
-- * 'drcnoMaxRecords'
--
-- * 'drcnoMarker'
--
-- * 'drcnoOfferingType'
--
-- * 'drcnoDuration'
--
-- * 'drcnoReservedCacheNodesOfferingId'
data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings'
    { _drcnoCacheNodeType                :: !(Maybe Text)
    , _drcnoProductDescription           :: !(Maybe Text)
    , _drcnoMaxRecords                   :: !(Maybe Int)
    , _drcnoMarker                       :: !(Maybe Text)
    , _drcnoOfferingType                 :: !(Maybe Text)
    , _drcnoDuration                     :: !(Maybe Text)
    , _drcnoReservedCacheNodesOfferingId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedCacheNodesOfferings' smart constructor.
describeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings
describeReservedCacheNodesOfferings =
    DescribeReservedCacheNodesOfferings'
    { _drcnoCacheNodeType = Nothing
    , _drcnoProductDescription = Nothing
    , _drcnoMaxRecords = Nothing
    , _drcnoMarker = Nothing
    , _drcnoOfferingType = Nothing
    , _drcnoDuration = Nothing
    , _drcnoReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only the
-- available offerings matching the specified cache node type.
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
drcnoCacheNodeType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoCacheNodeType = lens _drcnoCacheNodeType (\ s a -> s{_drcnoCacheNodeType = a});

-- | The product description filter value. Use this parameter to show only
-- the available offerings matching the specified product description.
drcnoProductDescription :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoProductDescription = lens _drcnoProductDescription (\ s a -> s{_drcnoProductDescription = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
drcnoMaxRecords :: Lens' DescribeReservedCacheNodesOfferings (Maybe Int)
drcnoMaxRecords = lens _drcnoMaxRecords (\ s a -> s{_drcnoMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
drcnoMarker :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoMarker = lens _drcnoMarker (\ s a -> s{_drcnoMarker = a});

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values:
-- @\"Light Utilization\"|\"Medium Utilization\"|\"Heavy Utilization\"@
drcnoOfferingType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoOfferingType = lens _drcnoOfferingType (\ s a -> s{_drcnoOfferingType = a});

-- | Duration filter value, specified in years or seconds. Use this parameter
-- to show only reservations for a given duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
drcnoDuration :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoDuration = lens _drcnoDuration (\ s a -> s{_drcnoDuration = a});

-- | The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
drcnoReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoReservedCacheNodesOfferingId = lens _drcnoReservedCacheNodesOfferingId (\ s a -> s{_drcnoReservedCacheNodesOfferingId = a});

instance AWSPager DescribeReservedCacheNodesOfferings
         where
        page rq rs
          | stop (rs ^. drcnorsMarker) = Nothing
          | stop (rs ^. drcnorsReservedCacheNodesOfferings) =
            Nothing
          | otherwise =
            Just $ rq & drcnoMarker .~ rs ^. drcnorsMarker

instance AWSRequest
         DescribeReservedCacheNodesOfferings where
        type Sv DescribeReservedCacheNodesOfferings =
             ElastiCache
        type Rs DescribeReservedCacheNodesOfferings =
             DescribeReservedCacheNodesOfferingsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeReservedCacheNodesOfferingsResult"
              (\ s h x ->
                 DescribeReservedCacheNodesOfferingsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ReservedCacheNodesOfferings" .!@ mempty >>=
                        may (parseXMLList "ReservedCacheNodesOffering"))
                     <*> (pure (fromEnum s)))

instance ToHeaders
         DescribeReservedCacheNodesOfferings where
        toHeaders = const mempty

instance ToPath DescribeReservedCacheNodesOfferings
         where
        toPath = const "/"

instance ToQuery DescribeReservedCacheNodesOfferings
         where
        toQuery DescribeReservedCacheNodesOfferings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedCacheNodesOfferings" ::
                    ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheNodeType" =: _drcnoCacheNodeType,
               "ProductDescription" =: _drcnoProductDescription,
               "MaxRecords" =: _drcnoMaxRecords,
               "Marker" =: _drcnoMarker,
               "OfferingType" =: _drcnoOfferingType,
               "Duration" =: _drcnoDuration,
               "ReservedCacheNodesOfferingId" =:
                 _drcnoReservedCacheNodesOfferingId]

-- | Represents the output of a /DescribeReservedCacheNodesOfferings/ action.
--
-- /See:/ 'describeReservedCacheNodesOfferingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnorsMarker'
--
-- * 'drcnorsReservedCacheNodesOfferings'
--
-- * 'drcnorsStatus'
data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse'
    { _drcnorsMarker                      :: !(Maybe Text)
    , _drcnorsReservedCacheNodesOfferings :: !(Maybe [ReservedCacheNodesOffering])
    , _drcnorsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedCacheNodesOfferingsResponse' smart constructor.
describeReservedCacheNodesOfferingsResponse :: Int -> DescribeReservedCacheNodesOfferingsResponse
describeReservedCacheNodesOfferingsResponse pStatus_ =
    DescribeReservedCacheNodesOfferingsResponse'
    { _drcnorsMarker = Nothing
    , _drcnorsReservedCacheNodesOfferings = Nothing
    , _drcnorsStatus = pStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnorsMarker :: Lens' DescribeReservedCacheNodesOfferingsResponse (Maybe Text)
drcnorsMarker = lens _drcnorsMarker (\ s a -> s{_drcnorsMarker = a});

-- | A list of reserved cache node offerings. Each element in the list
-- contains detailed information about one offering.
drcnorsReservedCacheNodesOfferings :: Lens' DescribeReservedCacheNodesOfferingsResponse [ReservedCacheNodesOffering]
drcnorsReservedCacheNodesOfferings = lens _drcnorsReservedCacheNodesOfferings (\ s a -> s{_drcnorsReservedCacheNodesOfferings = a}) . _Default;

-- | FIXME: Undocumented member.
drcnorsStatus :: Lens' DescribeReservedCacheNodesOfferingsResponse Int
drcnorsStatus = lens _drcnorsStatus (\ s a -> s{_drcnorsStatus = a});
