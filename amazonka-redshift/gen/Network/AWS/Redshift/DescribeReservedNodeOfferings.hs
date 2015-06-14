{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DescribeReservedNodeOfferings
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

-- | Returns a list of the available reserved node offerings by Amazon
-- Redshift with their descriptions including the node type, the fixed and
-- recurring costs of reserving the node and duration the node will be
-- reserved for you. These descriptions help you determine which reserve
-- node offering you want to purchase. You then use the unique offering ID
-- in you call to PurchaseReservedNodeOffering to reserve one or more nodes
-- for your Amazon Redshift cluster.
--
-- For more information about managing parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeReservedNodeOfferings.html>
module Network.AWS.Redshift.DescribeReservedNodeOfferings
    (
    -- * Request
      DescribeReservedNodeOfferings
    -- ** Request constructor
    , describeReservedNodeOfferings
    -- ** Request lenses
    , drnoReservedNodeOfferingId
    , drnoMaxRecords
    , drnoMarker

    -- * Response
    , DescribeReservedNodeOfferingsResponse
    -- ** Response constructor
    , describeReservedNodeOfferingsResponse
    -- ** Response lenses
    , drnorReservedNodeOfferings
    , drnorMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'describeReservedNodeOfferings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnoReservedNodeOfferingId'
--
-- * 'drnoMaxRecords'
--
-- * 'drnoMarker'
data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings'{_drnoReservedNodeOfferingId :: Maybe Text, _drnoMaxRecords :: Maybe Int, _drnoMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeReservedNodeOfferings' smart constructor.
describeReservedNodeOfferings :: DescribeReservedNodeOfferings
describeReservedNodeOfferings = DescribeReservedNodeOfferings'{_drnoReservedNodeOfferingId = Nothing, _drnoMaxRecords = Nothing, _drnoMarker = Nothing};

-- | The unique identifier for the offering.
drnoReservedNodeOfferingId :: Lens' DescribeReservedNodeOfferings (Maybe Text)
drnoReservedNodeOfferingId = lens _drnoReservedNodeOfferingId (\ s a -> s{_drnoReservedNodeOfferingId = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
drnoMaxRecords :: Lens' DescribeReservedNodeOfferings (Maybe Int)
drnoMaxRecords = lens _drnoMaxRecords (\ s a -> s{_drnoMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
drnoMarker :: Lens' DescribeReservedNodeOfferings (Maybe Text)
drnoMarker = lens _drnoMarker (\ s a -> s{_drnoMarker = a});

instance AWSRequest DescribeReservedNodeOfferings
         where
        type Sv DescribeReservedNodeOfferings = Redshift
        type Rs DescribeReservedNodeOfferings =
             DescribeReservedNodeOfferingsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeReservedNodeOfferingsResult"
              (\ s h x ->
                 DescribeReservedNodeOfferingsResponse' <$>
                   (x .@? "ReservedNodeOfferings" .!@ mempty >>=
                      parseXMLList "ReservedNodeOffering")
                     <*> x .@? "Marker")

instance ToHeaders DescribeReservedNodeOfferings
         where
        toHeaders = const mempty

instance ToPath DescribeReservedNodeOfferings where
        toPath = const "/"

instance ToQuery DescribeReservedNodeOfferings where
        toQuery DescribeReservedNodeOfferings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedNodeOfferings" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ReservedNodeOfferingId" =:
                 _drnoReservedNodeOfferingId,
               "MaxRecords" =: _drnoMaxRecords,
               "Marker" =: _drnoMarker]

-- | /See:/ 'describeReservedNodeOfferingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnorReservedNodeOfferings'
--
-- * 'drnorMarker'
data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'{_drnorReservedNodeOfferings :: Maybe [ReservedNodeOffering], _drnorMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeReservedNodeOfferingsResponse' smart constructor.
describeReservedNodeOfferingsResponse :: DescribeReservedNodeOfferingsResponse
describeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'{_drnorReservedNodeOfferings = Nothing, _drnorMarker = Nothing};

-- | A list of reserved node offerings.
drnorReservedNodeOfferings :: Lens' DescribeReservedNodeOfferingsResponse (Maybe [ReservedNodeOffering])
drnorReservedNodeOfferings = lens _drnorReservedNodeOfferings (\ s a -> s{_drnorReservedNodeOfferings = a});

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
drnorMarker :: Lens' DescribeReservedNodeOfferingsResponse (Maybe Text)
drnorMarker = lens _drnorMarker (\ s a -> s{_drnorMarker = a});
