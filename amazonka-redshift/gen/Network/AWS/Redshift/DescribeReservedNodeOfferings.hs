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
-- Module      : Network.AWS.Redshift.DescribeReservedNodeOfferings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available reserved node offerings by Amazon
-- Redshift with their descriptions including the node type, the fixed and
-- recurring costs of reserving the node and duration the node will be
-- reserved for you. These descriptions help you determine which reserve
-- node offering you want to purchase. You then use the unique offering ID
-- in you call to PurchaseReservedNodeOffering to reserve one or more nodes
-- for your Amazon Redshift cluster.
--
-- For more information about reserved node offerings, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeReservedNodeOfferings.html AWS API Reference> for DescribeReservedNodeOfferings.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeReservedNodeOfferings
    (
    -- * Creating a Request
      describeReservedNodeOfferings
    , DescribeReservedNodeOfferings
    -- * Request Lenses
    , drnoReservedNodeOfferingId
    , drnoMaxRecords
    , drnoMarker

    -- * Destructuring the Response
    , describeReservedNodeOfferingsResponse
    , DescribeReservedNodeOfferingsResponse
    -- * Response Lenses
    , drnorsReservedNodeOfferings
    , drnorsMarker
    , drnorsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeReservedNodeOfferings' smart constructor.
data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings'
    { _drnoReservedNodeOfferingId :: !(Maybe Text)
    , _drnoMaxRecords             :: !(Maybe Int)
    , _drnoMarker                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeReservedNodeOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drnoReservedNodeOfferingId'
--
-- * 'drnoMaxRecords'
--
-- * 'drnoMarker'
describeReservedNodeOfferings
    :: DescribeReservedNodeOfferings
describeReservedNodeOfferings =
    DescribeReservedNodeOfferings'
    { _drnoReservedNodeOfferingId = Nothing
    , _drnoMaxRecords = Nothing
    , _drnoMarker = Nothing
    }

-- | The unique identifier for the offering.
drnoReservedNodeOfferingId :: Lens' DescribeReservedNodeOfferings (Maybe Text)
drnoReservedNodeOfferingId = lens _drnoReservedNodeOfferingId (\ s a -> s{_drnoReservedNodeOfferingId = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified 'MaxRecords'
-- value, a value is returned in a 'marker' field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
drnoMaxRecords :: Lens' DescribeReservedNodeOfferings (Maybe Int)
drnoMaxRecords = lens _drnoMaxRecords (\ s a -> s{_drnoMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in 'MaxRecords', AWS returns a value
-- in the 'Marker' field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the 'Marker'
-- parameter and retrying the request.
drnoMarker :: Lens' DescribeReservedNodeOfferings (Maybe Text)
drnoMarker = lens _drnoMarker (\ s a -> s{_drnoMarker = a});

instance AWSPager DescribeReservedNodeOfferings where
        page rq rs
          | stop (rs ^. drnorsMarker) = Nothing
          | stop (rs ^. drnorsReservedNodeOfferings) = Nothing
          | otherwise =
            Just $ rq & drnoMarker .~ rs ^. drnorsMarker

instance AWSRequest DescribeReservedNodeOfferings
         where
        type Sv DescribeReservedNodeOfferings = Redshift
        type Rs DescribeReservedNodeOfferings =
             DescribeReservedNodeOfferingsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeReservedNodeOfferingsResult"
              (\ s h x ->
                 DescribeReservedNodeOfferingsResponse' <$>
                   (x .@? "ReservedNodeOfferings" .!@ mempty >>=
                      may (parseXMLList "ReservedNodeOffering"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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

-- | Contains the output from the DescribeReservedNodeOfferings action.
--
-- /See:/ 'describeReservedNodeOfferingsResponse' smart constructor.
data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'
    { _drnorsReservedNodeOfferings :: !(Maybe [ReservedNodeOffering])
    , _drnorsMarker                :: !(Maybe Text)
    , _drnorsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeReservedNodeOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drnorsReservedNodeOfferings'
--
-- * 'drnorsMarker'
--
-- * 'drnorsStatus'
describeReservedNodeOfferingsResponse
    :: Int -- ^ 'drnorsStatus'
    -> DescribeReservedNodeOfferingsResponse
describeReservedNodeOfferingsResponse pStatus_ =
    DescribeReservedNodeOfferingsResponse'
    { _drnorsReservedNodeOfferings = Nothing
    , _drnorsMarker = Nothing
    , _drnorsStatus = pStatus_
    }

-- | A list of reserved node offerings.
drnorsReservedNodeOfferings :: Lens' DescribeReservedNodeOfferingsResponse [ReservedNodeOffering]
drnorsReservedNodeOfferings = lens _drnorsReservedNodeOfferings (\ s a -> s{_drnorsReservedNodeOfferings = a}) . _Default . _Coerce;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the 'Marker' parameter and retrying the command. If the
-- 'Marker' field is empty, all response records have been retrieved for
-- the request.
drnorsMarker :: Lens' DescribeReservedNodeOfferingsResponse (Maybe Text)
drnorsMarker = lens _drnorsMarker (\ s a -> s{_drnorsMarker = a});

-- | The response status code.
drnorsStatus :: Lens' DescribeReservedNodeOfferingsResponse Int
drnorsStatus = lens _drnorsStatus (\ s a -> s{_drnorsStatus = a});
