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
-- Module      : Network.AWS.EC2.DescribeCapacityReservations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Capacity Reservations. The results describe only the Capacity Reservations in the AWS Region that you're currently using.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCapacityReservations
    (
    -- * Creating a Request
      describeCapacityReservations
    , DescribeCapacityReservations
    -- * Request Lenses
    , dcrCapacityReservationIds
    , dcrFilters
    , dcrNextToken
    , dcrDryRun
    , dcrMaxResults

    -- * Destructuring the Response
    , describeCapacityReservationsResponse
    , DescribeCapacityReservationsResponse
    -- * Response Lenses
    , dcrrsCapacityReservations
    , dcrrsNextToken
    , dcrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCapacityReservations' smart constructor.
data DescribeCapacityReservations = DescribeCapacityReservations'
  { _dcrCapacityReservationIds :: !(Maybe [Text])
  , _dcrFilters                :: !(Maybe [Filter])
  , _dcrNextToken              :: !(Maybe Text)
  , _dcrDryRun                 :: !(Maybe Bool)
  , _dcrMaxResults             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCapacityReservations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrCapacityReservationIds' - The ID of the Capacity Reservation.
--
-- * 'dcrFilters' - One or more filters.
--
-- * 'dcrNextToken' - The token to retrieve the next page of results.
--
-- * 'dcrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcrMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned nextToken value.
describeCapacityReservations
    :: DescribeCapacityReservations
describeCapacityReservations =
  DescribeCapacityReservations'
    { _dcrCapacityReservationIds = Nothing
    , _dcrFilters = Nothing
    , _dcrNextToken = Nothing
    , _dcrDryRun = Nothing
    , _dcrMaxResults = Nothing
    }


-- | The ID of the Capacity Reservation.
dcrCapacityReservationIds :: Lens' DescribeCapacityReservations [Text]
dcrCapacityReservationIds = lens _dcrCapacityReservationIds (\ s a -> s{_dcrCapacityReservationIds = a}) . _Default . _Coerce

-- | One or more filters.
dcrFilters :: Lens' DescribeCapacityReservations [Filter]
dcrFilters = lens _dcrFilters (\ s a -> s{_dcrFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dcrNextToken :: Lens' DescribeCapacityReservations (Maybe Text)
dcrNextToken = lens _dcrNextToken (\ s a -> s{_dcrNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcrDryRun :: Lens' DescribeCapacityReservations (Maybe Bool)
dcrDryRun = lens _dcrDryRun (\ s a -> s{_dcrDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned nextToken value.
dcrMaxResults :: Lens' DescribeCapacityReservations (Maybe Int)
dcrMaxResults = lens _dcrMaxResults (\ s a -> s{_dcrMaxResults = a})

instance AWSPager DescribeCapacityReservations where
        page rq rs
          | stop (rs ^. dcrrsNextToken) = Nothing
          | stop (rs ^. dcrrsCapacityReservations) = Nothing
          | otherwise =
            Just $ rq & dcrNextToken .~ rs ^. dcrrsNextToken

instance AWSRequest DescribeCapacityReservations
         where
        type Rs DescribeCapacityReservations =
             DescribeCapacityReservationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeCapacityReservationsResponse' <$>
                   (x .@? "capacityReservationSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCapacityReservations where

instance NFData DescribeCapacityReservations where

instance ToHeaders DescribeCapacityReservations where
        toHeaders = const mempty

instance ToPath DescribeCapacityReservations where
        toPath = const "/"

instance ToQuery DescribeCapacityReservations where
        toQuery DescribeCapacityReservations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCapacityReservations" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "CapacityReservationId" <$>
                    _dcrCapacityReservationIds),
               toQuery (toQueryList "Filter" <$> _dcrFilters),
               "NextToken" =: _dcrNextToken, "DryRun" =: _dcrDryRun,
               "MaxResults" =: _dcrMaxResults]

-- | /See:/ 'describeCapacityReservationsResponse' smart constructor.
data DescribeCapacityReservationsResponse = DescribeCapacityReservationsResponse'
  { _dcrrsCapacityReservations :: !(Maybe [CapacityReservation])
  , _dcrrsNextToken            :: !(Maybe Text)
  , _dcrrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCapacityReservationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrrsCapacityReservations' - Information about the Capacity Reservations.
--
-- * 'dcrrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dcrrsResponseStatus' - -- | The response status code.
describeCapacityReservationsResponse
    :: Int -- ^ 'dcrrsResponseStatus'
    -> DescribeCapacityReservationsResponse
describeCapacityReservationsResponse pResponseStatus_ =
  DescribeCapacityReservationsResponse'
    { _dcrrsCapacityReservations = Nothing
    , _dcrrsNextToken = Nothing
    , _dcrrsResponseStatus = pResponseStatus_
    }


-- | Information about the Capacity Reservations.
dcrrsCapacityReservations :: Lens' DescribeCapacityReservationsResponse [CapacityReservation]
dcrrsCapacityReservations = lens _dcrrsCapacityReservations (\ s a -> s{_dcrrsCapacityReservations = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dcrrsNextToken :: Lens' DescribeCapacityReservationsResponse (Maybe Text)
dcrrsNextToken = lens _dcrrsNextToken (\ s a -> s{_dcrrsNextToken = a})

-- | -- | The response status code.
dcrrsResponseStatus :: Lens' DescribeCapacityReservationsResponse Int
dcrrsResponseStatus = lens _dcrrsResponseStatus (\ s a -> s{_dcrrsResponseStatus = a})

instance NFData DescribeCapacityReservationsResponse
         where
