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
-- Module      : Network.AWS.EC2.DescribeHostReservations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Dedicated Host Reservations which are associated with Dedicated Hosts in your account.
--
--
module Network.AWS.EC2.DescribeHostReservations
    (
    -- * Creating a Request
      describeHostReservations
    , DescribeHostReservations
    -- * Request Lenses
    , dhrNextToken
    , dhrHostReservationIdSet
    , dhrFilter
    , dhrMaxResults

    -- * Destructuring the Response
    , describeHostReservationsResponse
    , DescribeHostReservationsResponse
    -- * Response Lenses
    , dhrrsNextToken
    , dhrrsHostReservationSet
    , dhrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeHostReservations' smart constructor.
data DescribeHostReservations = DescribeHostReservations'
  { _dhrNextToken            :: !(Maybe Text)
  , _dhrHostReservationIdSet :: !(Maybe [Text])
  , _dhrFilter               :: !(Maybe [Filter])
  , _dhrMaxResults           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHostReservations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrNextToken' - The token to use to retrieve the next page of results.
--
-- * 'dhrHostReservationIdSet' - One or more host reservation IDs.
--
-- * 'dhrFilter' - One or more filters.     * @instance-family@ - The instance family (e.g., @m4@ ).     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
--
-- * 'dhrMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500; if @maxResults@ is given a larger value than 500, you will receive an error.
describeHostReservations
    :: DescribeHostReservations
describeHostReservations =
  DescribeHostReservations'
    { _dhrNextToken = Nothing
    , _dhrHostReservationIdSet = Nothing
    , _dhrFilter = Nothing
    , _dhrMaxResults = Nothing
    }


-- | The token to use to retrieve the next page of results.
dhrNextToken :: Lens' DescribeHostReservations (Maybe Text)
dhrNextToken = lens _dhrNextToken (\ s a -> s{_dhrNextToken = a})

-- | One or more host reservation IDs.
dhrHostReservationIdSet :: Lens' DescribeHostReservations [Text]
dhrHostReservationIdSet = lens _dhrHostReservationIdSet (\ s a -> s{_dhrHostReservationIdSet = a}) . _Default . _Coerce

-- | One or more filters.     * @instance-family@ - The instance family (e.g., @m4@ ).     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
dhrFilter :: Lens' DescribeHostReservations [Filter]
dhrFilter = lens _dhrFilter (\ s a -> s{_dhrFilter = a}) . _Default . _Coerce

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500; if @maxResults@ is given a larger value than 500, you will receive an error.
dhrMaxResults :: Lens' DescribeHostReservations (Maybe Int)
dhrMaxResults = lens _dhrMaxResults (\ s a -> s{_dhrMaxResults = a})

instance AWSRequest DescribeHostReservations where
        type Rs DescribeHostReservations =
             DescribeHostReservationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeHostReservationsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "hostReservationSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeHostReservations where

instance NFData DescribeHostReservations where

instance ToHeaders DescribeHostReservations where
        toHeaders = const mempty

instance ToPath DescribeHostReservations where
        toPath = const "/"

instance ToQuery DescribeHostReservations where
        toQuery DescribeHostReservations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeHostReservations" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NextToken" =: _dhrNextToken,
               toQuery
                 (toQueryList "HostReservationIdSet" <$>
                    _dhrHostReservationIdSet),
               toQuery (toQueryList "Filter" <$> _dhrFilter),
               "MaxResults" =: _dhrMaxResults]

-- | /See:/ 'describeHostReservationsResponse' smart constructor.
data DescribeHostReservationsResponse = DescribeHostReservationsResponse'
  { _dhrrsNextToken          :: !(Maybe Text)
  , _dhrrsHostReservationSet :: !(Maybe [HostReservation])
  , _dhrrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHostReservationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dhrrsHostReservationSet' - Details about the reservation's configuration.
--
-- * 'dhrrsResponseStatus' - -- | The response status code.
describeHostReservationsResponse
    :: Int -- ^ 'dhrrsResponseStatus'
    -> DescribeHostReservationsResponse
describeHostReservationsResponse pResponseStatus_ =
  DescribeHostReservationsResponse'
    { _dhrrsNextToken = Nothing
    , _dhrrsHostReservationSet = Nothing
    , _dhrrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dhrrsNextToken :: Lens' DescribeHostReservationsResponse (Maybe Text)
dhrrsNextToken = lens _dhrrsNextToken (\ s a -> s{_dhrrsNextToken = a})

-- | Details about the reservation's configuration.
dhrrsHostReservationSet :: Lens' DescribeHostReservationsResponse [HostReservation]
dhrrsHostReservationSet = lens _dhrrsHostReservationSet (\ s a -> s{_dhrrsHostReservationSet = a}) . _Default . _Coerce

-- | -- | The response status code.
dhrrsResponseStatus :: Lens' DescribeHostReservationsResponse Int
dhrrsResponseStatus = lens _dhrrsResponseStatus (\ s a -> s{_dhrrsResponseStatus = a})

instance NFData DescribeHostReservationsResponse
         where
