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
-- Module      : Network.AWS.EC2.DescribeHostReservationOfferings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Dedicated Host Reservations that are available to purchase.
--
--
-- The results describe all the Dedicated Host Reservation offerings, including offerings that may not match the instance family and region of your Dedicated Hosts. When purchasing an offering, ensure that the the instance family and region of the offering matches that of the Dedicated Host/s it will be associated with. For an overview of supported instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Hosts Overview> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeHostReservationOfferings
    (
    -- * Creating a Request
      describeHostReservationOfferings
    , DescribeHostReservationOfferings
    -- * Request Lenses
    , dhroMaxDuration
    , dhroNextToken
    , dhroMinDuration
    , dhroOfferingId
    , dhroFilter
    , dhroMaxResults

    -- * Destructuring the Response
    , describeHostReservationOfferingsResponse
    , DescribeHostReservationOfferingsResponse
    -- * Response Lenses
    , dhrorsOfferingSet
    , dhrorsNextToken
    , dhrorsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeHostReservationOfferings' smart constructor.
data DescribeHostReservationOfferings = DescribeHostReservationOfferings'
  { _dhroMaxDuration :: !(Maybe Int)
  , _dhroNextToken   :: !(Maybe Text)
  , _dhroMinDuration :: !(Maybe Int)
  , _dhroOfferingId  :: !(Maybe Text)
  , _dhroFilter      :: !(Maybe [Filter])
  , _dhroMaxResults  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHostReservationOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhroMaxDuration' - This is the maximum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 94608000 for three years.
--
-- * 'dhroNextToken' - The token to use to retrieve the next page of results.
--
-- * 'dhroMinDuration' - This is the minimum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 31536000 for one year.
--
-- * 'dhroOfferingId' - The ID of the reservation offering.
--
-- * 'dhroFilter' - One or more filters.     * @instance-family@ - The instance family of the offering (e.g., @m4@ ).     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
--
-- * 'dhroMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500; if @maxResults@ is given a larger value than 500, you will receive an error.
describeHostReservationOfferings
    :: DescribeHostReservationOfferings
describeHostReservationOfferings =
  DescribeHostReservationOfferings'
    { _dhroMaxDuration = Nothing
    , _dhroNextToken = Nothing
    , _dhroMinDuration = Nothing
    , _dhroOfferingId = Nothing
    , _dhroFilter = Nothing
    , _dhroMaxResults = Nothing
    }


-- | This is the maximum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 94608000 for three years.
dhroMaxDuration :: Lens' DescribeHostReservationOfferings (Maybe Int)
dhroMaxDuration = lens _dhroMaxDuration (\ s a -> s{_dhroMaxDuration = a})

-- | The token to use to retrieve the next page of results.
dhroNextToken :: Lens' DescribeHostReservationOfferings (Maybe Text)
dhroNextToken = lens _dhroNextToken (\ s a -> s{_dhroNextToken = a})

-- | This is the minimum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 31536000 for one year.
dhroMinDuration :: Lens' DescribeHostReservationOfferings (Maybe Int)
dhroMinDuration = lens _dhroMinDuration (\ s a -> s{_dhroMinDuration = a})

-- | The ID of the reservation offering.
dhroOfferingId :: Lens' DescribeHostReservationOfferings (Maybe Text)
dhroOfferingId = lens _dhroOfferingId (\ s a -> s{_dhroOfferingId = a})

-- | One or more filters.     * @instance-family@ - The instance family of the offering (e.g., @m4@ ).     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
dhroFilter :: Lens' DescribeHostReservationOfferings [Filter]
dhroFilter = lens _dhroFilter (\ s a -> s{_dhroFilter = a}) . _Default . _Coerce

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500; if @maxResults@ is given a larger value than 500, you will receive an error.
dhroMaxResults :: Lens' DescribeHostReservationOfferings (Maybe Int)
dhroMaxResults = lens _dhroMaxResults (\ s a -> s{_dhroMaxResults = a})

instance AWSRequest DescribeHostReservationOfferings
         where
        type Rs DescribeHostReservationOfferings =
             DescribeHostReservationOfferingsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeHostReservationOfferingsResponse' <$>
                   (x .@? "offeringSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeHostReservationOfferings
         where

instance NFData DescribeHostReservationOfferings
         where

instance ToHeaders DescribeHostReservationOfferings
         where
        toHeaders = const mempty

instance ToPath DescribeHostReservationOfferings
         where
        toPath = const "/"

instance ToQuery DescribeHostReservationOfferings
         where
        toQuery DescribeHostReservationOfferings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeHostReservationOfferings" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "MaxDuration" =: _dhroMaxDuration,
               "NextToken" =: _dhroNextToken,
               "MinDuration" =: _dhroMinDuration,
               "OfferingId" =: _dhroOfferingId,
               toQuery (toQueryList "Filter" <$> _dhroFilter),
               "MaxResults" =: _dhroMaxResults]

-- | /See:/ 'describeHostReservationOfferingsResponse' smart constructor.
data DescribeHostReservationOfferingsResponse = DescribeHostReservationOfferingsResponse'
  { _dhrorsOfferingSet    :: !(Maybe [HostOffering])
  , _dhrorsNextToken      :: !(Maybe Text)
  , _dhrorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHostReservationOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrorsOfferingSet' - Information about the offerings.
--
-- * 'dhrorsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dhrorsResponseStatus' - -- | The response status code.
describeHostReservationOfferingsResponse
    :: Int -- ^ 'dhrorsResponseStatus'
    -> DescribeHostReservationOfferingsResponse
describeHostReservationOfferingsResponse pResponseStatus_ =
  DescribeHostReservationOfferingsResponse'
    { _dhrorsOfferingSet = Nothing
    , _dhrorsNextToken = Nothing
    , _dhrorsResponseStatus = pResponseStatus_
    }


-- | Information about the offerings.
dhrorsOfferingSet :: Lens' DescribeHostReservationOfferingsResponse [HostOffering]
dhrorsOfferingSet = lens _dhrorsOfferingSet (\ s a -> s{_dhrorsOfferingSet = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dhrorsNextToken :: Lens' DescribeHostReservationOfferingsResponse (Maybe Text)
dhrorsNextToken = lens _dhrorsNextToken (\ s a -> s{_dhrorsNextToken = a})

-- | -- | The response status code.
dhrorsResponseStatus :: Lens' DescribeHostReservationOfferingsResponse Int
dhrorsResponseStatus = lens _dhrorsResponseStatus (\ s a -> s{_dhrorsResponseStatus = a})

instance NFData
           DescribeHostReservationOfferingsResponse
         where
