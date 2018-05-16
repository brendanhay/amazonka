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
-- Module      : Network.AWS.EC2.PurchaseHostReservation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchase a reservation with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation. This action results in the specified reservation being purchased and charged to your account.
--
--
module Network.AWS.EC2.PurchaseHostReservation
    (
    -- * Creating a Request
      purchaseHostReservation
    , PurchaseHostReservation
    -- * Request Lenses
    , phrCurrencyCode
    , phrClientToken
    , phrLimitPrice
    , phrHostIdSet
    , phrOfferingId

    -- * Destructuring the Response
    , purchaseHostReservationResponse
    , PurchaseHostReservationResponse
    -- * Response Lenses
    , phrrsCurrencyCode
    , phrrsClientToken
    , phrrsTotalHourlyPrice
    , phrrsTotalUpfrontPrice
    , phrrsPurchase
    , phrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'purchaseHostReservation' smart constructor.
data PurchaseHostReservation = PurchaseHostReservation'
  { _phrCurrencyCode :: !(Maybe CurrencyCodeValues)
  , _phrClientToken  :: !(Maybe Text)
  , _phrLimitPrice   :: !(Maybe Text)
  , _phrHostIdSet    :: ![Text]
  , _phrOfferingId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseHostReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phrCurrencyCode' - The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- * 'phrClientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'phrLimitPrice' - The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request will fail. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
--
-- * 'phrHostIdSet' - The ID/s of the Dedicated Host/s that the reservation will be associated with.
--
-- * 'phrOfferingId' - The ID of the offering.
purchaseHostReservation
    :: Text -- ^ 'phrOfferingId'
    -> PurchaseHostReservation
purchaseHostReservation pOfferingId_ =
  PurchaseHostReservation'
    { _phrCurrencyCode = Nothing
    , _phrClientToken = Nothing
    , _phrLimitPrice = Nothing
    , _phrHostIdSet = mempty
    , _phrOfferingId = pOfferingId_
    }


-- | The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
phrCurrencyCode :: Lens' PurchaseHostReservation (Maybe CurrencyCodeValues)
phrCurrencyCode = lens _phrCurrencyCode (\ s a -> s{_phrCurrencyCode = a})

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
phrClientToken :: Lens' PurchaseHostReservation (Maybe Text)
phrClientToken = lens _phrClientToken (\ s a -> s{_phrClientToken = a})

-- | The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request will fail. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
phrLimitPrice :: Lens' PurchaseHostReservation (Maybe Text)
phrLimitPrice = lens _phrLimitPrice (\ s a -> s{_phrLimitPrice = a})

-- | The ID/s of the Dedicated Host/s that the reservation will be associated with.
phrHostIdSet :: Lens' PurchaseHostReservation [Text]
phrHostIdSet = lens _phrHostIdSet (\ s a -> s{_phrHostIdSet = a}) . _Coerce

-- | The ID of the offering.
phrOfferingId :: Lens' PurchaseHostReservation Text
phrOfferingId = lens _phrOfferingId (\ s a -> s{_phrOfferingId = a})

instance AWSRequest PurchaseHostReservation where
        type Rs PurchaseHostReservation =
             PurchaseHostReservationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 PurchaseHostReservationResponse' <$>
                   (x .@? "currencyCode") <*> (x .@? "clientToken") <*>
                     (x .@? "totalHourlyPrice")
                     <*> (x .@? "totalUpfrontPrice")
                     <*>
                     (x .@? "purchase" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable PurchaseHostReservation where

instance NFData PurchaseHostReservation where

instance ToHeaders PurchaseHostReservation where
        toHeaders = const mempty

instance ToPath PurchaseHostReservation where
        toPath = const "/"

instance ToQuery PurchaseHostReservation where
        toQuery PurchaseHostReservation'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseHostReservation" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "CurrencyCode" =: _phrCurrencyCode,
               "ClientToken" =: _phrClientToken,
               "LimitPrice" =: _phrLimitPrice,
               toQueryList "HostIdSet" _phrHostIdSet,
               "OfferingId" =: _phrOfferingId]

-- | /See:/ 'purchaseHostReservationResponse' smart constructor.
data PurchaseHostReservationResponse = PurchaseHostReservationResponse'
  { _phrrsCurrencyCode      :: !(Maybe CurrencyCodeValues)
  , _phrrsClientToken       :: !(Maybe Text)
  , _phrrsTotalHourlyPrice  :: !(Maybe Text)
  , _phrrsTotalUpfrontPrice :: !(Maybe Text)
  , _phrrsPurchase          :: !(Maybe [Purchase])
  , _phrrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseHostReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phrrsCurrencyCode' - The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- * 'phrrsClientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/
--
-- * 'phrrsTotalHourlyPrice' - The total hourly price of the reservation calculated per hour.
--
-- * 'phrrsTotalUpfrontPrice' - The total amount that will be charged to your account when you purchase the reservation.
--
-- * 'phrrsPurchase' - Describes the details of the purchase.
--
-- * 'phrrsResponseStatus' - -- | The response status code.
purchaseHostReservationResponse
    :: Int -- ^ 'phrrsResponseStatus'
    -> PurchaseHostReservationResponse
purchaseHostReservationResponse pResponseStatus_ =
  PurchaseHostReservationResponse'
    { _phrrsCurrencyCode = Nothing
    , _phrrsClientToken = Nothing
    , _phrrsTotalHourlyPrice = Nothing
    , _phrrsTotalUpfrontPrice = Nothing
    , _phrrsPurchase = Nothing
    , _phrrsResponseStatus = pResponseStatus_
    }


-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
phrrsCurrencyCode :: Lens' PurchaseHostReservationResponse (Maybe CurrencyCodeValues)
phrrsCurrencyCode = lens _phrrsCurrencyCode (\ s a -> s{_phrrsCurrencyCode = a})

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/
phrrsClientToken :: Lens' PurchaseHostReservationResponse (Maybe Text)
phrrsClientToken = lens _phrrsClientToken (\ s a -> s{_phrrsClientToken = a})

-- | The total hourly price of the reservation calculated per hour.
phrrsTotalHourlyPrice :: Lens' PurchaseHostReservationResponse (Maybe Text)
phrrsTotalHourlyPrice = lens _phrrsTotalHourlyPrice (\ s a -> s{_phrrsTotalHourlyPrice = a})

-- | The total amount that will be charged to your account when you purchase the reservation.
phrrsTotalUpfrontPrice :: Lens' PurchaseHostReservationResponse (Maybe Text)
phrrsTotalUpfrontPrice = lens _phrrsTotalUpfrontPrice (\ s a -> s{_phrrsTotalUpfrontPrice = a})

-- | Describes the details of the purchase.
phrrsPurchase :: Lens' PurchaseHostReservationResponse [Purchase]
phrrsPurchase = lens _phrrsPurchase (\ s a -> s{_phrrsPurchase = a}) . _Default . _Coerce

-- | -- | The response status code.
phrrsResponseStatus :: Lens' PurchaseHostReservationResponse Int
phrrsResponseStatus = lens _phrrsResponseStatus (\ s a -> s{_phrrsResponseStatus = a})

instance NFData PurchaseHostReservationResponse where
