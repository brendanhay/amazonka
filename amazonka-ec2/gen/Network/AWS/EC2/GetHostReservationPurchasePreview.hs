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
-- Module      : Network.AWS.EC2.GetHostReservationPurchasePreview
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Preview a reservation purchase with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation.
--
--
-- This is a preview of the 'PurchaseHostReservation' action and does not result in the offering being purchased.
--
module Network.AWS.EC2.GetHostReservationPurchasePreview
    (
    -- * Creating a Request
      getHostReservationPurchasePreview
    , GetHostReservationPurchasePreview
    -- * Request Lenses
    , ghrppHostIdSet
    , ghrppOfferingId

    -- * Destructuring the Response
    , getHostReservationPurchasePreviewResponse
    , GetHostReservationPurchasePreviewResponse
    -- * Response Lenses
    , ghrpprsCurrencyCode
    , ghrpprsTotalHourlyPrice
    , ghrpprsTotalUpfrontPrice
    , ghrpprsPurchase
    , ghrpprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getHostReservationPurchasePreview' smart constructor.
data GetHostReservationPurchasePreview = GetHostReservationPurchasePreview'
  { _ghrppHostIdSet  :: ![Text]
  , _ghrppOfferingId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHostReservationPurchasePreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghrppHostIdSet' - The IDs of the Dedicated Hosts with which the reservation is associated.
--
-- * 'ghrppOfferingId' - The offering ID of the reservation.
getHostReservationPurchasePreview
    :: Text -- ^ 'ghrppOfferingId'
    -> GetHostReservationPurchasePreview
getHostReservationPurchasePreview pOfferingId_ =
  GetHostReservationPurchasePreview'
    {_ghrppHostIdSet = mempty, _ghrppOfferingId = pOfferingId_}


-- | The IDs of the Dedicated Hosts with which the reservation is associated.
ghrppHostIdSet :: Lens' GetHostReservationPurchasePreview [Text]
ghrppHostIdSet = lens _ghrppHostIdSet (\ s a -> s{_ghrppHostIdSet = a}) . _Coerce

-- | The offering ID of the reservation.
ghrppOfferingId :: Lens' GetHostReservationPurchasePreview Text
ghrppOfferingId = lens _ghrppOfferingId (\ s a -> s{_ghrppOfferingId = a})

instance AWSRequest GetHostReservationPurchasePreview
         where
        type Rs GetHostReservationPurchasePreview =
             GetHostReservationPurchasePreviewResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 GetHostReservationPurchasePreviewResponse' <$>
                   (x .@? "currencyCode") <*> (x .@? "totalHourlyPrice")
                     <*> (x .@? "totalUpfrontPrice")
                     <*>
                     (x .@? "purchase" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable GetHostReservationPurchasePreview
         where

instance NFData GetHostReservationPurchasePreview
         where

instance ToHeaders GetHostReservationPurchasePreview
         where
        toHeaders = const mempty

instance ToPath GetHostReservationPurchasePreview
         where
        toPath = const "/"

instance ToQuery GetHostReservationPurchasePreview
         where
        toQuery GetHostReservationPurchasePreview'{..}
          = mconcat
              ["Action" =:
                 ("GetHostReservationPurchasePreview" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQueryList "HostIdSet" _ghrppHostIdSet,
               "OfferingId" =: _ghrppOfferingId]

-- | /See:/ 'getHostReservationPurchasePreviewResponse' smart constructor.
data GetHostReservationPurchasePreviewResponse = GetHostReservationPurchasePreviewResponse'
  { _ghrpprsCurrencyCode      :: !(Maybe CurrencyCodeValues)
  , _ghrpprsTotalHourlyPrice  :: !(Maybe Text)
  , _ghrpprsTotalUpfrontPrice :: !(Maybe Text)
  , _ghrpprsPurchase          :: !(Maybe [Purchase])
  , _ghrpprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHostReservationPurchasePreviewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghrpprsCurrencyCode' - The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- * 'ghrpprsTotalHourlyPrice' - The potential total hourly price of the reservation per hour.
--
-- * 'ghrpprsTotalUpfrontPrice' - The potential total upfront price. This is billed immediately.
--
-- * 'ghrpprsPurchase' - The purchase information of the Dedicated Host reservation and the Dedicated Hosts associated with it.
--
-- * 'ghrpprsResponseStatus' - -- | The response status code.
getHostReservationPurchasePreviewResponse
    :: Int -- ^ 'ghrpprsResponseStatus'
    -> GetHostReservationPurchasePreviewResponse
getHostReservationPurchasePreviewResponse pResponseStatus_ =
  GetHostReservationPurchasePreviewResponse'
    { _ghrpprsCurrencyCode = Nothing
    , _ghrpprsTotalHourlyPrice = Nothing
    , _ghrpprsTotalUpfrontPrice = Nothing
    , _ghrpprsPurchase = Nothing
    , _ghrpprsResponseStatus = pResponseStatus_
    }


-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
ghrpprsCurrencyCode :: Lens' GetHostReservationPurchasePreviewResponse (Maybe CurrencyCodeValues)
ghrpprsCurrencyCode = lens _ghrpprsCurrencyCode (\ s a -> s{_ghrpprsCurrencyCode = a})

-- | The potential total hourly price of the reservation per hour.
ghrpprsTotalHourlyPrice :: Lens' GetHostReservationPurchasePreviewResponse (Maybe Text)
ghrpprsTotalHourlyPrice = lens _ghrpprsTotalHourlyPrice (\ s a -> s{_ghrpprsTotalHourlyPrice = a})

-- | The potential total upfront price. This is billed immediately.
ghrpprsTotalUpfrontPrice :: Lens' GetHostReservationPurchasePreviewResponse (Maybe Text)
ghrpprsTotalUpfrontPrice = lens _ghrpprsTotalUpfrontPrice (\ s a -> s{_ghrpprsTotalUpfrontPrice = a})

-- | The purchase information of the Dedicated Host reservation and the Dedicated Hosts associated with it.
ghrpprsPurchase :: Lens' GetHostReservationPurchasePreviewResponse [Purchase]
ghrpprsPurchase = lens _ghrpprsPurchase (\ s a -> s{_ghrpprsPurchase = a}) . _Default . _Coerce

-- | -- | The response status code.
ghrpprsResponseStatus :: Lens' GetHostReservationPurchasePreviewResponse Int
ghrpprsResponseStatus = lens _ghrpprsResponseStatus (\ s a -> s{_ghrpprsResponseStatus = a})

instance NFData
           GetHostReservationPurchasePreviewResponse
         where
