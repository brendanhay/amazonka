{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Purchase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Purchase where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the result of the purchase.
--
--
--
-- /See:/ 'purchase' smart constructor.
data Purchase = Purchase'
  { _pInstanceFamily :: !(Maybe Text),
    _pCurrencyCode :: !(Maybe CurrencyCodeValues),
    _pHostReservationId :: !(Maybe Text),
    _pHourlyPrice :: !(Maybe Text),
    _pUpfrontPrice :: !(Maybe Text),
    _pHostIdSet :: !(Maybe [Text]),
    _pDuration :: !(Maybe Int),
    _pPaymentOption :: !(Maybe PaymentOption)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Purchase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pInstanceFamily' - The instance family on the Dedicated Host that the reservation can be associated with.
--
-- * 'pCurrencyCode' - The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- * 'pHostReservationId' - The ID of the reservation.
--
-- * 'pHourlyPrice' - The hourly price of the reservation per hour.
--
-- * 'pUpfrontPrice' - The upfront price of the reservation.
--
-- * 'pHostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- * 'pDuration' - The duration of the reservation's term in seconds.
--
-- * 'pPaymentOption' - The payment option for the reservation.
purchase ::
  Purchase
purchase =
  Purchase'
    { _pInstanceFamily = Nothing,
      _pCurrencyCode = Nothing,
      _pHostReservationId = Nothing,
      _pHourlyPrice = Nothing,
      _pUpfrontPrice = Nothing,
      _pHostIdSet = Nothing,
      _pDuration = Nothing,
      _pPaymentOption = Nothing
    }

-- | The instance family on the Dedicated Host that the reservation can be associated with.
pInstanceFamily :: Lens' Purchase (Maybe Text)
pInstanceFamily = lens _pInstanceFamily (\s a -> s {_pInstanceFamily = a})

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
pCurrencyCode :: Lens' Purchase (Maybe CurrencyCodeValues)
pCurrencyCode = lens _pCurrencyCode (\s a -> s {_pCurrencyCode = a})

-- | The ID of the reservation.
pHostReservationId :: Lens' Purchase (Maybe Text)
pHostReservationId = lens _pHostReservationId (\s a -> s {_pHostReservationId = a})

-- | The hourly price of the reservation per hour.
pHourlyPrice :: Lens' Purchase (Maybe Text)
pHourlyPrice = lens _pHourlyPrice (\s a -> s {_pHourlyPrice = a})

-- | The upfront price of the reservation.
pUpfrontPrice :: Lens' Purchase (Maybe Text)
pUpfrontPrice = lens _pUpfrontPrice (\s a -> s {_pUpfrontPrice = a})

-- | The IDs of the Dedicated Hosts associated with the reservation.
pHostIdSet :: Lens' Purchase [Text]
pHostIdSet = lens _pHostIdSet (\s a -> s {_pHostIdSet = a}) . _Default . _Coerce

-- | The duration of the reservation's term in seconds.
pDuration :: Lens' Purchase (Maybe Int)
pDuration = lens _pDuration (\s a -> s {_pDuration = a})

-- | The payment option for the reservation.
pPaymentOption :: Lens' Purchase (Maybe PaymentOption)
pPaymentOption = lens _pPaymentOption (\s a -> s {_pPaymentOption = a})

instance FromXML Purchase where
  parseXML x =
    Purchase'
      <$> (x .@? "instanceFamily")
      <*> (x .@? "currencyCode")
      <*> (x .@? "hostReservationId")
      <*> (x .@? "hourlyPrice")
      <*> (x .@? "upfrontPrice")
      <*> (x .@? "hostIdSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "duration")
      <*> (x .@? "paymentOption")

instance Hashable Purchase

instance NFData Purchase
