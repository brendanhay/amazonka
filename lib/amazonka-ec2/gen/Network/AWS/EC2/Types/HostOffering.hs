{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostOffering where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the Dedicated Host Reservation offering.
--
--
--
-- /See:/ 'hostOffering' smart constructor.
data HostOffering = HostOffering'
  { _hoInstanceFamily ::
      !(Maybe Text),
    _hoCurrencyCode :: !(Maybe CurrencyCodeValues),
    _hoHourlyPrice :: !(Maybe Text),
    _hoUpfrontPrice :: !(Maybe Text),
    _hoOfferingId :: !(Maybe Text),
    _hoDuration :: !(Maybe Int),
    _hoPaymentOption :: !(Maybe PaymentOption)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoInstanceFamily' - The instance family of the offering.
--
-- * 'hoCurrencyCode' - The currency of the offering.
--
-- * 'hoHourlyPrice' - The hourly price of the offering.
--
-- * 'hoUpfrontPrice' - The upfront price of the offering. Does not apply to No Upfront offerings.
--
-- * 'hoOfferingId' - The ID of the offering.
--
-- * 'hoDuration' - The duration of the offering (in seconds).
--
-- * 'hoPaymentOption' - The available payment option.
hostOffering ::
  HostOffering
hostOffering =
  HostOffering'
    { _hoInstanceFamily = Nothing,
      _hoCurrencyCode = Nothing,
      _hoHourlyPrice = Nothing,
      _hoUpfrontPrice = Nothing,
      _hoOfferingId = Nothing,
      _hoDuration = Nothing,
      _hoPaymentOption = Nothing
    }

-- | The instance family of the offering.
hoInstanceFamily :: Lens' HostOffering (Maybe Text)
hoInstanceFamily = lens _hoInstanceFamily (\s a -> s {_hoInstanceFamily = a})

-- | The currency of the offering.
hoCurrencyCode :: Lens' HostOffering (Maybe CurrencyCodeValues)
hoCurrencyCode = lens _hoCurrencyCode (\s a -> s {_hoCurrencyCode = a})

-- | The hourly price of the offering.
hoHourlyPrice :: Lens' HostOffering (Maybe Text)
hoHourlyPrice = lens _hoHourlyPrice (\s a -> s {_hoHourlyPrice = a})

-- | The upfront price of the offering. Does not apply to No Upfront offerings.
hoUpfrontPrice :: Lens' HostOffering (Maybe Text)
hoUpfrontPrice = lens _hoUpfrontPrice (\s a -> s {_hoUpfrontPrice = a})

-- | The ID of the offering.
hoOfferingId :: Lens' HostOffering (Maybe Text)
hoOfferingId = lens _hoOfferingId (\s a -> s {_hoOfferingId = a})

-- | The duration of the offering (in seconds).
hoDuration :: Lens' HostOffering (Maybe Int)
hoDuration = lens _hoDuration (\s a -> s {_hoDuration = a})

-- | The available payment option.
hoPaymentOption :: Lens' HostOffering (Maybe PaymentOption)
hoPaymentOption = lens _hoPaymentOption (\s a -> s {_hoPaymentOption = a})

instance FromXML HostOffering where
  parseXML x =
    HostOffering'
      <$> (x .@? "instanceFamily")
      <*> (x .@? "currencyCode")
      <*> (x .@? "hourlyPrice")
      <*> (x .@? "upfrontPrice")
      <*> (x .@? "offeringId")
      <*> (x .@? "duration")
      <*> (x .@? "paymentOption")

instance Hashable HostOffering

instance NFData HostOffering
