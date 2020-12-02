{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Offering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Offering where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import Network.AWS.Prelude

-- | Reserved resources available for purchase
--
-- /See:/ 'offering' smart constructor.
data Offering = Offering'
  { _oResourceSpecification ::
      !(Maybe ReservationResourceSpecification),
    _oCurrencyCode :: !(Maybe Text),
    _oARN :: !(Maybe Text),
    _oOfferingId :: !(Maybe Text),
    _oRegion :: !(Maybe Text),
    _oOfferingType :: !(Maybe OfferingType),
    _oUsagePrice :: !(Maybe Double),
    _oFixedPrice :: !(Maybe Double),
    _oDurationUnits :: !(Maybe OfferingDurationUnits),
    _oOfferingDescription :: !(Maybe Text),
    _oDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Offering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oResourceSpecification' - Resource configuration details
--
-- * 'oCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'oARN' - Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
--
-- * 'oOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'oRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'oOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'oUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'oFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'oDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'oOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'oDuration' - Lease duration, e.g. '12'
offering ::
  Offering
offering =
  Offering'
    { _oResourceSpecification = Nothing,
      _oCurrencyCode = Nothing,
      _oARN = Nothing,
      _oOfferingId = Nothing,
      _oRegion = Nothing,
      _oOfferingType = Nothing,
      _oUsagePrice = Nothing,
      _oFixedPrice = Nothing,
      _oDurationUnits = Nothing,
      _oOfferingDescription = Nothing,
      _oDuration = Nothing
    }

-- | Resource configuration details
oResourceSpecification :: Lens' Offering (Maybe ReservationResourceSpecification)
oResourceSpecification = lens _oResourceSpecification (\s a -> s {_oResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
oCurrencyCode :: Lens' Offering (Maybe Text)
oCurrencyCode = lens _oCurrencyCode (\s a -> s {_oCurrencyCode = a})

-- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
oARN :: Lens' Offering (Maybe Text)
oARN = lens _oARN (\s a -> s {_oARN = a})

-- | Unique offering ID, e.g. '87654321'
oOfferingId :: Lens' Offering (Maybe Text)
oOfferingId = lens _oOfferingId (\s a -> s {_oOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
oRegion :: Lens' Offering (Maybe Text)
oRegion = lens _oRegion (\s a -> s {_oRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
oOfferingType :: Lens' Offering (Maybe OfferingType)
oOfferingType = lens _oOfferingType (\s a -> s {_oOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
oUsagePrice :: Lens' Offering (Maybe Double)
oUsagePrice = lens _oUsagePrice (\s a -> s {_oUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
oFixedPrice :: Lens' Offering (Maybe Double)
oFixedPrice = lens _oFixedPrice (\s a -> s {_oFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
oDurationUnits :: Lens' Offering (Maybe OfferingDurationUnits)
oDurationUnits = lens _oDurationUnits (\s a -> s {_oDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
oOfferingDescription :: Lens' Offering (Maybe Text)
oOfferingDescription = lens _oOfferingDescription (\s a -> s {_oOfferingDescription = a})

-- | Lease duration, e.g. '12'
oDuration :: Lens' Offering (Maybe Int)
oDuration = lens _oDuration (\s a -> s {_oDuration = a})

instance FromJSON Offering where
  parseJSON =
    withObject
      "Offering"
      ( \x ->
          Offering'
            <$> (x .:? "resourceSpecification")
            <*> (x .:? "currencyCode")
            <*> (x .:? "arn")
            <*> (x .:? "offeringId")
            <*> (x .:? "region")
            <*> (x .:? "offeringType")
            <*> (x .:? "usagePrice")
            <*> (x .:? "fixedPrice")
            <*> (x .:? "durationUnits")
            <*> (x .:? "offeringDescription")
            <*> (x .:? "duration")
      )

instance Hashable Offering

instance NFData Offering
