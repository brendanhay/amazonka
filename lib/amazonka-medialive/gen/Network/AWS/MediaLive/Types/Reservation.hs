{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Reservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Reservation where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import Network.AWS.MediaLive.Types.ReservationState
import Network.AWS.Prelude

-- | Reserved resources available to use
--
-- /See:/ 'reservation' smart constructor.
data Reservation = Reservation'
  { _rState ::
      !(Maybe ReservationState),
    _rResourceSpecification ::
      !(Maybe ReservationResourceSpecification),
    _rCurrencyCode :: !(Maybe Text),
    _rARN :: !(Maybe Text),
    _rStart :: !(Maybe Text),
    _rCount :: !(Maybe Int),
    _rEnd :: !(Maybe Text),
    _rName :: !(Maybe Text),
    _rReservationId :: !(Maybe Text),
    _rOfferingId :: !(Maybe Text),
    _rRegion :: !(Maybe Text),
    _rOfferingType :: !(Maybe OfferingType),
    _rUsagePrice :: !(Maybe Double),
    _rFixedPrice :: !(Maybe Double),
    _rDurationUnits :: !(Maybe OfferingDurationUnits),
    _rOfferingDescription :: !(Maybe Text),
    _rDuration :: !(Maybe Int),
    _rTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Reservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rState' - Current state of reservation, e.g. 'ACTIVE'
--
-- * 'rResourceSpecification' - Resource configuration details
--
-- * 'rCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'rARN' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- * 'rStart' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- * 'rCount' - Number of reserved resources
--
-- * 'rEnd' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- * 'rName' - User specified reservation name
--
-- * 'rReservationId' - Unique reservation ID, e.g. '1234567'
--
-- * 'rOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'rRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'rOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'rUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'rFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'rDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'rOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'rDuration' - Lease duration, e.g. '12'
--
-- * 'rTags' - A collection of key-value pairs
reservation ::
  Reservation
reservation =
  Reservation'
    { _rState = Nothing,
      _rResourceSpecification = Nothing,
      _rCurrencyCode = Nothing,
      _rARN = Nothing,
      _rStart = Nothing,
      _rCount = Nothing,
      _rEnd = Nothing,
      _rName = Nothing,
      _rReservationId = Nothing,
      _rOfferingId = Nothing,
      _rRegion = Nothing,
      _rOfferingType = Nothing,
      _rUsagePrice = Nothing,
      _rFixedPrice = Nothing,
      _rDurationUnits = Nothing,
      _rOfferingDescription = Nothing,
      _rDuration = Nothing,
      _rTags = Nothing
    }

-- | Current state of reservation, e.g. 'ACTIVE'
rState :: Lens' Reservation (Maybe ReservationState)
rState = lens _rState (\s a -> s {_rState = a})

-- | Resource configuration details
rResourceSpecification :: Lens' Reservation (Maybe ReservationResourceSpecification)
rResourceSpecification = lens _rResourceSpecification (\s a -> s {_rResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
rCurrencyCode :: Lens' Reservation (Maybe Text)
rCurrencyCode = lens _rCurrencyCode (\s a -> s {_rCurrencyCode = a})

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
rARN :: Lens' Reservation (Maybe Text)
rARN = lens _rARN (\s a -> s {_rARN = a})

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
rStart :: Lens' Reservation (Maybe Text)
rStart = lens _rStart (\s a -> s {_rStart = a})

-- | Number of reserved resources
rCount :: Lens' Reservation (Maybe Int)
rCount = lens _rCount (\s a -> s {_rCount = a})

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
rEnd :: Lens' Reservation (Maybe Text)
rEnd = lens _rEnd (\s a -> s {_rEnd = a})

-- | User specified reservation name
rName :: Lens' Reservation (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | Unique reservation ID, e.g. '1234567'
rReservationId :: Lens' Reservation (Maybe Text)
rReservationId = lens _rReservationId (\s a -> s {_rReservationId = a})

-- | Unique offering ID, e.g. '87654321'
rOfferingId :: Lens' Reservation (Maybe Text)
rOfferingId = lens _rOfferingId (\s a -> s {_rOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
rRegion :: Lens' Reservation (Maybe Text)
rRegion = lens _rRegion (\s a -> s {_rRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
rOfferingType :: Lens' Reservation (Maybe OfferingType)
rOfferingType = lens _rOfferingType (\s a -> s {_rOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
rUsagePrice :: Lens' Reservation (Maybe Double)
rUsagePrice = lens _rUsagePrice (\s a -> s {_rUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
rFixedPrice :: Lens' Reservation (Maybe Double)
rFixedPrice = lens _rFixedPrice (\s a -> s {_rFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
rDurationUnits :: Lens' Reservation (Maybe OfferingDurationUnits)
rDurationUnits = lens _rDurationUnits (\s a -> s {_rDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
rOfferingDescription :: Lens' Reservation (Maybe Text)
rOfferingDescription = lens _rOfferingDescription (\s a -> s {_rOfferingDescription = a})

-- | Lease duration, e.g. '12'
rDuration :: Lens' Reservation (Maybe Int)
rDuration = lens _rDuration (\s a -> s {_rDuration = a})

-- | A collection of key-value pairs
rTags :: Lens' Reservation (HashMap Text (Text))
rTags = lens _rTags (\s a -> s {_rTags = a}) . _Default . _Map

instance FromJSON Reservation where
  parseJSON =
    withObject
      "Reservation"
      ( \x ->
          Reservation'
            <$> (x .:? "state")
            <*> (x .:? "resourceSpecification")
            <*> (x .:? "currencyCode")
            <*> (x .:? "arn")
            <*> (x .:? "start")
            <*> (x .:? "count")
            <*> (x .:? "end")
            <*> (x .:? "name")
            <*> (x .:? "reservationId")
            <*> (x .:? "offeringId")
            <*> (x .:? "region")
            <*> (x .:? "offeringType")
            <*> (x .:? "usagePrice")
            <*> (x .:? "fixedPrice")
            <*> (x .:? "durationUnits")
            <*> (x .:? "offeringDescription")
            <*> (x .:? "duration")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Reservation

instance NFData Reservation
