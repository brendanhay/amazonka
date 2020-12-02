{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a bandwidth rate limit interval for a gateway. A bandwidth rate limit schedule consists of one or more bandwidth rate limit intervals. A bandwidth rate limit interval defines a period of time on one or more days of the week, during which bandwidth rate limits are specified for uploading, downloading, or both.
--
--
--
-- /See:/ 'bandwidthRateLimitInterval' smart constructor.
data BandwidthRateLimitInterval = BandwidthRateLimitInterval'
  { _brliAverageUploadRateLimitInBitsPerSec ::
      !(Maybe Nat),
    _brliAverageDownloadRateLimitInBitsPerSec ::
      !(Maybe Nat),
    _brliStartHourOfDay :: !Nat,
    _brliStartMinuteOfHour :: !Nat,
    _brliEndHourOfDay :: !Nat,
    _brliEndMinuteOfHour :: !Nat,
    _brliDaysOfWeek :: !(List1 Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BandwidthRateLimitInterval' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brliAverageUploadRateLimitInBitsPerSec' - The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set.
--
-- * 'brliAverageDownloadRateLimitInBitsPerSec' - The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set.
--
-- * 'brliStartHourOfDay' - The hour of the day to start the bandwidth rate limit interval.
--
-- * 'brliStartMinuteOfHour' - The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ .
--
-- * 'brliEndHourOfDay' - The hour of the day to end the bandwidth rate limit interval.
--
-- * 'brliEndMinuteOfHour' - The minute of the hour to end the bandwidth rate limit interval.  /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
--
-- * 'brliDaysOfWeek' - The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
bandwidthRateLimitInterval ::
  -- | 'brliStartHourOfDay'
  Natural ->
  -- | 'brliStartMinuteOfHour'
  Natural ->
  -- | 'brliEndHourOfDay'
  Natural ->
  -- | 'brliEndMinuteOfHour'
  Natural ->
  -- | 'brliDaysOfWeek'
  NonEmpty Natural ->
  BandwidthRateLimitInterval
bandwidthRateLimitInterval
  pStartHourOfDay_
  pStartMinuteOfHour_
  pEndHourOfDay_
  pEndMinuteOfHour_
  pDaysOfWeek_ =
    BandwidthRateLimitInterval'
      { _brliAverageUploadRateLimitInBitsPerSec =
          Nothing,
        _brliAverageDownloadRateLimitInBitsPerSec = Nothing,
        _brliStartHourOfDay = _Nat # pStartHourOfDay_,
        _brliStartMinuteOfHour = _Nat # pStartMinuteOfHour_,
        _brliEndHourOfDay = _Nat # pEndHourOfDay_,
        _brliEndMinuteOfHour = _Nat # pEndMinuteOfHour_,
        _brliDaysOfWeek = _List1 # pDaysOfWeek_
      }

-- | The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set.
brliAverageUploadRateLimitInBitsPerSec :: Lens' BandwidthRateLimitInterval (Maybe Natural)
brliAverageUploadRateLimitInBitsPerSec = lens _brliAverageUploadRateLimitInBitsPerSec (\s a -> s {_brliAverageUploadRateLimitInBitsPerSec = a}) . mapping _Nat

-- | The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set.
brliAverageDownloadRateLimitInBitsPerSec :: Lens' BandwidthRateLimitInterval (Maybe Natural)
brliAverageDownloadRateLimitInBitsPerSec = lens _brliAverageDownloadRateLimitInBitsPerSec (\s a -> s {_brliAverageDownloadRateLimitInBitsPerSec = a}) . mapping _Nat

-- | The hour of the day to start the bandwidth rate limit interval.
brliStartHourOfDay :: Lens' BandwidthRateLimitInterval Natural
brliStartHourOfDay = lens _brliStartHourOfDay (\s a -> s {_brliStartHourOfDay = a}) . _Nat

-- | The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ .
brliStartMinuteOfHour :: Lens' BandwidthRateLimitInterval Natural
brliStartMinuteOfHour = lens _brliStartMinuteOfHour (\s a -> s {_brliStartMinuteOfHour = a}) . _Nat

-- | The hour of the day to end the bandwidth rate limit interval.
brliEndHourOfDay :: Lens' BandwidthRateLimitInterval Natural
brliEndHourOfDay = lens _brliEndHourOfDay (\s a -> s {_brliEndHourOfDay = a}) . _Nat

-- | The minute of the hour to end the bandwidth rate limit interval.  /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
brliEndMinuteOfHour :: Lens' BandwidthRateLimitInterval Natural
brliEndMinuteOfHour = lens _brliEndMinuteOfHour (\s a -> s {_brliEndMinuteOfHour = a}) . _Nat

-- | The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
brliDaysOfWeek :: Lens' BandwidthRateLimitInterval (NonEmpty Natural)
brliDaysOfWeek = lens _brliDaysOfWeek (\s a -> s {_brliDaysOfWeek = a}) . _List1

instance FromJSON BandwidthRateLimitInterval where
  parseJSON =
    withObject
      "BandwidthRateLimitInterval"
      ( \x ->
          BandwidthRateLimitInterval'
            <$> (x .:? "AverageUploadRateLimitInBitsPerSec")
            <*> (x .:? "AverageDownloadRateLimitInBitsPerSec")
            <*> (x .: "StartHourOfDay")
            <*> (x .: "StartMinuteOfHour")
            <*> (x .: "EndHourOfDay")
            <*> (x .: "EndMinuteOfHour")
            <*> (x .: "DaysOfWeek")
      )

instance Hashable BandwidthRateLimitInterval

instance NFData BandwidthRateLimitInterval

instance ToJSON BandwidthRateLimitInterval where
  toJSON BandwidthRateLimitInterval' {..} =
    object
      ( catMaybes
          [ ("AverageUploadRateLimitInBitsPerSec" .=)
              <$> _brliAverageUploadRateLimitInBitsPerSec,
            ("AverageDownloadRateLimitInBitsPerSec" .=)
              <$> _brliAverageDownloadRateLimitInBitsPerSec,
            Just ("StartHourOfDay" .= _brliStartHourOfDay),
            Just ("StartMinuteOfHour" .= _brliStartMinuteOfHour),
            Just ("EndHourOfDay" .= _brliEndHourOfDay),
            Just ("EndMinuteOfHour" .= _brliEndMinuteOfHour),
            Just ("DaysOfWeek" .= _brliDaysOfWeek)
          ]
      )
