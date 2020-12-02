{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TimeSeriesServiceStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.ForecastStatistics
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.ServiceStatistics

-- | A list of TimeSeriesStatistic structures.
--
--
--
-- /See:/ 'timeSeriesServiceStatistics' smart constructor.
data TimeSeriesServiceStatistics = TimeSeriesServiceStatistics'
  { _tsssServiceSummaryStatistics ::
      !(Maybe ServiceStatistics),
    _tsssResponseTimeHistogram ::
      !(Maybe [HistogramEntry]),
    _tsssEdgeSummaryStatistics ::
      !(Maybe EdgeStatistics),
    _tsssServiceForecastStatistics ::
      !(Maybe ForecastStatistics),
    _tsssTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeSeriesServiceStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsssServiceSummaryStatistics' - Undocumented member.
--
-- * 'tsssResponseTimeHistogram' - The response time histogram for the selected entities.
--
-- * 'tsssEdgeSummaryStatistics' - Undocumented member.
--
-- * 'tsssServiceForecastStatistics' - The forecasted high and low fault count values.
--
-- * 'tsssTimestamp' - Timestamp of the window for which statistics are aggregated.
timeSeriesServiceStatistics ::
  TimeSeriesServiceStatistics
timeSeriesServiceStatistics =
  TimeSeriesServiceStatistics'
    { _tsssServiceSummaryStatistics =
        Nothing,
      _tsssResponseTimeHistogram = Nothing,
      _tsssEdgeSummaryStatistics = Nothing,
      _tsssServiceForecastStatistics = Nothing,
      _tsssTimestamp = Nothing
    }

-- | Undocumented member.
tsssServiceSummaryStatistics :: Lens' TimeSeriesServiceStatistics (Maybe ServiceStatistics)
tsssServiceSummaryStatistics = lens _tsssServiceSummaryStatistics (\s a -> s {_tsssServiceSummaryStatistics = a})

-- | The response time histogram for the selected entities.
tsssResponseTimeHistogram :: Lens' TimeSeriesServiceStatistics [HistogramEntry]
tsssResponseTimeHistogram = lens _tsssResponseTimeHistogram (\s a -> s {_tsssResponseTimeHistogram = a}) . _Default . _Coerce

-- | Undocumented member.
tsssEdgeSummaryStatistics :: Lens' TimeSeriesServiceStatistics (Maybe EdgeStatistics)
tsssEdgeSummaryStatistics = lens _tsssEdgeSummaryStatistics (\s a -> s {_tsssEdgeSummaryStatistics = a})

-- | The forecasted high and low fault count values.
tsssServiceForecastStatistics :: Lens' TimeSeriesServiceStatistics (Maybe ForecastStatistics)
tsssServiceForecastStatistics = lens _tsssServiceForecastStatistics (\s a -> s {_tsssServiceForecastStatistics = a})

-- | Timestamp of the window for which statistics are aggregated.
tsssTimestamp :: Lens' TimeSeriesServiceStatistics (Maybe UTCTime)
tsssTimestamp = lens _tsssTimestamp (\s a -> s {_tsssTimestamp = a}) . mapping _Time

instance FromJSON TimeSeriesServiceStatistics where
  parseJSON =
    withObject
      "TimeSeriesServiceStatistics"
      ( \x ->
          TimeSeriesServiceStatistics'
            <$> (x .:? "ServiceSummaryStatistics")
            <*> (x .:? "ResponseTimeHistogram" .!= mempty)
            <*> (x .:? "EdgeSummaryStatistics")
            <*> (x .:? "ServiceForecastStatistics")
            <*> (x .:? "Timestamp")
      )

instance Hashable TimeSeriesServiceStatistics

instance NFData TimeSeriesServiceStatistics
