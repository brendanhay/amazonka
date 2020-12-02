{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetric where

import Network.AWS.Connect.Types.HistoricalMetricName
import Network.AWS.Connect.Types.Statistic
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.Unit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a historical metric. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
--
--
-- /See:/ 'historicalMetric' smart constructor.
data HistoricalMetric = HistoricalMetric'
  { _hmName ::
      !(Maybe HistoricalMetricName),
    _hmThreshold :: !(Maybe Threshold),
    _hmUnit :: !(Maybe Unit),
    _hmStatistic :: !(Maybe Statistic)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistoricalMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmName' - The name of the metric.
--
-- * 'hmThreshold' - The threshold for the metric, used with service level metrics.
--
-- * 'hmUnit' - The unit for the metric.
--
-- * 'hmStatistic' - The statistic for the metric.
historicalMetric ::
  HistoricalMetric
historicalMetric =
  HistoricalMetric'
    { _hmName = Nothing,
      _hmThreshold = Nothing,
      _hmUnit = Nothing,
      _hmStatistic = Nothing
    }

-- | The name of the metric.
hmName :: Lens' HistoricalMetric (Maybe HistoricalMetricName)
hmName = lens _hmName (\s a -> s {_hmName = a})

-- | The threshold for the metric, used with service level metrics.
hmThreshold :: Lens' HistoricalMetric (Maybe Threshold)
hmThreshold = lens _hmThreshold (\s a -> s {_hmThreshold = a})

-- | The unit for the metric.
hmUnit :: Lens' HistoricalMetric (Maybe Unit)
hmUnit = lens _hmUnit (\s a -> s {_hmUnit = a})

-- | The statistic for the metric.
hmStatistic :: Lens' HistoricalMetric (Maybe Statistic)
hmStatistic = lens _hmStatistic (\s a -> s {_hmStatistic = a})

instance FromJSON HistoricalMetric where
  parseJSON =
    withObject
      "HistoricalMetric"
      ( \x ->
          HistoricalMetric'
            <$> (x .:? "Name")
            <*> (x .:? "Threshold")
            <*> (x .:? "Unit")
            <*> (x .:? "Statistic")
      )

instance Hashable HistoricalMetric

instance NFData HistoricalMetric

instance ToJSON HistoricalMetric where
  toJSON HistoricalMetric' {..} =
    object
      ( catMaybes
          [ ("Name" .=) <$> _hmName,
            ("Threshold" .=) <$> _hmThreshold,
            ("Unit" .=) <$> _hmUnit,
            ("Statistic" .=) <$> _hmStatistic
          ]
      )
