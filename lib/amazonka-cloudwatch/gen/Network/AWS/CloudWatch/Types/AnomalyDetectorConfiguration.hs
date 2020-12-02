{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration where

import Network.AWS.CloudWatch.Types.Range
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model and the time zone to use for the metric.
--
--
--
-- /See:/ 'anomalyDetectorConfiguration' smart constructor.
data AnomalyDetectorConfiguration = AnomalyDetectorConfiguration'
  { _adcMetricTimezone ::
      !(Maybe Text),
    _adcExcludedTimeRanges ::
      !(Maybe [Range])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalyDetectorConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adcMetricTimezone' - The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes. To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
--
-- * 'adcExcludedTimeRanges' - An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
anomalyDetectorConfiguration ::
  AnomalyDetectorConfiguration
anomalyDetectorConfiguration =
  AnomalyDetectorConfiguration'
    { _adcMetricTimezone = Nothing,
      _adcExcludedTimeRanges = Nothing
    }

-- | The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes. To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
adcMetricTimezone :: Lens' AnomalyDetectorConfiguration (Maybe Text)
adcMetricTimezone = lens _adcMetricTimezone (\s a -> s {_adcMetricTimezone = a})

-- | An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
adcExcludedTimeRanges :: Lens' AnomalyDetectorConfiguration [Range]
adcExcludedTimeRanges = lens _adcExcludedTimeRanges (\s a -> s {_adcExcludedTimeRanges = a}) . _Default . _Coerce

instance FromXML AnomalyDetectorConfiguration where
  parseXML x =
    AnomalyDetectorConfiguration'
      <$> (x .@? "MetricTimezone")
      <*> ( x .@? "ExcludedTimeRanges" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable AnomalyDetectorConfiguration

instance NFData AnomalyDetectorConfiguration

instance ToQuery AnomalyDetectorConfiguration where
  toQuery AnomalyDetectorConfiguration' {..} =
    mconcat
      [ "MetricTimezone" =: _adcMetricTimezone,
        "ExcludedTimeRanges"
          =: toQuery (toQueryList "member" <$> _adcExcludedTimeRanges)
      ]
