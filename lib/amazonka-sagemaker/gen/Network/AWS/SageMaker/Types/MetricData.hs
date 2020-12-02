{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The name, value, and date and time of a metric that was emitted to Amazon CloudWatch.
--
--
--
-- /See:/ 'metricData' smart constructor.
data MetricData = MetricData'
  { _mdMetricName :: !(Maybe Text),
    _mdValue :: !(Maybe Double),
    _mdTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdMetricName' - The name of the metric.
--
-- * 'mdValue' - The value of the metric.
--
-- * 'mdTimestamp' - The date and time that the algorithm emitted the metric.
metricData ::
  MetricData
metricData =
  MetricData'
    { _mdMetricName = Nothing,
      _mdValue = Nothing,
      _mdTimestamp = Nothing
    }

-- | The name of the metric.
mdMetricName :: Lens' MetricData (Maybe Text)
mdMetricName = lens _mdMetricName (\s a -> s {_mdMetricName = a})

-- | The value of the metric.
mdValue :: Lens' MetricData (Maybe Double)
mdValue = lens _mdValue (\s a -> s {_mdValue = a})

-- | The date and time that the algorithm emitted the metric.
mdTimestamp :: Lens' MetricData (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\s a -> s {_mdTimestamp = a}) . mapping _Time

instance FromJSON MetricData where
  parseJSON =
    withObject
      "MetricData"
      ( \x ->
          MetricData'
            <$> (x .:? "MetricName") <*> (x .:? "Value") <*> (x .:? "Timestamp")
      )

instance Hashable MetricData

instance NFData MetricData
