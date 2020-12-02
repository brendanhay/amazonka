{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricToRetain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricToRetain where

import Network.AWS.IoT.Types.MetricDimension
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metric you want to retain. Dimensions are optional.
--
--
--
-- /See:/ 'metricToRetain' smart constructor.
data MetricToRetain = MetricToRetain'
  { _mtrMetricDimension ::
      !(Maybe MetricDimension),
    _mtrMetric :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricToRetain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtrMetricDimension' - The dimension of a metric.
--
-- * 'mtrMetric' - What is measured by the behavior.
metricToRetain ::
  -- | 'mtrMetric'
  Text ->
  MetricToRetain
metricToRetain pMetric_ =
  MetricToRetain'
    { _mtrMetricDimension = Nothing,
      _mtrMetric = pMetric_
    }

-- | The dimension of a metric.
mtrMetricDimension :: Lens' MetricToRetain (Maybe MetricDimension)
mtrMetricDimension = lens _mtrMetricDimension (\s a -> s {_mtrMetricDimension = a})

-- | What is measured by the behavior.
mtrMetric :: Lens' MetricToRetain Text
mtrMetric = lens _mtrMetric (\s a -> s {_mtrMetric = a})

instance FromJSON MetricToRetain where
  parseJSON =
    withObject
      "MetricToRetain"
      ( \x ->
          MetricToRetain' <$> (x .:? "metricDimension") <*> (x .: "metric")
      )

instance Hashable MetricToRetain

instance NFData MetricToRetain

instance ToJSON MetricToRetain where
  toJSON MetricToRetain' {..} =
    object
      ( catMaybes
          [ ("metricDimension" .=) <$> _mtrMetricDimension,
            Just ("metric" .= _mtrMetric)
          ]
      )
