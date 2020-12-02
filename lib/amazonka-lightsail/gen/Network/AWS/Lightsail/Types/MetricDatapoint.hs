{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricDatapoint where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.MetricUnit
import Network.AWS.Prelude

-- | Describes the metric data point.
--
--
--
-- /See:/ 'metricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
  { _mdSampleCount ::
      !(Maybe Double),
    _mdMaximum :: !(Maybe Double),
    _mdAverage :: !(Maybe Double),
    _mdMinimum :: !(Maybe Double),
    _mdSum :: !(Maybe Double),
    _mdTimestamp :: !(Maybe POSIX),
    _mdUnit :: !(Maybe MetricUnit)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDatapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdSampleCount' - The sample count.
--
-- * 'mdMaximum' - The maximum.
--
-- * 'mdAverage' - The average.
--
-- * 'mdMinimum' - The minimum.
--
-- * 'mdSum' - The sum.
--
-- * 'mdTimestamp' - The timestamp (e.g., @1479816991.349@ ).
--
-- * 'mdUnit' - The unit.
metricDatapoint ::
  MetricDatapoint
metricDatapoint =
  MetricDatapoint'
    { _mdSampleCount = Nothing,
      _mdMaximum = Nothing,
      _mdAverage = Nothing,
      _mdMinimum = Nothing,
      _mdSum = Nothing,
      _mdTimestamp = Nothing,
      _mdUnit = Nothing
    }

-- | The sample count.
mdSampleCount :: Lens' MetricDatapoint (Maybe Double)
mdSampleCount = lens _mdSampleCount (\s a -> s {_mdSampleCount = a})

-- | The maximum.
mdMaximum :: Lens' MetricDatapoint (Maybe Double)
mdMaximum = lens _mdMaximum (\s a -> s {_mdMaximum = a})

-- | The average.
mdAverage :: Lens' MetricDatapoint (Maybe Double)
mdAverage = lens _mdAverage (\s a -> s {_mdAverage = a})

-- | The minimum.
mdMinimum :: Lens' MetricDatapoint (Maybe Double)
mdMinimum = lens _mdMinimum (\s a -> s {_mdMinimum = a})

-- | The sum.
mdSum :: Lens' MetricDatapoint (Maybe Double)
mdSum = lens _mdSum (\s a -> s {_mdSum = a})

-- | The timestamp (e.g., @1479816991.349@ ).
mdTimestamp :: Lens' MetricDatapoint (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\s a -> s {_mdTimestamp = a}) . mapping _Time

-- | The unit.
mdUnit :: Lens' MetricDatapoint (Maybe MetricUnit)
mdUnit = lens _mdUnit (\s a -> s {_mdUnit = a})

instance FromJSON MetricDatapoint where
  parseJSON =
    withObject
      "MetricDatapoint"
      ( \x ->
          MetricDatapoint'
            <$> (x .:? "sampleCount")
            <*> (x .:? "maximum")
            <*> (x .:? "average")
            <*> (x .:? "minimum")
            <*> (x .:? "sum")
            <*> (x .:? "timestamp")
            <*> (x .:? "unit")
      )

instance Hashable MetricDatapoint

instance NFData MetricDatapoint
