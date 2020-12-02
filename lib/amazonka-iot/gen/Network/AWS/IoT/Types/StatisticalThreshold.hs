{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StatisticalThreshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StatisticalThreshold where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
--
--
--
-- /See:/ 'statisticalThreshold' smart constructor.
newtype StatisticalThreshold = StatisticalThreshold'
  { _stStatistic ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatisticalThreshold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stStatistic' - The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
statisticalThreshold ::
  StatisticalThreshold
statisticalThreshold =
  StatisticalThreshold' {_stStatistic = Nothing}

-- | The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
stStatistic :: Lens' StatisticalThreshold (Maybe Text)
stStatistic = lens _stStatistic (\s a -> s {_stStatistic = a})

instance FromJSON StatisticalThreshold where
  parseJSON =
    withObject
      "StatisticalThreshold"
      (\x -> StatisticalThreshold' <$> (x .:? "statistic"))

instance Hashable StatisticalThreshold

instance NFData StatisticalThreshold

instance ToJSON StatisticalThreshold where
  toJSON StatisticalThreshold' {..} =
    object (catMaybes [("statistic" .=) <$> _stStatistic])
