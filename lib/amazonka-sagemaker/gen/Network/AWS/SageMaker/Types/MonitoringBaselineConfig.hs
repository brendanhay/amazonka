{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringBaselineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringBaselineConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource

-- | Configuration for monitoring constraints and monitoring statistics. These baseline resources are compared against the results of the current job from the series of jobs scheduled to collect data periodically.
--
--
--
-- /See:/ 'monitoringBaselineConfig' smart constructor.
data MonitoringBaselineConfig = MonitoringBaselineConfig'
  { _mbcConstraintsResource ::
      !(Maybe MonitoringConstraintsResource),
    _mbcStatisticsResource ::
      !(Maybe MonitoringStatisticsResource)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringBaselineConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbcConstraintsResource' - The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
--
-- * 'mbcStatisticsResource' - The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
monitoringBaselineConfig ::
  MonitoringBaselineConfig
monitoringBaselineConfig =
  MonitoringBaselineConfig'
    { _mbcConstraintsResource = Nothing,
      _mbcStatisticsResource = Nothing
    }

-- | The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
mbcConstraintsResource :: Lens' MonitoringBaselineConfig (Maybe MonitoringConstraintsResource)
mbcConstraintsResource = lens _mbcConstraintsResource (\s a -> s {_mbcConstraintsResource = a})

-- | The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
mbcStatisticsResource :: Lens' MonitoringBaselineConfig (Maybe MonitoringStatisticsResource)
mbcStatisticsResource = lens _mbcStatisticsResource (\s a -> s {_mbcStatisticsResource = a})

instance FromJSON MonitoringBaselineConfig where
  parseJSON =
    withObject
      "MonitoringBaselineConfig"
      ( \x ->
          MonitoringBaselineConfig'
            <$> (x .:? "ConstraintsResource") <*> (x .:? "StatisticsResource")
      )

instance Hashable MonitoringBaselineConfig

instance NFData MonitoringBaselineConfig

instance ToJSON MonitoringBaselineConfig where
  toJSON MonitoringBaselineConfig' {..} =
    object
      ( catMaybes
          [ ("ConstraintsResource" .=) <$> _mbcConstraintsResource,
            ("StatisticsResource" .=) <$> _mbcStatisticsResource
          ]
      )
