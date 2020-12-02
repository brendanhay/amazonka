{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EnhancedMonitoringOutput where

import Network.AWS.Kinesis.Types.MetricsName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output for 'EnableEnhancedMonitoring' and 'DisableEnhancedMonitoring' .
--
--
--
-- /See:/ 'enhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { _emoDesiredShardLevelMetrics ::
      !(Maybe [MetricsName]),
    _emoCurrentShardLevelMetrics ::
      !(Maybe [MetricsName]),
    _emoStreamName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnhancedMonitoringOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emoDesiredShardLevelMetrics' - Represents the list of all the metrics that would be in the enhanced state after the operation.
--
-- * 'emoCurrentShardLevelMetrics' - Represents the current state of the metrics that are in the enhanced state before the operation.
--
-- * 'emoStreamName' - The name of the Kinesis data stream.
enhancedMonitoringOutput ::
  EnhancedMonitoringOutput
enhancedMonitoringOutput =
  EnhancedMonitoringOutput'
    { _emoDesiredShardLevelMetrics = Nothing,
      _emoCurrentShardLevelMetrics = Nothing,
      _emoStreamName = Nothing
    }

-- | Represents the list of all the metrics that would be in the enhanced state after the operation.
emoDesiredShardLevelMetrics :: Lens' EnhancedMonitoringOutput [MetricsName]
emoDesiredShardLevelMetrics = lens _emoDesiredShardLevelMetrics (\s a -> s {_emoDesiredShardLevelMetrics = a}) . _Default . _Coerce

-- | Represents the current state of the metrics that are in the enhanced state before the operation.
emoCurrentShardLevelMetrics :: Lens' EnhancedMonitoringOutput [MetricsName]
emoCurrentShardLevelMetrics = lens _emoCurrentShardLevelMetrics (\s a -> s {_emoCurrentShardLevelMetrics = a}) . _Default . _Coerce

-- | The name of the Kinesis data stream.
emoStreamName :: Lens' EnhancedMonitoringOutput (Maybe Text)
emoStreamName = lens _emoStreamName (\s a -> s {_emoStreamName = a})

instance FromJSON EnhancedMonitoringOutput where
  parseJSON =
    withObject
      "EnhancedMonitoringOutput"
      ( \x ->
          EnhancedMonitoringOutput'
            <$> (x .:? "DesiredShardLevelMetrics" .!= mempty)
            <*> (x .:? "CurrentShardLevelMetrics" .!= mempty)
            <*> (x .:? "StreamName")
      )

instance Hashable EnhancedMonitoringOutput

instance NFData EnhancedMonitoringOutput
