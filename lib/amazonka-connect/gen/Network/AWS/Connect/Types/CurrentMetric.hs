{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetric where

import Network.AWS.Connect.Types.CurrentMetricName
import Network.AWS.Connect.Types.Unit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a real-time metric. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
--
--
-- /See:/ 'currentMetric' smart constructor.
data CurrentMetric = CurrentMetric'
  { _cmName ::
      !(Maybe CurrentMetricName),
    _cmUnit :: !(Maybe Unit)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CurrentMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmName' - The name of the metric.
--
-- * 'cmUnit' - The unit for the metric.
currentMetric ::
  CurrentMetric
currentMetric =
  CurrentMetric' {_cmName = Nothing, _cmUnit = Nothing}

-- | The name of the metric.
cmName :: Lens' CurrentMetric (Maybe CurrentMetricName)
cmName = lens _cmName (\s a -> s {_cmName = a})

-- | The unit for the metric.
cmUnit :: Lens' CurrentMetric (Maybe Unit)
cmUnit = lens _cmUnit (\s a -> s {_cmUnit = a})

instance FromJSON CurrentMetric where
  parseJSON =
    withObject
      "CurrentMetric"
      (\x -> CurrentMetric' <$> (x .:? "Name") <*> (x .:? "Unit"))

instance Hashable CurrentMetric

instance NFData CurrentMetric

instance ToJSON CurrentMetric where
  toJSON CurrentMetric' {..} =
    object
      (catMaybes [("Name" .=) <$> _cmName, ("Unit" .=) <$> _cmUnit])
