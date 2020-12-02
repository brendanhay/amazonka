{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MetricValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MetricValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The aggregated value for a metric.
--
--
--
-- /See:/ 'metricValue' smart constructor.
data MetricValue = MetricValue'
  { _mvAmount :: !(Maybe Text),
    _mvUnit :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvAmount' - The actual number that represents the metric.
--
-- * 'mvUnit' - The unit that the metric is given in.
metricValue ::
  MetricValue
metricValue = MetricValue' {_mvAmount = Nothing, _mvUnit = Nothing}

-- | The actual number that represents the metric.
mvAmount :: Lens' MetricValue (Maybe Text)
mvAmount = lens _mvAmount (\s a -> s {_mvAmount = a})

-- | The unit that the metric is given in.
mvUnit :: Lens' MetricValue (Maybe Text)
mvUnit = lens _mvUnit (\s a -> s {_mvUnit = a})

instance FromJSON MetricValue where
  parseJSON =
    withObject
      "MetricValue"
      (\x -> MetricValue' <$> (x .:? "Amount") <*> (x .:? "Unit"))

instance Hashable MetricValue

instance NFData MetricValue
