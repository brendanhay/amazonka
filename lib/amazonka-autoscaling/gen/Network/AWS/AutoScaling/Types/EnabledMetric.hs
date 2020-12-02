{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.EnabledMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.EnabledMetric where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an enabled metric.
--
--
--
-- /See:/ 'enabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
  { _emGranularity ::
      !(Maybe Text),
    _emMetric :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnabledMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emGranularity' - The granularity of the metric. The only valid value is @1Minute@ .
--
-- * 'emMetric' - One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@      * @GroupInServiceCapacity@      * @GroupPendingCapacity@      * @GroupStandbyCapacity@      * @GroupTerminatingCapacity@      * @GroupTotalCapacity@
enabledMetric ::
  EnabledMetric
enabledMetric =
  EnabledMetric' {_emGranularity = Nothing, _emMetric = Nothing}

-- | The granularity of the metric. The only valid value is @1Minute@ .
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\s a -> s {_emGranularity = a})

-- | One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@      * @GroupInServiceCapacity@      * @GroupPendingCapacity@      * @GroupStandbyCapacity@      * @GroupTerminatingCapacity@      * @GroupTotalCapacity@
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\s a -> s {_emMetric = a})

instance FromXML EnabledMetric where
  parseXML x =
    EnabledMetric' <$> (x .@? "Granularity") <*> (x .@? "Metric")

instance Hashable EnabledMetric

instance NFData EnabledMetric
