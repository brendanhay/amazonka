{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricCollectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricCollectionType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a metric.
--
--
--
-- /See:/ 'metricCollectionType' smart constructor.
newtype MetricCollectionType = MetricCollectionType'
  { _mctMetric ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricCollectionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mctMetric' - One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@      * @GroupInServiceCapacity@      * @GroupPendingCapacity@      * @GroupStandbyCapacity@      * @GroupTerminatingCapacity@      * @GroupTotalCapacity@
metricCollectionType ::
  MetricCollectionType
metricCollectionType = MetricCollectionType' {_mctMetric = Nothing}

-- | One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@      * @GroupInServiceCapacity@      * @GroupPendingCapacity@      * @GroupStandbyCapacity@      * @GroupTerminatingCapacity@      * @GroupTotalCapacity@
mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\s a -> s {_mctMetric = a})

instance FromXML MetricCollectionType where
  parseXML x = MetricCollectionType' <$> (x .@? "Metric")

instance Hashable MetricCollectionType

instance NFData MetricCollectionType
