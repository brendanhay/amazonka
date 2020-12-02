{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricGranularityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricGranularityType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a granularity of a metric.
--
--
--
-- /See:/ 'metricGranularityType' smart constructor.
newtype MetricGranularityType = MetricGranularityType'
  { _mgtGranularity ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricGranularityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mgtGranularity' - The granularity. The only valid value is @1Minute@ .
metricGranularityType ::
  MetricGranularityType
metricGranularityType =
  MetricGranularityType' {_mgtGranularity = Nothing}

-- | The granularity. The only valid value is @1Minute@ .
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\s a -> s {_mgtGranularity = a})

instance FromXML MetricGranularityType where
  parseXML x = MetricGranularityType' <$> (x .@? "Granularity")

instance Hashable MetricGranularityType

instance NFData MetricGranularityType
