{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
  ( MetricAggregationType
    ( MetricAggregationType'
    , MetricAggregationTypeAverage
    , MetricAggregationTypeMinimum
    , MetricAggregationTypeMaximum
    , fromMetricAggregationType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MetricAggregationType = MetricAggregationType'{fromMetricAggregationType
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern MetricAggregationTypeAverage :: MetricAggregationType
pattern MetricAggregationTypeAverage = MetricAggregationType' "Average"

pattern MetricAggregationTypeMinimum :: MetricAggregationType
pattern MetricAggregationTypeMinimum = MetricAggregationType' "Minimum"

pattern MetricAggregationTypeMaximum :: MetricAggregationType
pattern MetricAggregationTypeMaximum = MetricAggregationType' "Maximum"

{-# COMPLETE 
  MetricAggregationTypeAverage,

  MetricAggregationTypeMinimum,

  MetricAggregationTypeMaximum,
  MetricAggregationType'
  #-}
