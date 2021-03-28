{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Metric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Metric
  ( Metric
    ( Metric'
    , MetricBlendedCost
    , MetricUnblendedCost
    , MetricAmortizedCost
    , MetricNetUnblendedCost
    , MetricNetAmortizedCost
    , MetricUsageQuantity
    , MetricNormalizedUsageAmount
    , fromMetric
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Metric = Metric'{fromMetric :: Core.Text}
                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                   Core.Generic)
                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                     Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                     Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern MetricBlendedCost :: Metric
pattern MetricBlendedCost = Metric' "BLENDED_COST"

pattern MetricUnblendedCost :: Metric
pattern MetricUnblendedCost = Metric' "UNBLENDED_COST"

pattern MetricAmortizedCost :: Metric
pattern MetricAmortizedCost = Metric' "AMORTIZED_COST"

pattern MetricNetUnblendedCost :: Metric
pattern MetricNetUnblendedCost = Metric' "NET_UNBLENDED_COST"

pattern MetricNetAmortizedCost :: Metric
pattern MetricNetAmortizedCost = Metric' "NET_AMORTIZED_COST"

pattern MetricUsageQuantity :: Metric
pattern MetricUsageQuantity = Metric' "USAGE_QUANTITY"

pattern MetricNormalizedUsageAmount :: Metric
pattern MetricNormalizedUsageAmount = Metric' "NORMALIZED_USAGE_AMOUNT"

{-# COMPLETE 
  MetricBlendedCost,

  MetricUnblendedCost,

  MetricAmortizedCost,

  MetricNetUnblendedCost,

  MetricNetAmortizedCost,

  MetricUsageQuantity,

  MetricNormalizedUsageAmount,
  Metric'
  #-}
