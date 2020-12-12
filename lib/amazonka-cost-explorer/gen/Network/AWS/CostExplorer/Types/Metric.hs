{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Metric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Metric
  ( Metric
      ( Metric',
        AmortizedCost,
        BlendedCost,
        NetAmortizedCost,
        NetUnblendedCost,
        NormalizedUsageAmount,
        UnblendedCost,
        UsageQuantity
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Metric = Metric' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AmortizedCost :: Metric
pattern AmortizedCost = Metric' "AMORTIZED_COST"

pattern BlendedCost :: Metric
pattern BlendedCost = Metric' "BLENDED_COST"

pattern NetAmortizedCost :: Metric
pattern NetAmortizedCost = Metric' "NET_AMORTIZED_COST"

pattern NetUnblendedCost :: Metric
pattern NetUnblendedCost = Metric' "NET_UNBLENDED_COST"

pattern NormalizedUsageAmount :: Metric
pattern NormalizedUsageAmount = Metric' "NORMALIZED_USAGE_AMOUNT"

pattern UnblendedCost :: Metric
pattern UnblendedCost = Metric' "UNBLENDED_COST"

pattern UsageQuantity :: Metric
pattern UsageQuantity = Metric' "USAGE_QUANTITY"

{-# COMPLETE
  AmortizedCost,
  BlendedCost,
  NetAmortizedCost,
  NetUnblendedCost,
  NormalizedUsageAmount,
  UnblendedCost,
  UsageQuantity,
  Metric'
  #-}
