{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Metric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Metric where

import Network.AWS.Prelude

data Metric
  = AmortizedCost
  | BlendedCost
  | NetAmortizedCost
  | NetUnblendedCost
  | NormalizedUsageAmount
  | UnblendedCost
  | UsageQuantity
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText Metric where
  parser =
    takeLowerText >>= \case
      "amortized_cost" -> pure AmortizedCost
      "blended_cost" -> pure BlendedCost
      "net_amortized_cost" -> pure NetAmortizedCost
      "net_unblended_cost" -> pure NetUnblendedCost
      "normalized_usage_amount" -> pure NormalizedUsageAmount
      "unblended_cost" -> pure UnblendedCost
      "usage_quantity" -> pure UsageQuantity
      e ->
        fromTextError $
          "Failure parsing Metric from value: '" <> e
            <> "'. Accepted values: amortized_cost, blended_cost, net_amortized_cost, net_unblended_cost, normalized_usage_amount, unblended_cost, usage_quantity"

instance ToText Metric where
  toText = \case
    AmortizedCost -> "AMORTIZED_COST"
    BlendedCost -> "BLENDED_COST"
    NetAmortizedCost -> "NET_AMORTIZED_COST"
    NetUnblendedCost -> "NET_UNBLENDED_COST"
    NormalizedUsageAmount -> "NORMALIZED_USAGE_AMOUNT"
    UnblendedCost -> "UNBLENDED_COST"
    UsageQuantity -> "USAGE_QUANTITY"

instance Hashable Metric

instance NFData Metric

instance ToByteString Metric

instance ToQuery Metric

instance ToHeader Metric

instance ToJSON Metric where
  toJSON = toJSONText
