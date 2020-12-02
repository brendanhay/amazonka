{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PricingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PricingPlan where

import Network.AWS.Prelude

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
data PricingPlan
  = OnDemand
  | Reserved
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

instance FromText PricingPlan where
  parser =
    takeLowerText >>= \case
      "on_demand" -> pure OnDemand
      "reserved" -> pure Reserved
      e ->
        fromTextError $
          "Failure parsing PricingPlan from value: '" <> e
            <> "'. Accepted values: on_demand, reserved"

instance ToText PricingPlan where
  toText = \case
    OnDemand -> "ON_DEMAND"
    Reserved -> "RESERVED"

instance Hashable PricingPlan

instance NFData PricingPlan

instance ToByteString PricingPlan

instance ToQuery PricingPlan

instance ToHeader PricingPlan

instance ToJSON PricingPlan where
  toJSON = toJSONText

instance FromJSON PricingPlan where
  parseJSON = parseJSONText "PricingPlan"
