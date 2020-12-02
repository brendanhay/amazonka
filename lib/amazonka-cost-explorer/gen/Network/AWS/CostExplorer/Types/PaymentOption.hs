{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.PaymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.PaymentOption where

import Network.AWS.Prelude

data PaymentOption
  = AllUpfront
  | HeavyUtilization
  | LightUtilization
  | MediumUtilization
  | NoUpfront
  | PartialUpfront
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

instance FromText PaymentOption where
  parser =
    takeLowerText >>= \case
      "all_upfront" -> pure AllUpfront
      "heavy_utilization" -> pure HeavyUtilization
      "light_utilization" -> pure LightUtilization
      "medium_utilization" -> pure MediumUtilization
      "no_upfront" -> pure NoUpfront
      "partial_upfront" -> pure PartialUpfront
      e ->
        fromTextError $
          "Failure parsing PaymentOption from value: '" <> e
            <> "'. Accepted values: all_upfront, heavy_utilization, light_utilization, medium_utilization, no_upfront, partial_upfront"

instance ToText PaymentOption where
  toText = \case
    AllUpfront -> "ALL_UPFRONT"
    HeavyUtilization -> "HEAVY_UTILIZATION"
    LightUtilization -> "LIGHT_UTILIZATION"
    MediumUtilization -> "MEDIUM_UTILIZATION"
    NoUpfront -> "NO_UPFRONT"
    PartialUpfront -> "PARTIAL_UPFRONT"

instance Hashable PaymentOption

instance NFData PaymentOption

instance ToByteString PaymentOption

instance ToQuery PaymentOption

instance ToHeader PaymentOption

instance ToJSON PaymentOption where
  toJSON = toJSONText

instance FromJSON PaymentOption where
  parseJSON = parseJSONText "PaymentOption"
