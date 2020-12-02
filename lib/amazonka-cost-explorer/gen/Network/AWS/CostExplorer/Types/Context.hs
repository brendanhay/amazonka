{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Context
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Context where

import Network.AWS.Prelude

data Context
  = CostAndUsage
  | Reservations
  | SavingsPlans
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

instance FromText Context where
  parser =
    takeLowerText >>= \case
      "cost_and_usage" -> pure CostAndUsage
      "reservations" -> pure Reservations
      "savings_plans" -> pure SavingsPlans
      e ->
        fromTextError $
          "Failure parsing Context from value: '" <> e
            <> "'. Accepted values: cost_and_usage, reservations, savings_plans"

instance ToText Context where
  toText = \case
    CostAndUsage -> "COST_AND_USAGE"
    Reservations -> "RESERVATIONS"
    SavingsPlans -> "SAVINGS_PLANS"

instance Hashable Context

instance NFData Context

instance ToByteString Context

instance ToQuery Context

instance ToHeader Context

instance ToJSON Context where
  toJSON = toJSONText
