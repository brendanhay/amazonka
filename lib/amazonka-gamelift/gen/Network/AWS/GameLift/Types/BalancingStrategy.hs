{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BalancingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.BalancingStrategy where

import Network.AWS.Prelude

data BalancingStrategy
  = OnDemandOnly
  | SpotOnly
  | SpotPreferred
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

instance FromText BalancingStrategy where
  parser =
    takeLowerText >>= \case
      "on_demand_only" -> pure OnDemandOnly
      "spot_only" -> pure SpotOnly
      "spot_preferred" -> pure SpotPreferred
      e ->
        fromTextError $
          "Failure parsing BalancingStrategy from value: '" <> e
            <> "'. Accepted values: on_demand_only, spot_only, spot_preferred"

instance ToText BalancingStrategy where
  toText = \case
    OnDemandOnly -> "ON_DEMAND_ONLY"
    SpotOnly -> "SPOT_ONLY"
    SpotPreferred -> "SPOT_PREFERRED"

instance Hashable BalancingStrategy

instance NFData BalancingStrategy

instance ToByteString BalancingStrategy

instance ToQuery BalancingStrategy

instance ToHeader BalancingStrategy

instance ToJSON BalancingStrategy where
  toJSON = toJSONText

instance FromJSON BalancingStrategy where
  parseJSON = parseJSONText "BalancingStrategy"
