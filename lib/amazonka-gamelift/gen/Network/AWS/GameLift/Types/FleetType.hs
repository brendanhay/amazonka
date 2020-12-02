{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetType where

import Network.AWS.Prelude

data FleetType
  = OnDemand
  | Spot
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

instance FromText FleetType where
  parser =
    takeLowerText >>= \case
      "on_demand" -> pure OnDemand
      "spot" -> pure Spot
      e ->
        fromTextError $
          "Failure parsing FleetType from value: '" <> e
            <> "'. Accepted values: on_demand, spot"

instance ToText FleetType where
  toText = \case
    OnDemand -> "ON_DEMAND"
    Spot -> "SPOT"

instance Hashable FleetType

instance NFData FleetType

instance ToByteString FleetType

instance ToQuery FleetType

instance ToHeader FleetType

instance ToJSON FleetType where
  toJSON = toJSONText

instance FromJSON FleetType where
  parseJSON = parseJSONText "FleetType"
