{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetType where

import Network.AWS.Prelude

data FleetType
  = AlwaysOn
  | OnDemand
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
      "always_on" -> pure AlwaysOn
      "on_demand" -> pure OnDemand
      e ->
        fromTextError $
          "Failure parsing FleetType from value: '" <> e
            <> "'. Accepted values: always_on, on_demand"

instance ToText FleetType where
  toText = \case
    AlwaysOn -> "ALWAYS_ON"
    OnDemand -> "ON_DEMAND"

instance Hashable FleetType

instance NFData FleetType

instance ToByteString FleetType

instance ToQuery FleetType

instance ToHeader FleetType

instance ToJSON FleetType where
  toJSON = toJSONText

instance FromJSON FleetType where
  parseJSON = parseJSONText "FleetType"
