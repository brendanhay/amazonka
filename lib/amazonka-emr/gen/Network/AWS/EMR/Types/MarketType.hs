{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.MarketType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.MarketType where

import Network.AWS.Prelude

data MarketType
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

instance FromText MarketType where
  parser =
    takeLowerText >>= \case
      "on_demand" -> pure OnDemand
      "spot" -> pure Spot
      e ->
        fromTextError $
          "Failure parsing MarketType from value: '" <> e
            <> "'. Accepted values: on_demand, spot"

instance ToText MarketType where
  toText = \case
    OnDemand -> "ON_DEMAND"
    Spot -> "SPOT"

instance Hashable MarketType

instance NFData MarketType

instance ToByteString MarketType

instance ToQuery MarketType

instance ToHeader MarketType

instance ToJSON MarketType where
  toJSON = toJSONText

instance FromJSON MarketType where
  parseJSON = parseJSONText "MarketType"
