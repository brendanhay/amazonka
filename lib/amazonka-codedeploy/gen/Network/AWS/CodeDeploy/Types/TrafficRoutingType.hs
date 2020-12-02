{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoutingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoutingType where

import Network.AWS.Prelude

data TrafficRoutingType
  = AllAtOnce
  | TimeBasedCanary
  | TimeBasedLinear
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

instance FromText TrafficRoutingType where
  parser =
    takeLowerText >>= \case
      "allatonce" -> pure AllAtOnce
      "timebasedcanary" -> pure TimeBasedCanary
      "timebasedlinear" -> pure TimeBasedLinear
      e ->
        fromTextError $
          "Failure parsing TrafficRoutingType from value: '" <> e
            <> "'. Accepted values: allatonce, timebasedcanary, timebasedlinear"

instance ToText TrafficRoutingType where
  toText = \case
    AllAtOnce -> "AllAtOnce"
    TimeBasedCanary -> "TimeBasedCanary"
    TimeBasedLinear -> "TimeBasedLinear"

instance Hashable TrafficRoutingType

instance NFData TrafficRoutingType

instance ToByteString TrafficRoutingType

instance ToQuery TrafficRoutingType

instance ToHeader TrafficRoutingType

instance ToJSON TrafficRoutingType where
  toJSON = toJSONText

instance FromJSON TrafficRoutingType where
  parseJSON = parseJSONText "TrafficRoutingType"
