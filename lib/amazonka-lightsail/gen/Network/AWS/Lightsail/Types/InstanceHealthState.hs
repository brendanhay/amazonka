{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthState where

import Network.AWS.Prelude

data InstanceHealthState
  = Draining
  | Healthy
  | Initial
  | Unavailable
  | Unhealthy
  | Unused
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

instance FromText InstanceHealthState where
  parser =
    takeLowerText >>= \case
      "draining" -> pure Draining
      "healthy" -> pure Healthy
      "initial" -> pure Initial
      "unavailable" -> pure Unavailable
      "unhealthy" -> pure Unhealthy
      "unused" -> pure Unused
      e ->
        fromTextError $
          "Failure parsing InstanceHealthState from value: '" <> e
            <> "'. Accepted values: draining, healthy, initial, unavailable, unhealthy, unused"

instance ToText InstanceHealthState where
  toText = \case
    Draining -> "draining"
    Healthy -> "healthy"
    Initial -> "initial"
    Unavailable -> "unavailable"
    Unhealthy -> "unhealthy"
    Unused -> "unused"

instance Hashable InstanceHealthState

instance NFData InstanceHealthState

instance ToByteString InstanceHealthState

instance ToQuery InstanceHealthState

instance ToHeader InstanceHealthState

instance FromJSON InstanceHealthState where
  parseJSON = parseJSONText "InstanceHealthState"
