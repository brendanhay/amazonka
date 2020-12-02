{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingIndexingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingMode where

import Network.AWS.Prelude

data ThingIndexingMode
  = TIMOff
  | TIMRegistry
  | TIMRegistryAndShadow
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

instance FromText ThingIndexingMode where
  parser =
    takeLowerText >>= \case
      "off" -> pure TIMOff
      "registry" -> pure TIMRegistry
      "registry_and_shadow" -> pure TIMRegistryAndShadow
      e ->
        fromTextError $
          "Failure parsing ThingIndexingMode from value: '" <> e
            <> "'. Accepted values: off, registry, registry_and_shadow"

instance ToText ThingIndexingMode where
  toText = \case
    TIMOff -> "OFF"
    TIMRegistry -> "REGISTRY"
    TIMRegistryAndShadow -> "REGISTRY_AND_SHADOW"

instance Hashable ThingIndexingMode

instance NFData ThingIndexingMode

instance ToByteString ThingIndexingMode

instance ToQuery ThingIndexingMode

instance ToHeader ThingIndexingMode

instance ToJSON ThingIndexingMode where
  toJSON = toJSONText

instance FromJSON ThingIndexingMode where
  parseJSON = parseJSONText "ThingIndexingMode"
