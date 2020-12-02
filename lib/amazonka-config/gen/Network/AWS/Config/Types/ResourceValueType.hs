{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceValueType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceValueType where

import Network.AWS.Prelude

data ResourceValueType = ResourceId
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

instance FromText ResourceValueType where
  parser =
    takeLowerText >>= \case
      "resource_id" -> pure ResourceId
      e ->
        fromTextError $
          "Failure parsing ResourceValueType from value: '" <> e
            <> "'. Accepted values: resource_id"

instance ToText ResourceValueType where
  toText = \case
    ResourceId -> "RESOURCE_ID"

instance Hashable ResourceValueType

instance NFData ResourceValueType

instance ToByteString ResourceValueType

instance ToQuery ResourceValueType

instance ToHeader ResourceValueType

instance ToJSON ResourceValueType where
  toJSON = toJSONText

instance FromJSON ResourceValueType where
  parseJSON = parseJSONText "ResourceValueType"
