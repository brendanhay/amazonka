{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTargetType where

import Network.AWS.Prelude

data LogTargetType
  = LTTDefault
  | LTTThingGroup
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

instance FromText LogTargetType where
  parser =
    takeLowerText >>= \case
      "default" -> pure LTTDefault
      "thing_group" -> pure LTTThingGroup
      e ->
        fromTextError $
          "Failure parsing LogTargetType from value: '" <> e
            <> "'. Accepted values: default, thing_group"

instance ToText LogTargetType where
  toText = \case
    LTTDefault -> "DEFAULT"
    LTTThingGroup -> "THING_GROUP"

instance Hashable LogTargetType

instance NFData LogTargetType

instance ToByteString LogTargetType

instance ToQuery LogTargetType

instance ToHeader LogTargetType

instance ToJSON LogTargetType where
  toJSON = toJSONText

instance FromJSON LogTargetType where
  parseJSON = parseJSONText "LogTargetType"
