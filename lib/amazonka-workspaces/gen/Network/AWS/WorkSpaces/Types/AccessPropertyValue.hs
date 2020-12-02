{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AccessPropertyValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AccessPropertyValue where

import Network.AWS.Prelude

data AccessPropertyValue
  = Allow
  | Deny
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

instance FromText AccessPropertyValue where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing AccessPropertyValue from value: '" <> e
            <> "'. Accepted values: allow, deny"

instance ToText AccessPropertyValue where
  toText = \case
    Allow -> "ALLOW"
    Deny -> "DENY"

instance Hashable AccessPropertyValue

instance NFData AccessPropertyValue

instance ToByteString AccessPropertyValue

instance ToQuery AccessPropertyValue

instance ToHeader AccessPropertyValue

instance ToJSON AccessPropertyValue where
  toJSON = toJSONText

instance FromJSON AccessPropertyValue where
  parseJSON = parseJSONText "AccessPropertyValue"
