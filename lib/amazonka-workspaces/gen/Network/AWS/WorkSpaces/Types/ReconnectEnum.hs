{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ReconnectEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ReconnectEnum where

import Network.AWS.Prelude

data ReconnectEnum
  = Disabled
  | Enabled
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

instance FromText ReconnectEnum where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing ReconnectEnum from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ReconnectEnum where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable ReconnectEnum

instance NFData ReconnectEnum

instance ToByteString ReconnectEnum

instance ToQuery ReconnectEnum

instance ToHeader ReconnectEnum

instance ToJSON ReconnectEnum where
  toJSON = toJSONText

instance FromJSON ReconnectEnum where
  parseJSON = parseJSONText "ReconnectEnum"
