{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.Status
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.Status where

import Network.AWS.Prelude

data Status
  = Disabled
  | Disabling
  | Enabled
  | Enabling
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

instance FromText Status where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "disabling" -> pure Disabling
      "enabled" -> pure Enabled
      "enabling" -> pure Enabling
      e ->
        fromTextError $
          "Failure parsing Status from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText Status where
  toText = \case
    Disabled -> "DISABLED"
    Disabling -> "DISABLING"
    Enabled -> "ENABLED"
    Enabling -> "ENABLING"

instance Hashable Status

instance NFData Status

instance ToByteString Status

instance ToQuery Status

instance ToHeader Status

instance ToJSON Status where
  toJSON = toJSONText

instance FromJSON Status where
  parseJSON = parseJSONText "Status"
