{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Feature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Feature where

import Network.AWS.Prelude

data Feature
  = All
  | Bluetooth
  | Lists
  | NetworkProfile
  | Notifications
  | Settings
  | Skills
  | Volume
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

instance FromText Feature where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "bluetooth" -> pure Bluetooth
      "lists" -> pure Lists
      "network_profile" -> pure NetworkProfile
      "notifications" -> pure Notifications
      "settings" -> pure Settings
      "skills" -> pure Skills
      "volume" -> pure Volume
      e ->
        fromTextError $
          "Failure parsing Feature from value: '" <> e
            <> "'. Accepted values: all, bluetooth, lists, network_profile, notifications, settings, skills, volume"

instance ToText Feature where
  toText = \case
    All -> "ALL"
    Bluetooth -> "BLUETOOTH"
    Lists -> "LISTS"
    NetworkProfile -> "NETWORK_PROFILE"
    Notifications -> "NOTIFICATIONS"
    Settings -> "SETTINGS"
    Skills -> "SKILLS"
    Volume -> "VOLUME"

instance Hashable Feature

instance NFData Feature

instance ToByteString Feature

instance ToQuery Feature

instance ToHeader Feature

instance ToJSON Feature where
  toJSON = toJSONText

instance FromJSON Feature where
  parseJSON = parseJSONText "Feature"
