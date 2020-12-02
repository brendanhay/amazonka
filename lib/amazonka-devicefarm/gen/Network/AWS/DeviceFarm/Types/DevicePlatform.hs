{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePlatform where

import Network.AWS.Prelude

data DevicePlatform
  = Android
  | Ios
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

instance FromText DevicePlatform where
  parser =
    takeLowerText >>= \case
      "android" -> pure Android
      "ios" -> pure Ios
      e ->
        fromTextError $
          "Failure parsing DevicePlatform from value: '" <> e
            <> "'. Accepted values: android, ios"

instance ToText DevicePlatform where
  toText = \case
    Android -> "ANDROID"
    Ios -> "IOS"

instance Hashable DevicePlatform

instance NFData DevicePlatform

instance ToByteString DevicePlatform

instance ToQuery DevicePlatform

instance ToHeader DevicePlatform

instance FromJSON DevicePlatform where
  parseJSON = parseJSONText "DevicePlatform"
