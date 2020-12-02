{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Platform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.Platform where

import Network.AWS.Prelude

-- | Developer desktop or target mobile app or website platform.
data Platform
  = Android
  | Javascript
  | Linux
  | OSx
  | Objc
  | Swift
  | Windows
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

instance FromText Platform where
  parser =
    takeLowerText >>= \case
      "android" -> pure Android
      "javascript" -> pure Javascript
      "linux" -> pure Linux
      "osx" -> pure OSx
      "objc" -> pure Objc
      "swift" -> pure Swift
      "windows" -> pure Windows
      e ->
        fromTextError $
          "Failure parsing Platform from value: '" <> e
            <> "'. Accepted values: android, javascript, linux, osx, objc, swift, windows"

instance ToText Platform where
  toText = \case
    Android -> "ANDROID"
    Javascript -> "JAVASCRIPT"
    Linux -> "LINUX"
    OSx -> "OSX"
    Objc -> "OBJC"
    Swift -> "SWIFT"
    Windows -> "WINDOWS"

instance Hashable Platform

instance NFData Platform

instance ToByteString Platform

instance ToQuery Platform

instance ToHeader Platform

instance ToJSON Platform where
  toJSON = toJSONText

instance FromJSON Platform where
  parseJSON = parseJSONText "Platform"
