{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.Sum where

import Network.AWS.Prelude

-- | Developer desktop or target mobile app or website platform.
--
--
data Platform
  = Android
  | Javascript
  | Linux
  | OSx
  | Objc
  | Swift
  | Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Platform where
    parser = takeLowerText >>= \case
        "android" -> pure Android
        "javascript" -> pure Javascript
        "linux" -> pure Linux
        "osx" -> pure OSx
        "objc" -> pure Objc
        "swift" -> pure Swift
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing Platform from value: '" <> e
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

instance Hashable     Platform
instance NFData       Platform
instance ToByteString Platform
instance ToQuery      Platform
instance ToHeader     Platform

instance ToJSON Platform where
    toJSON = toJSONText

instance FromJSON Platform where
    parseJSON = parseJSONText "Platform"

-- | Synchronization state for a project.
--
--
data ProjectState
  = Importing
  | Normal
  | Syncing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProjectState where
    parser = takeLowerText >>= \case
        "importing" -> pure Importing
        "normal" -> pure Normal
        "syncing" -> pure Syncing
        e -> fromTextError $ "Failure parsing ProjectState from value: '" <> e
           <> "'. Accepted values: importing, normal, syncing"

instance ToText ProjectState where
    toText = \case
        Importing -> "IMPORTING"
        Normal -> "NORMAL"
        Syncing -> "SYNCING"

instance Hashable     ProjectState
instance NFData       ProjectState
instance ToByteString ProjectState
instance ToQuery      ProjectState
instance ToHeader     ProjectState

instance FromJSON ProjectState where
    parseJSON = parseJSONText "ProjectState"
