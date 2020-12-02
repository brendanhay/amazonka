{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PlatformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PlatformType where

import Network.AWS.Prelude

data PlatformType
  = PTLinux
  | PTWindows
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

instance FromText PlatformType where
  parser =
    takeLowerText >>= \case
      "linux" -> pure PTLinux
      "windows" -> pure PTWindows
      e ->
        fromTextError $
          "Failure parsing PlatformType from value: '" <> e
            <> "'. Accepted values: linux, windows"

instance ToText PlatformType where
  toText = \case
    PTLinux -> "Linux"
    PTWindows -> "Windows"

instance Hashable PlatformType

instance NFData PlatformType

instance ToByteString PlatformType

instance ToQuery PlatformType

instance ToHeader PlatformType

instance FromJSON PlatformType where
  parseJSON = parseJSONText "PlatformType"
