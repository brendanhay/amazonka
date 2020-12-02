{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.PlatformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.PlatformType where

import Network.AWS.Prelude

data PlatformType
  = Windows
  | WindowsServer2016
  | WindowsServer2019
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
      "windows" -> pure Windows
      "windows_server_2016" -> pure WindowsServer2016
      "windows_server_2019" -> pure WindowsServer2019
      e ->
        fromTextError $
          "Failure parsing PlatformType from value: '" <> e
            <> "'. Accepted values: windows, windows_server_2016, windows_server_2019"

instance ToText PlatformType where
  toText = \case
    Windows -> "WINDOWS"
    WindowsServer2016 -> "WINDOWS_SERVER_2016"
    WindowsServer2019 -> "WINDOWS_SERVER_2019"

instance Hashable PlatformType

instance NFData PlatformType

instance ToByteString PlatformType

instance ToQuery PlatformType

instance ToHeader PlatformType

instance FromJSON PlatformType where
  parseJSON = parseJSONText "PlatformType"
