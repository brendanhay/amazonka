{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.PlatformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.PlatformType where

import Network.AWS.Prelude

data PlatformType
  = AmazonLinux
  | Debian
  | Ubuntu
  | WindowsServer
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
      "amazon_linux" -> pure AmazonLinux
      "debian" -> pure Debian
      "ubuntu" -> pure Ubuntu
      "windows_server" -> pure WindowsServer
      e ->
        fromTextError $
          "Failure parsing PlatformType from value: '" <> e
            <> "'. Accepted values: amazon_linux, debian, ubuntu, windows_server"

instance ToText PlatformType where
  toText = \case
    AmazonLinux -> "AMAZON_LINUX"
    Debian -> "DEBIAN"
    Ubuntu -> "UBUNTU"
    WindowsServer -> "WINDOWS_SERVER"

instance Hashable PlatformType

instance NFData PlatformType

instance ToByteString PlatformType

instance ToQuery PlatformType

instance ToHeader PlatformType

instance FromJSON PlatformType where
  parseJSON = parseJSONText "PlatformType"
