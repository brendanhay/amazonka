{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem where

import Network.AWS.Prelude

-- | The operating system of the cores which are the targets of an update.
data UpdateTargetsOperatingSystem
  = AmazonLinux
  | Openwrt
  | Raspbian
  | Ubuntu
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

instance FromText UpdateTargetsOperatingSystem where
  parser =
    takeLowerText >>= \case
      "amazon_linux" -> pure AmazonLinux
      "openwrt" -> pure Openwrt
      "raspbian" -> pure Raspbian
      "ubuntu" -> pure Ubuntu
      e ->
        fromTextError $
          "Failure parsing UpdateTargetsOperatingSystem from value: '" <> e
            <> "'. Accepted values: amazon_linux, openwrt, raspbian, ubuntu"

instance ToText UpdateTargetsOperatingSystem where
  toText = \case
    AmazonLinux -> "amazon_linux"
    Openwrt -> "openwrt"
    Raspbian -> "raspbian"
    Ubuntu -> "ubuntu"

instance Hashable UpdateTargetsOperatingSystem

instance NFData UpdateTargetsOperatingSystem

instance ToByteString UpdateTargetsOperatingSystem

instance ToQuery UpdateTargetsOperatingSystem

instance ToHeader UpdateTargetsOperatingSystem

instance ToJSON UpdateTargetsOperatingSystem where
  toJSON = toJSONText
