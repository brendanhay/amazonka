{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstancePlatform where

import Network.AWS.Prelude

data InstancePlatform
  = LinuxUnix
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

instance FromText InstancePlatform where
  parser =
    takeLowerText >>= \case
      "linux_unix" -> pure LinuxUnix
      "windows" -> pure Windows
      e ->
        fromTextError $
          "Failure parsing InstancePlatform from value: '" <> e
            <> "'. Accepted values: linux_unix, windows"

instance ToText InstancePlatform where
  toText = \case
    LinuxUnix -> "LINUX_UNIX"
    Windows -> "WINDOWS"

instance Hashable InstancePlatform

instance NFData InstancePlatform

instance ToByteString InstancePlatform

instance ToQuery InstancePlatform

instance ToHeader InstancePlatform

instance FromJSON InstancePlatform where
  parseJSON = parseJSONText "InstancePlatform"
