{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.OperatingSystem where

import Network.AWS.Prelude

data OperatingSystem
  = AmazonLinux
  | AmazonLinux2
  | Windows2012
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

instance FromText OperatingSystem where
  parser =
    takeLowerText >>= \case
      "amazon_linux" -> pure AmazonLinux
      "amazon_linux_2" -> pure AmazonLinux2
      "windows_2012" -> pure Windows2012
      e ->
        fromTextError $
          "Failure parsing OperatingSystem from value: '" <> e
            <> "'. Accepted values: amazon_linux, amazon_linux_2, windows_2012"

instance ToText OperatingSystem where
  toText = \case
    AmazonLinux -> "AMAZON_LINUX"
    AmazonLinux2 -> "AMAZON_LINUX_2"
    Windows2012 -> "WINDOWS_2012"

instance Hashable OperatingSystem

instance NFData OperatingSystem

instance ToByteString OperatingSystem

instance ToQuery OperatingSystem

instance ToHeader OperatingSystem

instance ToJSON OperatingSystem where
  toJSON = toJSONText

instance FromJSON OperatingSystem where
  parseJSON = parseJSONText "OperatingSystem"
