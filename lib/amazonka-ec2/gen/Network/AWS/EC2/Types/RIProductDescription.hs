{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RIProductDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RIProductDescription where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RIProductDescription
  = RIDLinuxUnix
  | RIDLinuxUnixAmazonVPC
  | RIDWindows
  | RIDWindowsAmazonVPC
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

instance FromText RIProductDescription where
  parser =
    takeLowerText >>= \case
      "linux/unix" -> pure RIDLinuxUnix
      "linux/unix (amazon vpc)" -> pure RIDLinuxUnixAmazonVPC
      "windows" -> pure RIDWindows
      "windows (amazon vpc)" -> pure RIDWindowsAmazonVPC
      e ->
        fromTextError $
          "Failure parsing RIProductDescription from value: '" <> e
            <> "'. Accepted values: linux/unix, linux/unix (amazon vpc), windows, windows (amazon vpc)"

instance ToText RIProductDescription where
  toText = \case
    RIDLinuxUnix -> "Linux/UNIX"
    RIDLinuxUnixAmazonVPC -> "Linux/UNIX (Amazon VPC)"
    RIDWindows -> "Windows"
    RIDWindowsAmazonVPC -> "Windows (Amazon VPC)"

instance Hashable RIProductDescription

instance NFData RIProductDescription

instance ToByteString RIProductDescription

instance ToQuery RIProductDescription

instance ToHeader RIProductDescription

instance FromXML RIProductDescription where
  parseXML = parseXMLText "RIProductDescription"
