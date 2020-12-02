{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlatformValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlatformValues where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PlatformValues = Windows
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

instance FromText PlatformValues where
  parser =
    takeLowerText >>= \case
      "windows" -> pure Windows
      e ->
        fromTextError $
          "Failure parsing PlatformValues from value: '" <> e
            <> "'. Accepted values: windows"

instance ToText PlatformValues where
  toText = \case
    Windows -> "Windows"

instance Hashable PlatformValues

instance NFData PlatformValues

instance ToByteString PlatformValues

instance ToQuery PlatformValues

instance ToHeader PlatformValues

instance FromXML PlatformValues where
  parseXML = parseXMLText "PlatformValues"
