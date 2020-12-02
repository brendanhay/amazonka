{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceIPScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceIPScheme where

import Network.AWS.Prelude

-- | Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
data InputDeviceIPScheme
  = DHCP
  | Static
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

instance FromText InputDeviceIPScheme where
  parser =
    takeLowerText >>= \case
      "dhcp" -> pure DHCP
      "static" -> pure Static
      e ->
        fromTextError $
          "Failure parsing InputDeviceIPScheme from value: '" <> e
            <> "'. Accepted values: dhcp, static"

instance ToText InputDeviceIPScheme where
  toText = \case
    DHCP -> "DHCP"
    Static -> "STATIC"

instance Hashable InputDeviceIPScheme

instance NFData InputDeviceIPScheme

instance ToByteString InputDeviceIPScheme

instance ToQuery InputDeviceIPScheme

instance ToHeader InputDeviceIPScheme

instance FromJSON InputDeviceIPScheme where
  parseJSON = parseJSONText "InputDeviceIPScheme"
