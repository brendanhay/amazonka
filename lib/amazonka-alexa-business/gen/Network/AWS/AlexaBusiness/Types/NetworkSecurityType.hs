{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkSecurityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkSecurityType where

import Network.AWS.Prelude

data NetworkSecurityType
  = Open
  | WPA2Enterprise
  | WPA2Psk
  | Wep
  | WpaPsk
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

instance FromText NetworkSecurityType where
  parser =
    takeLowerText >>= \case
      "open" -> pure Open
      "wpa2_enterprise" -> pure WPA2Enterprise
      "wpa2_psk" -> pure WPA2Psk
      "wep" -> pure Wep
      "wpa_psk" -> pure WpaPsk
      e ->
        fromTextError $
          "Failure parsing NetworkSecurityType from value: '" <> e
            <> "'. Accepted values: open, wpa2_enterprise, wpa2_psk, wep, wpa_psk"

instance ToText NetworkSecurityType where
  toText = \case
    Open -> "OPEN"
    WPA2Enterprise -> "WPA2_ENTERPRISE"
    WPA2Psk -> "WPA2_PSK"
    Wep -> "WEP"
    WpaPsk -> "WPA_PSK"

instance Hashable NetworkSecurityType

instance NFData NetworkSecurityType

instance ToByteString NetworkSecurityType

instance ToQuery NetworkSecurityType

instance ToHeader NetworkSecurityType

instance ToJSON NetworkSecurityType where
  toJSON = toJSONText

instance FromJSON NetworkSecurityType where
  parseJSON = parseJSONText "NetworkSecurityType"
