{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.NetworkProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.NetworkProtocol where

import Network.AWS.Prelude

data NetworkProtocol
  = All
  | ICMP
  | TCP
  | Udp
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

instance FromText NetworkProtocol where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "icmp" -> pure ICMP
      "tcp" -> pure TCP
      "udp" -> pure Udp
      e ->
        fromTextError $
          "Failure parsing NetworkProtocol from value: '" <> e
            <> "'. Accepted values: all, icmp, tcp, udp"

instance ToText NetworkProtocol where
  toText = \case
    All -> "all"
    ICMP -> "icmp"
    TCP -> "tcp"
    Udp -> "udp"

instance Hashable NetworkProtocol

instance NFData NetworkProtocol

instance ToByteString NetworkProtocol

instance ToQuery NetworkProtocol

instance ToHeader NetworkProtocol

instance ToJSON NetworkProtocol where
  toJSON = toJSONText

instance FromJSON NetworkProtocol where
  parseJSON = parseJSONText "NetworkProtocol"
