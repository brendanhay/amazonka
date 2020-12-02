{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.AddressFamily
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.AddressFamily where

import Network.AWS.Prelude

data AddressFamily
  = IPV4
  | IPV6
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

instance FromText AddressFamily where
  parser =
    takeLowerText >>= \case
      "ipv4" -> pure IPV4
      "ipv6" -> pure IPV6
      e ->
        fromTextError $
          "Failure parsing AddressFamily from value: '" <> e
            <> "'. Accepted values: ipv4, ipv6"

instance ToText AddressFamily where
  toText = \case
    IPV4 -> "ipv4"
    IPV6 -> "ipv6"

instance Hashable AddressFamily

instance NFData AddressFamily

instance ToByteString AddressFamily

instance ToQuery AddressFamily

instance ToHeader AddressFamily

instance ToJSON AddressFamily where
  toJSON = toJSONText

instance FromJSON AddressFamily where
  parseJSON = parseJSONText "AddressFamily"
