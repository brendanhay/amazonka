{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Unit where

import Network.AWS.Prelude

data Unit
  = Bits
  | Bytes
  | Packets
  | Requests
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

instance FromText Unit where
  parser =
    takeLowerText >>= \case
      "bits" -> pure Bits
      "bytes" -> pure Bytes
      "packets" -> pure Packets
      "requests" -> pure Requests
      e ->
        fromTextError $
          "Failure parsing Unit from value: '" <> e
            <> "'. Accepted values: bits, bytes, packets, requests"

instance ToText Unit where
  toText = \case
    Bits -> "BITS"
    Bytes -> "BYTES"
    Packets -> "PACKETS"
    Requests -> "REQUESTS"

instance Hashable Unit

instance NFData Unit

instance ToByteString Unit

instance ToQuery Unit

instance ToHeader Unit

instance FromJSON Unit where
  parseJSON = parseJSONText "Unit"
