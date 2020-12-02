{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationEndpointTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationEndpointTypeValue where

import Network.AWS.Prelude

data ReplicationEndpointTypeValue
  = Source
  | Target
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

instance FromText ReplicationEndpointTypeValue where
  parser =
    takeLowerText >>= \case
      "source" -> pure Source
      "target" -> pure Target
      e ->
        fromTextError $
          "Failure parsing ReplicationEndpointTypeValue from value: '" <> e
            <> "'. Accepted values: source, target"

instance ToText ReplicationEndpointTypeValue where
  toText = \case
    Source -> "source"
    Target -> "target"

instance Hashable ReplicationEndpointTypeValue

instance NFData ReplicationEndpointTypeValue

instance ToByteString ReplicationEndpointTypeValue

instance ToQuery ReplicationEndpointTypeValue

instance ToHeader ReplicationEndpointTypeValue

instance ToJSON ReplicationEndpointTypeValue where
  toJSON = toJSONText

instance FromJSON ReplicationEndpointTypeValue where
  parseJSON = parseJSONText "ReplicationEndpointTypeValue"
