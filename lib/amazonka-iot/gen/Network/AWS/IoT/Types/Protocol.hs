{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Protocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Protocol where

import Network.AWS.Prelude

data Protocol
  = HTTP
  | Mqtt
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

instance FromText Protocol where
  parser =
    takeLowerText >>= \case
      "http" -> pure HTTP
      "mqtt" -> pure Mqtt
      e ->
        fromTextError $
          "Failure parsing Protocol from value: '" <> e
            <> "'. Accepted values: http, mqtt"

instance ToText Protocol where
  toText = \case
    HTTP -> "HTTP"
    Mqtt -> "MQTT"

instance Hashable Protocol

instance NFData Protocol

instance ToByteString Protocol

instance ToQuery Protocol

instance ToHeader Protocol

instance ToJSON Protocol where
  toJSON = toJSONText

instance FromJSON Protocol where
  parseJSON = parseJSONText "Protocol"
