{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelProtocol where

import Network.AWS.Prelude

data ChannelProtocol
  = HTTPS
  | Wss
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

instance FromText ChannelProtocol where
  parser =
    takeLowerText >>= \case
      "https" -> pure HTTPS
      "wss" -> pure Wss
      e ->
        fromTextError $
          "Failure parsing ChannelProtocol from value: '" <> e
            <> "'. Accepted values: https, wss"

instance ToText ChannelProtocol where
  toText = \case
    HTTPS -> "HTTPS"
    Wss -> "WSS"

instance Hashable ChannelProtocol

instance NFData ChannelProtocol

instance ToByteString ChannelProtocol

instance ToQuery ChannelProtocol

instance ToHeader ChannelProtocol

instance ToJSON ChannelProtocol where
  toJSON = toJSONText

instance FromJSON ChannelProtocol where
  parseJSON = parseJSONText "ChannelProtocol"
