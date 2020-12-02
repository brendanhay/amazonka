{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Channel where

import Network.AWS.Prelude

data Channel
  = Chat
  | Voice
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

instance FromText Channel where
  parser =
    takeLowerText >>= \case
      "chat" -> pure Chat
      "voice" -> pure Voice
      e ->
        fromTextError $
          "Failure parsing Channel from value: '" <> e
            <> "'. Accepted values: chat, voice"

instance ToText Channel where
  toText = \case
    Chat -> "CHAT"
    Voice -> "VOICE"

instance Hashable Channel

instance NFData Channel

instance ToByteString Channel

instance ToQuery Channel

instance ToHeader Channel

instance ToJSON Channel where
  toJSON = toJSONText

instance FromJSON Channel where
  parseJSON = parseJSONText "Channel"
