{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ActionType where

import Network.AWS.Prelude

data ActionType
  = Connect
  | Publish
  | Receive
  | Subscribe
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

instance FromText ActionType where
  parser =
    takeLowerText >>= \case
      "connect" -> pure Connect
      "publish" -> pure Publish
      "receive" -> pure Receive
      "subscribe" -> pure Subscribe
      e ->
        fromTextError $
          "Failure parsing ActionType from value: '" <> e
            <> "'. Accepted values: connect, publish, receive, subscribe"

instance ToText ActionType where
  toText = \case
    Connect -> "CONNECT"
    Publish -> "PUBLISH"
    Receive -> "RECEIVE"
    Subscribe -> "SUBSCRIBE"

instance Hashable ActionType

instance NFData ActionType

instance ToByteString ActionType

instance ToQuery ActionType

instance ToHeader ActionType

instance ToJSON ActionType where
  toJSON = toJSONText

instance FromJSON ActionType where
  parseJSON = parseJSONText "ActionType"
