{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourcePosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EventSourcePosition where

import Network.AWS.Prelude

data EventSourcePosition
  = AtTimestamp
  | Latest
  | TrimHorizon
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

instance FromText EventSourcePosition where
  parser =
    takeLowerText >>= \case
      "at_timestamp" -> pure AtTimestamp
      "latest" -> pure Latest
      "trim_horizon" -> pure TrimHorizon
      e ->
        fromTextError $
          "Failure parsing EventSourcePosition from value: '" <> e
            <> "'. Accepted values: at_timestamp, latest, trim_horizon"

instance ToText EventSourcePosition where
  toText = \case
    AtTimestamp -> "AT_TIMESTAMP"
    Latest -> "LATEST"
    TrimHorizon -> "TRIM_HORIZON"

instance Hashable EventSourcePosition

instance NFData EventSourcePosition

instance ToByteString EventSourcePosition

instance ToQuery EventSourcePosition

instance ToHeader EventSourcePosition

instance ToJSON EventSourcePosition where
  toJSON = toJSONText

instance FromJSON EventSourcePosition where
  parseJSON = parseJSONText "EventSourcePosition"
