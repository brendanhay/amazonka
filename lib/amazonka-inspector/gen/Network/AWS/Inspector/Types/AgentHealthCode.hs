{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentHealthCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentHealthCode where

import Network.AWS.Prelude

data AgentHealthCode
  = Idle
  | Running
  | Shutdown
  | Throttled
  | Unhealthy
  | Unknown
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

instance FromText AgentHealthCode where
  parser =
    takeLowerText >>= \case
      "idle" -> pure Idle
      "running" -> pure Running
      "shutdown" -> pure Shutdown
      "throttled" -> pure Throttled
      "unhealthy" -> pure Unhealthy
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing AgentHealthCode from value: '" <> e
            <> "'. Accepted values: idle, running, shutdown, throttled, unhealthy, unknown"

instance ToText AgentHealthCode where
  toText = \case
    Idle -> "IDLE"
    Running -> "RUNNING"
    Shutdown -> "SHUTDOWN"
    Throttled -> "THROTTLED"
    Unhealthy -> "UNHEALTHY"
    Unknown -> "UNKNOWN"

instance Hashable AgentHealthCode

instance NFData AgentHealthCode

instance ToByteString AgentHealthCode

instance ToQuery AgentHealthCode

instance ToHeader AgentHealthCode

instance ToJSON AgentHealthCode where
  toJSON = toJSONText

instance FromJSON AgentHealthCode where
  parseJSON = parseJSONText "AgentHealthCode"
