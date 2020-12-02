{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentStatus where

import Network.AWS.Prelude

data AgentStatus
  = Blacklisted
  | Healthy
  | Running
  | Shutdown
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

instance FromText AgentStatus where
  parser =
    takeLowerText >>= \case
      "blacklisted" -> pure Blacklisted
      "healthy" -> pure Healthy
      "running" -> pure Running
      "shutdown" -> pure Shutdown
      "unhealthy" -> pure Unhealthy
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing AgentStatus from value: '" <> e
            <> "'. Accepted values: blacklisted, healthy, running, shutdown, unhealthy, unknown"

instance ToText AgentStatus where
  toText = \case
    Blacklisted -> "BLACKLISTED"
    Healthy -> "HEALTHY"
    Running -> "RUNNING"
    Shutdown -> "SHUTDOWN"
    Unhealthy -> "UNHEALTHY"
    Unknown -> "UNKNOWN"

instance Hashable AgentStatus

instance NFData AgentStatus

instance ToByteString AgentStatus

instance ToQuery AgentStatus

instance ToHeader AgentStatus

instance FromJSON AgentStatus where
  parseJSON = parseJSONText "AgentStatus"
