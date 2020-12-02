{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentHealth where

import Network.AWS.Prelude

data AgentHealth
  = AHHealthy
  | AHUnhealthy
  | AHUnknown
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

instance FromText AgentHealth where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure AHHealthy
      "unhealthy" -> pure AHUnhealthy
      "unknown" -> pure AHUnknown
      e ->
        fromTextError $
          "Failure parsing AgentHealth from value: '" <> e
            <> "'. Accepted values: healthy, unhealthy, unknown"

instance ToText AgentHealth where
  toText = \case
    AHHealthy -> "HEALTHY"
    AHUnhealthy -> "UNHEALTHY"
    AHUnknown -> "UNKNOWN"

instance Hashable AgentHealth

instance NFData AgentHealth

instance ToByteString AgentHealth

instance ToQuery AgentHealth

instance ToHeader AgentHealth

instance ToJSON AgentHealth where
  toJSON = toJSONText

instance FromJSON AgentHealth where
  parseJSON = parseJSONText "AgentHealth"
