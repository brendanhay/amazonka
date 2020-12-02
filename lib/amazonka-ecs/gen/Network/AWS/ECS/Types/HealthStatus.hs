{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HealthStatus where

import Network.AWS.Prelude

data HealthStatus
  = HSHealthy
  | HSUnhealthy
  | HSUnknown
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

instance FromText HealthStatus where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure HSHealthy
      "unhealthy" -> pure HSUnhealthy
      "unknown" -> pure HSUnknown
      e ->
        fromTextError $
          "Failure parsing HealthStatus from value: '" <> e
            <> "'. Accepted values: healthy, unhealthy, unknown"

instance ToText HealthStatus where
  toText = \case
    HSHealthy -> "HEALTHY"
    HSUnhealthy -> "UNHEALTHY"
    HSUnknown -> "UNKNOWN"

instance Hashable HealthStatus

instance NFData HealthStatus

instance ToByteString HealthStatus

instance ToQuery HealthStatus

instance ToHeader HealthStatus

instance FromJSON HealthStatus where
  parseJSON = parseJSONText "HealthStatus"
