{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.CustomHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.CustomHealthStatus where

import Network.AWS.Prelude

data CustomHealthStatus
  = CHSHealthy
  | CHSUnhealthy
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

instance FromText CustomHealthStatus where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure CHSHealthy
      "unhealthy" -> pure CHSUnhealthy
      e ->
        fromTextError $
          "Failure parsing CustomHealthStatus from value: '" <> e
            <> "'. Accepted values: healthy, unhealthy"

instance ToText CustomHealthStatus where
  toText = \case
    CHSHealthy -> "HEALTHY"
    CHSUnhealthy -> "UNHEALTHY"

instance Hashable CustomHealthStatus

instance NFData CustomHealthStatus

instance ToByteString CustomHealthStatus

instance ToQuery CustomHealthStatus

instance ToHeader CustomHealthStatus

instance ToJSON CustomHealthStatus where
  toJSON = toJSONText
