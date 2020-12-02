{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.InsufficientDataHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.InsufficientDataHealthStatus where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data InsufficientDataHealthStatus
  = Healthy
  | LastKnownStatus
  | Unhealthy
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

instance FromText InsufficientDataHealthStatus where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure Healthy
      "lastknownstatus" -> pure LastKnownStatus
      "unhealthy" -> pure Unhealthy
      e ->
        fromTextError $
          "Failure parsing InsufficientDataHealthStatus from value: '" <> e
            <> "'. Accepted values: healthy, lastknownstatus, unhealthy"

instance ToText InsufficientDataHealthStatus where
  toText = \case
    Healthy -> "Healthy"
    LastKnownStatus -> "LastKnownStatus"
    Unhealthy -> "Unhealthy"

instance Hashable InsufficientDataHealthStatus

instance NFData InsufficientDataHealthStatus

instance ToByteString InsufficientDataHealthStatus

instance ToQuery InsufficientDataHealthStatus

instance ToHeader InsufficientDataHealthStatus

instance FromXML InsufficientDataHealthStatus where
  parseXML = parseXMLText "InsufficientDataHealthStatus"

instance ToXML InsufficientDataHealthStatus where
  toXML = toXMLText
