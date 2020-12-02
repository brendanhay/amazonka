{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TelemetryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TelemetryStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TelemetryStatus
  = Down
  | UP
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

instance FromText TelemetryStatus where
  parser =
    takeLowerText >>= \case
      "down" -> pure Down
      "up" -> pure UP
      e ->
        fromTextError $
          "Failure parsing TelemetryStatus from value: '" <> e
            <> "'. Accepted values: down, up"

instance ToText TelemetryStatus where
  toText = \case
    Down -> "DOWN"
    UP -> "UP"

instance Hashable TelemetryStatus

instance NFData TelemetryStatus

instance ToByteString TelemetryStatus

instance ToQuery TelemetryStatus

instance ToHeader TelemetryStatus

instance FromXML TelemetryStatus where
  parseXML = parseXMLText "TelemetryStatus"
