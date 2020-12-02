{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetStatus where

import Network.AWS.Prelude

data FleetStatus
  = FSActivating
  | FSActive
  | FSBuilding
  | FSDeleting
  | FSDownloading
  | FSError'
  | FSNew
  | FSTerminated
  | FSValidating
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

instance FromText FleetStatus where
  parser =
    takeLowerText >>= \case
      "activating" -> pure FSActivating
      "active" -> pure FSActive
      "building" -> pure FSBuilding
      "deleting" -> pure FSDeleting
      "downloading" -> pure FSDownloading
      "error" -> pure FSError'
      "new" -> pure FSNew
      "terminated" -> pure FSTerminated
      "validating" -> pure FSValidating
      e ->
        fromTextError $
          "Failure parsing FleetStatus from value: '" <> e
            <> "'. Accepted values: activating, active, building, deleting, downloading, error, new, terminated, validating"

instance ToText FleetStatus where
  toText = \case
    FSActivating -> "ACTIVATING"
    FSActive -> "ACTIVE"
    FSBuilding -> "BUILDING"
    FSDeleting -> "DELETING"
    FSDownloading -> "DOWNLOADING"
    FSError' -> "ERROR"
    FSNew -> "NEW"
    FSTerminated -> "TERMINATED"
    FSValidating -> "VALIDATING"

instance Hashable FleetStatus

instance NFData FleetStatus

instance ToByteString FleetStatus

instance ToQuery FleetStatus

instance ToHeader FleetStatus

instance FromJSON FleetStatus where
  parseJSON = parseJSONText "FleetStatus"
