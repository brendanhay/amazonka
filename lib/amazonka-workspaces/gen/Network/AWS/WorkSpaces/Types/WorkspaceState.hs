{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceState where

import Network.AWS.Prelude

data WorkspaceState
  = WSAdminMaintenance
  | WSAvailable
  | WSError'
  | WSImpaired
  | WSMaintenance
  | WSPending
  | WSRebooting
  | WSRebuilding
  | WSRestoring
  | WSStarting
  | WSStopped
  | WSStopping
  | WSSuspended
  | WSTerminated
  | WSTerminating
  | WSUnhealthy
  | WSUpdating
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

instance FromText WorkspaceState where
  parser =
    takeLowerText >>= \case
      "admin_maintenance" -> pure WSAdminMaintenance
      "available" -> pure WSAvailable
      "error" -> pure WSError'
      "impaired" -> pure WSImpaired
      "maintenance" -> pure WSMaintenance
      "pending" -> pure WSPending
      "rebooting" -> pure WSRebooting
      "rebuilding" -> pure WSRebuilding
      "restoring" -> pure WSRestoring
      "starting" -> pure WSStarting
      "stopped" -> pure WSStopped
      "stopping" -> pure WSStopping
      "suspended" -> pure WSSuspended
      "terminated" -> pure WSTerminated
      "terminating" -> pure WSTerminating
      "unhealthy" -> pure WSUnhealthy
      "updating" -> pure WSUpdating
      e ->
        fromTextError $
          "Failure parsing WorkspaceState from value: '" <> e
            <> "'. Accepted values: admin_maintenance, available, error, impaired, maintenance, pending, rebooting, rebuilding, restoring, starting, stopped, stopping, suspended, terminated, terminating, unhealthy, updating"

instance ToText WorkspaceState where
  toText = \case
    WSAdminMaintenance -> "ADMIN_MAINTENANCE"
    WSAvailable -> "AVAILABLE"
    WSError' -> "ERROR"
    WSImpaired -> "IMPAIRED"
    WSMaintenance -> "MAINTENANCE"
    WSPending -> "PENDING"
    WSRebooting -> "REBOOTING"
    WSRebuilding -> "REBUILDING"
    WSRestoring -> "RESTORING"
    WSStarting -> "STARTING"
    WSStopped -> "STOPPED"
    WSStopping -> "STOPPING"
    WSSuspended -> "SUSPENDED"
    WSTerminated -> "TERMINATED"
    WSTerminating -> "TERMINATING"
    WSUnhealthy -> "UNHEALTHY"
    WSUpdating -> "UPDATING"

instance Hashable WorkspaceState

instance NFData WorkspaceState

instance ToByteString WorkspaceState

instance ToQuery WorkspaceState

instance ToHeader WorkspaceState

instance FromJSON WorkspaceState where
  parseJSON = parseJSONText "WorkspaceState"
