{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.ServerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.ServerStatus where

import Network.AWS.Prelude

data ServerStatus
  = BackingUp
  | ConnectionLost
  | Creating
  | Deleting
  | Failed
  | Healthy
  | Modifying
  | Restoring
  | Running
  | Setup
  | Terminated
  | UnderMaintenance
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

instance FromText ServerStatus where
  parser =
    takeLowerText >>= \case
      "backing_up" -> pure BackingUp
      "connection_lost" -> pure ConnectionLost
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "healthy" -> pure Healthy
      "modifying" -> pure Modifying
      "restoring" -> pure Restoring
      "running" -> pure Running
      "setup" -> pure Setup
      "terminated" -> pure Terminated
      "under_maintenance" -> pure UnderMaintenance
      "unhealthy" -> pure Unhealthy
      e ->
        fromTextError $
          "Failure parsing ServerStatus from value: '" <> e
            <> "'. Accepted values: backing_up, connection_lost, creating, deleting, failed, healthy, modifying, restoring, running, setup, terminated, under_maintenance, unhealthy"

instance ToText ServerStatus where
  toText = \case
    BackingUp -> "BACKING_UP"
    ConnectionLost -> "CONNECTION_LOST"
    Creating -> "CREATING"
    Deleting -> "DELETING"
    Failed -> "FAILED"
    Healthy -> "HEALTHY"
    Modifying -> "MODIFYING"
    Restoring -> "RESTORING"
    Running -> "RUNNING"
    Setup -> "SETUP"
    Terminated -> "TERMINATED"
    UnderMaintenance -> "UNDER_MAINTENANCE"
    Unhealthy -> "UNHEALTHY"

instance Hashable ServerStatus

instance NFData ServerStatus

instance ToByteString ServerStatus

instance ToQuery ServerStatus

instance ToHeader ServerStatus

instance FromJSON ServerStatus where
  parseJSON = parseJSONText "ServerStatus"
