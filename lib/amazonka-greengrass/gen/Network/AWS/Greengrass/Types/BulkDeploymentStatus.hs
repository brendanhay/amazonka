{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentStatus where

import Network.AWS.Prelude

-- | The current status of the bulk deployment.
data BulkDeploymentStatus
  = Completed
  | Failed
  | Initializing
  | Running
  | Stopped
  | Stopping
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

instance FromText BulkDeploymentStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "failed" -> pure Failed
      "initializing" -> pure Initializing
      "running" -> pure Running
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      e ->
        fromTextError $
          "Failure parsing BulkDeploymentStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, initializing, running, stopped, stopping"

instance ToText BulkDeploymentStatus where
  toText = \case
    Completed -> "Completed"
    Failed -> "Failed"
    Initializing -> "Initializing"
    Running -> "Running"
    Stopped -> "Stopped"
    Stopping -> "Stopping"

instance Hashable BulkDeploymentStatus

instance NFData BulkDeploymentStatus

instance ToByteString BulkDeploymentStatus

instance ToQuery BulkDeploymentStatus

instance ToHeader BulkDeploymentStatus

instance FromJSON BulkDeploymentStatus where
  parseJSON = parseJSONText "BulkDeploymentStatus"
