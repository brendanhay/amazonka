{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentStatus where

import Network.AWS.Prelude

data DeploymentStatus
  = Baking
  | Created
  | Failed
  | InProgress
  | Queued
  | Ready
  | Stopped
  | Succeeded
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

instance FromText DeploymentStatus where
  parser =
    takeLowerText >>= \case
      "baking" -> pure Baking
      "created" -> pure Created
      "failed" -> pure Failed
      "inprogress" -> pure InProgress
      "queued" -> pure Queued
      "ready" -> pure Ready
      "stopped" -> pure Stopped
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing DeploymentStatus from value: '" <> e
            <> "'. Accepted values: baking, created, failed, inprogress, queued, ready, stopped, succeeded"

instance ToText DeploymentStatus where
  toText = \case
    Baking -> "Baking"
    Created -> "Created"
    Failed -> "Failed"
    InProgress -> "InProgress"
    Queued -> "Queued"
    Ready -> "Ready"
    Stopped -> "Stopped"
    Succeeded -> "Succeeded"

instance Hashable DeploymentStatus

instance NFData DeploymentStatus

instance ToByteString DeploymentStatus

instance ToQuery DeploymentStatus

instance ToHeader DeploymentStatus

instance ToJSON DeploymentStatus where
  toJSON = toJSONText

instance FromJSON DeploymentStatus where
  parseJSON = parseJSONText "DeploymentStatus"
