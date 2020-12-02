{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TriggerEventType where

import Network.AWS.Prelude

data TriggerEventType
  = DeploymentFailure
  | DeploymentReady
  | DeploymentRollback
  | DeploymentStart
  | DeploymentStop
  | DeploymentSuccess
  | InstanceFailure
  | InstanceReady
  | InstanceStart
  | InstanceSuccess
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

instance FromText TriggerEventType where
  parser =
    takeLowerText >>= \case
      "deploymentfailure" -> pure DeploymentFailure
      "deploymentready" -> pure DeploymentReady
      "deploymentrollback" -> pure DeploymentRollback
      "deploymentstart" -> pure DeploymentStart
      "deploymentstop" -> pure DeploymentStop
      "deploymentsuccess" -> pure DeploymentSuccess
      "instancefailure" -> pure InstanceFailure
      "instanceready" -> pure InstanceReady
      "instancestart" -> pure InstanceStart
      "instancesuccess" -> pure InstanceSuccess
      e ->
        fromTextError $
          "Failure parsing TriggerEventType from value: '" <> e
            <> "'. Accepted values: deploymentfailure, deploymentready, deploymentrollback, deploymentstart, deploymentstop, deploymentsuccess, instancefailure, instanceready, instancestart, instancesuccess"

instance ToText TriggerEventType where
  toText = \case
    DeploymentFailure -> "DeploymentFailure"
    DeploymentReady -> "DeploymentReady"
    DeploymentRollback -> "DeploymentRollback"
    DeploymentStart -> "DeploymentStart"
    DeploymentStop -> "DeploymentStop"
    DeploymentSuccess -> "DeploymentSuccess"
    InstanceFailure -> "InstanceFailure"
    InstanceReady -> "InstanceReady"
    InstanceStart -> "InstanceStart"
    InstanceSuccess -> "InstanceSuccess"

instance Hashable TriggerEventType

instance NFData TriggerEventType

instance ToByteString TriggerEventType

instance ToQuery TriggerEventType

instance ToHeader TriggerEventType

instance ToJSON TriggerEventType where
  toJSON = toJSONText

instance FromJSON TriggerEventType where
  parseJSON = parseJSONText "TriggerEventType"
