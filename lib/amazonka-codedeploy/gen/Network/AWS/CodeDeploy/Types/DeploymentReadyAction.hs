{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentReadyAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentReadyAction where

import Network.AWS.Prelude

data DeploymentReadyAction
  = ContinueDeployment
  | StopDeployment
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

instance FromText DeploymentReadyAction where
  parser =
    takeLowerText >>= \case
      "continue_deployment" -> pure ContinueDeployment
      "stop_deployment" -> pure StopDeployment
      e ->
        fromTextError $
          "Failure parsing DeploymentReadyAction from value: '" <> e
            <> "'. Accepted values: continue_deployment, stop_deployment"

instance ToText DeploymentReadyAction where
  toText = \case
    ContinueDeployment -> "CONTINUE_DEPLOYMENT"
    StopDeployment -> "STOP_DEPLOYMENT"

instance Hashable DeploymentReadyAction

instance NFData DeploymentReadyAction

instance ToByteString DeploymentReadyAction

instance ToQuery DeploymentReadyAction

instance ToHeader DeploymentReadyAction

instance ToJSON DeploymentReadyAction where
  toJSON = toJSONText

instance FromJSON DeploymentReadyAction where
  parseJSON = parseJSONText "DeploymentReadyAction"
