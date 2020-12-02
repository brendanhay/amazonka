{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DeploymentCommandName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DeploymentCommandName where

import Network.AWS.Prelude

data DeploymentCommandName
  = Configure
  | Deploy
  | ExecuteRecipes
  | InstallDependencies
  | Restart
  | Rollback
  | Setup
  | Start
  | Stop
  | Undeploy
  | UpdateCustomCookbooks
  | UpdateDependencies
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

instance FromText DeploymentCommandName where
  parser =
    takeLowerText >>= \case
      "configure" -> pure Configure
      "deploy" -> pure Deploy
      "execute_recipes" -> pure ExecuteRecipes
      "install_dependencies" -> pure InstallDependencies
      "restart" -> pure Restart
      "rollback" -> pure Rollback
      "setup" -> pure Setup
      "start" -> pure Start
      "stop" -> pure Stop
      "undeploy" -> pure Undeploy
      "update_custom_cookbooks" -> pure UpdateCustomCookbooks
      "update_dependencies" -> pure UpdateDependencies
      e ->
        fromTextError $
          "Failure parsing DeploymentCommandName from value: '" <> e
            <> "'. Accepted values: configure, deploy, execute_recipes, install_dependencies, restart, rollback, setup, start, stop, undeploy, update_custom_cookbooks, update_dependencies"

instance ToText DeploymentCommandName where
  toText = \case
    Configure -> "configure"
    Deploy -> "deploy"
    ExecuteRecipes -> "execute_recipes"
    InstallDependencies -> "install_dependencies"
    Restart -> "restart"
    Rollback -> "rollback"
    Setup -> "setup"
    Start -> "start"
    Stop -> "stop"
    Undeploy -> "undeploy"
    UpdateCustomCookbooks -> "update_custom_cookbooks"
    UpdateDependencies -> "update_dependencies"

instance Hashable DeploymentCommandName

instance NFData DeploymentCommandName

instance ToByteString DeploymentCommandName

instance ToQuery DeploymentCommandName

instance ToHeader DeploymentCommandName

instance ToJSON DeploymentCommandName where
  toJSON = toJSONText

instance FromJSON DeploymentCommandName where
  parseJSON = parseJSONText "DeploymentCommandName"
