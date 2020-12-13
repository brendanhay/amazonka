{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DeploymentCommandName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DeploymentCommandName
  ( DeploymentCommandName
      ( DeploymentCommandName',
        InstallDependencies,
        UpdateDependencies,
        UpdateCustomCookbooks,
        ExecuteRecipes,
        Configure,
        Setup,
        Deploy,
        Rollback,
        Start,
        Stop,
        Restart,
        Undeploy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeploymentCommandName = DeploymentCommandName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern InstallDependencies :: DeploymentCommandName
pattern InstallDependencies = DeploymentCommandName' "install_dependencies"

pattern UpdateDependencies :: DeploymentCommandName
pattern UpdateDependencies = DeploymentCommandName' "update_dependencies"

pattern UpdateCustomCookbooks :: DeploymentCommandName
pattern UpdateCustomCookbooks = DeploymentCommandName' "update_custom_cookbooks"

pattern ExecuteRecipes :: DeploymentCommandName
pattern ExecuteRecipes = DeploymentCommandName' "execute_recipes"

pattern Configure :: DeploymentCommandName
pattern Configure = DeploymentCommandName' "configure"

pattern Setup :: DeploymentCommandName
pattern Setup = DeploymentCommandName' "setup"

pattern Deploy :: DeploymentCommandName
pattern Deploy = DeploymentCommandName' "deploy"

pattern Rollback :: DeploymentCommandName
pattern Rollback = DeploymentCommandName' "rollback"

pattern Start :: DeploymentCommandName
pattern Start = DeploymentCommandName' "start"

pattern Stop :: DeploymentCommandName
pattern Stop = DeploymentCommandName' "stop"

pattern Restart :: DeploymentCommandName
pattern Restart = DeploymentCommandName' "restart"

pattern Undeploy :: DeploymentCommandName
pattern Undeploy = DeploymentCommandName' "undeploy"

{-# COMPLETE
  InstallDependencies,
  UpdateDependencies,
  UpdateCustomCookbooks,
  ExecuteRecipes,
  Configure,
  Setup,
  Deploy,
  Rollback,
  Start,
  Stop,
  Restart,
  Undeploy,
  DeploymentCommandName'
  #-}
