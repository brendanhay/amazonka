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
        Configure,
        Deploy,
        ExecuteRecipes,
        InstallDependencies,
        Restart,
        Rollback,
        Setup,
        Start,
        Stop,
        Undeploy,
        UpdateCustomCookbooks,
        UpdateDependencies
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

pattern Configure :: DeploymentCommandName
pattern Configure = DeploymentCommandName' "configure"

pattern Deploy :: DeploymentCommandName
pattern Deploy = DeploymentCommandName' "deploy"

pattern ExecuteRecipes :: DeploymentCommandName
pattern ExecuteRecipes = DeploymentCommandName' "execute_recipes"

pattern InstallDependencies :: DeploymentCommandName
pattern InstallDependencies = DeploymentCommandName' "install_dependencies"

pattern Restart :: DeploymentCommandName
pattern Restart = DeploymentCommandName' "restart"

pattern Rollback :: DeploymentCommandName
pattern Rollback = DeploymentCommandName' "rollback"

pattern Setup :: DeploymentCommandName
pattern Setup = DeploymentCommandName' "setup"

pattern Start :: DeploymentCommandName
pattern Start = DeploymentCommandName' "start"

pattern Stop :: DeploymentCommandName
pattern Stop = DeploymentCommandName' "stop"

pattern Undeploy :: DeploymentCommandName
pattern Undeploy = DeploymentCommandName' "undeploy"

pattern UpdateCustomCookbooks :: DeploymentCommandName
pattern UpdateCustomCookbooks = DeploymentCommandName' "update_custom_cookbooks"

pattern UpdateDependencies :: DeploymentCommandName
pattern UpdateDependencies = DeploymentCommandName' "update_dependencies"

{-# COMPLETE
  Configure,
  Deploy,
  ExecuteRecipes,
  InstallDependencies,
  Restart,
  Rollback,
  Setup,
  Start,
  Stop,
  Undeploy,
  UpdateCustomCookbooks,
  UpdateDependencies,
  DeploymentCommandName'
  #-}
