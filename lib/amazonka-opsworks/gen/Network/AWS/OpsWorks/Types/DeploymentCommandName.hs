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
        DeploymentCommandNameInstallDependencies,
        DeploymentCommandNameUpdateDependencies,
        DeploymentCommandNameUpdateCustomCookbooks,
        DeploymentCommandNameExecuteRecipes,
        DeploymentCommandNameConfigure,
        DeploymentCommandNameSetup,
        DeploymentCommandNameDeploy,
        DeploymentCommandNameRollback,
        DeploymentCommandNameStart,
        DeploymentCommandNameStop,
        DeploymentCommandNameRestart,
        DeploymentCommandNameUndeploy,
        fromDeploymentCommandName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DeploymentCommandName = DeploymentCommandName'
  { fromDeploymentCommandName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DeploymentCommandNameInstallDependencies :: DeploymentCommandName
pattern DeploymentCommandNameInstallDependencies = DeploymentCommandName' "install_dependencies"

pattern DeploymentCommandNameUpdateDependencies :: DeploymentCommandName
pattern DeploymentCommandNameUpdateDependencies = DeploymentCommandName' "update_dependencies"

pattern DeploymentCommandNameUpdateCustomCookbooks :: DeploymentCommandName
pattern DeploymentCommandNameUpdateCustomCookbooks = DeploymentCommandName' "update_custom_cookbooks"

pattern DeploymentCommandNameExecuteRecipes :: DeploymentCommandName
pattern DeploymentCommandNameExecuteRecipes = DeploymentCommandName' "execute_recipes"

pattern DeploymentCommandNameConfigure :: DeploymentCommandName
pattern DeploymentCommandNameConfigure = DeploymentCommandName' "configure"

pattern DeploymentCommandNameSetup :: DeploymentCommandName
pattern DeploymentCommandNameSetup = DeploymentCommandName' "setup"

pattern DeploymentCommandNameDeploy :: DeploymentCommandName
pattern DeploymentCommandNameDeploy = DeploymentCommandName' "deploy"

pattern DeploymentCommandNameRollback :: DeploymentCommandName
pattern DeploymentCommandNameRollback = DeploymentCommandName' "rollback"

pattern DeploymentCommandNameStart :: DeploymentCommandName
pattern DeploymentCommandNameStart = DeploymentCommandName' "start"

pattern DeploymentCommandNameStop :: DeploymentCommandName
pattern DeploymentCommandNameStop = DeploymentCommandName' "stop"

pattern DeploymentCommandNameRestart :: DeploymentCommandName
pattern DeploymentCommandNameRestart = DeploymentCommandName' "restart"

pattern DeploymentCommandNameUndeploy :: DeploymentCommandName
pattern DeploymentCommandNameUndeploy = DeploymentCommandName' "undeploy"

{-# COMPLETE
  DeploymentCommandNameInstallDependencies,
  DeploymentCommandNameUpdateDependencies,
  DeploymentCommandNameUpdateCustomCookbooks,
  DeploymentCommandNameExecuteRecipes,
  DeploymentCommandNameConfigure,
  DeploymentCommandNameSetup,
  DeploymentCommandNameDeploy,
  DeploymentCommandNameRollback,
  DeploymentCommandNameStart,
  DeploymentCommandNameStop,
  DeploymentCommandNameRestart,
  DeploymentCommandNameUndeploy,
  DeploymentCommandName'
  #-}
