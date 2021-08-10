{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DeploymentCommandName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DeploymentCommandName
  ( DeploymentCommandName
      ( ..,
        DeploymentCommandName_Configure,
        DeploymentCommandName_Deploy,
        DeploymentCommandName_Execute_recipes,
        DeploymentCommandName_Install_dependencies,
        DeploymentCommandName_Restart,
        DeploymentCommandName_Rollback,
        DeploymentCommandName_Setup,
        DeploymentCommandName_Start,
        DeploymentCommandName_Stop,
        DeploymentCommandName_Undeploy,
        DeploymentCommandName_Update_custom_cookbooks,
        DeploymentCommandName_Update_dependencies
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DeploymentCommandName = DeploymentCommandName'
  { fromDeploymentCommandName ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DeploymentCommandName_Configure :: DeploymentCommandName
pattern DeploymentCommandName_Configure = DeploymentCommandName' "configure"

pattern DeploymentCommandName_Deploy :: DeploymentCommandName
pattern DeploymentCommandName_Deploy = DeploymentCommandName' "deploy"

pattern DeploymentCommandName_Execute_recipes :: DeploymentCommandName
pattern DeploymentCommandName_Execute_recipes = DeploymentCommandName' "execute_recipes"

pattern DeploymentCommandName_Install_dependencies :: DeploymentCommandName
pattern DeploymentCommandName_Install_dependencies = DeploymentCommandName' "install_dependencies"

pattern DeploymentCommandName_Restart :: DeploymentCommandName
pattern DeploymentCommandName_Restart = DeploymentCommandName' "restart"

pattern DeploymentCommandName_Rollback :: DeploymentCommandName
pattern DeploymentCommandName_Rollback = DeploymentCommandName' "rollback"

pattern DeploymentCommandName_Setup :: DeploymentCommandName
pattern DeploymentCommandName_Setup = DeploymentCommandName' "setup"

pattern DeploymentCommandName_Start :: DeploymentCommandName
pattern DeploymentCommandName_Start = DeploymentCommandName' "start"

pattern DeploymentCommandName_Stop :: DeploymentCommandName
pattern DeploymentCommandName_Stop = DeploymentCommandName' "stop"

pattern DeploymentCommandName_Undeploy :: DeploymentCommandName
pattern DeploymentCommandName_Undeploy = DeploymentCommandName' "undeploy"

pattern DeploymentCommandName_Update_custom_cookbooks :: DeploymentCommandName
pattern DeploymentCommandName_Update_custom_cookbooks = DeploymentCommandName' "update_custom_cookbooks"

pattern DeploymentCommandName_Update_dependencies :: DeploymentCommandName
pattern DeploymentCommandName_Update_dependencies = DeploymentCommandName' "update_dependencies"

{-# COMPLETE
  DeploymentCommandName_Configure,
  DeploymentCommandName_Deploy,
  DeploymentCommandName_Execute_recipes,
  DeploymentCommandName_Install_dependencies,
  DeploymentCommandName_Restart,
  DeploymentCommandName_Rollback,
  DeploymentCommandName_Setup,
  DeploymentCommandName_Start,
  DeploymentCommandName_Stop,
  DeploymentCommandName_Undeploy,
  DeploymentCommandName_Update_custom_cookbooks,
  DeploymentCommandName_Update_dependencies,
  DeploymentCommandName'
  #-}
