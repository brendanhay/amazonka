{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentCreator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentCreator
  ( DeploymentCreator
    ( DeploymentCreator'
    , DeploymentCreatorUser
    , DeploymentCreatorAutoscaling
    , DeploymentCreatorCodeDeployRollback
    , DeploymentCreatorCodeDeploy
    , DeploymentCreatorCloudFormation
    , DeploymentCreatorCloudFormationRollback
    , fromDeploymentCreator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeploymentCreator = DeploymentCreator'{fromDeploymentCreator
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern DeploymentCreatorUser :: DeploymentCreator
pattern DeploymentCreatorUser = DeploymentCreator' "user"

pattern DeploymentCreatorAutoscaling :: DeploymentCreator
pattern DeploymentCreatorAutoscaling = DeploymentCreator' "autoscaling"

pattern DeploymentCreatorCodeDeployRollback :: DeploymentCreator
pattern DeploymentCreatorCodeDeployRollback = DeploymentCreator' "codeDeployRollback"

pattern DeploymentCreatorCodeDeploy :: DeploymentCreator
pattern DeploymentCreatorCodeDeploy = DeploymentCreator' "CodeDeploy"

pattern DeploymentCreatorCloudFormation :: DeploymentCreator
pattern DeploymentCreatorCloudFormation = DeploymentCreator' "CloudFormation"

pattern DeploymentCreatorCloudFormationRollback :: DeploymentCreator
pattern DeploymentCreatorCloudFormationRollback = DeploymentCreator' "CloudFormationRollback"

{-# COMPLETE 
  DeploymentCreatorUser,

  DeploymentCreatorAutoscaling,

  DeploymentCreatorCodeDeployRollback,

  DeploymentCreatorCodeDeploy,

  DeploymentCreatorCloudFormation,

  DeploymentCreatorCloudFormationRollback,
  DeploymentCreator'
  #-}
