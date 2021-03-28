{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentTargetType
  ( DeploymentTargetType
    ( DeploymentTargetType'
    , DeploymentTargetTypeInstanceTarget
    , DeploymentTargetTypeLambdaTarget
    , DeploymentTargetTypeECSTarget
    , DeploymentTargetTypeCloudFormationTarget
    , fromDeploymentTargetType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeploymentTargetType = DeploymentTargetType'{fromDeploymentTargetType
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern DeploymentTargetTypeInstanceTarget :: DeploymentTargetType
pattern DeploymentTargetTypeInstanceTarget = DeploymentTargetType' "InstanceTarget"

pattern DeploymentTargetTypeLambdaTarget :: DeploymentTargetType
pattern DeploymentTargetTypeLambdaTarget = DeploymentTargetType' "LambdaTarget"

pattern DeploymentTargetTypeECSTarget :: DeploymentTargetType
pattern DeploymentTargetTypeECSTarget = DeploymentTargetType' "ECSTarget"

pattern DeploymentTargetTypeCloudFormationTarget :: DeploymentTargetType
pattern DeploymentTargetTypeCloudFormationTarget = DeploymentTargetType' "CloudFormationTarget"

{-# COMPLETE 
  DeploymentTargetTypeInstanceTarget,

  DeploymentTargetTypeLambdaTarget,

  DeploymentTargetTypeECSTarget,

  DeploymentTargetTypeCloudFormationTarget,
  DeploymentTargetType'
  #-}
