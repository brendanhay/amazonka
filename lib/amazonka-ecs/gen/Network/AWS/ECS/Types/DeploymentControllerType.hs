{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentControllerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.DeploymentControllerType
  ( DeploymentControllerType
    ( DeploymentControllerType'
    , DeploymentControllerTypeEcs
    , DeploymentControllerTypeCodeDeploy
    , DeploymentControllerTypeExternal
    , fromDeploymentControllerType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeploymentControllerType = DeploymentControllerType'{fromDeploymentControllerType
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern DeploymentControllerTypeEcs :: DeploymentControllerType
pattern DeploymentControllerTypeEcs = DeploymentControllerType' "ECS"

pattern DeploymentControllerTypeCodeDeploy :: DeploymentControllerType
pattern DeploymentControllerTypeCodeDeploy = DeploymentControllerType' "CODE_DEPLOY"

pattern DeploymentControllerTypeExternal :: DeploymentControllerType
pattern DeploymentControllerTypeExternal = DeploymentControllerType' "EXTERNAL"

{-# COMPLETE 
  DeploymentControllerTypeEcs,

  DeploymentControllerTypeCodeDeploy,

  DeploymentControllerTypeExternal,
  DeploymentControllerType'
  #-}
