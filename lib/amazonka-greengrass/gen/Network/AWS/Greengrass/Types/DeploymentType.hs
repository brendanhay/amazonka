{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeploymentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.DeploymentType
  ( DeploymentType
    ( DeploymentType'
    , DeploymentTypeNewDeployment
    , DeploymentTypeRedeployment
    , DeploymentTypeResetDeployment
    , DeploymentTypeForceResetDeployment
    , fromDeploymentType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
newtype DeploymentType = DeploymentType'{fromDeploymentType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern DeploymentTypeNewDeployment :: DeploymentType
pattern DeploymentTypeNewDeployment = DeploymentType' "NewDeployment"

pattern DeploymentTypeRedeployment :: DeploymentType
pattern DeploymentTypeRedeployment = DeploymentType' "Redeployment"

pattern DeploymentTypeResetDeployment :: DeploymentType
pattern DeploymentTypeResetDeployment = DeploymentType' "ResetDeployment"

pattern DeploymentTypeForceResetDeployment :: DeploymentType
pattern DeploymentTypeForceResetDeployment = DeploymentType' "ForceResetDeployment"

{-# COMPLETE 
  DeploymentTypeNewDeployment,

  DeploymentTypeRedeployment,

  DeploymentTypeResetDeployment,

  DeploymentTypeForceResetDeployment,
  DeploymentType'
  #-}
