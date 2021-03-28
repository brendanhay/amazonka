{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DeploymentMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.DeploymentMode
  ( DeploymentMode
    ( DeploymentMode'
    , DeploymentModeSingleInstance
    , DeploymentModeActiveStandbyMultiAz
    , DeploymentModeClusterMultiAz
    , fromDeploymentMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The deployment mode of the broker.
newtype DeploymentMode = DeploymentMode'{fromDeploymentMode ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern DeploymentModeSingleInstance :: DeploymentMode
pattern DeploymentModeSingleInstance = DeploymentMode' "SINGLE_INSTANCE"

pattern DeploymentModeActiveStandbyMultiAz :: DeploymentMode
pattern DeploymentModeActiveStandbyMultiAz = DeploymentMode' "ACTIVE_STANDBY_MULTI_AZ"

pattern DeploymentModeClusterMultiAz :: DeploymentMode
pattern DeploymentModeClusterMultiAz = DeploymentMode' "CLUSTER_MULTI_AZ"

{-# COMPLETE 
  DeploymentModeSingleInstance,

  DeploymentModeActiveStandbyMultiAz,

  DeploymentModeClusterMultiAz,
  DeploymentMode'
  #-}
