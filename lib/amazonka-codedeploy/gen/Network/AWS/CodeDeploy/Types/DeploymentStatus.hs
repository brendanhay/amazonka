{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentStatus
  ( DeploymentStatus
    ( DeploymentStatus'
    , DeploymentStatusCreated
    , DeploymentStatusQueued
    , DeploymentStatusInProgress
    , DeploymentStatusBaking
    , DeploymentStatusSucceeded
    , DeploymentStatusFailed
    , DeploymentStatusStopped
    , DeploymentStatusReady
    , fromDeploymentStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeploymentStatus = DeploymentStatus'{fromDeploymentStatus
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern DeploymentStatusCreated :: DeploymentStatus
pattern DeploymentStatusCreated = DeploymentStatus' "Created"

pattern DeploymentStatusQueued :: DeploymentStatus
pattern DeploymentStatusQueued = DeploymentStatus' "Queued"

pattern DeploymentStatusInProgress :: DeploymentStatus
pattern DeploymentStatusInProgress = DeploymentStatus' "InProgress"

pattern DeploymentStatusBaking :: DeploymentStatus
pattern DeploymentStatusBaking = DeploymentStatus' "Baking"

pattern DeploymentStatusSucceeded :: DeploymentStatus
pattern DeploymentStatusSucceeded = DeploymentStatus' "Succeeded"

pattern DeploymentStatusFailed :: DeploymentStatus
pattern DeploymentStatusFailed = DeploymentStatus' "Failed"

pattern DeploymentStatusStopped :: DeploymentStatus
pattern DeploymentStatusStopped = DeploymentStatus' "Stopped"

pattern DeploymentStatusReady :: DeploymentStatus
pattern DeploymentStatusReady = DeploymentStatus' "Ready"

{-# COMPLETE 
  DeploymentStatusCreated,

  DeploymentStatusQueued,

  DeploymentStatusInProgress,

  DeploymentStatusBaking,

  DeploymentStatusSucceeded,

  DeploymentStatusFailed,

  DeploymentStatusStopped,

  DeploymentStatusReady,
  DeploymentStatus'
  #-}
