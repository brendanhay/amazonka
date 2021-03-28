{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.BulkDeploymentStatus
  ( BulkDeploymentStatus
    ( BulkDeploymentStatus'
    , BulkDeploymentStatusInitializing
    , BulkDeploymentStatusRunning
    , BulkDeploymentStatusCompleted
    , BulkDeploymentStatusStopping
    , BulkDeploymentStatusStopped
    , BulkDeploymentStatusFailed
    , fromBulkDeploymentStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The current status of the bulk deployment.
newtype BulkDeploymentStatus = BulkDeploymentStatus'{fromBulkDeploymentStatus
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern BulkDeploymentStatusInitializing :: BulkDeploymentStatus
pattern BulkDeploymentStatusInitializing = BulkDeploymentStatus' "Initializing"

pattern BulkDeploymentStatusRunning :: BulkDeploymentStatus
pattern BulkDeploymentStatusRunning = BulkDeploymentStatus' "Running"

pattern BulkDeploymentStatusCompleted :: BulkDeploymentStatus
pattern BulkDeploymentStatusCompleted = BulkDeploymentStatus' "Completed"

pattern BulkDeploymentStatusStopping :: BulkDeploymentStatus
pattern BulkDeploymentStatusStopping = BulkDeploymentStatus' "Stopping"

pattern BulkDeploymentStatusStopped :: BulkDeploymentStatus
pattern BulkDeploymentStatusStopped = BulkDeploymentStatus' "Stopped"

pattern BulkDeploymentStatusFailed :: BulkDeploymentStatus
pattern BulkDeploymentStatusFailed = BulkDeploymentStatus' "Failed"

{-# COMPLETE 
  BulkDeploymentStatusInitializing,

  BulkDeploymentStatusRunning,

  BulkDeploymentStatusCompleted,

  BulkDeploymentStatusStopping,

  BulkDeploymentStatusStopped,

  BulkDeploymentStatusFailed,
  BulkDeploymentStatus'
  #-}
