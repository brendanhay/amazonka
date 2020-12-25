{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ProcessedUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ProcessedUpdateAction
  ( ProcessedUpdateAction (..),

    -- * Smart constructor
    mkProcessedUpdateAction,

    -- * Lenses
    puaCacheClusterId,
    puaReplicationGroupId,
    puaServiceUpdateName,
    puaUpdateActionStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.ElastiCache.Types.UpdateActionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Update action that has been processed for the corresponding apply/stop request
--
-- /See:/ 'mkProcessedUpdateAction' smart constructor.
data ProcessedUpdateAction = ProcessedUpdateAction'
  { -- | The ID of the cache cluster
    cacheClusterId :: Core.Maybe Types.String,
    -- | The ID of the replication group
    replicationGroupId :: Core.Maybe Types.String,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Types.String,
    -- | The status of the update action on the Redis cluster
    updateActionStatus :: Core.Maybe Types.UpdateActionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessedUpdateAction' value with any optional fields omitted.
mkProcessedUpdateAction ::
  ProcessedUpdateAction
mkProcessedUpdateAction =
  ProcessedUpdateAction'
    { cacheClusterId = Core.Nothing,
      replicationGroupId = Core.Nothing,
      serviceUpdateName = Core.Nothing,
      updateActionStatus = Core.Nothing
    }

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaCacheClusterId :: Lens.Lens' ProcessedUpdateAction (Core.Maybe Types.String)
puaCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED puaCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The ID of the replication group
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaReplicationGroupId :: Lens.Lens' ProcessedUpdateAction (Core.Maybe Types.String)
puaReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED puaReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaServiceUpdateName :: Lens.Lens' ProcessedUpdateAction (Core.Maybe Types.String)
puaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED puaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The status of the update action on the Redis cluster
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaUpdateActionStatus :: Lens.Lens' ProcessedUpdateAction (Core.Maybe Types.UpdateActionStatus)
puaUpdateActionStatus = Lens.field @"updateActionStatus"
{-# DEPRECATED puaUpdateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead." #-}

instance Core.FromXML ProcessedUpdateAction where
  parseXML x =
    ProcessedUpdateAction'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "ServiceUpdateName")
      Core.<*> (x Core..@? "UpdateActionStatus")
