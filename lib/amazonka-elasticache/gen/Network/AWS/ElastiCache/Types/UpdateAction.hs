{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateAction
  ( UpdateAction (..),

    -- * Smart constructor
    mkUpdateAction,

    -- * Lenses
    uaCacheClusterId,
    uaCacheNodeUpdateStatus,
    uaEngine,
    uaEstimatedUpdateTime,
    uaNodeGroupUpdateStatus,
    uaNodesUpdated,
    uaReplicationGroupId,
    uaServiceUpdateName,
    uaServiceUpdateRecommendedApplyByDate,
    uaServiceUpdateReleaseDate,
    uaServiceUpdateSeverity,
    uaServiceUpdateStatus,
    uaServiceUpdateType,
    uaSlaMet,
    uaUpdateActionAvailableDate,
    uaUpdateActionStatus,
    uaUpdateActionStatusModifiedDate,
  )
where

import qualified Network.AWS.ElastiCache.Types.CacheClusterId as Types
import qualified Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.Engine as Types
import qualified Network.AWS.ElastiCache.Types.EstimatedUpdateTime as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.NodesUpdated as Types
import qualified Network.AWS.ElastiCache.Types.ReplicationGroupId as Types
import qualified Network.AWS.ElastiCache.Types.ServiceUpdateName as Types
import qualified Network.AWS.ElastiCache.Types.ServiceUpdateSeverity as Types
import qualified Network.AWS.ElastiCache.Types.ServiceUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.ServiceUpdateType as Types
import qualified Network.AWS.ElastiCache.Types.SlaMet as Types
import qualified Network.AWS.ElastiCache.Types.UpdateActionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the service update for a specific replication group
--
-- /See:/ 'mkUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | The ID of the cache cluster
    cacheClusterId :: Core.Maybe Types.CacheClusterId,
    -- | The status of the service update on the cache node
    cacheNodeUpdateStatus :: Core.Maybe [Types.CacheNodeUpdateStatus],
    -- | The Elasticache engine to which the update applies. Either Redis or Memcached
    engine :: Core.Maybe Types.Engine,
    -- | The estimated length of time for the update to complete
    estimatedUpdateTime :: Core.Maybe Types.EstimatedUpdateTime,
    -- | The status of the service update on the node group
    nodeGroupUpdateStatus :: Core.Maybe [Types.NodeGroupUpdateStatus],
    -- | The progress of the service update on the replication group
    nodesUpdated :: Core.Maybe Types.NodesUpdated,
    -- | The ID of the replication group
    replicationGroupId :: Core.Maybe Types.ReplicationGroupId,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Types.ServiceUpdateName,
    -- | The recommended date to apply the service update to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
    serviceUpdateRecommendedApplyByDate :: Core.Maybe Core.UTCTime,
    -- | The date the update is first available
    serviceUpdateReleaseDate :: Core.Maybe Core.UTCTime,
    -- | The severity of the service update
    serviceUpdateSeverity :: Core.Maybe Types.ServiceUpdateSeverity,
    -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe Types.ServiceUpdateStatus,
    -- | Reflects the nature of the service update
    serviceUpdateType :: Core.Maybe Types.ServiceUpdateType,
    -- | If yes, all nodes in the replication group have been updated by the recommended apply-by date. If no, at least one node in the replication group have not been updated by the recommended apply-by date. If N/A, the replication group was created after the recommended apply-by date.
    slaMet :: Core.Maybe Types.SlaMet,
    -- | The date that the service update is available to a replication group
    updateActionAvailableDate :: Core.Maybe Core.UTCTime,
    -- | The status of the update action
    updateActionStatus :: Core.Maybe Types.UpdateActionStatus,
    -- | The date when the UpdateActionStatus was last modified
    updateActionStatusModifiedDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateAction' value with any optional fields omitted.
mkUpdateAction ::
  UpdateAction
mkUpdateAction =
  UpdateAction'
    { cacheClusterId = Core.Nothing,
      cacheNodeUpdateStatus = Core.Nothing,
      engine = Core.Nothing,
      estimatedUpdateTime = Core.Nothing,
      nodeGroupUpdateStatus = Core.Nothing,
      nodesUpdated = Core.Nothing,
      replicationGroupId = Core.Nothing,
      serviceUpdateName = Core.Nothing,
      serviceUpdateRecommendedApplyByDate = Core.Nothing,
      serviceUpdateReleaseDate = Core.Nothing,
      serviceUpdateSeverity = Core.Nothing,
      serviceUpdateStatus = Core.Nothing,
      serviceUpdateType = Core.Nothing,
      slaMet = Core.Nothing,
      updateActionAvailableDate = Core.Nothing,
      updateActionStatus = Core.Nothing,
      updateActionStatusModifiedDate = Core.Nothing
    }

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCacheClusterId :: Lens.Lens' UpdateAction (Core.Maybe Types.CacheClusterId)
uaCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED uaCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The status of the service update on the cache node
--
-- /Note:/ Consider using 'cacheNodeUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCacheNodeUpdateStatus :: Lens.Lens' UpdateAction (Core.Maybe [Types.CacheNodeUpdateStatus])
uaCacheNodeUpdateStatus = Lens.field @"cacheNodeUpdateStatus"
{-# DEPRECATED uaCacheNodeUpdateStatus "Use generic-lens or generic-optics with 'cacheNodeUpdateStatus' instead." #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEngine :: Lens.Lens' UpdateAction (Core.Maybe Types.Engine)
uaEngine = Lens.field @"engine"
{-# DEPRECATED uaEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The estimated length of time for the update to complete
--
-- /Note:/ Consider using 'estimatedUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEstimatedUpdateTime :: Lens.Lens' UpdateAction (Core.Maybe Types.EstimatedUpdateTime)
uaEstimatedUpdateTime = Lens.field @"estimatedUpdateTime"
{-# DEPRECATED uaEstimatedUpdateTime "Use generic-lens or generic-optics with 'estimatedUpdateTime' instead." #-}

-- | The status of the service update on the node group
--
-- /Note:/ Consider using 'nodeGroupUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaNodeGroupUpdateStatus :: Lens.Lens' UpdateAction (Core.Maybe [Types.NodeGroupUpdateStatus])
uaNodeGroupUpdateStatus = Lens.field @"nodeGroupUpdateStatus"
{-# DEPRECATED uaNodeGroupUpdateStatus "Use generic-lens or generic-optics with 'nodeGroupUpdateStatus' instead." #-}

-- | The progress of the service update on the replication group
--
-- /Note:/ Consider using 'nodesUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaNodesUpdated :: Lens.Lens' UpdateAction (Core.Maybe Types.NodesUpdated)
uaNodesUpdated = Lens.field @"nodesUpdated"
{-# DEPRECATED uaNodesUpdated "Use generic-lens or generic-optics with 'nodesUpdated' instead." #-}

-- | The ID of the replication group
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaReplicationGroupId :: Lens.Lens' UpdateAction (Core.Maybe Types.ReplicationGroupId)
uaReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED uaReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateName :: Lens.Lens' UpdateAction (Core.Maybe Types.ServiceUpdateName)
uaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED uaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The recommended date to apply the service update to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
--
-- /Note:/ Consider using 'serviceUpdateRecommendedApplyByDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateRecommendedApplyByDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
uaServiceUpdateRecommendedApplyByDate = Lens.field @"serviceUpdateRecommendedApplyByDate"
{-# DEPRECATED uaServiceUpdateRecommendedApplyByDate "Use generic-lens or generic-optics with 'serviceUpdateRecommendedApplyByDate' instead." #-}

-- | The date the update is first available
--
-- /Note:/ Consider using 'serviceUpdateReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateReleaseDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
uaServiceUpdateReleaseDate = Lens.field @"serviceUpdateReleaseDate"
{-# DEPRECATED uaServiceUpdateReleaseDate "Use generic-lens or generic-optics with 'serviceUpdateReleaseDate' instead." #-}

-- | The severity of the service update
--
-- /Note:/ Consider using 'serviceUpdateSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateSeverity :: Lens.Lens' UpdateAction (Core.Maybe Types.ServiceUpdateSeverity)
uaServiceUpdateSeverity = Lens.field @"serviceUpdateSeverity"
{-# DEPRECATED uaServiceUpdateSeverity "Use generic-lens or generic-optics with 'serviceUpdateSeverity' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateStatus :: Lens.Lens' UpdateAction (Core.Maybe Types.ServiceUpdateStatus)
uaServiceUpdateStatus = Lens.field @"serviceUpdateStatus"
{-# DEPRECATED uaServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

-- | Reflects the nature of the service update
--
-- /Note:/ Consider using 'serviceUpdateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateType :: Lens.Lens' UpdateAction (Core.Maybe Types.ServiceUpdateType)
uaServiceUpdateType = Lens.field @"serviceUpdateType"
{-# DEPRECATED uaServiceUpdateType "Use generic-lens or generic-optics with 'serviceUpdateType' instead." #-}

-- | If yes, all nodes in the replication group have been updated by the recommended apply-by date. If no, at least one node in the replication group have not been updated by the recommended apply-by date. If N/A, the replication group was created after the recommended apply-by date.
--
-- /Note:/ Consider using 'slaMet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaSlaMet :: Lens.Lens' UpdateAction (Core.Maybe Types.SlaMet)
uaSlaMet = Lens.field @"slaMet"
{-# DEPRECATED uaSlaMet "Use generic-lens or generic-optics with 'slaMet' instead." #-}

-- | The date that the service update is available to a replication group
--
-- /Note:/ Consider using 'updateActionAvailableDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaUpdateActionAvailableDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
uaUpdateActionAvailableDate = Lens.field @"updateActionAvailableDate"
{-# DEPRECATED uaUpdateActionAvailableDate "Use generic-lens or generic-optics with 'updateActionAvailableDate' instead." #-}

-- | The status of the update action
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaUpdateActionStatus :: Lens.Lens' UpdateAction (Core.Maybe Types.UpdateActionStatus)
uaUpdateActionStatus = Lens.field @"updateActionStatus"
{-# DEPRECATED uaUpdateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead." #-}

-- | The date when the UpdateActionStatus was last modified
--
-- /Note:/ Consider using 'updateActionStatusModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaUpdateActionStatusModifiedDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
uaUpdateActionStatusModifiedDate = Lens.field @"updateActionStatusModifiedDate"
{-# DEPRECATED uaUpdateActionStatusModifiedDate "Use generic-lens or generic-optics with 'updateActionStatusModifiedDate' instead." #-}

instance Core.FromXML UpdateAction where
  parseXML x =
    UpdateAction'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> ( x Core..@? "CacheNodeUpdateStatus"
                   Core..<@> Core.parseXMLList "CacheNodeUpdateStatus"
               )
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EstimatedUpdateTime")
      Core.<*> ( x Core..@? "NodeGroupUpdateStatus"
                   Core..<@> Core.parseXMLList "NodeGroupUpdateStatus"
               )
      Core.<*> (x Core..@? "NodesUpdated")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "ServiceUpdateName")
      Core.<*> (x Core..@? "ServiceUpdateRecommendedApplyByDate")
      Core.<*> (x Core..@? "ServiceUpdateReleaseDate")
      Core.<*> (x Core..@? "ServiceUpdateSeverity")
      Core.<*> (x Core..@? "ServiceUpdateStatus")
      Core.<*> (x Core..@? "ServiceUpdateType")
      Core.<*> (x Core..@? "SlaMet")
      Core.<*> (x Core..@? "UpdateActionAvailableDate")
      Core.<*> (x Core..@? "UpdateActionStatus")
      Core.<*> (x Core..@? "UpdateActionStatusModifiedDate")
