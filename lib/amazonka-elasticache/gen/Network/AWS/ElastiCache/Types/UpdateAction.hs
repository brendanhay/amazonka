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
    uaServiceUpdateType,
    uaSlaMet,
    uaCacheClusterId,
    uaServiceUpdateName,
    uaUpdateActionStatus,
    uaEngine,
    uaNodesUpdated,
    uaUpdateActionStatusModifiedDate,
    uaServiceUpdateReleaseDate,
    uaCacheNodeUpdateStatus,
    uaServiceUpdateSeverity,
    uaNodeGroupUpdateStatus,
    uaServiceUpdateRecommendedApplyByDate,
    uaUpdateActionAvailableDate,
    uaServiceUpdateStatus,
    uaEstimatedUpdateTime,
    uaReplicationGroupId,
  )
where

import Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
import Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateType
import Network.AWS.ElastiCache.Types.SlaMet
import Network.AWS.ElastiCache.Types.UpdateActionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the service update for a specific replication group
--
-- /See:/ 'mkUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | Reflects the nature of the service update
    serviceUpdateType :: Lude.Maybe ServiceUpdateType,
    -- | If yes, all nodes in the replication group have been updated by the recommended apply-by date. If no, at least one node in the replication group have not been updated by the recommended apply-by date. If N/A, the replication group was created after the recommended apply-by date.
    slaMet :: Lude.Maybe SlaMet,
    -- | The ID of the cache cluster
    cacheClusterId :: Lude.Maybe Lude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Lude.Maybe Lude.Text,
    -- | The status of the update action
    updateActionStatus :: Lude.Maybe UpdateActionStatus,
    -- | The Elasticache engine to which the update applies. Either Redis or Memcached
    engine :: Lude.Maybe Lude.Text,
    -- | The progress of the service update on the replication group
    nodesUpdated :: Lude.Maybe Lude.Text,
    -- | The date when the UpdateActionStatus was last modified
    updateActionStatusModifiedDate :: Lude.Maybe Lude.DateTime,
    -- | The date the update is first available
    serviceUpdateReleaseDate :: Lude.Maybe Lude.DateTime,
    -- | The status of the service update on the cache node
    cacheNodeUpdateStatus :: Lude.Maybe [CacheNodeUpdateStatus],
    -- | The severity of the service update
    serviceUpdateSeverity :: Lude.Maybe ServiceUpdateSeverity,
    -- | The status of the service update on the node group
    nodeGroupUpdateStatus :: Lude.Maybe [NodeGroupUpdateStatus],
    -- | The recommended date to apply the service update to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
    serviceUpdateRecommendedApplyByDate :: Lude.Maybe Lude.DateTime,
    -- | The date that the service update is available to a replication group
    updateActionAvailableDate :: Lude.Maybe Lude.DateTime,
    -- | The status of the service update
    serviceUpdateStatus :: Lude.Maybe ServiceUpdateStatus,
    -- | The estimated length of time for the update to complete
    estimatedUpdateTime :: Lude.Maybe Lude.Text,
    -- | The ID of the replication group
    replicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAction' with the minimum fields required to make a request.
--
-- * 'serviceUpdateType' - Reflects the nature of the service update
-- * 'slaMet' - If yes, all nodes in the replication group have been updated by the recommended apply-by date. If no, at least one node in the replication group have not been updated by the recommended apply-by date. If N/A, the replication group was created after the recommended apply-by date.
-- * 'cacheClusterId' - The ID of the cache cluster
-- * 'serviceUpdateName' - The unique ID of the service update
-- * 'updateActionStatus' - The status of the update action
-- * 'engine' - The Elasticache engine to which the update applies. Either Redis or Memcached
-- * 'nodesUpdated' - The progress of the service update on the replication group
-- * 'updateActionStatusModifiedDate' - The date when the UpdateActionStatus was last modified
-- * 'serviceUpdateReleaseDate' - The date the update is first available
-- * 'cacheNodeUpdateStatus' - The status of the service update on the cache node
-- * 'serviceUpdateSeverity' - The severity of the service update
-- * 'nodeGroupUpdateStatus' - The status of the service update on the node group
-- * 'serviceUpdateRecommendedApplyByDate' - The recommended date to apply the service update to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
-- * 'updateActionAvailableDate' - The date that the service update is available to a replication group
-- * 'serviceUpdateStatus' - The status of the service update
-- * 'estimatedUpdateTime' - The estimated length of time for the update to complete
-- * 'replicationGroupId' - The ID of the replication group
mkUpdateAction ::
  UpdateAction
mkUpdateAction =
  UpdateAction'
    { serviceUpdateType = Lude.Nothing,
      slaMet = Lude.Nothing,
      cacheClusterId = Lude.Nothing,
      serviceUpdateName = Lude.Nothing,
      updateActionStatus = Lude.Nothing,
      engine = Lude.Nothing,
      nodesUpdated = Lude.Nothing,
      updateActionStatusModifiedDate = Lude.Nothing,
      serviceUpdateReleaseDate = Lude.Nothing,
      cacheNodeUpdateStatus = Lude.Nothing,
      serviceUpdateSeverity = Lude.Nothing,
      nodeGroupUpdateStatus = Lude.Nothing,
      serviceUpdateRecommendedApplyByDate = Lude.Nothing,
      updateActionAvailableDate = Lude.Nothing,
      serviceUpdateStatus = Lude.Nothing,
      estimatedUpdateTime = Lude.Nothing,
      replicationGroupId = Lude.Nothing
    }

-- | Reflects the nature of the service update
--
-- /Note:/ Consider using 'serviceUpdateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateType :: Lens.Lens' UpdateAction (Lude.Maybe ServiceUpdateType)
uaServiceUpdateType = Lens.lens (serviceUpdateType :: UpdateAction -> Lude.Maybe ServiceUpdateType) (\s a -> s {serviceUpdateType = a} :: UpdateAction)
{-# DEPRECATED uaServiceUpdateType "Use generic-lens or generic-optics with 'serviceUpdateType' instead." #-}

-- | If yes, all nodes in the replication group have been updated by the recommended apply-by date. If no, at least one node in the replication group have not been updated by the recommended apply-by date. If N/A, the replication group was created after the recommended apply-by date.
--
-- /Note:/ Consider using 'slaMet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaSlaMet :: Lens.Lens' UpdateAction (Lude.Maybe SlaMet)
uaSlaMet = Lens.lens (slaMet :: UpdateAction -> Lude.Maybe SlaMet) (\s a -> s {slaMet = a} :: UpdateAction)
{-# DEPRECATED uaSlaMet "Use generic-lens or generic-optics with 'slaMet' instead." #-}

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCacheClusterId :: Lens.Lens' UpdateAction (Lude.Maybe Lude.Text)
uaCacheClusterId = Lens.lens (cacheClusterId :: UpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: UpdateAction)
{-# DEPRECATED uaCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateName :: Lens.Lens' UpdateAction (Lude.Maybe Lude.Text)
uaServiceUpdateName = Lens.lens (serviceUpdateName :: UpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateName = a} :: UpdateAction)
{-# DEPRECATED uaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The status of the update action
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaUpdateActionStatus :: Lens.Lens' UpdateAction (Lude.Maybe UpdateActionStatus)
uaUpdateActionStatus = Lens.lens (updateActionStatus :: UpdateAction -> Lude.Maybe UpdateActionStatus) (\s a -> s {updateActionStatus = a} :: UpdateAction)
{-# DEPRECATED uaUpdateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead." #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEngine :: Lens.Lens' UpdateAction (Lude.Maybe Lude.Text)
uaEngine = Lens.lens (engine :: UpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: UpdateAction)
{-# DEPRECATED uaEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The progress of the service update on the replication group
--
-- /Note:/ Consider using 'nodesUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaNodesUpdated :: Lens.Lens' UpdateAction (Lude.Maybe Lude.Text)
uaNodesUpdated = Lens.lens (nodesUpdated :: UpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {nodesUpdated = a} :: UpdateAction)
{-# DEPRECATED uaNodesUpdated "Use generic-lens or generic-optics with 'nodesUpdated' instead." #-}

-- | The date when the UpdateActionStatus was last modified
--
-- /Note:/ Consider using 'updateActionStatusModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaUpdateActionStatusModifiedDate :: Lens.Lens' UpdateAction (Lude.Maybe Lude.DateTime)
uaUpdateActionStatusModifiedDate = Lens.lens (updateActionStatusModifiedDate :: UpdateAction -> Lude.Maybe Lude.DateTime) (\s a -> s {updateActionStatusModifiedDate = a} :: UpdateAction)
{-# DEPRECATED uaUpdateActionStatusModifiedDate "Use generic-lens or generic-optics with 'updateActionStatusModifiedDate' instead." #-}

-- | The date the update is first available
--
-- /Note:/ Consider using 'serviceUpdateReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateReleaseDate :: Lens.Lens' UpdateAction (Lude.Maybe Lude.DateTime)
uaServiceUpdateReleaseDate = Lens.lens (serviceUpdateReleaseDate :: UpdateAction -> Lude.Maybe Lude.DateTime) (\s a -> s {serviceUpdateReleaseDate = a} :: UpdateAction)
{-# DEPRECATED uaServiceUpdateReleaseDate "Use generic-lens or generic-optics with 'serviceUpdateReleaseDate' instead." #-}

-- | The status of the service update on the cache node
--
-- /Note:/ Consider using 'cacheNodeUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCacheNodeUpdateStatus :: Lens.Lens' UpdateAction (Lude.Maybe [CacheNodeUpdateStatus])
uaCacheNodeUpdateStatus = Lens.lens (cacheNodeUpdateStatus :: UpdateAction -> Lude.Maybe [CacheNodeUpdateStatus]) (\s a -> s {cacheNodeUpdateStatus = a} :: UpdateAction)
{-# DEPRECATED uaCacheNodeUpdateStatus "Use generic-lens or generic-optics with 'cacheNodeUpdateStatus' instead." #-}

-- | The severity of the service update
--
-- /Note:/ Consider using 'serviceUpdateSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateSeverity :: Lens.Lens' UpdateAction (Lude.Maybe ServiceUpdateSeverity)
uaServiceUpdateSeverity = Lens.lens (serviceUpdateSeverity :: UpdateAction -> Lude.Maybe ServiceUpdateSeverity) (\s a -> s {serviceUpdateSeverity = a} :: UpdateAction)
{-# DEPRECATED uaServiceUpdateSeverity "Use generic-lens or generic-optics with 'serviceUpdateSeverity' instead." #-}

-- | The status of the service update on the node group
--
-- /Note:/ Consider using 'nodeGroupUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaNodeGroupUpdateStatus :: Lens.Lens' UpdateAction (Lude.Maybe [NodeGroupUpdateStatus])
uaNodeGroupUpdateStatus = Lens.lens (nodeGroupUpdateStatus :: UpdateAction -> Lude.Maybe [NodeGroupUpdateStatus]) (\s a -> s {nodeGroupUpdateStatus = a} :: UpdateAction)
{-# DEPRECATED uaNodeGroupUpdateStatus "Use generic-lens or generic-optics with 'nodeGroupUpdateStatus' instead." #-}

-- | The recommended date to apply the service update to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
--
-- /Note:/ Consider using 'serviceUpdateRecommendedApplyByDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateRecommendedApplyByDate :: Lens.Lens' UpdateAction (Lude.Maybe Lude.DateTime)
uaServiceUpdateRecommendedApplyByDate = Lens.lens (serviceUpdateRecommendedApplyByDate :: UpdateAction -> Lude.Maybe Lude.DateTime) (\s a -> s {serviceUpdateRecommendedApplyByDate = a} :: UpdateAction)
{-# DEPRECATED uaServiceUpdateRecommendedApplyByDate "Use generic-lens or generic-optics with 'serviceUpdateRecommendedApplyByDate' instead." #-}

-- | The date that the service update is available to a replication group
--
-- /Note:/ Consider using 'updateActionAvailableDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaUpdateActionAvailableDate :: Lens.Lens' UpdateAction (Lude.Maybe Lude.DateTime)
uaUpdateActionAvailableDate = Lens.lens (updateActionAvailableDate :: UpdateAction -> Lude.Maybe Lude.DateTime) (\s a -> s {updateActionAvailableDate = a} :: UpdateAction)
{-# DEPRECATED uaUpdateActionAvailableDate "Use generic-lens or generic-optics with 'updateActionAvailableDate' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServiceUpdateStatus :: Lens.Lens' UpdateAction (Lude.Maybe ServiceUpdateStatus)
uaServiceUpdateStatus = Lens.lens (serviceUpdateStatus :: UpdateAction -> Lude.Maybe ServiceUpdateStatus) (\s a -> s {serviceUpdateStatus = a} :: UpdateAction)
{-# DEPRECATED uaServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

-- | The estimated length of time for the update to complete
--
-- /Note:/ Consider using 'estimatedUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEstimatedUpdateTime :: Lens.Lens' UpdateAction (Lude.Maybe Lude.Text)
uaEstimatedUpdateTime = Lens.lens (estimatedUpdateTime :: UpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {estimatedUpdateTime = a} :: UpdateAction)
{-# DEPRECATED uaEstimatedUpdateTime "Use generic-lens or generic-optics with 'estimatedUpdateTime' instead." #-}

-- | The ID of the replication group
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaReplicationGroupId :: Lens.Lens' UpdateAction (Lude.Maybe Lude.Text)
uaReplicationGroupId = Lens.lens (replicationGroupId :: UpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: UpdateAction)
{-# DEPRECATED uaReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.FromXML UpdateAction where
  parseXML x =
    UpdateAction'
      Lude.<$> (x Lude..@? "ServiceUpdateType")
      Lude.<*> (x Lude..@? "SlaMet")
      Lude.<*> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "ServiceUpdateName")
      Lude.<*> (x Lude..@? "UpdateActionStatus")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "NodesUpdated")
      Lude.<*> (x Lude..@? "UpdateActionStatusModifiedDate")
      Lude.<*> (x Lude..@? "ServiceUpdateReleaseDate")
      Lude.<*> ( x Lude..@? "CacheNodeUpdateStatus" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheNodeUpdateStatus")
               )
      Lude.<*> (x Lude..@? "ServiceUpdateSeverity")
      Lude.<*> ( x Lude..@? "NodeGroupUpdateStatus" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "NodeGroupUpdateStatus")
               )
      Lude.<*> (x Lude..@? "ServiceUpdateRecommendedApplyByDate")
      Lude.<*> (x Lude..@? "UpdateActionAvailableDate")
      Lude.<*> (x Lude..@? "ServiceUpdateStatus")
      Lude.<*> (x Lude..@? "EstimatedUpdateTime")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
