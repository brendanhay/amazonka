{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateAction where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
import Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateType
import Network.AWS.ElastiCache.Types.SlaMet
import Network.AWS.ElastiCache.Types.UpdateActionStatus
import qualified Network.AWS.Lens as Lens

-- | The status of the service update for a specific replication group
--
-- /See:/ 'newUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | The severity of the service update
    serviceUpdateSeverity :: Core.Maybe ServiceUpdateSeverity,
    -- | The date the update is first available
    serviceUpdateReleaseDate :: Core.Maybe Core.ISO8601,
    -- | The ID of the replication group
    replicationGroupId :: Core.Maybe Core.Text,
    -- | The status of the update action
    updateActionStatus :: Core.Maybe UpdateActionStatus,
    -- | The ID of the cache cluster
    cacheClusterId :: Core.Maybe Core.Text,
    -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe ServiceUpdateStatus,
    -- | If yes, all nodes in the replication group have been updated by the
    -- recommended apply-by date. If no, at least one node in the replication
    -- group have not been updated by the recommended apply-by date. If N\/A,
    -- the replication group was created after the recommended apply-by date.
    slaMet :: Core.Maybe SlaMet,
    -- | The date that the service update is available to a replication group
    updateActionAvailableDate :: Core.Maybe Core.ISO8601,
    -- | The status of the service update on the node group
    nodeGroupUpdateStatus :: Core.Maybe [NodeGroupUpdateStatus],
    -- | The recommended date to apply the service update to ensure compliance.
    -- For information on compliance, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
    serviceUpdateRecommendedApplyByDate :: Core.Maybe Core.ISO8601,
    -- | Reflects the nature of the service update
    serviceUpdateType :: Core.Maybe ServiceUpdateType,
    -- | The status of the service update on the cache node
    cacheNodeUpdateStatus :: Core.Maybe [CacheNodeUpdateStatus],
    -- | The progress of the service update on the replication group
    nodesUpdated :: Core.Maybe Core.Text,
    -- | The estimated length of time for the update to complete
    estimatedUpdateTime :: Core.Maybe Core.Text,
    -- | The date when the UpdateActionStatus was last modified
    updateActionStatusModifiedDate :: Core.Maybe Core.ISO8601,
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Core.Maybe Core.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdateSeverity', 'updateAction_serviceUpdateSeverity' - The severity of the service update
--
-- 'serviceUpdateReleaseDate', 'updateAction_serviceUpdateReleaseDate' - The date the update is first available
--
-- 'replicationGroupId', 'updateAction_replicationGroupId' - The ID of the replication group
--
-- 'updateActionStatus', 'updateAction_updateActionStatus' - The status of the update action
--
-- 'cacheClusterId', 'updateAction_cacheClusterId' - The ID of the cache cluster
--
-- 'serviceUpdateStatus', 'updateAction_serviceUpdateStatus' - The status of the service update
--
-- 'slaMet', 'updateAction_slaMet' - If yes, all nodes in the replication group have been updated by the
-- recommended apply-by date. If no, at least one node in the replication
-- group have not been updated by the recommended apply-by date. If N\/A,
-- the replication group was created after the recommended apply-by date.
--
-- 'updateActionAvailableDate', 'updateAction_updateActionAvailableDate' - The date that the service update is available to a replication group
--
-- 'nodeGroupUpdateStatus', 'updateAction_nodeGroupUpdateStatus' - The status of the service update on the node group
--
-- 'serviceUpdateRecommendedApplyByDate', 'updateAction_serviceUpdateRecommendedApplyByDate' - The recommended date to apply the service update to ensure compliance.
-- For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
--
-- 'serviceUpdateType', 'updateAction_serviceUpdateType' - Reflects the nature of the service update
--
-- 'cacheNodeUpdateStatus', 'updateAction_cacheNodeUpdateStatus' - The status of the service update on the cache node
--
-- 'nodesUpdated', 'updateAction_nodesUpdated' - The progress of the service update on the replication group
--
-- 'estimatedUpdateTime', 'updateAction_estimatedUpdateTime' - The estimated length of time for the update to complete
--
-- 'updateActionStatusModifiedDate', 'updateAction_updateActionStatusModifiedDate' - The date when the UpdateActionStatus was last modified
--
-- 'engine', 'updateAction_engine' - The Elasticache engine to which the update applies. Either Redis or
-- Memcached
--
-- 'serviceUpdateName', 'updateAction_serviceUpdateName' - The unique ID of the service update
newUpdateAction ::
  UpdateAction
newUpdateAction =
  UpdateAction'
    { serviceUpdateSeverity = Core.Nothing,
      serviceUpdateReleaseDate = Core.Nothing,
      replicationGroupId = Core.Nothing,
      updateActionStatus = Core.Nothing,
      cacheClusterId = Core.Nothing,
      serviceUpdateStatus = Core.Nothing,
      slaMet = Core.Nothing,
      updateActionAvailableDate = Core.Nothing,
      nodeGroupUpdateStatus = Core.Nothing,
      serviceUpdateRecommendedApplyByDate = Core.Nothing,
      serviceUpdateType = Core.Nothing,
      cacheNodeUpdateStatus = Core.Nothing,
      nodesUpdated = Core.Nothing,
      estimatedUpdateTime = Core.Nothing,
      updateActionStatusModifiedDate = Core.Nothing,
      engine = Core.Nothing,
      serviceUpdateName = Core.Nothing
    }

-- | The severity of the service update
updateAction_serviceUpdateSeverity :: Lens.Lens' UpdateAction (Core.Maybe ServiceUpdateSeverity)
updateAction_serviceUpdateSeverity = Lens.lens (\UpdateAction' {serviceUpdateSeverity} -> serviceUpdateSeverity) (\s@UpdateAction' {} a -> s {serviceUpdateSeverity = a} :: UpdateAction)

-- | The date the update is first available
updateAction_serviceUpdateReleaseDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
updateAction_serviceUpdateReleaseDate = Lens.lens (\UpdateAction' {serviceUpdateReleaseDate} -> serviceUpdateReleaseDate) (\s@UpdateAction' {} a -> s {serviceUpdateReleaseDate = a} :: UpdateAction) Core.. Lens.mapping Core._Time

-- | The ID of the replication group
updateAction_replicationGroupId :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_replicationGroupId = Lens.lens (\UpdateAction' {replicationGroupId} -> replicationGroupId) (\s@UpdateAction' {} a -> s {replicationGroupId = a} :: UpdateAction)

-- | The status of the update action
updateAction_updateActionStatus :: Lens.Lens' UpdateAction (Core.Maybe UpdateActionStatus)
updateAction_updateActionStatus = Lens.lens (\UpdateAction' {updateActionStatus} -> updateActionStatus) (\s@UpdateAction' {} a -> s {updateActionStatus = a} :: UpdateAction)

-- | The ID of the cache cluster
updateAction_cacheClusterId :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_cacheClusterId = Lens.lens (\UpdateAction' {cacheClusterId} -> cacheClusterId) (\s@UpdateAction' {} a -> s {cacheClusterId = a} :: UpdateAction)

-- | The status of the service update
updateAction_serviceUpdateStatus :: Lens.Lens' UpdateAction (Core.Maybe ServiceUpdateStatus)
updateAction_serviceUpdateStatus = Lens.lens (\UpdateAction' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@UpdateAction' {} a -> s {serviceUpdateStatus = a} :: UpdateAction)

-- | If yes, all nodes in the replication group have been updated by the
-- recommended apply-by date. If no, at least one node in the replication
-- group have not been updated by the recommended apply-by date. If N\/A,
-- the replication group was created after the recommended apply-by date.
updateAction_slaMet :: Lens.Lens' UpdateAction (Core.Maybe SlaMet)
updateAction_slaMet = Lens.lens (\UpdateAction' {slaMet} -> slaMet) (\s@UpdateAction' {} a -> s {slaMet = a} :: UpdateAction)

-- | The date that the service update is available to a replication group
updateAction_updateActionAvailableDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
updateAction_updateActionAvailableDate = Lens.lens (\UpdateAction' {updateActionAvailableDate} -> updateActionAvailableDate) (\s@UpdateAction' {} a -> s {updateActionAvailableDate = a} :: UpdateAction) Core.. Lens.mapping Core._Time

-- | The status of the service update on the node group
updateAction_nodeGroupUpdateStatus :: Lens.Lens' UpdateAction (Core.Maybe [NodeGroupUpdateStatus])
updateAction_nodeGroupUpdateStatus = Lens.lens (\UpdateAction' {nodeGroupUpdateStatus} -> nodeGroupUpdateStatus) (\s@UpdateAction' {} a -> s {nodeGroupUpdateStatus = a} :: UpdateAction) Core.. Lens.mapping Lens._Coerce

-- | The recommended date to apply the service update to ensure compliance.
-- For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
updateAction_serviceUpdateRecommendedApplyByDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
updateAction_serviceUpdateRecommendedApplyByDate = Lens.lens (\UpdateAction' {serviceUpdateRecommendedApplyByDate} -> serviceUpdateRecommendedApplyByDate) (\s@UpdateAction' {} a -> s {serviceUpdateRecommendedApplyByDate = a} :: UpdateAction) Core.. Lens.mapping Core._Time

-- | Reflects the nature of the service update
updateAction_serviceUpdateType :: Lens.Lens' UpdateAction (Core.Maybe ServiceUpdateType)
updateAction_serviceUpdateType = Lens.lens (\UpdateAction' {serviceUpdateType} -> serviceUpdateType) (\s@UpdateAction' {} a -> s {serviceUpdateType = a} :: UpdateAction)

-- | The status of the service update on the cache node
updateAction_cacheNodeUpdateStatus :: Lens.Lens' UpdateAction (Core.Maybe [CacheNodeUpdateStatus])
updateAction_cacheNodeUpdateStatus = Lens.lens (\UpdateAction' {cacheNodeUpdateStatus} -> cacheNodeUpdateStatus) (\s@UpdateAction' {} a -> s {cacheNodeUpdateStatus = a} :: UpdateAction) Core.. Lens.mapping Lens._Coerce

-- | The progress of the service update on the replication group
updateAction_nodesUpdated :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_nodesUpdated = Lens.lens (\UpdateAction' {nodesUpdated} -> nodesUpdated) (\s@UpdateAction' {} a -> s {nodesUpdated = a} :: UpdateAction)

-- | The estimated length of time for the update to complete
updateAction_estimatedUpdateTime :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_estimatedUpdateTime = Lens.lens (\UpdateAction' {estimatedUpdateTime} -> estimatedUpdateTime) (\s@UpdateAction' {} a -> s {estimatedUpdateTime = a} :: UpdateAction)

-- | The date when the UpdateActionStatus was last modified
updateAction_updateActionStatusModifiedDate :: Lens.Lens' UpdateAction (Core.Maybe Core.UTCTime)
updateAction_updateActionStatusModifiedDate = Lens.lens (\UpdateAction' {updateActionStatusModifiedDate} -> updateActionStatusModifiedDate) (\s@UpdateAction' {} a -> s {updateActionStatusModifiedDate = a} :: UpdateAction) Core.. Lens.mapping Core._Time

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
updateAction_engine :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_engine = Lens.lens (\UpdateAction' {engine} -> engine) (\s@UpdateAction' {} a -> s {engine = a} :: UpdateAction)

-- | The unique ID of the service update
updateAction_serviceUpdateName :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_serviceUpdateName = Lens.lens (\UpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@UpdateAction' {} a -> s {serviceUpdateName = a} :: UpdateAction)

instance Core.FromXML UpdateAction where
  parseXML x =
    UpdateAction'
      Core.<$> (x Core..@? "ServiceUpdateSeverity")
      Core.<*> (x Core..@? "ServiceUpdateReleaseDate")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "UpdateActionStatus")
      Core.<*> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "ServiceUpdateStatus")
      Core.<*> (x Core..@? "SlaMet")
      Core.<*> (x Core..@? "UpdateActionAvailableDate")
      Core.<*> ( x Core..@? "NodeGroupUpdateStatus"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "NodeGroupUpdateStatus")
               )
      Core.<*> (x Core..@? "ServiceUpdateRecommendedApplyByDate")
      Core.<*> (x Core..@? "ServiceUpdateType")
      Core.<*> ( x Core..@? "CacheNodeUpdateStatus"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CacheNodeUpdateStatus")
               )
      Core.<*> (x Core..@? "NodesUpdated")
      Core.<*> (x Core..@? "EstimatedUpdateTime")
      Core.<*> (x Core..@? "UpdateActionStatusModifiedDate")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "ServiceUpdateName")

instance Core.Hashable UpdateAction

instance Core.NFData UpdateAction
