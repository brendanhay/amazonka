{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
import Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateType
import Network.AWS.ElastiCache.Types.SlaMet
import Network.AWS.ElastiCache.Types.UpdateActionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of the service update for a specific replication group
--
-- /See:/ 'newUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | The severity of the service update
    serviceUpdateSeverity :: Prelude.Maybe ServiceUpdateSeverity,
    -- | The date the update is first available
    serviceUpdateReleaseDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the replication group
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The status of the update action
    updateActionStatus :: Prelude.Maybe UpdateActionStatus,
    -- | The ID of the cache cluster
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe ServiceUpdateStatus,
    -- | If yes, all nodes in the replication group have been updated by the
    -- recommended apply-by date. If no, at least one node in the replication
    -- group have not been updated by the recommended apply-by date. If N\/A,
    -- the replication group was created after the recommended apply-by date.
    slaMet :: Prelude.Maybe SlaMet,
    -- | The date that the service update is available to a replication group
    updateActionAvailableDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The status of the service update on the node group
    nodeGroupUpdateStatus :: Prelude.Maybe [NodeGroupUpdateStatus],
    -- | The recommended date to apply the service update to ensure compliance.
    -- For information on compliance, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
    serviceUpdateRecommendedApplyByDate :: Prelude.Maybe Prelude.ISO8601,
    -- | Reflects the nature of the service update
    serviceUpdateType :: Prelude.Maybe ServiceUpdateType,
    -- | The status of the service update on the cache node
    cacheNodeUpdateStatus :: Prelude.Maybe [CacheNodeUpdateStatus],
    -- | The progress of the service update on the replication group
    nodesUpdated :: Prelude.Maybe Prelude.Text,
    -- | The estimated length of time for the update to complete
    estimatedUpdateTime :: Prelude.Maybe Prelude.Text,
    -- | The date when the UpdateActionStatus was last modified
    updateActionStatusModifiedDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { serviceUpdateSeverity =
        Prelude.Nothing,
      serviceUpdateReleaseDate = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      updateActionStatus = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing,
      slaMet = Prelude.Nothing,
      updateActionAvailableDate = Prelude.Nothing,
      nodeGroupUpdateStatus = Prelude.Nothing,
      serviceUpdateRecommendedApplyByDate =
        Prelude.Nothing,
      serviceUpdateType = Prelude.Nothing,
      cacheNodeUpdateStatus = Prelude.Nothing,
      nodesUpdated = Prelude.Nothing,
      estimatedUpdateTime = Prelude.Nothing,
      updateActionStatusModifiedDate = Prelude.Nothing,
      engine = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing
    }

-- | The severity of the service update
updateAction_serviceUpdateSeverity :: Lens.Lens' UpdateAction (Prelude.Maybe ServiceUpdateSeverity)
updateAction_serviceUpdateSeverity = Lens.lens (\UpdateAction' {serviceUpdateSeverity} -> serviceUpdateSeverity) (\s@UpdateAction' {} a -> s {serviceUpdateSeverity = a} :: UpdateAction)

-- | The date the update is first available
updateAction_serviceUpdateReleaseDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_serviceUpdateReleaseDate = Lens.lens (\UpdateAction' {serviceUpdateReleaseDate} -> serviceUpdateReleaseDate) (\s@UpdateAction' {} a -> s {serviceUpdateReleaseDate = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the replication group
updateAction_replicationGroupId :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_replicationGroupId = Lens.lens (\UpdateAction' {replicationGroupId} -> replicationGroupId) (\s@UpdateAction' {} a -> s {replicationGroupId = a} :: UpdateAction)

-- | The status of the update action
updateAction_updateActionStatus :: Lens.Lens' UpdateAction (Prelude.Maybe UpdateActionStatus)
updateAction_updateActionStatus = Lens.lens (\UpdateAction' {updateActionStatus} -> updateActionStatus) (\s@UpdateAction' {} a -> s {updateActionStatus = a} :: UpdateAction)

-- | The ID of the cache cluster
updateAction_cacheClusterId :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_cacheClusterId = Lens.lens (\UpdateAction' {cacheClusterId} -> cacheClusterId) (\s@UpdateAction' {} a -> s {cacheClusterId = a} :: UpdateAction)

-- | The status of the service update
updateAction_serviceUpdateStatus :: Lens.Lens' UpdateAction (Prelude.Maybe ServiceUpdateStatus)
updateAction_serviceUpdateStatus = Lens.lens (\UpdateAction' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@UpdateAction' {} a -> s {serviceUpdateStatus = a} :: UpdateAction)

-- | If yes, all nodes in the replication group have been updated by the
-- recommended apply-by date. If no, at least one node in the replication
-- group have not been updated by the recommended apply-by date. If N\/A,
-- the replication group was created after the recommended apply-by date.
updateAction_slaMet :: Lens.Lens' UpdateAction (Prelude.Maybe SlaMet)
updateAction_slaMet = Lens.lens (\UpdateAction' {slaMet} -> slaMet) (\s@UpdateAction' {} a -> s {slaMet = a} :: UpdateAction)

-- | The date that the service update is available to a replication group
updateAction_updateActionAvailableDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_updateActionAvailableDate = Lens.lens (\UpdateAction' {updateActionAvailableDate} -> updateActionAvailableDate) (\s@UpdateAction' {} a -> s {updateActionAvailableDate = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Time

-- | The status of the service update on the node group
updateAction_nodeGroupUpdateStatus :: Lens.Lens' UpdateAction (Prelude.Maybe [NodeGroupUpdateStatus])
updateAction_nodeGroupUpdateStatus = Lens.lens (\UpdateAction' {nodeGroupUpdateStatus} -> nodeGroupUpdateStatus) (\s@UpdateAction' {} a -> s {nodeGroupUpdateStatus = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The recommended date to apply the service update to ensure compliance.
-- For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
updateAction_serviceUpdateRecommendedApplyByDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_serviceUpdateRecommendedApplyByDate = Lens.lens (\UpdateAction' {serviceUpdateRecommendedApplyByDate} -> serviceUpdateRecommendedApplyByDate) (\s@UpdateAction' {} a -> s {serviceUpdateRecommendedApplyByDate = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Time

-- | Reflects the nature of the service update
updateAction_serviceUpdateType :: Lens.Lens' UpdateAction (Prelude.Maybe ServiceUpdateType)
updateAction_serviceUpdateType = Lens.lens (\UpdateAction' {serviceUpdateType} -> serviceUpdateType) (\s@UpdateAction' {} a -> s {serviceUpdateType = a} :: UpdateAction)

-- | The status of the service update on the cache node
updateAction_cacheNodeUpdateStatus :: Lens.Lens' UpdateAction (Prelude.Maybe [CacheNodeUpdateStatus])
updateAction_cacheNodeUpdateStatus = Lens.lens (\UpdateAction' {cacheNodeUpdateStatus} -> cacheNodeUpdateStatus) (\s@UpdateAction' {} a -> s {cacheNodeUpdateStatus = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The progress of the service update on the replication group
updateAction_nodesUpdated :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_nodesUpdated = Lens.lens (\UpdateAction' {nodesUpdated} -> nodesUpdated) (\s@UpdateAction' {} a -> s {nodesUpdated = a} :: UpdateAction)

-- | The estimated length of time for the update to complete
updateAction_estimatedUpdateTime :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_estimatedUpdateTime = Lens.lens (\UpdateAction' {estimatedUpdateTime} -> estimatedUpdateTime) (\s@UpdateAction' {} a -> s {estimatedUpdateTime = a} :: UpdateAction)

-- | The date when the UpdateActionStatus was last modified
updateAction_updateActionStatusModifiedDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_updateActionStatusModifiedDate = Lens.lens (\UpdateAction' {updateActionStatusModifiedDate} -> updateActionStatusModifiedDate) (\s@UpdateAction' {} a -> s {updateActionStatusModifiedDate = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Time

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
updateAction_engine :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_engine = Lens.lens (\UpdateAction' {engine} -> engine) (\s@UpdateAction' {} a -> s {engine = a} :: UpdateAction)

-- | The unique ID of the service update
updateAction_serviceUpdateName :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_serviceUpdateName = Lens.lens (\UpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@UpdateAction' {} a -> s {serviceUpdateName = a} :: UpdateAction)

instance Prelude.FromXML UpdateAction where
  parseXML x =
    UpdateAction'
      Prelude.<$> (x Prelude..@? "ServiceUpdateSeverity")
      Prelude.<*> (x Prelude..@? "ServiceUpdateReleaseDate")
      Prelude.<*> (x Prelude..@? "ReplicationGroupId")
      Prelude.<*> (x Prelude..@? "UpdateActionStatus")
      Prelude.<*> (x Prelude..@? "CacheClusterId")
      Prelude.<*> (x Prelude..@? "ServiceUpdateStatus")
      Prelude.<*> (x Prelude..@? "SlaMet")
      Prelude.<*> (x Prelude..@? "UpdateActionAvailableDate")
      Prelude.<*> ( x Prelude..@? "NodeGroupUpdateStatus"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "NodeGroupUpdateStatus")
                  )
      Prelude.<*> (x Prelude..@? "ServiceUpdateRecommendedApplyByDate")
      Prelude.<*> (x Prelude..@? "ServiceUpdateType")
      Prelude.<*> ( x Prelude..@? "CacheNodeUpdateStatus"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "CacheNodeUpdateStatus")
                  )
      Prelude.<*> (x Prelude..@? "NodesUpdated")
      Prelude.<*> (x Prelude..@? "EstimatedUpdateTime")
      Prelude.<*> (x Prelude..@? "UpdateActionStatusModifiedDate")
      Prelude.<*> (x Prelude..@? "Engine")
      Prelude.<*> (x Prelude..@? "ServiceUpdateName")

instance Prelude.Hashable UpdateAction

instance Prelude.NFData UpdateAction
