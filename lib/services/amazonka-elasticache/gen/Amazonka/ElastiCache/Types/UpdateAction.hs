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
-- Module      : Amazonka.ElastiCache.Types.UpdateAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.UpdateAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.CacheNodeUpdateStatus
import Amazonka.ElastiCache.Types.NodeGroupUpdateStatus
import Amazonka.ElastiCache.Types.ServiceUpdateSeverity
import Amazonka.ElastiCache.Types.ServiceUpdateStatus
import Amazonka.ElastiCache.Types.ServiceUpdateType
import Amazonka.ElastiCache.Types.SlaMet
import Amazonka.ElastiCache.Types.UpdateActionStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the service update for a specific replication group
--
-- /See:/ 'newUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | Reflects the nature of the service update
    serviceUpdateType :: Prelude.Maybe ServiceUpdateType,
    -- | The status of the service update on the node group
    nodeGroupUpdateStatus :: Prelude.Maybe [NodeGroupUpdateStatus],
    -- | The date the update is first available
    serviceUpdateReleaseDate :: Prelude.Maybe Data.ISO8601,
    -- | The estimated length of time for the update to complete
    estimatedUpdateTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the update action
    updateActionStatus :: Prelude.Maybe UpdateActionStatus,
    -- | The date that the service update is available to a replication group
    updateActionAvailableDate :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the cache cluster
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The recommended date to apply the service update to ensure compliance.
    -- For information on compliance, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
    serviceUpdateRecommendedApplyByDate :: Prelude.Maybe Data.ISO8601,
    -- | The severity of the service update
    serviceUpdateSeverity :: Prelude.Maybe ServiceUpdateSeverity,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe ServiceUpdateStatus,
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Prelude.Maybe Prelude.Text,
    -- | If yes, all nodes in the replication group have been updated by the
    -- recommended apply-by date. If no, at least one node in the replication
    -- group have not been updated by the recommended apply-by date. If N\/A,
    -- the replication group was created after the recommended apply-by date.
    slaMet :: Prelude.Maybe SlaMet,
    -- | The ID of the replication group
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update on the cache node
    cacheNodeUpdateStatus :: Prelude.Maybe [CacheNodeUpdateStatus],
    -- | The progress of the service update on the replication group
    nodesUpdated :: Prelude.Maybe Prelude.Text,
    -- | The date when the UpdateActionStatus was last modified
    updateActionStatusModifiedDate :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdateType', 'updateAction_serviceUpdateType' - Reflects the nature of the service update
--
-- 'nodeGroupUpdateStatus', 'updateAction_nodeGroupUpdateStatus' - The status of the service update on the node group
--
-- 'serviceUpdateReleaseDate', 'updateAction_serviceUpdateReleaseDate' - The date the update is first available
--
-- 'estimatedUpdateTime', 'updateAction_estimatedUpdateTime' - The estimated length of time for the update to complete
--
-- 'updateActionStatus', 'updateAction_updateActionStatus' - The status of the update action
--
-- 'updateActionAvailableDate', 'updateAction_updateActionAvailableDate' - The date that the service update is available to a replication group
--
-- 'cacheClusterId', 'updateAction_cacheClusterId' - The ID of the cache cluster
--
-- 'serviceUpdateRecommendedApplyByDate', 'updateAction_serviceUpdateRecommendedApplyByDate' - The recommended date to apply the service update to ensure compliance.
-- For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
--
-- 'serviceUpdateSeverity', 'updateAction_serviceUpdateSeverity' - The severity of the service update
--
-- 'serviceUpdateName', 'updateAction_serviceUpdateName' - The unique ID of the service update
--
-- 'serviceUpdateStatus', 'updateAction_serviceUpdateStatus' - The status of the service update
--
-- 'engine', 'updateAction_engine' - The Elasticache engine to which the update applies. Either Redis or
-- Memcached
--
-- 'slaMet', 'updateAction_slaMet' - If yes, all nodes in the replication group have been updated by the
-- recommended apply-by date. If no, at least one node in the replication
-- group have not been updated by the recommended apply-by date. If N\/A,
-- the replication group was created after the recommended apply-by date.
--
-- 'replicationGroupId', 'updateAction_replicationGroupId' - The ID of the replication group
--
-- 'cacheNodeUpdateStatus', 'updateAction_cacheNodeUpdateStatus' - The status of the service update on the cache node
--
-- 'nodesUpdated', 'updateAction_nodesUpdated' - The progress of the service update on the replication group
--
-- 'updateActionStatusModifiedDate', 'updateAction_updateActionStatusModifiedDate' - The date when the UpdateActionStatus was last modified
newUpdateAction ::
  UpdateAction
newUpdateAction =
  UpdateAction'
    { serviceUpdateType = Prelude.Nothing,
      nodeGroupUpdateStatus = Prelude.Nothing,
      serviceUpdateReleaseDate = Prelude.Nothing,
      estimatedUpdateTime = Prelude.Nothing,
      updateActionStatus = Prelude.Nothing,
      updateActionAvailableDate = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      serviceUpdateRecommendedApplyByDate =
        Prelude.Nothing,
      serviceUpdateSeverity = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing,
      engine = Prelude.Nothing,
      slaMet = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      cacheNodeUpdateStatus = Prelude.Nothing,
      nodesUpdated = Prelude.Nothing,
      updateActionStatusModifiedDate = Prelude.Nothing
    }

-- | Reflects the nature of the service update
updateAction_serviceUpdateType :: Lens.Lens' UpdateAction (Prelude.Maybe ServiceUpdateType)
updateAction_serviceUpdateType = Lens.lens (\UpdateAction' {serviceUpdateType} -> serviceUpdateType) (\s@UpdateAction' {} a -> s {serviceUpdateType = a} :: UpdateAction)

-- | The status of the service update on the node group
updateAction_nodeGroupUpdateStatus :: Lens.Lens' UpdateAction (Prelude.Maybe [NodeGroupUpdateStatus])
updateAction_nodeGroupUpdateStatus = Lens.lens (\UpdateAction' {nodeGroupUpdateStatus} -> nodeGroupUpdateStatus) (\s@UpdateAction' {} a -> s {nodeGroupUpdateStatus = a} :: UpdateAction) Prelude.. Lens.mapping Lens.coerced

-- | The date the update is first available
updateAction_serviceUpdateReleaseDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_serviceUpdateReleaseDate = Lens.lens (\UpdateAction' {serviceUpdateReleaseDate} -> serviceUpdateReleaseDate) (\s@UpdateAction' {} a -> s {serviceUpdateReleaseDate = a} :: UpdateAction) Prelude.. Lens.mapping Data._Time

-- | The estimated length of time for the update to complete
updateAction_estimatedUpdateTime :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_estimatedUpdateTime = Lens.lens (\UpdateAction' {estimatedUpdateTime} -> estimatedUpdateTime) (\s@UpdateAction' {} a -> s {estimatedUpdateTime = a} :: UpdateAction)

-- | The status of the update action
updateAction_updateActionStatus :: Lens.Lens' UpdateAction (Prelude.Maybe UpdateActionStatus)
updateAction_updateActionStatus = Lens.lens (\UpdateAction' {updateActionStatus} -> updateActionStatus) (\s@UpdateAction' {} a -> s {updateActionStatus = a} :: UpdateAction)

-- | The date that the service update is available to a replication group
updateAction_updateActionAvailableDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_updateActionAvailableDate = Lens.lens (\UpdateAction' {updateActionAvailableDate} -> updateActionAvailableDate) (\s@UpdateAction' {} a -> s {updateActionAvailableDate = a} :: UpdateAction) Prelude.. Lens.mapping Data._Time

-- | The ID of the cache cluster
updateAction_cacheClusterId :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_cacheClusterId = Lens.lens (\UpdateAction' {cacheClusterId} -> cacheClusterId) (\s@UpdateAction' {} a -> s {cacheClusterId = a} :: UpdateAction)

-- | The recommended date to apply the service update to ensure compliance.
-- For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
updateAction_serviceUpdateRecommendedApplyByDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_serviceUpdateRecommendedApplyByDate = Lens.lens (\UpdateAction' {serviceUpdateRecommendedApplyByDate} -> serviceUpdateRecommendedApplyByDate) (\s@UpdateAction' {} a -> s {serviceUpdateRecommendedApplyByDate = a} :: UpdateAction) Prelude.. Lens.mapping Data._Time

-- | The severity of the service update
updateAction_serviceUpdateSeverity :: Lens.Lens' UpdateAction (Prelude.Maybe ServiceUpdateSeverity)
updateAction_serviceUpdateSeverity = Lens.lens (\UpdateAction' {serviceUpdateSeverity} -> serviceUpdateSeverity) (\s@UpdateAction' {} a -> s {serviceUpdateSeverity = a} :: UpdateAction)

-- | The unique ID of the service update
updateAction_serviceUpdateName :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_serviceUpdateName = Lens.lens (\UpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@UpdateAction' {} a -> s {serviceUpdateName = a} :: UpdateAction)

-- | The status of the service update
updateAction_serviceUpdateStatus :: Lens.Lens' UpdateAction (Prelude.Maybe ServiceUpdateStatus)
updateAction_serviceUpdateStatus = Lens.lens (\UpdateAction' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@UpdateAction' {} a -> s {serviceUpdateStatus = a} :: UpdateAction)

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
updateAction_engine :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_engine = Lens.lens (\UpdateAction' {engine} -> engine) (\s@UpdateAction' {} a -> s {engine = a} :: UpdateAction)

-- | If yes, all nodes in the replication group have been updated by the
-- recommended apply-by date. If no, at least one node in the replication
-- group have not been updated by the recommended apply-by date. If N\/A,
-- the replication group was created after the recommended apply-by date.
updateAction_slaMet :: Lens.Lens' UpdateAction (Prelude.Maybe SlaMet)
updateAction_slaMet = Lens.lens (\UpdateAction' {slaMet} -> slaMet) (\s@UpdateAction' {} a -> s {slaMet = a} :: UpdateAction)

-- | The ID of the replication group
updateAction_replicationGroupId :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_replicationGroupId = Lens.lens (\UpdateAction' {replicationGroupId} -> replicationGroupId) (\s@UpdateAction' {} a -> s {replicationGroupId = a} :: UpdateAction)

-- | The status of the service update on the cache node
updateAction_cacheNodeUpdateStatus :: Lens.Lens' UpdateAction (Prelude.Maybe [CacheNodeUpdateStatus])
updateAction_cacheNodeUpdateStatus = Lens.lens (\UpdateAction' {cacheNodeUpdateStatus} -> cacheNodeUpdateStatus) (\s@UpdateAction' {} a -> s {cacheNodeUpdateStatus = a} :: UpdateAction) Prelude.. Lens.mapping Lens.coerced

-- | The progress of the service update on the replication group
updateAction_nodesUpdated :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_nodesUpdated = Lens.lens (\UpdateAction' {nodesUpdated} -> nodesUpdated) (\s@UpdateAction' {} a -> s {nodesUpdated = a} :: UpdateAction)

-- | The date when the UpdateActionStatus was last modified
updateAction_updateActionStatusModifiedDate :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.UTCTime)
updateAction_updateActionStatusModifiedDate = Lens.lens (\UpdateAction' {updateActionStatusModifiedDate} -> updateActionStatusModifiedDate) (\s@UpdateAction' {} a -> s {updateActionStatusModifiedDate = a} :: UpdateAction) Prelude.. Lens.mapping Data._Time

instance Data.FromXML UpdateAction where
  parseXML x =
    UpdateAction'
      Prelude.<$> (x Data..@? "ServiceUpdateType")
      Prelude.<*> ( x Data..@? "NodeGroupUpdateStatus"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "NodeGroupUpdateStatus")
                  )
      Prelude.<*> (x Data..@? "ServiceUpdateReleaseDate")
      Prelude.<*> (x Data..@? "EstimatedUpdateTime")
      Prelude.<*> (x Data..@? "UpdateActionStatus")
      Prelude.<*> (x Data..@? "UpdateActionAvailableDate")
      Prelude.<*> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "ServiceUpdateRecommendedApplyByDate")
      Prelude.<*> (x Data..@? "ServiceUpdateSeverity")
      Prelude.<*> (x Data..@? "ServiceUpdateName")
      Prelude.<*> (x Data..@? "ServiceUpdateStatus")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "SlaMet")
      Prelude.<*> (x Data..@? "ReplicationGroupId")
      Prelude.<*> ( x Data..@? "CacheNodeUpdateStatus"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CacheNodeUpdateStatus")
                  )
      Prelude.<*> (x Data..@? "NodesUpdated")
      Prelude.<*> (x Data..@? "UpdateActionStatusModifiedDate")

instance Prelude.Hashable UpdateAction where
  hashWithSalt _salt UpdateAction' {..} =
    _salt `Prelude.hashWithSalt` serviceUpdateType
      `Prelude.hashWithSalt` nodeGroupUpdateStatus
      `Prelude.hashWithSalt` serviceUpdateReleaseDate
      `Prelude.hashWithSalt` estimatedUpdateTime
      `Prelude.hashWithSalt` updateActionStatus
      `Prelude.hashWithSalt` updateActionAvailableDate
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` serviceUpdateRecommendedApplyByDate
      `Prelude.hashWithSalt` serviceUpdateSeverity
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` serviceUpdateStatus
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` slaMet
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` cacheNodeUpdateStatus
      `Prelude.hashWithSalt` nodesUpdated
      `Prelude.hashWithSalt` updateActionStatusModifiedDate

instance Prelude.NFData UpdateAction where
  rnf UpdateAction' {..} =
    Prelude.rnf serviceUpdateType
      `Prelude.seq` Prelude.rnf nodeGroupUpdateStatus
      `Prelude.seq` Prelude.rnf serviceUpdateReleaseDate
      `Prelude.seq` Prelude.rnf estimatedUpdateTime
      `Prelude.seq` Prelude.rnf updateActionStatus
      `Prelude.seq` Prelude.rnf updateActionAvailableDate
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf serviceUpdateRecommendedApplyByDate
      `Prelude.seq` Prelude.rnf serviceUpdateSeverity
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf serviceUpdateStatus
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf slaMet
      `Prelude.seq` Prelude.rnf replicationGroupId
      `Prelude.seq` Prelude.rnf cacheNodeUpdateStatus
      `Prelude.seq` Prelude.rnf nodesUpdated
      `Prelude.seq` Prelude.rnf
        updateActionStatusModifiedDate
