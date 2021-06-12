{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterMaintenance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the maintenance settings of a cluster.
module Network.AWS.Redshift.ModifyClusterMaintenance
  ( -- * Creating a Request
    ModifyClusterMaintenance (..),
    newModifyClusterMaintenance,

    -- * Request Lenses
    modifyClusterMaintenance_deferMaintenanceIdentifier,
    modifyClusterMaintenance_deferMaintenanceDuration,
    modifyClusterMaintenance_deferMaintenanceStartTime,
    modifyClusterMaintenance_deferMaintenanceEndTime,
    modifyClusterMaintenance_deferMaintenance,
    modifyClusterMaintenance_clusterIdentifier,

    -- * Destructuring the Response
    ModifyClusterMaintenanceResponse (..),
    newModifyClusterMaintenanceResponse,

    -- * Response Lenses
    modifyClusterMaintenanceResponse_cluster,
    modifyClusterMaintenanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyClusterMaintenance' smart constructor.
data ModifyClusterMaintenance = ModifyClusterMaintenance'
  { -- | A unique identifier for the deferred maintenance window.
    deferMaintenanceIdentifier :: Core.Maybe Core.Text,
    -- | An integer indicating the duration of the maintenance window in days. If
    -- you specify a duration, you can\'t specify an end time. The duration
    -- must be 45 days or less.
    deferMaintenanceDuration :: Core.Maybe Core.Int,
    -- | A timestamp indicating the start time for the deferred maintenance
    -- window.
    deferMaintenanceStartTime :: Core.Maybe Core.ISO8601,
    -- | A timestamp indicating end time for the deferred maintenance window. If
    -- you specify an end time, you can\'t specify a duration.
    deferMaintenanceEndTime :: Core.Maybe Core.ISO8601,
    -- | A boolean indicating whether to enable the deferred maintenance window.
    deferMaintenance :: Core.Maybe Core.Bool,
    -- | A unique identifier for the cluster.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterMaintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deferMaintenanceIdentifier', 'modifyClusterMaintenance_deferMaintenanceIdentifier' - A unique identifier for the deferred maintenance window.
--
-- 'deferMaintenanceDuration', 'modifyClusterMaintenance_deferMaintenanceDuration' - An integer indicating the duration of the maintenance window in days. If
-- you specify a duration, you can\'t specify an end time. The duration
-- must be 45 days or less.
--
-- 'deferMaintenanceStartTime', 'modifyClusterMaintenance_deferMaintenanceStartTime' - A timestamp indicating the start time for the deferred maintenance
-- window.
--
-- 'deferMaintenanceEndTime', 'modifyClusterMaintenance_deferMaintenanceEndTime' - A timestamp indicating end time for the deferred maintenance window. If
-- you specify an end time, you can\'t specify a duration.
--
-- 'deferMaintenance', 'modifyClusterMaintenance_deferMaintenance' - A boolean indicating whether to enable the deferred maintenance window.
--
-- 'clusterIdentifier', 'modifyClusterMaintenance_clusterIdentifier' - A unique identifier for the cluster.
newModifyClusterMaintenance ::
  -- | 'clusterIdentifier'
  Core.Text ->
  ModifyClusterMaintenance
newModifyClusterMaintenance pClusterIdentifier_ =
  ModifyClusterMaintenance'
    { deferMaintenanceIdentifier =
        Core.Nothing,
      deferMaintenanceDuration = Core.Nothing,
      deferMaintenanceStartTime = Core.Nothing,
      deferMaintenanceEndTime = Core.Nothing,
      deferMaintenance = Core.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | A unique identifier for the deferred maintenance window.
modifyClusterMaintenance_deferMaintenanceIdentifier :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Text)
modifyClusterMaintenance_deferMaintenanceIdentifier = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceIdentifier} -> deferMaintenanceIdentifier) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceIdentifier = a} :: ModifyClusterMaintenance)

-- | An integer indicating the duration of the maintenance window in days. If
-- you specify a duration, you can\'t specify an end time. The duration
-- must be 45 days or less.
modifyClusterMaintenance_deferMaintenanceDuration :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Int)
modifyClusterMaintenance_deferMaintenanceDuration = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceDuration} -> deferMaintenanceDuration) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceDuration = a} :: ModifyClusterMaintenance)

-- | A timestamp indicating the start time for the deferred maintenance
-- window.
modifyClusterMaintenance_deferMaintenanceStartTime :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.UTCTime)
modifyClusterMaintenance_deferMaintenanceStartTime = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceStartTime} -> deferMaintenanceStartTime) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceStartTime = a} :: ModifyClusterMaintenance) Core.. Lens.mapping Core._Time

-- | A timestamp indicating end time for the deferred maintenance window. If
-- you specify an end time, you can\'t specify a duration.
modifyClusterMaintenance_deferMaintenanceEndTime :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.UTCTime)
modifyClusterMaintenance_deferMaintenanceEndTime = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceEndTime} -> deferMaintenanceEndTime) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceEndTime = a} :: ModifyClusterMaintenance) Core.. Lens.mapping Core._Time

-- | A boolean indicating whether to enable the deferred maintenance window.
modifyClusterMaintenance_deferMaintenance :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Bool)
modifyClusterMaintenance_deferMaintenance = Lens.lens (\ModifyClusterMaintenance' {deferMaintenance} -> deferMaintenance) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenance = a} :: ModifyClusterMaintenance)

-- | A unique identifier for the cluster.
modifyClusterMaintenance_clusterIdentifier :: Lens.Lens' ModifyClusterMaintenance Core.Text
modifyClusterMaintenance_clusterIdentifier = Lens.lens (\ModifyClusterMaintenance' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyClusterMaintenance' {} a -> s {clusterIdentifier = a} :: ModifyClusterMaintenance)

instance Core.AWSRequest ModifyClusterMaintenance where
  type
    AWSResponse ModifyClusterMaintenance =
      ModifyClusterMaintenanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterMaintenanceResult"
      ( \s h x ->
          ModifyClusterMaintenanceResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyClusterMaintenance

instance Core.NFData ModifyClusterMaintenance

instance Core.ToHeaders ModifyClusterMaintenance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyClusterMaintenance where
  toPath = Core.const "/"

instance Core.ToQuery ModifyClusterMaintenance where
  toQuery ModifyClusterMaintenance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyClusterMaintenance" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "DeferMaintenanceIdentifier"
          Core.=: deferMaintenanceIdentifier,
        "DeferMaintenanceDuration"
          Core.=: deferMaintenanceDuration,
        "DeferMaintenanceStartTime"
          Core.=: deferMaintenanceStartTime,
        "DeferMaintenanceEndTime"
          Core.=: deferMaintenanceEndTime,
        "DeferMaintenance" Core.=: deferMaintenance,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyClusterMaintenanceResponse' smart constructor.
data ModifyClusterMaintenanceResponse = ModifyClusterMaintenanceResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterMaintenanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'modifyClusterMaintenanceResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterMaintenanceResponse_httpStatus' - The response's http status code.
newModifyClusterMaintenanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyClusterMaintenanceResponse
newModifyClusterMaintenanceResponse pHttpStatus_ =
  ModifyClusterMaintenanceResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterMaintenanceResponse_cluster :: Lens.Lens' ModifyClusterMaintenanceResponse (Core.Maybe Cluster)
modifyClusterMaintenanceResponse_cluster = Lens.lens (\ModifyClusterMaintenanceResponse' {cluster} -> cluster) (\s@ModifyClusterMaintenanceResponse' {} a -> s {cluster = a} :: ModifyClusterMaintenanceResponse)

-- | The response's http status code.
modifyClusterMaintenanceResponse_httpStatus :: Lens.Lens' ModifyClusterMaintenanceResponse Core.Int
modifyClusterMaintenanceResponse_httpStatus = Lens.lens (\ModifyClusterMaintenanceResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterMaintenanceResponse' {} a -> s {httpStatus = a} :: ModifyClusterMaintenanceResponse)

instance Core.NFData ModifyClusterMaintenanceResponse
