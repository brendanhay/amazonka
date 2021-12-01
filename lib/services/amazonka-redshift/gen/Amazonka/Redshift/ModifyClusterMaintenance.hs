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
-- Module      : Amazonka.Redshift.ModifyClusterMaintenance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the maintenance settings of a cluster.
module Amazonka.Redshift.ModifyClusterMaintenance
  ( -- * Creating a Request
    ModifyClusterMaintenance (..),
    newModifyClusterMaintenance,

    -- * Request Lenses
    modifyClusterMaintenance_deferMaintenanceEndTime,
    modifyClusterMaintenance_deferMaintenance,
    modifyClusterMaintenance_deferMaintenanceDuration,
    modifyClusterMaintenance_deferMaintenanceStartTime,
    modifyClusterMaintenance_deferMaintenanceIdentifier,
    modifyClusterMaintenance_clusterIdentifier,

    -- * Destructuring the Response
    ModifyClusterMaintenanceResponse (..),
    newModifyClusterMaintenanceResponse,

    -- * Response Lenses
    modifyClusterMaintenanceResponse_cluster,
    modifyClusterMaintenanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyClusterMaintenance' smart constructor.
data ModifyClusterMaintenance = ModifyClusterMaintenance'
  { -- | A timestamp indicating end time for the deferred maintenance window. If
    -- you specify an end time, you can\'t specify a duration.
    deferMaintenanceEndTime :: Prelude.Maybe Core.ISO8601,
    -- | A boolean indicating whether to enable the deferred maintenance window.
    deferMaintenance :: Prelude.Maybe Prelude.Bool,
    -- | An integer indicating the duration of the maintenance window in days. If
    -- you specify a duration, you can\'t specify an end time. The duration
    -- must be 45 days or less.
    deferMaintenanceDuration :: Prelude.Maybe Prelude.Int,
    -- | A timestamp indicating the start time for the deferred maintenance
    -- window.
    deferMaintenanceStartTime :: Prelude.Maybe Core.ISO8601,
    -- | A unique identifier for the deferred maintenance window.
    deferMaintenanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the cluster.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterMaintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deferMaintenanceEndTime', 'modifyClusterMaintenance_deferMaintenanceEndTime' - A timestamp indicating end time for the deferred maintenance window. If
-- you specify an end time, you can\'t specify a duration.
--
-- 'deferMaintenance', 'modifyClusterMaintenance_deferMaintenance' - A boolean indicating whether to enable the deferred maintenance window.
--
-- 'deferMaintenanceDuration', 'modifyClusterMaintenance_deferMaintenanceDuration' - An integer indicating the duration of the maintenance window in days. If
-- you specify a duration, you can\'t specify an end time. The duration
-- must be 45 days or less.
--
-- 'deferMaintenanceStartTime', 'modifyClusterMaintenance_deferMaintenanceStartTime' - A timestamp indicating the start time for the deferred maintenance
-- window.
--
-- 'deferMaintenanceIdentifier', 'modifyClusterMaintenance_deferMaintenanceIdentifier' - A unique identifier for the deferred maintenance window.
--
-- 'clusterIdentifier', 'modifyClusterMaintenance_clusterIdentifier' - A unique identifier for the cluster.
newModifyClusterMaintenance ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ModifyClusterMaintenance
newModifyClusterMaintenance pClusterIdentifier_ =
  ModifyClusterMaintenance'
    { deferMaintenanceEndTime =
        Prelude.Nothing,
      deferMaintenance = Prelude.Nothing,
      deferMaintenanceDuration = Prelude.Nothing,
      deferMaintenanceStartTime = Prelude.Nothing,
      deferMaintenanceIdentifier = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | A timestamp indicating end time for the deferred maintenance window. If
-- you specify an end time, you can\'t specify a duration.
modifyClusterMaintenance_deferMaintenanceEndTime :: Lens.Lens' ModifyClusterMaintenance (Prelude.Maybe Prelude.UTCTime)
modifyClusterMaintenance_deferMaintenanceEndTime = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceEndTime} -> deferMaintenanceEndTime) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceEndTime = a} :: ModifyClusterMaintenance) Prelude.. Lens.mapping Core._Time

-- | A boolean indicating whether to enable the deferred maintenance window.
modifyClusterMaintenance_deferMaintenance :: Lens.Lens' ModifyClusterMaintenance (Prelude.Maybe Prelude.Bool)
modifyClusterMaintenance_deferMaintenance = Lens.lens (\ModifyClusterMaintenance' {deferMaintenance} -> deferMaintenance) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenance = a} :: ModifyClusterMaintenance)

-- | An integer indicating the duration of the maintenance window in days. If
-- you specify a duration, you can\'t specify an end time. The duration
-- must be 45 days or less.
modifyClusterMaintenance_deferMaintenanceDuration :: Lens.Lens' ModifyClusterMaintenance (Prelude.Maybe Prelude.Int)
modifyClusterMaintenance_deferMaintenanceDuration = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceDuration} -> deferMaintenanceDuration) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceDuration = a} :: ModifyClusterMaintenance)

-- | A timestamp indicating the start time for the deferred maintenance
-- window.
modifyClusterMaintenance_deferMaintenanceStartTime :: Lens.Lens' ModifyClusterMaintenance (Prelude.Maybe Prelude.UTCTime)
modifyClusterMaintenance_deferMaintenanceStartTime = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceStartTime} -> deferMaintenanceStartTime) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceStartTime = a} :: ModifyClusterMaintenance) Prelude.. Lens.mapping Core._Time

-- | A unique identifier for the deferred maintenance window.
modifyClusterMaintenance_deferMaintenanceIdentifier :: Lens.Lens' ModifyClusterMaintenance (Prelude.Maybe Prelude.Text)
modifyClusterMaintenance_deferMaintenanceIdentifier = Lens.lens (\ModifyClusterMaintenance' {deferMaintenanceIdentifier} -> deferMaintenanceIdentifier) (\s@ModifyClusterMaintenance' {} a -> s {deferMaintenanceIdentifier = a} :: ModifyClusterMaintenance)

-- | A unique identifier for the cluster.
modifyClusterMaintenance_clusterIdentifier :: Lens.Lens' ModifyClusterMaintenance Prelude.Text
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
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyClusterMaintenance where
  hashWithSalt salt' ModifyClusterMaintenance' {..} =
    salt' `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` deferMaintenanceIdentifier
      `Prelude.hashWithSalt` deferMaintenanceStartTime
      `Prelude.hashWithSalt` deferMaintenanceDuration
      `Prelude.hashWithSalt` deferMaintenance
      `Prelude.hashWithSalt` deferMaintenanceEndTime

instance Prelude.NFData ModifyClusterMaintenance where
  rnf ModifyClusterMaintenance' {..} =
    Prelude.rnf deferMaintenanceEndTime
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf deferMaintenanceIdentifier
      `Prelude.seq` Prelude.rnf deferMaintenanceStartTime
      `Prelude.seq` Prelude.rnf deferMaintenanceDuration
      `Prelude.seq` Prelude.rnf deferMaintenance

instance Core.ToHeaders ModifyClusterMaintenance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyClusterMaintenance where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyClusterMaintenance where
  toQuery ModifyClusterMaintenance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyClusterMaintenance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "DeferMaintenanceEndTime"
          Core.=: deferMaintenanceEndTime,
        "DeferMaintenance" Core.=: deferMaintenance,
        "DeferMaintenanceDuration"
          Core.=: deferMaintenanceDuration,
        "DeferMaintenanceStartTime"
          Core.=: deferMaintenanceStartTime,
        "DeferMaintenanceIdentifier"
          Core.=: deferMaintenanceIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyClusterMaintenanceResponse' smart constructor.
data ModifyClusterMaintenanceResponse = ModifyClusterMaintenanceResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyClusterMaintenanceResponse
newModifyClusterMaintenanceResponse pHttpStatus_ =
  ModifyClusterMaintenanceResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterMaintenanceResponse_cluster :: Lens.Lens' ModifyClusterMaintenanceResponse (Prelude.Maybe Cluster)
modifyClusterMaintenanceResponse_cluster = Lens.lens (\ModifyClusterMaintenanceResponse' {cluster} -> cluster) (\s@ModifyClusterMaintenanceResponse' {} a -> s {cluster = a} :: ModifyClusterMaintenanceResponse)

-- | The response's http status code.
modifyClusterMaintenanceResponse_httpStatus :: Lens.Lens' ModifyClusterMaintenanceResponse Prelude.Int
modifyClusterMaintenanceResponse_httpStatus = Lens.lens (\ModifyClusterMaintenanceResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterMaintenanceResponse' {} a -> s {httpStatus = a} :: ModifyClusterMaintenanceResponse)

instance
  Prelude.NFData
    ModifyClusterMaintenanceResponse
  where
  rnf ModifyClusterMaintenanceResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
