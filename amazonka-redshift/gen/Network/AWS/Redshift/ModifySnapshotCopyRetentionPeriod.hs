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
-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of days to retain snapshots in the destination AWS
-- Region after they are copied from the source AWS Region. By default,
-- this operation only changes the retention period of copied automated
-- snapshots. The retention periods for both new and existing copied
-- automated snapshots are updated with the new retention period. You can
-- set the manual option to change only the retention periods of copied
-- manual snapshots. If you set this option, only newly copied manual
-- snapshots have the new retention period.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
  ( -- * Creating a Request
    ModifySnapshotCopyRetentionPeriod (..),
    newModifySnapshotCopyRetentionPeriod,

    -- * Request Lenses
    modifySnapshotCopyRetentionPeriod_manual,
    modifySnapshotCopyRetentionPeriod_clusterIdentifier,
    modifySnapshotCopyRetentionPeriod_retentionPeriod,

    -- * Destructuring the Response
    ModifySnapshotCopyRetentionPeriodResponse (..),
    newModifySnapshotCopyRetentionPeriodResponse,

    -- * Response Lenses
    modifySnapshotCopyRetentionPeriodResponse_cluster,
    modifySnapshotCopyRetentionPeriodResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifySnapshotCopyRetentionPeriod' smart constructor.
data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod'
  { -- | Indicates whether to apply the snapshot retention period to newly copied
    -- manual snapshots instead of automated snapshots.
    manual :: Core.Maybe Core.Bool,
    -- | The unique identifier of the cluster for which you want to change the
    -- retention period for either automated or manual snapshots that are
    -- copied to a destination AWS Region.
    --
    -- Constraints: Must be the valid name of an existing cluster that has
    -- cross-region snapshot copy enabled.
    clusterIdentifier :: Core.Text,
    -- | The number of days to retain automated snapshots in the destination AWS
    -- Region after they are copied from the source AWS Region.
    --
    -- By default, this only changes the retention period of copied automated
    -- snapshots.
    --
    -- If you decrease the retention period for automated snapshots that are
    -- copied to a destination AWS Region, Amazon Redshift deletes any existing
    -- automated snapshots that were copied to the destination AWS Region and
    -- that fall outside of the new retention period.
    --
    -- Constraints: Must be at least 1 and no more than 35 for automated
    -- snapshots.
    --
    -- If you specify the @manual@ option, only newly copied manual snapshots
    -- will have the new retention period.
    --
    -- If you specify the value of -1 newly copied manual snapshots are
    -- retained indefinitely.
    --
    -- Constraints: The number of days must be either -1 or an integer between
    -- 1 and 3,653 for manual snapshots.
    retentionPeriod :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifySnapshotCopyRetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manual', 'modifySnapshotCopyRetentionPeriod_manual' - Indicates whether to apply the snapshot retention period to newly copied
-- manual snapshots instead of automated snapshots.
--
-- 'clusterIdentifier', 'modifySnapshotCopyRetentionPeriod_clusterIdentifier' - The unique identifier of the cluster for which you want to change the
-- retention period for either automated or manual snapshots that are
-- copied to a destination AWS Region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
--
-- 'retentionPeriod', 'modifySnapshotCopyRetentionPeriod_retentionPeriod' - The number of days to retain automated snapshots in the destination AWS
-- Region after they are copied from the source AWS Region.
--
-- By default, this only changes the retention period of copied automated
-- snapshots.
--
-- If you decrease the retention period for automated snapshots that are
-- copied to a destination AWS Region, Amazon Redshift deletes any existing
-- automated snapshots that were copied to the destination AWS Region and
-- that fall outside of the new retention period.
--
-- Constraints: Must be at least 1 and no more than 35 for automated
-- snapshots.
--
-- If you specify the @manual@ option, only newly copied manual snapshots
-- will have the new retention period.
--
-- If you specify the value of -1 newly copied manual snapshots are
-- retained indefinitely.
--
-- Constraints: The number of days must be either -1 or an integer between
-- 1 and 3,653 for manual snapshots.
newModifySnapshotCopyRetentionPeriod ::
  -- | 'clusterIdentifier'
  Core.Text ->
  -- | 'retentionPeriod'
  Core.Int ->
  ModifySnapshotCopyRetentionPeriod
newModifySnapshotCopyRetentionPeriod
  pClusterIdentifier_
  pRetentionPeriod_ =
    ModifySnapshotCopyRetentionPeriod'
      { manual =
          Core.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        retentionPeriod = pRetentionPeriod_
      }

-- | Indicates whether to apply the snapshot retention period to newly copied
-- manual snapshots instead of automated snapshots.
modifySnapshotCopyRetentionPeriod_manual :: Lens.Lens' ModifySnapshotCopyRetentionPeriod (Core.Maybe Core.Bool)
modifySnapshotCopyRetentionPeriod_manual = Lens.lens (\ModifySnapshotCopyRetentionPeriod' {manual} -> manual) (\s@ModifySnapshotCopyRetentionPeriod' {} a -> s {manual = a} :: ModifySnapshotCopyRetentionPeriod)

-- | The unique identifier of the cluster for which you want to change the
-- retention period for either automated or manual snapshots that are
-- copied to a destination AWS Region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
modifySnapshotCopyRetentionPeriod_clusterIdentifier :: Lens.Lens' ModifySnapshotCopyRetentionPeriod Core.Text
modifySnapshotCopyRetentionPeriod_clusterIdentifier = Lens.lens (\ModifySnapshotCopyRetentionPeriod' {clusterIdentifier} -> clusterIdentifier) (\s@ModifySnapshotCopyRetentionPeriod' {} a -> s {clusterIdentifier = a} :: ModifySnapshotCopyRetentionPeriod)

-- | The number of days to retain automated snapshots in the destination AWS
-- Region after they are copied from the source AWS Region.
--
-- By default, this only changes the retention period of copied automated
-- snapshots.
--
-- If you decrease the retention period for automated snapshots that are
-- copied to a destination AWS Region, Amazon Redshift deletes any existing
-- automated snapshots that were copied to the destination AWS Region and
-- that fall outside of the new retention period.
--
-- Constraints: Must be at least 1 and no more than 35 for automated
-- snapshots.
--
-- If you specify the @manual@ option, only newly copied manual snapshots
-- will have the new retention period.
--
-- If you specify the value of -1 newly copied manual snapshots are
-- retained indefinitely.
--
-- Constraints: The number of days must be either -1 or an integer between
-- 1 and 3,653 for manual snapshots.
modifySnapshotCopyRetentionPeriod_retentionPeriod :: Lens.Lens' ModifySnapshotCopyRetentionPeriod Core.Int
modifySnapshotCopyRetentionPeriod_retentionPeriod = Lens.lens (\ModifySnapshotCopyRetentionPeriod' {retentionPeriod} -> retentionPeriod) (\s@ModifySnapshotCopyRetentionPeriod' {} a -> s {retentionPeriod = a} :: ModifySnapshotCopyRetentionPeriod)

instance
  Core.AWSRequest
    ModifySnapshotCopyRetentionPeriod
  where
  type
    AWSResponse ModifySnapshotCopyRetentionPeriod =
      ModifySnapshotCopyRetentionPeriodResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifySnapshotCopyRetentionPeriodResult"
      ( \s h x ->
          ModifySnapshotCopyRetentionPeriodResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ModifySnapshotCopyRetentionPeriod

instance
  Core.NFData
    ModifySnapshotCopyRetentionPeriod

instance
  Core.ToHeaders
    ModifySnapshotCopyRetentionPeriod
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ModifySnapshotCopyRetentionPeriod
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ModifySnapshotCopyRetentionPeriod
  where
  toQuery ModifySnapshotCopyRetentionPeriod' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ModifySnapshotCopyRetentionPeriod" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Manual" Core.=: manual,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "RetentionPeriod" Core.=: retentionPeriod
      ]

-- | /See:/ 'newModifySnapshotCopyRetentionPeriodResponse' smart constructor.
data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifySnapshotCopyRetentionPeriodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'modifySnapshotCopyRetentionPeriodResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'modifySnapshotCopyRetentionPeriodResponse_httpStatus' - The response's http status code.
newModifySnapshotCopyRetentionPeriodResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifySnapshotCopyRetentionPeriodResponse
newModifySnapshotCopyRetentionPeriodResponse
  pHttpStatus_ =
    ModifySnapshotCopyRetentionPeriodResponse'
      { cluster =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
modifySnapshotCopyRetentionPeriodResponse_cluster :: Lens.Lens' ModifySnapshotCopyRetentionPeriodResponse (Core.Maybe Cluster)
modifySnapshotCopyRetentionPeriodResponse_cluster = Lens.lens (\ModifySnapshotCopyRetentionPeriodResponse' {cluster} -> cluster) (\s@ModifySnapshotCopyRetentionPeriodResponse' {} a -> s {cluster = a} :: ModifySnapshotCopyRetentionPeriodResponse)

-- | The response's http status code.
modifySnapshotCopyRetentionPeriodResponse_httpStatus :: Lens.Lens' ModifySnapshotCopyRetentionPeriodResponse Core.Int
modifySnapshotCopyRetentionPeriodResponse_httpStatus = Lens.lens (\ModifySnapshotCopyRetentionPeriodResponse' {httpStatus} -> httpStatus) (\s@ModifySnapshotCopyRetentionPeriodResponse' {} a -> s {httpStatus = a} :: ModifySnapshotCopyRetentionPeriodResponse)

instance
  Core.NFData
    ModifySnapshotCopyRetentionPeriodResponse
