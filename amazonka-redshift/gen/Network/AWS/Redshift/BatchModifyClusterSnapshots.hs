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
-- Module      : Network.AWS.Redshift.BatchModifyClusterSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a set of cluster snapshots.
module Network.AWS.Redshift.BatchModifyClusterSnapshots
  ( -- * Creating a Request
    BatchModifyClusterSnapshots (..),
    newBatchModifyClusterSnapshots,

    -- * Request Lenses
    batchModifyClusterSnapshots_force,
    batchModifyClusterSnapshots_manualSnapshotRetentionPeriod,
    batchModifyClusterSnapshots_snapshotIdentifierList,

    -- * Destructuring the Response
    BatchModifyClusterSnapshotsResponse (..),
    newBatchModifyClusterSnapshotsResponse,

    -- * Response Lenses
    batchModifyClusterSnapshotsResponse_resources,
    batchModifyClusterSnapshotsResponse_errors,
    batchModifyClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchModifyClusterSnapshots' smart constructor.
data BatchModifyClusterSnapshots = BatchModifyClusterSnapshots'
  { -- | A boolean value indicating whether to override an exception if the
    -- retention period has passed.
    force :: Core.Maybe Core.Bool,
    -- | The number of days that a manual snapshot is retained. If you specify
    -- the value -1, the manual snapshot is retained indefinitely.
    --
    -- The number must be either -1 or an integer between 1 and 3,653.
    --
    -- If you decrease the manual snapshot retention period from its current
    -- value, existing manual snapshots that fall outside of the new retention
    -- period will return an error. If you want to suppress the errors and
    -- delete the snapshots, use the force option.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | A list of snapshot identifiers you want to modify.
    snapshotIdentifierList :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchModifyClusterSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'batchModifyClusterSnapshots_force' - A boolean value indicating whether to override an exception if the
-- retention period has passed.
--
-- 'manualSnapshotRetentionPeriod', 'batchModifyClusterSnapshots_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If you specify
-- the value -1, the manual snapshot is retained indefinitely.
--
-- The number must be either -1 or an integer between 1 and 3,653.
--
-- If you decrease the manual snapshot retention period from its current
-- value, existing manual snapshots that fall outside of the new retention
-- period will return an error. If you want to suppress the errors and
-- delete the snapshots, use the force option.
--
-- 'snapshotIdentifierList', 'batchModifyClusterSnapshots_snapshotIdentifierList' - A list of snapshot identifiers you want to modify.
newBatchModifyClusterSnapshots ::
  BatchModifyClusterSnapshots
newBatchModifyClusterSnapshots =
  BatchModifyClusterSnapshots'
    { force = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      snapshotIdentifierList = Core.mempty
    }

-- | A boolean value indicating whether to override an exception if the
-- retention period has passed.
batchModifyClusterSnapshots_force :: Lens.Lens' BatchModifyClusterSnapshots (Core.Maybe Core.Bool)
batchModifyClusterSnapshots_force = Lens.lens (\BatchModifyClusterSnapshots' {force} -> force) (\s@BatchModifyClusterSnapshots' {} a -> s {force = a} :: BatchModifyClusterSnapshots)

-- | The number of days that a manual snapshot is retained. If you specify
-- the value -1, the manual snapshot is retained indefinitely.
--
-- The number must be either -1 or an integer between 1 and 3,653.
--
-- If you decrease the manual snapshot retention period from its current
-- value, existing manual snapshots that fall outside of the new retention
-- period will return an error. If you want to suppress the errors and
-- delete the snapshots, use the force option.
batchModifyClusterSnapshots_manualSnapshotRetentionPeriod :: Lens.Lens' BatchModifyClusterSnapshots (Core.Maybe Core.Int)
batchModifyClusterSnapshots_manualSnapshotRetentionPeriod = Lens.lens (\BatchModifyClusterSnapshots' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@BatchModifyClusterSnapshots' {} a -> s {manualSnapshotRetentionPeriod = a} :: BatchModifyClusterSnapshots)

-- | A list of snapshot identifiers you want to modify.
batchModifyClusterSnapshots_snapshotIdentifierList :: Lens.Lens' BatchModifyClusterSnapshots [Core.Text]
batchModifyClusterSnapshots_snapshotIdentifierList = Lens.lens (\BatchModifyClusterSnapshots' {snapshotIdentifierList} -> snapshotIdentifierList) (\s@BatchModifyClusterSnapshots' {} a -> s {snapshotIdentifierList = a} :: BatchModifyClusterSnapshots) Core.. Lens._Coerce

instance Core.AWSRequest BatchModifyClusterSnapshots where
  type
    AWSResponse BatchModifyClusterSnapshots =
      BatchModifyClusterSnapshotsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BatchModifyClusterSnapshotsResult"
      ( \s h x ->
          BatchModifyClusterSnapshotsResponse'
            Core.<$> ( x Core..@? "Resources" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "String")
                     )
            Core.<*> ( x Core..@? "Errors" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "SnapshotErrorMessage")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchModifyClusterSnapshots

instance Core.NFData BatchModifyClusterSnapshots

instance Core.ToHeaders BatchModifyClusterSnapshots where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BatchModifyClusterSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery BatchModifyClusterSnapshots where
  toQuery BatchModifyClusterSnapshots' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BatchModifyClusterSnapshots" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Force" Core.=: force,
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "SnapshotIdentifierList"
          Core.=: Core.toQueryList "String" snapshotIdentifierList
      ]

-- | /See:/ 'newBatchModifyClusterSnapshotsResponse' smart constructor.
data BatchModifyClusterSnapshotsResponse = BatchModifyClusterSnapshotsResponse'
  { -- | A list of the snapshots that were modified.
    resources :: Core.Maybe [Core.Text],
    -- | A list of any errors returned.
    errors :: Core.Maybe [SnapshotErrorMessage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchModifyClusterSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'batchModifyClusterSnapshotsResponse_resources' - A list of the snapshots that were modified.
--
-- 'errors', 'batchModifyClusterSnapshotsResponse_errors' - A list of any errors returned.
--
-- 'httpStatus', 'batchModifyClusterSnapshotsResponse_httpStatus' - The response's http status code.
newBatchModifyClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchModifyClusterSnapshotsResponse
newBatchModifyClusterSnapshotsResponse pHttpStatus_ =
  BatchModifyClusterSnapshotsResponse'
    { resources =
        Core.Nothing,
      errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the snapshots that were modified.
batchModifyClusterSnapshotsResponse_resources :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Core.Maybe [Core.Text])
batchModifyClusterSnapshotsResponse_resources = Lens.lens (\BatchModifyClusterSnapshotsResponse' {resources} -> resources) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {resources = a} :: BatchModifyClusterSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of any errors returned.
batchModifyClusterSnapshotsResponse_errors :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Core.Maybe [SnapshotErrorMessage])
batchModifyClusterSnapshotsResponse_errors = Lens.lens (\BatchModifyClusterSnapshotsResponse' {errors} -> errors) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {errors = a} :: BatchModifyClusterSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchModifyClusterSnapshotsResponse_httpStatus :: Lens.Lens' BatchModifyClusterSnapshotsResponse Core.Int
batchModifyClusterSnapshotsResponse_httpStatus = Lens.lens (\BatchModifyClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: BatchModifyClusterSnapshotsResponse)

instance
  Core.NFData
    BatchModifyClusterSnapshotsResponse
