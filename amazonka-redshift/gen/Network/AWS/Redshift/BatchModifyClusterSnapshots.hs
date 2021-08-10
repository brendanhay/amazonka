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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchModifyClusterSnapshots' smart constructor.
data BatchModifyClusterSnapshots = BatchModifyClusterSnapshots'
  { -- | A boolean value indicating whether to override an exception if the
    -- retention period has passed.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that a manual snapshot is retained. If you specify
    -- the value -1, the manual snapshot is retained indefinitely.
    --
    -- The number must be either -1 or an integer between 1 and 3,653.
    --
    -- If you decrease the manual snapshot retention period from its current
    -- value, existing manual snapshots that fall outside of the new retention
    -- period will return an error. If you want to suppress the errors and
    -- delete the snapshots, use the force option.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A list of snapshot identifiers you want to modify.
    snapshotIdentifierList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { force =
        Prelude.Nothing,
      manualSnapshotRetentionPeriod =
        Prelude.Nothing,
      snapshotIdentifierList = Prelude.mempty
    }

-- | A boolean value indicating whether to override an exception if the
-- retention period has passed.
batchModifyClusterSnapshots_force :: Lens.Lens' BatchModifyClusterSnapshots (Prelude.Maybe Prelude.Bool)
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
batchModifyClusterSnapshots_manualSnapshotRetentionPeriod :: Lens.Lens' BatchModifyClusterSnapshots (Prelude.Maybe Prelude.Int)
batchModifyClusterSnapshots_manualSnapshotRetentionPeriod = Lens.lens (\BatchModifyClusterSnapshots' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@BatchModifyClusterSnapshots' {} a -> s {manualSnapshotRetentionPeriod = a} :: BatchModifyClusterSnapshots)

-- | A list of snapshot identifiers you want to modify.
batchModifyClusterSnapshots_snapshotIdentifierList :: Lens.Lens' BatchModifyClusterSnapshots [Prelude.Text]
batchModifyClusterSnapshots_snapshotIdentifierList = Lens.lens (\BatchModifyClusterSnapshots' {snapshotIdentifierList} -> snapshotIdentifierList) (\s@BatchModifyClusterSnapshots' {} a -> s {snapshotIdentifierList = a} :: BatchModifyClusterSnapshots) Prelude.. Lens._Coerce

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
            Prelude.<$> ( x Core..@? "Resources" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "String")
                        )
            Prelude.<*> ( x Core..@? "Errors" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "SnapshotErrorMessage")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchModifyClusterSnapshots

instance Prelude.NFData BatchModifyClusterSnapshots

instance Core.ToHeaders BatchModifyClusterSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath BatchModifyClusterSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchModifyClusterSnapshots where
  toQuery BatchModifyClusterSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "BatchModifyClusterSnapshots" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Force" Core.=: force,
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "SnapshotIdentifierList"
          Core.=: Core.toQueryList "String" snapshotIdentifierList
      ]

-- | /See:/ 'newBatchModifyClusterSnapshotsResponse' smart constructor.
data BatchModifyClusterSnapshotsResponse = BatchModifyClusterSnapshotsResponse'
  { -- | A list of the snapshots that were modified.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | A list of any errors returned.
    errors :: Prelude.Maybe [SnapshotErrorMessage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchModifyClusterSnapshotsResponse
newBatchModifyClusterSnapshotsResponse pHttpStatus_ =
  BatchModifyClusterSnapshotsResponse'
    { resources =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the snapshots that were modified.
batchModifyClusterSnapshotsResponse_resources :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Prelude.Maybe [Prelude.Text])
batchModifyClusterSnapshotsResponse_resources = Lens.lens (\BatchModifyClusterSnapshotsResponse' {resources} -> resources) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {resources = a} :: BatchModifyClusterSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of any errors returned.
batchModifyClusterSnapshotsResponse_errors :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Prelude.Maybe [SnapshotErrorMessage])
batchModifyClusterSnapshotsResponse_errors = Lens.lens (\BatchModifyClusterSnapshotsResponse' {errors} -> errors) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {errors = a} :: BatchModifyClusterSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchModifyClusterSnapshotsResponse_httpStatus :: Lens.Lens' BatchModifyClusterSnapshotsResponse Prelude.Int
batchModifyClusterSnapshotsResponse_httpStatus = Lens.lens (\BatchModifyClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: BatchModifyClusterSnapshotsResponse)

instance
  Prelude.NFData
    BatchModifyClusterSnapshotsResponse
