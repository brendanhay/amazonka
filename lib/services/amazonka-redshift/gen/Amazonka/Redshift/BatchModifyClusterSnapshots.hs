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
-- Module      : Amazonka.Redshift.BatchModifyClusterSnapshots
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a set of cluster snapshots.
module Amazonka.Redshift.BatchModifyClusterSnapshots
  ( -- * Creating a Request
    BatchModifyClusterSnapshots (..),
    newBatchModifyClusterSnapshots,

    -- * Request Lenses
    batchModifyClusterSnapshots_manualSnapshotRetentionPeriod,
    batchModifyClusterSnapshots_force,
    batchModifyClusterSnapshots_snapshotIdentifierList,

    -- * Destructuring the Response
    BatchModifyClusterSnapshotsResponse (..),
    newBatchModifyClusterSnapshotsResponse,

    -- * Response Lenses
    batchModifyClusterSnapshotsResponse_errors,
    batchModifyClusterSnapshotsResponse_resources,
    batchModifyClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchModifyClusterSnapshots' smart constructor.
data BatchModifyClusterSnapshots = BatchModifyClusterSnapshots'
  { -- | The number of days that a manual snapshot is retained. If you specify
    -- the value -1, the manual snapshot is retained indefinitely.
    --
    -- The number must be either -1 or an integer between 1 and 3,653.
    --
    -- If you decrease the manual snapshot retention period from its current
    -- value, existing manual snapshots that fall outside of the new retention
    -- period will return an error. If you want to suppress the errors and
    -- delete the snapshots, use the force option.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A boolean value indicating whether to override an exception if the
    -- retention period has passed.
    force :: Prelude.Maybe Prelude.Bool,
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
-- 'force', 'batchModifyClusterSnapshots_force' - A boolean value indicating whether to override an exception if the
-- retention period has passed.
--
-- 'snapshotIdentifierList', 'batchModifyClusterSnapshots_snapshotIdentifierList' - A list of snapshot identifiers you want to modify.
newBatchModifyClusterSnapshots ::
  BatchModifyClusterSnapshots
newBatchModifyClusterSnapshots =
  BatchModifyClusterSnapshots'
    { manualSnapshotRetentionPeriod =
        Prelude.Nothing,
      force = Prelude.Nothing,
      snapshotIdentifierList = Prelude.mempty
    }

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

-- | A boolean value indicating whether to override an exception if the
-- retention period has passed.
batchModifyClusterSnapshots_force :: Lens.Lens' BatchModifyClusterSnapshots (Prelude.Maybe Prelude.Bool)
batchModifyClusterSnapshots_force = Lens.lens (\BatchModifyClusterSnapshots' {force} -> force) (\s@BatchModifyClusterSnapshots' {} a -> s {force = a} :: BatchModifyClusterSnapshots)

-- | A list of snapshot identifiers you want to modify.
batchModifyClusterSnapshots_snapshotIdentifierList :: Lens.Lens' BatchModifyClusterSnapshots [Prelude.Text]
batchModifyClusterSnapshots_snapshotIdentifierList = Lens.lens (\BatchModifyClusterSnapshots' {snapshotIdentifierList} -> snapshotIdentifierList) (\s@BatchModifyClusterSnapshots' {} a -> s {snapshotIdentifierList = a} :: BatchModifyClusterSnapshots) Prelude.. Lens.coerced

instance Core.AWSRequest BatchModifyClusterSnapshots where
  type
    AWSResponse BatchModifyClusterSnapshots =
      BatchModifyClusterSnapshotsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "BatchModifyClusterSnapshotsResult"
      ( \s h x ->
          BatchModifyClusterSnapshotsResponse'
            Prelude.<$> ( x Data..@? "Errors" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "SnapshotErrorMessage")
                        )
            Prelude.<*> ( x Data..@? "Resources" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "String")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchModifyClusterSnapshots where
  hashWithSalt _salt BatchModifyClusterSnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` snapshotIdentifierList

instance Prelude.NFData BatchModifyClusterSnapshots where
  rnf BatchModifyClusterSnapshots' {..} =
    Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf snapshotIdentifierList

instance Data.ToHeaders BatchModifyClusterSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath BatchModifyClusterSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchModifyClusterSnapshots where
  toQuery BatchModifyClusterSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "BatchModifyClusterSnapshots" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Data.=: manualSnapshotRetentionPeriod,
        "Force" Data.=: force,
        "SnapshotIdentifierList"
          Data.=: Data.toQueryList "String" snapshotIdentifierList
      ]

-- | /See:/ 'newBatchModifyClusterSnapshotsResponse' smart constructor.
data BatchModifyClusterSnapshotsResponse = BatchModifyClusterSnapshotsResponse'
  { -- | A list of any errors returned.
    errors :: Prelude.Maybe [SnapshotErrorMessage],
    -- | A list of the snapshots that were modified.
    resources :: Prelude.Maybe [Prelude.Text],
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
-- 'errors', 'batchModifyClusterSnapshotsResponse_errors' - A list of any errors returned.
--
-- 'resources', 'batchModifyClusterSnapshotsResponse_resources' - A list of the snapshots that were modified.
--
-- 'httpStatus', 'batchModifyClusterSnapshotsResponse_httpStatus' - The response's http status code.
newBatchModifyClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchModifyClusterSnapshotsResponse
newBatchModifyClusterSnapshotsResponse pHttpStatus_ =
  BatchModifyClusterSnapshotsResponse'
    { errors =
        Prelude.Nothing,
      resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of any errors returned.
batchModifyClusterSnapshotsResponse_errors :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Prelude.Maybe [SnapshotErrorMessage])
batchModifyClusterSnapshotsResponse_errors = Lens.lens (\BatchModifyClusterSnapshotsResponse' {errors} -> errors) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {errors = a} :: BatchModifyClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the snapshots that were modified.
batchModifyClusterSnapshotsResponse_resources :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Prelude.Maybe [Prelude.Text])
batchModifyClusterSnapshotsResponse_resources = Lens.lens (\BatchModifyClusterSnapshotsResponse' {resources} -> resources) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {resources = a} :: BatchModifyClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchModifyClusterSnapshotsResponse_httpStatus :: Lens.Lens' BatchModifyClusterSnapshotsResponse Prelude.Int
batchModifyClusterSnapshotsResponse_httpStatus = Lens.lens (\BatchModifyClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@BatchModifyClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: BatchModifyClusterSnapshotsResponse)

instance
  Prelude.NFData
    BatchModifyClusterSnapshotsResponse
  where
  rnf BatchModifyClusterSnapshotsResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
