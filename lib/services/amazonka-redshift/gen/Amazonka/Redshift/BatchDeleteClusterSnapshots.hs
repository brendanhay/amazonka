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
-- Module      : Amazonka.Redshift.BatchDeleteClusterSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of cluster snapshots.
module Amazonka.Redshift.BatchDeleteClusterSnapshots
  ( -- * Creating a Request
    BatchDeleteClusterSnapshots (..),
    newBatchDeleteClusterSnapshots,

    -- * Request Lenses
    batchDeleteClusterSnapshots_identifiers,

    -- * Destructuring the Response
    BatchDeleteClusterSnapshotsResponse (..),
    newBatchDeleteClusterSnapshotsResponse,

    -- * Response Lenses
    batchDeleteClusterSnapshotsResponse_errors,
    batchDeleteClusterSnapshotsResponse_resources,
    batchDeleteClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteClusterSnapshots' smart constructor.
data BatchDeleteClusterSnapshots = BatchDeleteClusterSnapshots'
  { -- | A list of identifiers for the snapshots that you want to delete.
    identifiers :: [DeleteClusterSnapshotMessage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteClusterSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifiers', 'batchDeleteClusterSnapshots_identifiers' - A list of identifiers for the snapshots that you want to delete.
newBatchDeleteClusterSnapshots ::
  BatchDeleteClusterSnapshots
newBatchDeleteClusterSnapshots =
  BatchDeleteClusterSnapshots'
    { identifiers =
        Prelude.mempty
    }

-- | A list of identifiers for the snapshots that you want to delete.
batchDeleteClusterSnapshots_identifiers :: Lens.Lens' BatchDeleteClusterSnapshots [DeleteClusterSnapshotMessage]
batchDeleteClusterSnapshots_identifiers = Lens.lens (\BatchDeleteClusterSnapshots' {identifiers} -> identifiers) (\s@BatchDeleteClusterSnapshots' {} a -> s {identifiers = a} :: BatchDeleteClusterSnapshots) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteClusterSnapshots where
  type
    AWSResponse BatchDeleteClusterSnapshots =
      BatchDeleteClusterSnapshotsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "BatchDeleteClusterSnapshotsResult"
      ( \s h x ->
          BatchDeleteClusterSnapshotsResponse'
            Prelude.<$> ( x Data..@? "Errors" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "SnapshotErrorMessage")
                        )
            Prelude.<*> ( x Data..@? "Resources" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "String")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteClusterSnapshots where
  hashWithSalt _salt BatchDeleteClusterSnapshots' {..} =
    _salt `Prelude.hashWithSalt` identifiers

instance Prelude.NFData BatchDeleteClusterSnapshots where
  rnf BatchDeleteClusterSnapshots' {..} =
    Prelude.rnf identifiers

instance Data.ToHeaders BatchDeleteClusterSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath BatchDeleteClusterSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteClusterSnapshots where
  toQuery BatchDeleteClusterSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "BatchDeleteClusterSnapshots" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Identifiers"
          Data.=: Data.toQueryList
            "DeleteClusterSnapshotMessage"
            identifiers
      ]

-- | /See:/ 'newBatchDeleteClusterSnapshotsResponse' smart constructor.
data BatchDeleteClusterSnapshotsResponse = BatchDeleteClusterSnapshotsResponse'
  { -- | A list of any errors returned.
    errors :: Prelude.Maybe [SnapshotErrorMessage],
    -- | A list of the snapshot identifiers that were deleted.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteClusterSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteClusterSnapshotsResponse_errors' - A list of any errors returned.
--
-- 'resources', 'batchDeleteClusterSnapshotsResponse_resources' - A list of the snapshot identifiers that were deleted.
--
-- 'httpStatus', 'batchDeleteClusterSnapshotsResponse_httpStatus' - The response's http status code.
newBatchDeleteClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteClusterSnapshotsResponse
newBatchDeleteClusterSnapshotsResponse pHttpStatus_ =
  BatchDeleteClusterSnapshotsResponse'
    { errors =
        Prelude.Nothing,
      resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of any errors returned.
batchDeleteClusterSnapshotsResponse_errors :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Prelude.Maybe [SnapshotErrorMessage])
batchDeleteClusterSnapshotsResponse_errors = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {errors} -> errors) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {errors = a} :: BatchDeleteClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the snapshot identifiers that were deleted.
batchDeleteClusterSnapshotsResponse_resources :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Prelude.Maybe [Prelude.Text])
batchDeleteClusterSnapshotsResponse_resources = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {resources} -> resources) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {resources = a} :: BatchDeleteClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteClusterSnapshotsResponse_httpStatus :: Lens.Lens' BatchDeleteClusterSnapshotsResponse Prelude.Int
batchDeleteClusterSnapshotsResponse_httpStatus = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: BatchDeleteClusterSnapshotsResponse)

instance
  Prelude.NFData
    BatchDeleteClusterSnapshotsResponse
  where
  rnf BatchDeleteClusterSnapshotsResponse' {..} =
    Prelude.rnf errors `Prelude.seq`
      Prelude.rnf resources `Prelude.seq`
        Prelude.rnf httpStatus
