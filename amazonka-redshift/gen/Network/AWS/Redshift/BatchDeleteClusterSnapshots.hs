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
-- Module      : Network.AWS.Redshift.BatchDeleteClusterSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of cluster snapshots.
module Network.AWS.Redshift.BatchDeleteClusterSnapshots
  ( -- * Creating a Request
    BatchDeleteClusterSnapshots (..),
    newBatchDeleteClusterSnapshots,

    -- * Request Lenses
    batchDeleteClusterSnapshots_identifiers,

    -- * Destructuring the Response
    BatchDeleteClusterSnapshotsResponse (..),
    newBatchDeleteClusterSnapshotsResponse,

    -- * Response Lenses
    batchDeleteClusterSnapshotsResponse_resources,
    batchDeleteClusterSnapshotsResponse_errors,
    batchDeleteClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
batchDeleteClusterSnapshots_identifiers = Lens.lens (\BatchDeleteClusterSnapshots' {identifiers} -> identifiers) (\s@BatchDeleteClusterSnapshots' {} a -> s {identifiers = a} :: BatchDeleteClusterSnapshots) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchDeleteClusterSnapshots where
  type
    AWSResponse BatchDeleteClusterSnapshots =
      BatchDeleteClusterSnapshotsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BatchDeleteClusterSnapshotsResult"
      ( \s h x ->
          BatchDeleteClusterSnapshotsResponse'
            Prelude.<$> ( x Core..@? "Resources" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "String")
                        )
            Prelude.<*> ( x Core..@? "Errors" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "SnapshotErrorMessage")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteClusterSnapshots

instance Prelude.NFData BatchDeleteClusterSnapshots

instance Core.ToHeaders BatchDeleteClusterSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath BatchDeleteClusterSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchDeleteClusterSnapshots where
  toQuery BatchDeleteClusterSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "BatchDeleteClusterSnapshots" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Identifiers"
          Core.=: Core.toQueryList
            "DeleteClusterSnapshotMessage"
            identifiers
      ]

-- | /See:/ 'newBatchDeleteClusterSnapshotsResponse' smart constructor.
data BatchDeleteClusterSnapshotsResponse = BatchDeleteClusterSnapshotsResponse'
  { -- | A list of the snapshot identifiers that were deleted.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | A list of any errors returned.
    errors :: Prelude.Maybe [SnapshotErrorMessage],
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
-- 'resources', 'batchDeleteClusterSnapshotsResponse_resources' - A list of the snapshot identifiers that were deleted.
--
-- 'errors', 'batchDeleteClusterSnapshotsResponse_errors' - A list of any errors returned.
--
-- 'httpStatus', 'batchDeleteClusterSnapshotsResponse_httpStatus' - The response's http status code.
newBatchDeleteClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteClusterSnapshotsResponse
newBatchDeleteClusterSnapshotsResponse pHttpStatus_ =
  BatchDeleteClusterSnapshotsResponse'
    { resources =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the snapshot identifiers that were deleted.
batchDeleteClusterSnapshotsResponse_resources :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Prelude.Maybe [Prelude.Text])
batchDeleteClusterSnapshotsResponse_resources = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {resources} -> resources) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {resources = a} :: BatchDeleteClusterSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of any errors returned.
batchDeleteClusterSnapshotsResponse_errors :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Prelude.Maybe [SnapshotErrorMessage])
batchDeleteClusterSnapshotsResponse_errors = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {errors} -> errors) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {errors = a} :: BatchDeleteClusterSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteClusterSnapshotsResponse_httpStatus :: Lens.Lens' BatchDeleteClusterSnapshotsResponse Prelude.Int
batchDeleteClusterSnapshotsResponse_httpStatus = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: BatchDeleteClusterSnapshotsResponse)

instance
  Prelude.NFData
    BatchDeleteClusterSnapshotsResponse
