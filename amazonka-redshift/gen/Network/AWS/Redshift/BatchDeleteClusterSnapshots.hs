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
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeleteClusterSnapshots' smart constructor.
data BatchDeleteClusterSnapshots = BatchDeleteClusterSnapshots'
  { -- | A list of identifiers for the snapshots that you want to delete.
    identifiers :: [DeleteClusterSnapshotMessage]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | A list of identifiers for the snapshots that you want to delete.
batchDeleteClusterSnapshots_identifiers :: Lens.Lens' BatchDeleteClusterSnapshots [DeleteClusterSnapshotMessage]
batchDeleteClusterSnapshots_identifiers = Lens.lens (\BatchDeleteClusterSnapshots' {identifiers} -> identifiers) (\s@BatchDeleteClusterSnapshots' {} a -> s {identifiers = a} :: BatchDeleteClusterSnapshots) Core.. Lens._Coerce

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
            Core.<$> ( x Core..@? "Resources" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "String")
                     )
            Core.<*> ( x Core..@? "Errors" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "SnapshotErrorMessage")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchDeleteClusterSnapshots

instance Core.NFData BatchDeleteClusterSnapshots

instance Core.ToHeaders BatchDeleteClusterSnapshots where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BatchDeleteClusterSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeleteClusterSnapshots where
  toQuery BatchDeleteClusterSnapshots' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BatchDeleteClusterSnapshots" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Identifiers"
          Core.=: Core.toQueryList
            "DeleteClusterSnapshotMessage"
            identifiers
      ]

-- | /See:/ 'newBatchDeleteClusterSnapshotsResponse' smart constructor.
data BatchDeleteClusterSnapshotsResponse = BatchDeleteClusterSnapshotsResponse'
  { -- | A list of the snapshot identifiers that were deleted.
    resources :: Core.Maybe [Core.Text],
    -- | A list of any errors returned.
    errors :: Core.Maybe [SnapshotErrorMessage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  BatchDeleteClusterSnapshotsResponse
newBatchDeleteClusterSnapshotsResponse pHttpStatus_ =
  BatchDeleteClusterSnapshotsResponse'
    { resources =
        Core.Nothing,
      errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the snapshot identifiers that were deleted.
batchDeleteClusterSnapshotsResponse_resources :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Core.Maybe [Core.Text])
batchDeleteClusterSnapshotsResponse_resources = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {resources} -> resources) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {resources = a} :: BatchDeleteClusterSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of any errors returned.
batchDeleteClusterSnapshotsResponse_errors :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Core.Maybe [SnapshotErrorMessage])
batchDeleteClusterSnapshotsResponse_errors = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {errors} -> errors) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {errors = a} :: BatchDeleteClusterSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteClusterSnapshotsResponse_httpStatus :: Lens.Lens' BatchDeleteClusterSnapshotsResponse Core.Int
batchDeleteClusterSnapshotsResponse_httpStatus = Lens.lens (\BatchDeleteClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: BatchDeleteClusterSnapshotsResponse)

instance
  Core.NFData
    BatchDeleteClusterSnapshotsResponse
