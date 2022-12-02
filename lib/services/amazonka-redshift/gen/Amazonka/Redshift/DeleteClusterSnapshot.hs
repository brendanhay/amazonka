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
-- Module      : Amazonka.Redshift.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified manual snapshot. The snapshot must be in the
-- @available@ state, with no other users authorized to access the
-- snapshot.
--
-- Unlike automated snapshots, manual snapshots are retained even after you
-- delete your cluster. Amazon Redshift does not delete your manual
-- snapshots. You must delete manual snapshot explicitly to avoid getting
-- charged. If other accounts are authorized to access the snapshot, you
-- must revoke all of the authorizations before you can delete the
-- snapshot.
module Amazonka.Redshift.DeleteClusterSnapshot
  ( -- * Creating a Request
    DeleteClusterSnapshot (..),
    newDeleteClusterSnapshot,

    -- * Request Lenses
    deleteClusterSnapshot_snapshotClusterIdentifier,
    deleteClusterSnapshot_snapshotIdentifier,

    -- * Destructuring the Response
    DeleteClusterSnapshotResponse (..),
    newDeleteClusterSnapshotResponse,

    -- * Response Lenses
    deleteClusterSnapshotResponse_snapshot,
    deleteClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteClusterSnapshot' smart constructor.
data DeleteClusterSnapshot = DeleteClusterSnapshot'
  { -- | The unique identifier of the cluster the snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    --
    -- Constraints: Must be the name of valid cluster.
    snapshotClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the manual snapshot to be deleted.
    --
    -- Constraints: Must be the name of an existing snapshot that is in the
    -- @available@, @failed@, or @cancelled@ state.
    snapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotClusterIdentifier', 'deleteClusterSnapshot_snapshotClusterIdentifier' - The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- 'snapshotIdentifier', 'deleteClusterSnapshot_snapshotIdentifier' - The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the
-- @available@, @failed@, or @cancelled@ state.
newDeleteClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Prelude.Text ->
  DeleteClusterSnapshot
newDeleteClusterSnapshot pSnapshotIdentifier_ =
  DeleteClusterSnapshot'
    { snapshotClusterIdentifier =
        Prelude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints: Must be the name of valid cluster.
deleteClusterSnapshot_snapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshot (Prelude.Maybe Prelude.Text)
deleteClusterSnapshot_snapshotClusterIdentifier = Lens.lens (\DeleteClusterSnapshot' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@DeleteClusterSnapshot' {} a -> s {snapshotClusterIdentifier = a} :: DeleteClusterSnapshot)

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the
-- @available@, @failed@, or @cancelled@ state.
deleteClusterSnapshot_snapshotIdentifier :: Lens.Lens' DeleteClusterSnapshot Prelude.Text
deleteClusterSnapshot_snapshotIdentifier = Lens.lens (\DeleteClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@DeleteClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: DeleteClusterSnapshot)

instance Core.AWSRequest DeleteClusterSnapshot where
  type
    AWSResponse DeleteClusterSnapshot =
      DeleteClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteClusterSnapshotResult"
      ( \s h x ->
          DeleteClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClusterSnapshot where
  hashWithSalt _salt DeleteClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` snapshotClusterIdentifier
      `Prelude.hashWithSalt` snapshotIdentifier

instance Prelude.NFData DeleteClusterSnapshot where
  rnf DeleteClusterSnapshot' {..} =
    Prelude.rnf snapshotClusterIdentifier
      `Prelude.seq` Prelude.rnf snapshotIdentifier

instance Data.ToHeaders DeleteClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteClusterSnapshot where
  toQuery DeleteClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "SnapshotClusterIdentifier"
          Data.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Data.=: snapshotIdentifier
      ]

-- | /See:/ 'newDeleteClusterSnapshotResponse' smart constructor.
data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'deleteClusterSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'deleteClusterSnapshotResponse_httpStatus' - The response's http status code.
newDeleteClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClusterSnapshotResponse
newDeleteClusterSnapshotResponse pHttpStatus_ =
  DeleteClusterSnapshotResponse'
    { snapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteClusterSnapshotResponse_snapshot :: Lens.Lens' DeleteClusterSnapshotResponse (Prelude.Maybe Snapshot)
deleteClusterSnapshotResponse_snapshot = Lens.lens (\DeleteClusterSnapshotResponse' {snapshot} -> snapshot) (\s@DeleteClusterSnapshotResponse' {} a -> s {snapshot = a} :: DeleteClusterSnapshotResponse)

-- | The response's http status code.
deleteClusterSnapshotResponse_httpStatus :: Lens.Lens' DeleteClusterSnapshotResponse Prelude.Int
deleteClusterSnapshotResponse_httpStatus = Lens.lens (\DeleteClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteClusterSnapshotResponse)

instance Prelude.NFData DeleteClusterSnapshotResponse where
  rnf DeleteClusterSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
