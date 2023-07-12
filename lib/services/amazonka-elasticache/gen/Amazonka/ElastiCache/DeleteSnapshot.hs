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
-- Module      : Amazonka.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing snapshot. When you receive a successful response
-- from this operation, ElastiCache immediately begins deleting the
-- snapshot; you cannot cancel or revert this operation.
--
-- This operation is valid for Redis only.
module Amazonka.ElastiCache.DeleteSnapshot
  ( -- * Creating a Request
    DeleteSnapshot (..),
    newDeleteSnapshot,

    -- * Request Lenses
    deleteSnapshot_snapshotName,

    -- * Destructuring the Response
    DeleteSnapshotResponse (..),
    newDeleteSnapshotResponse,

    -- * Response Lenses
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteSnapshot@ operation.
--
-- /See:/ 'newDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { -- | The name of the snapshot to be deleted.
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotName', 'deleteSnapshot_snapshotName' - The name of the snapshot to be deleted.
newDeleteSnapshot ::
  -- | 'snapshotName'
  Prelude.Text ->
  DeleteSnapshot
newDeleteSnapshot pSnapshotName_ =
  DeleteSnapshot' {snapshotName = pSnapshotName_}

-- | The name of the snapshot to be deleted.
deleteSnapshot_snapshotName :: Lens.Lens' DeleteSnapshot Prelude.Text
deleteSnapshot_snapshotName = Lens.lens (\DeleteSnapshot' {snapshotName} -> snapshotName) (\s@DeleteSnapshot' {} a -> s {snapshotName = a} :: DeleteSnapshot)

instance Core.AWSRequest DeleteSnapshot where
  type
    AWSResponse DeleteSnapshot =
      DeleteSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteSnapshotResult"
      ( \s h x ->
          DeleteSnapshotResponse'
            Prelude.<$> (x Data..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSnapshot where
  hashWithSalt _salt DeleteSnapshot' {..} =
    _salt `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData DeleteSnapshot where
  rnf DeleteSnapshot' {..} = Prelude.rnf snapshotName

instance Data.ToHeaders DeleteSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSnapshot where
  toQuery DeleteSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "SnapshotName" Data.=: snapshotName
      ]

-- | /See:/ 'newDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'deleteSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'deleteSnapshotResponse_httpStatus' - The response's http status code.
newDeleteSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSnapshotResponse
newDeleteSnapshotResponse pHttpStatus_ =
  DeleteSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteSnapshotResponse_snapshot :: Lens.Lens' DeleteSnapshotResponse (Prelude.Maybe Snapshot)
deleteSnapshotResponse_snapshot = Lens.lens (\DeleteSnapshotResponse' {snapshot} -> snapshot) (\s@DeleteSnapshotResponse' {} a -> s {snapshot = a} :: DeleteSnapshotResponse)

-- | The response's http status code.
deleteSnapshotResponse_httpStatus :: Lens.Lens' DeleteSnapshotResponse Prelude.Int
deleteSnapshotResponse_httpStatus = Lens.lens (\DeleteSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteSnapshotResponse)

instance Prelude.NFData DeleteSnapshotResponse where
  rnf DeleteSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
