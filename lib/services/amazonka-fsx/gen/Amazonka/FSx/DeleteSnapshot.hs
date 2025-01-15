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
-- Module      : Amazonka.FSx.DeleteSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon FSx for OpenZFS snapshot. After deletion, the snapshot
-- no longer exists, and its data is gone. Deleting a snapshot doesn\'t
-- affect snapshots stored in a file system backup.
--
-- The @DeleteSnapshot@ operation returns instantly. The snapshot appears
-- with the lifecycle status of @DELETING@ until the deletion is complete.
module Amazonka.FSx.DeleteSnapshot
  ( -- * Creating a Request
    DeleteSnapshot (..),
    newDeleteSnapshot,

    -- * Request Lenses
    deleteSnapshot_clientRequestToken,
    deleteSnapshot_snapshotId,

    -- * Destructuring the Response
    DeleteSnapshotResponse (..),
    newDeleteSnapshotResponse,

    -- * Response Lenses
    deleteSnapshotResponse_lifecycle,
    deleteSnapshotResponse_snapshotId,
    deleteSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot that you want to delete.
    snapshotId :: Prelude.Text
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
-- 'clientRequestToken', 'deleteSnapshot_clientRequestToken' - Undocumented member.
--
-- 'snapshotId', 'deleteSnapshot_snapshotId' - The ID of the snapshot that you want to delete.
newDeleteSnapshot ::
  -- | 'snapshotId'
  Prelude.Text ->
  DeleteSnapshot
newDeleteSnapshot pSnapshotId_ =
  DeleteSnapshot'
    { clientRequestToken =
        Prelude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | Undocumented member.
deleteSnapshot_clientRequestToken :: Lens.Lens' DeleteSnapshot (Prelude.Maybe Prelude.Text)
deleteSnapshot_clientRequestToken = Lens.lens (\DeleteSnapshot' {clientRequestToken} -> clientRequestToken) (\s@DeleteSnapshot' {} a -> s {clientRequestToken = a} :: DeleteSnapshot)

-- | The ID of the snapshot that you want to delete.
deleteSnapshot_snapshotId :: Lens.Lens' DeleteSnapshot Prelude.Text
deleteSnapshot_snapshotId = Lens.lens (\DeleteSnapshot' {snapshotId} -> snapshotId) (\s@DeleteSnapshot' {} a -> s {snapshotId = a} :: DeleteSnapshot)

instance Core.AWSRequest DeleteSnapshot where
  type
    AWSResponse DeleteSnapshot =
      DeleteSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSnapshotResponse'
            Prelude.<$> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "SnapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSnapshot where
  hashWithSalt _salt DeleteSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData DeleteSnapshot where
  rnf DeleteSnapshot' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf snapshotId

instance Data.ToHeaders DeleteSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSnapshot where
  toJSON DeleteSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("SnapshotId" Data..= snapshotId)
          ]
      )

instance Data.ToPath DeleteSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { -- | The lifecycle status of the snapshot. If the @DeleteSnapshot@ operation
    -- is successful, this status is @DELETING@.
    lifecycle :: Prelude.Maybe SnapshotLifecycle,
    -- | The ID of the deleted snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
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
-- 'lifecycle', 'deleteSnapshotResponse_lifecycle' - The lifecycle status of the snapshot. If the @DeleteSnapshot@ operation
-- is successful, this status is @DELETING@.
--
-- 'snapshotId', 'deleteSnapshotResponse_snapshotId' - The ID of the deleted snapshot.
--
-- 'httpStatus', 'deleteSnapshotResponse_httpStatus' - The response's http status code.
newDeleteSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSnapshotResponse
newDeleteSnapshotResponse pHttpStatus_ =
  DeleteSnapshotResponse'
    { lifecycle =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle status of the snapshot. If the @DeleteSnapshot@ operation
-- is successful, this status is @DELETING@.
deleteSnapshotResponse_lifecycle :: Lens.Lens' DeleteSnapshotResponse (Prelude.Maybe SnapshotLifecycle)
deleteSnapshotResponse_lifecycle = Lens.lens (\DeleteSnapshotResponse' {lifecycle} -> lifecycle) (\s@DeleteSnapshotResponse' {} a -> s {lifecycle = a} :: DeleteSnapshotResponse)

-- | The ID of the deleted snapshot.
deleteSnapshotResponse_snapshotId :: Lens.Lens' DeleteSnapshotResponse (Prelude.Maybe Prelude.Text)
deleteSnapshotResponse_snapshotId = Lens.lens (\DeleteSnapshotResponse' {snapshotId} -> snapshotId) (\s@DeleteSnapshotResponse' {} a -> s {snapshotId = a} :: DeleteSnapshotResponse)

-- | The response's http status code.
deleteSnapshotResponse_httpStatus :: Lens.Lens' DeleteSnapshotResponse Prelude.Int
deleteSnapshotResponse_httpStatus = Lens.lens (\DeleteSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteSnapshotResponse)

instance Prelude.NFData DeleteSnapshotResponse where
  rnf DeleteSnapshotResponse' {..} =
    Prelude.rnf lifecycle `Prelude.seq`
      Prelude.rnf snapshotId `Prelude.seq`
        Prelude.rnf httpStatus
