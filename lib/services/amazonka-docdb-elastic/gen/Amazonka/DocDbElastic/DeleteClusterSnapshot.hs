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
-- Module      : Amazonka.DocDbElastic.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a Elastic DocumentDB snapshot.
module Amazonka.DocDbElastic.DeleteClusterSnapshot
  ( -- * Creating a Request
    DeleteClusterSnapshot (..),
    newDeleteClusterSnapshot,

    -- * Request Lenses
    deleteClusterSnapshot_snapshotArn,

    -- * Destructuring the Response
    DeleteClusterSnapshotResponse (..),
    newDeleteClusterSnapshotResponse,

    -- * Response Lenses
    deleteClusterSnapshotResponse_httpStatus,
    deleteClusterSnapshotResponse_snapshot,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteClusterSnapshot' smart constructor.
data DeleteClusterSnapshot = DeleteClusterSnapshot'
  { -- | The arn of the Elastic DocumentDB snapshot that is to be deleted.
    snapshotArn :: Prelude.Text
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
-- 'snapshotArn', 'deleteClusterSnapshot_snapshotArn' - The arn of the Elastic DocumentDB snapshot that is to be deleted.
newDeleteClusterSnapshot ::
  -- | 'snapshotArn'
  Prelude.Text ->
  DeleteClusterSnapshot
newDeleteClusterSnapshot pSnapshotArn_ =
  DeleteClusterSnapshot' {snapshotArn = pSnapshotArn_}

-- | The arn of the Elastic DocumentDB snapshot that is to be deleted.
deleteClusterSnapshot_snapshotArn :: Lens.Lens' DeleteClusterSnapshot Prelude.Text
deleteClusterSnapshot_snapshotArn = Lens.lens (\DeleteClusterSnapshot' {snapshotArn} -> snapshotArn) (\s@DeleteClusterSnapshot' {} a -> s {snapshotArn = a} :: DeleteClusterSnapshot)

instance Core.AWSRequest DeleteClusterSnapshot where
  type
    AWSResponse DeleteClusterSnapshot =
      DeleteClusterSnapshotResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "snapshot")
      )

instance Prelude.Hashable DeleteClusterSnapshot where
  hashWithSalt _salt DeleteClusterSnapshot' {..} =
    _salt `Prelude.hashWithSalt` snapshotArn

instance Prelude.NFData DeleteClusterSnapshot where
  rnf DeleteClusterSnapshot' {..} =
    Prelude.rnf snapshotArn

instance Data.ToHeaders DeleteClusterSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteClusterSnapshot where
  toPath DeleteClusterSnapshot' {..} =
    Prelude.mconcat
      ["/cluster-snapshot/", Data.toBS snapshotArn]

instance Data.ToQuery DeleteClusterSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClusterSnapshotResponse' smart constructor.
data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about the newly deleted Elastic DocumentDB snapshot.
    snapshot :: ClusterSnapshot
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
-- 'httpStatus', 'deleteClusterSnapshotResponse_httpStatus' - The response's http status code.
--
-- 'snapshot', 'deleteClusterSnapshotResponse_snapshot' - Returns information about the newly deleted Elastic DocumentDB snapshot.
newDeleteClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'snapshot'
  ClusterSnapshot ->
  DeleteClusterSnapshotResponse
newDeleteClusterSnapshotResponse
  pHttpStatus_
  pSnapshot_ =
    DeleteClusterSnapshotResponse'
      { httpStatus =
          pHttpStatus_,
        snapshot = pSnapshot_
      }

-- | The response's http status code.
deleteClusterSnapshotResponse_httpStatus :: Lens.Lens' DeleteClusterSnapshotResponse Prelude.Int
deleteClusterSnapshotResponse_httpStatus = Lens.lens (\DeleteClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteClusterSnapshotResponse)

-- | Returns information about the newly deleted Elastic DocumentDB snapshot.
deleteClusterSnapshotResponse_snapshot :: Lens.Lens' DeleteClusterSnapshotResponse ClusterSnapshot
deleteClusterSnapshotResponse_snapshot = Lens.lens (\DeleteClusterSnapshotResponse' {snapshot} -> snapshot) (\s@DeleteClusterSnapshotResponse' {} a -> s {snapshot = a} :: DeleteClusterSnapshotResponse)

instance Prelude.NFData DeleteClusterSnapshotResponse where
  rnf DeleteClusterSnapshotResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf snapshot
