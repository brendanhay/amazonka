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
-- Module      : Amazonka.Lightsail.DeleteInstanceSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific snapshot of a virtual private server (or /instance/).
--
-- The @delete instance snapshot@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- @instance snapshot name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DeleteInstanceSnapshot
  ( -- * Creating a Request
    DeleteInstanceSnapshot (..),
    newDeleteInstanceSnapshot,

    -- * Request Lenses
    deleteInstanceSnapshot_instanceSnapshotName,

    -- * Destructuring the Response
    DeleteInstanceSnapshotResponse (..),
    newDeleteInstanceSnapshotResponse,

    -- * Response Lenses
    deleteInstanceSnapshotResponse_operations,
    deleteInstanceSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInstanceSnapshot' smart constructor.
data DeleteInstanceSnapshot = DeleteInstanceSnapshot'
  { -- | The name of the snapshot to delete.
    instanceSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSnapshotName', 'deleteInstanceSnapshot_instanceSnapshotName' - The name of the snapshot to delete.
newDeleteInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Prelude.Text ->
  DeleteInstanceSnapshot
newDeleteInstanceSnapshot pInstanceSnapshotName_ =
  DeleteInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_
    }

-- | The name of the snapshot to delete.
deleteInstanceSnapshot_instanceSnapshotName :: Lens.Lens' DeleteInstanceSnapshot Prelude.Text
deleteInstanceSnapshot_instanceSnapshotName = Lens.lens (\DeleteInstanceSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@DeleteInstanceSnapshot' {} a -> s {instanceSnapshotName = a} :: DeleteInstanceSnapshot)

instance Core.AWSRequest DeleteInstanceSnapshot where
  type
    AWSResponse DeleteInstanceSnapshot =
      DeleteInstanceSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInstanceSnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInstanceSnapshot where
  hashWithSalt _salt DeleteInstanceSnapshot' {..} =
    _salt `Prelude.hashWithSalt` instanceSnapshotName

instance Prelude.NFData DeleteInstanceSnapshot where
  rnf DeleteInstanceSnapshot' {..} =
    Prelude.rnf instanceSnapshotName

instance Data.ToHeaders DeleteInstanceSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteInstanceSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteInstanceSnapshot where
  toJSON DeleteInstanceSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "instanceSnapshotName"
                  Data..= instanceSnapshotName
              )
          ]
      )

instance Data.ToPath DeleteInstanceSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInstanceSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceSnapshotResponse' smart constructor.
data DeleteInstanceSnapshotResponse = DeleteInstanceSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteInstanceSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteInstanceSnapshotResponse_httpStatus' - The response's http status code.
newDeleteInstanceSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInstanceSnapshotResponse
newDeleteInstanceSnapshotResponse pHttpStatus_ =
  DeleteInstanceSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteInstanceSnapshotResponse_operations :: Lens.Lens' DeleteInstanceSnapshotResponse (Prelude.Maybe [Operation])
deleteInstanceSnapshotResponse_operations = Lens.lens (\DeleteInstanceSnapshotResponse' {operations} -> operations) (\s@DeleteInstanceSnapshotResponse' {} a -> s {operations = a} :: DeleteInstanceSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteInstanceSnapshotResponse_httpStatus :: Lens.Lens' DeleteInstanceSnapshotResponse Prelude.Int
deleteInstanceSnapshotResponse_httpStatus = Lens.lens (\DeleteInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteInstanceSnapshotResponse)

instance
  Prelude.NFData
    DeleteInstanceSnapshotResponse
  where
  rnf DeleteInstanceSnapshotResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
