{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.DeleteInstanceSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteInstanceSnapshot
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstanceSnapshot' smart constructor.
data DeleteInstanceSnapshot = DeleteInstanceSnapshot'
  { -- | The name of the snapshot to delete.
    instanceSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteInstanceSnapshot where
  type
    Rs DeleteInstanceSnapshot =
      DeleteInstanceSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInstanceSnapshotResponse'
            Prelude.<$> ( x Prelude..?> "operations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInstanceSnapshot

instance Prelude.NFData DeleteInstanceSnapshot

instance Prelude.ToHeaders DeleteInstanceSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.DeleteInstanceSnapshot" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteInstanceSnapshot where
  toJSON DeleteInstanceSnapshot' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "instanceSnapshotName"
                  Prelude..= instanceSnapshotName
              )
          ]
      )

instance Prelude.ToPath DeleteInstanceSnapshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteInstanceSnapshot where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
deleteInstanceSnapshotResponse_operations = Lens.lens (\DeleteInstanceSnapshotResponse' {operations} -> operations) (\s@DeleteInstanceSnapshotResponse' {} a -> s {operations = a} :: DeleteInstanceSnapshotResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
deleteInstanceSnapshotResponse_httpStatus :: Lens.Lens' DeleteInstanceSnapshotResponse Prelude.Int
deleteInstanceSnapshotResponse_httpStatus = Lens.lens (\DeleteInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteInstanceSnapshotResponse)

instance
  Prelude.NFData
    DeleteInstanceSnapshotResponse
