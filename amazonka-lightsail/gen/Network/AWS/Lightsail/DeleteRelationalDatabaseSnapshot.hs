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
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database snapshot in Amazon Lightsail.
--
-- The @delete relational database snapshot@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
  ( -- * Creating a Request
    DeleteRelationalDatabaseSnapshot (..),
    newDeleteRelationalDatabaseSnapshot,

    -- * Request Lenses
    deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,

    -- * Destructuring the Response
    DeleteRelationalDatabaseSnapshotResponse (..),
    newDeleteRelationalDatabaseSnapshotResponse,

    -- * Response Lenses
    deleteRelationalDatabaseSnapshotResponse_operations,
    deleteRelationalDatabaseSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRelationalDatabaseSnapshot' smart constructor.
data DeleteRelationalDatabaseSnapshot = DeleteRelationalDatabaseSnapshot'
  { -- | The name of the database snapshot that you are deleting.
    relationalDatabaseSnapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRelationalDatabaseSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseSnapshotName', 'deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName' - The name of the database snapshot that you are deleting.
newDeleteRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseSnapshotName'
  Core.Text ->
  DeleteRelationalDatabaseSnapshot
newDeleteRelationalDatabaseSnapshot
  pRelationalDatabaseSnapshotName_ =
    DeleteRelationalDatabaseSnapshot'
      { relationalDatabaseSnapshotName =
          pRelationalDatabaseSnapshotName_
      }

-- | The name of the database snapshot that you are deleting.
deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabaseSnapshot Core.Text
deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName = Lens.lens (\DeleteRelationalDatabaseSnapshot' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@DeleteRelationalDatabaseSnapshot' {} a -> s {relationalDatabaseSnapshotName = a} :: DeleteRelationalDatabaseSnapshot)

instance
  Core.AWSRequest
    DeleteRelationalDatabaseSnapshot
  where
  type
    AWSResponse DeleteRelationalDatabaseSnapshot =
      DeleteRelationalDatabaseSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteRelationalDatabaseSnapshot

instance Core.NFData DeleteRelationalDatabaseSnapshot

instance
  Core.ToHeaders
    DeleteRelationalDatabaseSnapshot
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteRelationalDatabaseSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRelationalDatabaseSnapshot where
  toJSON DeleteRelationalDatabaseSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "relationalDatabaseSnapshotName"
                  Core..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Core.ToPath DeleteRelationalDatabaseSnapshot where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteRelationalDatabaseSnapshot
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRelationalDatabaseSnapshotResponse' smart constructor.
data DeleteRelationalDatabaseSnapshotResponse = DeleteRelationalDatabaseSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRelationalDatabaseSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteRelationalDatabaseSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteRelationalDatabaseSnapshotResponse_httpStatus' - The response's http status code.
newDeleteRelationalDatabaseSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRelationalDatabaseSnapshotResponse
newDeleteRelationalDatabaseSnapshotResponse
  pHttpStatus_ =
    DeleteRelationalDatabaseSnapshotResponse'
      { operations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteRelationalDatabaseSnapshotResponse_operations :: Lens.Lens' DeleteRelationalDatabaseSnapshotResponse (Core.Maybe [Operation])
deleteRelationalDatabaseSnapshotResponse_operations = Lens.lens (\DeleteRelationalDatabaseSnapshotResponse' {operations} -> operations) (\s@DeleteRelationalDatabaseSnapshotResponse' {} a -> s {operations = a} :: DeleteRelationalDatabaseSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteRelationalDatabaseSnapshotResponse_httpStatus :: Lens.Lens' DeleteRelationalDatabaseSnapshotResponse Core.Int
deleteRelationalDatabaseSnapshotResponse_httpStatus = Lens.lens (\DeleteRelationalDatabaseSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteRelationalDatabaseSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteRelationalDatabaseSnapshotResponse)

instance
  Core.NFData
    DeleteRelationalDatabaseSnapshotResponse
