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
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database in Amazon Lightsail.
--
-- The @delete relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteRelationalDatabase
  ( -- * Creating a Request
    DeleteRelationalDatabase (..),
    newDeleteRelationalDatabase,

    -- * Request Lenses
    deleteRelationalDatabase_finalRelationalDatabaseSnapshotName,
    deleteRelationalDatabase_skipFinalSnapshot,
    deleteRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    DeleteRelationalDatabaseResponse (..),
    newDeleteRelationalDatabaseResponse,

    -- * Response Lenses
    deleteRelationalDatabaseResponse_operations,
    deleteRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRelationalDatabase' smart constructor.
data DeleteRelationalDatabase = DeleteRelationalDatabase'
  { -- | The name of the database snapshot created if @skip final snapshot@ is
    -- @false@, which is the default value for that parameter.
    --
    -- Specifying this parameter and also specifying the @skip final snapshot@
    -- parameter to @true@ results in an error.
    --
    -- Constraints:
    --
    -- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    -- -   The first and last character must be a letter or number.
    finalRelationalDatabaseSnapshotName :: Core.Maybe Core.Text,
    -- | Determines whether a final database snapshot is created before your
    -- database is deleted. If @true@ is specified, no database snapshot is
    -- created. If @false@ is specified, a database snapshot is created before
    -- your database is deleted.
    --
    -- You must specify the @final relational database snapshot name@ parameter
    -- if the @skip final snapshot@ parameter is @false@.
    --
    -- Default: @false@
    skipFinalSnapshot :: Core.Maybe Core.Bool,
    -- | The name of the database that you are deleting.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalRelationalDatabaseSnapshotName', 'deleteRelationalDatabase_finalRelationalDatabaseSnapshotName' - The name of the database snapshot created if @skip final snapshot@ is
-- @false@, which is the default value for that parameter.
--
-- Specifying this parameter and also specifying the @skip final snapshot@
-- parameter to @true@ results in an error.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
--
-- 'skipFinalSnapshot', 'deleteRelationalDatabase_skipFinalSnapshot' - Determines whether a final database snapshot is created before your
-- database is deleted. If @true@ is specified, no database snapshot is
-- created. If @false@ is specified, a database snapshot is created before
-- your database is deleted.
--
-- You must specify the @final relational database snapshot name@ parameter
-- if the @skip final snapshot@ parameter is @false@.
--
-- Default: @false@
--
-- 'relationalDatabaseName', 'deleteRelationalDatabase_relationalDatabaseName' - The name of the database that you are deleting.
newDeleteRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  DeleteRelationalDatabase
newDeleteRelationalDatabase pRelationalDatabaseName_ =
  DeleteRelationalDatabase'
    { finalRelationalDatabaseSnapshotName =
        Core.Nothing,
      skipFinalSnapshot = Core.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The name of the database snapshot created if @skip final snapshot@ is
-- @false@, which is the default value for that parameter.
--
-- Specifying this parameter and also specifying the @skip final snapshot@
-- parameter to @true@ results in an error.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
deleteRelationalDatabase_finalRelationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabase (Core.Maybe Core.Text)
deleteRelationalDatabase_finalRelationalDatabaseSnapshotName = Lens.lens (\DeleteRelationalDatabase' {finalRelationalDatabaseSnapshotName} -> finalRelationalDatabaseSnapshotName) (\s@DeleteRelationalDatabase' {} a -> s {finalRelationalDatabaseSnapshotName = a} :: DeleteRelationalDatabase)

-- | Determines whether a final database snapshot is created before your
-- database is deleted. If @true@ is specified, no database snapshot is
-- created. If @false@ is specified, a database snapshot is created before
-- your database is deleted.
--
-- You must specify the @final relational database snapshot name@ parameter
-- if the @skip final snapshot@ parameter is @false@.
--
-- Default: @false@
deleteRelationalDatabase_skipFinalSnapshot :: Lens.Lens' DeleteRelationalDatabase (Core.Maybe Core.Bool)
deleteRelationalDatabase_skipFinalSnapshot = Lens.lens (\DeleteRelationalDatabase' {skipFinalSnapshot} -> skipFinalSnapshot) (\s@DeleteRelationalDatabase' {} a -> s {skipFinalSnapshot = a} :: DeleteRelationalDatabase)

-- | The name of the database that you are deleting.
deleteRelationalDatabase_relationalDatabaseName :: Lens.Lens' DeleteRelationalDatabase Core.Text
deleteRelationalDatabase_relationalDatabaseName = Lens.lens (\DeleteRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@DeleteRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: DeleteRelationalDatabase)

instance Core.AWSRequest DeleteRelationalDatabase where
  type
    AWSResponse DeleteRelationalDatabase =
      DeleteRelationalDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRelationalDatabase

instance Core.NFData DeleteRelationalDatabase

instance Core.ToHeaders DeleteRelationalDatabase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteRelationalDatabase" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRelationalDatabase where
  toJSON DeleteRelationalDatabase' {..} =
    Core.object
      ( Core.catMaybes
          [ ("finalRelationalDatabaseSnapshotName" Core..=)
              Core.<$> finalRelationalDatabaseSnapshotName,
            ("skipFinalSnapshot" Core..=)
              Core.<$> skipFinalSnapshot,
            Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath DeleteRelationalDatabase where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRelationalDatabase where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRelationalDatabaseResponse' smart constructor.
data DeleteRelationalDatabaseResponse = DeleteRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteRelationalDatabaseResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteRelationalDatabaseResponse_httpStatus' - The response's http status code.
newDeleteRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRelationalDatabaseResponse
newDeleteRelationalDatabaseResponse pHttpStatus_ =
  DeleteRelationalDatabaseResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteRelationalDatabaseResponse_operations :: Lens.Lens' DeleteRelationalDatabaseResponse (Core.Maybe [Operation])
deleteRelationalDatabaseResponse_operations = Lens.lens (\DeleteRelationalDatabaseResponse' {operations} -> operations) (\s@DeleteRelationalDatabaseResponse' {} a -> s {operations = a} :: DeleteRelationalDatabaseResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteRelationalDatabaseResponse_httpStatus :: Lens.Lens' DeleteRelationalDatabaseResponse Core.Int
deleteRelationalDatabaseResponse_httpStatus = Lens.lens (\DeleteRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@DeleteRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: DeleteRelationalDatabaseResponse)

instance Core.NFData DeleteRelationalDatabaseResponse
