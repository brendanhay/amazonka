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
-- Module      : Amazonka.Lightsail.DeleteRelationalDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database in Amazon Lightsail.
--
-- The @delete relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DeleteRelationalDatabase
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    finalRelationalDatabaseSnapshotName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether a final database snapshot is created before your
    -- database is deleted. If @true@ is specified, no database snapshot is
    -- created. If @false@ is specified, a database snapshot is created before
    -- your database is deleted.
    --
    -- You must specify the @final relational database snapshot name@ parameter
    -- if the @skip final snapshot@ parameter is @false@.
    --
    -- Default: @false@
    skipFinalSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database that you are deleting.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteRelationalDatabase
newDeleteRelationalDatabase pRelationalDatabaseName_ =
  DeleteRelationalDatabase'
    { finalRelationalDatabaseSnapshotName =
        Prelude.Nothing,
      skipFinalSnapshot = Prelude.Nothing,
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
deleteRelationalDatabase_finalRelationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabase (Prelude.Maybe Prelude.Text)
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
deleteRelationalDatabase_skipFinalSnapshot :: Lens.Lens' DeleteRelationalDatabase (Prelude.Maybe Prelude.Bool)
deleteRelationalDatabase_skipFinalSnapshot = Lens.lens (\DeleteRelationalDatabase' {skipFinalSnapshot} -> skipFinalSnapshot) (\s@DeleteRelationalDatabase' {} a -> s {skipFinalSnapshot = a} :: DeleteRelationalDatabase)

-- | The name of the database that you are deleting.
deleteRelationalDatabase_relationalDatabaseName :: Lens.Lens' DeleteRelationalDatabase Prelude.Text
deleteRelationalDatabase_relationalDatabaseName = Lens.lens (\DeleteRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@DeleteRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: DeleteRelationalDatabase)

instance Core.AWSRequest DeleteRelationalDatabase where
  type
    AWSResponse DeleteRelationalDatabase =
      DeleteRelationalDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRelationalDatabase where
  hashWithSalt _salt DeleteRelationalDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` finalRelationalDatabaseSnapshotName
      `Prelude.hashWithSalt` skipFinalSnapshot
      `Prelude.hashWithSalt` relationalDatabaseName

instance Prelude.NFData DeleteRelationalDatabase where
  rnf DeleteRelationalDatabase' {..} =
    Prelude.rnf finalRelationalDatabaseSnapshotName
      `Prelude.seq` Prelude.rnf skipFinalSnapshot
      `Prelude.seq` Prelude.rnf relationalDatabaseName

instance Data.ToHeaders DeleteRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteRelationalDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRelationalDatabase where
  toJSON DeleteRelationalDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("finalRelationalDatabaseSnapshotName" Data..=)
              Prelude.<$> finalRelationalDatabaseSnapshotName,
            ("skipFinalSnapshot" Data..=)
              Prelude.<$> skipFinalSnapshot,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance Data.ToPath DeleteRelationalDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRelationalDatabaseResponse' smart constructor.
data DeleteRelationalDatabaseResponse = DeleteRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteRelationalDatabaseResponse
newDeleteRelationalDatabaseResponse pHttpStatus_ =
  DeleteRelationalDatabaseResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteRelationalDatabaseResponse_operations :: Lens.Lens' DeleteRelationalDatabaseResponse (Prelude.Maybe [Operation])
deleteRelationalDatabaseResponse_operations = Lens.lens (\DeleteRelationalDatabaseResponse' {operations} -> operations) (\s@DeleteRelationalDatabaseResponse' {} a -> s {operations = a} :: DeleteRelationalDatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteRelationalDatabaseResponse_httpStatus :: Lens.Lens' DeleteRelationalDatabaseResponse Prelude.Int
deleteRelationalDatabaseResponse_httpStatus = Lens.lens (\DeleteRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@DeleteRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: DeleteRelationalDatabaseResponse)

instance
  Prelude.NFData
    DeleteRelationalDatabaseResponse
  where
  rnf DeleteRelationalDatabaseResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
