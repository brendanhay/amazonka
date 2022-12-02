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
-- Module      : Amazonka.TimeStreamWrite.UpdateDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the KMS key for an existing database. While updating the
-- database, you must specify the database name and the identifier of the
-- new KMS key to be used (@KmsKeyId@). If there are any concurrent
-- @UpdateDatabase@ requests, first writer wins.
--
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.update-db.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.UpdateDatabase
  ( -- * Creating a Request
    UpdateDatabase (..),
    newUpdateDatabase,

    -- * Request Lenses
    updateDatabase_databaseName,
    updateDatabase_kmsKeyId,

    -- * Destructuring the Response
    UpdateDatabaseResponse (..),
    newUpdateDatabaseResponse,

    -- * Response Lenses
    updateDatabaseResponse_database,
    updateDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newUpdateDatabase' smart constructor.
data UpdateDatabase = UpdateDatabase'
  { -- | The name of the database.
    databaseName :: Prelude.Text,
    -- | The identifier of the new KMS key (@KmsKeyId@) to be used to encrypt the
    -- data stored in the database. If the @KmsKeyId@ currently registered with
    -- the database is the same as the @KmsKeyId@ in the request, there will
    -- not be any update.
    --
    -- You can specify the @KmsKeyId@ using any of the following:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-1:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias ARN: @arn:aws:kms:us-east-1:111122223333:alias\/ExampleAlias@
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'updateDatabase_databaseName' - The name of the database.
--
-- 'kmsKeyId', 'updateDatabase_kmsKeyId' - The identifier of the new KMS key (@KmsKeyId@) to be used to encrypt the
-- data stored in the database. If the @KmsKeyId@ currently registered with
-- the database is the same as the @KmsKeyId@ in the request, there will
-- not be any update.
--
-- You can specify the @KmsKeyId@ using any of the following:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-1:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-1:111122223333:alias\/ExampleAlias@
newUpdateDatabase ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  UpdateDatabase
newUpdateDatabase pDatabaseName_ pKmsKeyId_ =
  UpdateDatabase'
    { databaseName = pDatabaseName_,
      kmsKeyId = pKmsKeyId_
    }

-- | The name of the database.
updateDatabase_databaseName :: Lens.Lens' UpdateDatabase Prelude.Text
updateDatabase_databaseName = Lens.lens (\UpdateDatabase' {databaseName} -> databaseName) (\s@UpdateDatabase' {} a -> s {databaseName = a} :: UpdateDatabase)

-- | The identifier of the new KMS key (@KmsKeyId@) to be used to encrypt the
-- data stored in the database. If the @KmsKeyId@ currently registered with
-- the database is the same as the @KmsKeyId@ in the request, there will
-- not be any update.
--
-- You can specify the @KmsKeyId@ using any of the following:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-1:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-1:111122223333:alias\/ExampleAlias@
updateDatabase_kmsKeyId :: Lens.Lens' UpdateDatabase Prelude.Text
updateDatabase_kmsKeyId = Lens.lens (\UpdateDatabase' {kmsKeyId} -> kmsKeyId) (\s@UpdateDatabase' {} a -> s {kmsKeyId = a} :: UpdateDatabase)

instance Core.AWSRequest UpdateDatabase where
  type
    AWSResponse UpdateDatabase =
      UpdateDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDatabaseResponse'
            Prelude.<$> (x Data..?> "Database")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDatabase where
  hashWithSalt _salt UpdateDatabase' {..} =
    _salt `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData UpdateDatabase where
  rnf UpdateDatabase' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToHeaders UpdateDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.UpdateDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDatabase where
  toJSON UpdateDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("KmsKeyId" Data..= kmsKeyId)
          ]
      )

instance Data.ToPath UpdateDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatabaseResponse' smart constructor.
data UpdateDatabaseResponse = UpdateDatabaseResponse'
  { database :: Prelude.Maybe Database,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'updateDatabaseResponse_database' - Undocumented member.
--
-- 'httpStatus', 'updateDatabaseResponse_httpStatus' - The response's http status code.
newUpdateDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatabaseResponse
newUpdateDatabaseResponse pHttpStatus_ =
  UpdateDatabaseResponse'
    { database = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateDatabaseResponse_database :: Lens.Lens' UpdateDatabaseResponse (Prelude.Maybe Database)
updateDatabaseResponse_database = Lens.lens (\UpdateDatabaseResponse' {database} -> database) (\s@UpdateDatabaseResponse' {} a -> s {database = a} :: UpdateDatabaseResponse)

-- | The response's http status code.
updateDatabaseResponse_httpStatus :: Lens.Lens' UpdateDatabaseResponse Prelude.Int
updateDatabaseResponse_httpStatus = Lens.lens (\UpdateDatabaseResponse' {httpStatus} -> httpStatus) (\s@UpdateDatabaseResponse' {} a -> s {httpStatus = a} :: UpdateDatabaseResponse)

instance Prelude.NFData UpdateDatabaseResponse where
  rnf UpdateDatabaseResponse' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf httpStatus
