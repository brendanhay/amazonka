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
-- Module      : Amazonka.TimeStreamWrite.CreateDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Timestream database. If the KMS key is not specified, the
-- database will be encrypted with a Timestream managed KMS key located in
-- your account. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- For details, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.create-db.html code sample>.
module Amazonka.TimeStreamWrite.CreateDatabase
  ( -- * Creating a Request
    CreateDatabase (..),
    newCreateDatabase,

    -- * Request Lenses
    createDatabase_kmsKeyId,
    createDatabase_tags,
    createDatabase_databaseName,

    -- * Destructuring the Response
    CreateDatabaseResponse (..),
    newCreateDatabaseResponse,

    -- * Response Lenses
    createDatabaseResponse_database,
    createDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newCreateDatabase' smart constructor.
data CreateDatabase = CreateDatabase'
  { -- | The KMS key for the database. If the KMS key is not specified, the
    -- database will be encrypted with a Timestream managed KMS key located in
    -- your account. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs to label the table.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the Timestream database.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'createDatabase_kmsKeyId' - The KMS key for the database. If the KMS key is not specified, the
-- database will be encrypted with a Timestream managed KMS key located in
-- your account. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
--
-- 'tags', 'createDatabase_tags' - A list of key-value pairs to label the table.
--
-- 'databaseName', 'createDatabase_databaseName' - The name of the Timestream database.
newCreateDatabase ::
  -- | 'databaseName'
  Prelude.Text ->
  CreateDatabase
newCreateDatabase pDatabaseName_ =
  CreateDatabase'
    { kmsKeyId = Prelude.Nothing,
      tags = Prelude.Nothing,
      databaseName = pDatabaseName_
    }

-- | The KMS key for the database. If the KMS key is not specified, the
-- database will be encrypted with a Timestream managed KMS key located in
-- your account. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
createDatabase_kmsKeyId :: Lens.Lens' CreateDatabase (Prelude.Maybe Prelude.Text)
createDatabase_kmsKeyId = Lens.lens (\CreateDatabase' {kmsKeyId} -> kmsKeyId) (\s@CreateDatabase' {} a -> s {kmsKeyId = a} :: CreateDatabase)

-- | A list of key-value pairs to label the table.
createDatabase_tags :: Lens.Lens' CreateDatabase (Prelude.Maybe [Tag])
createDatabase_tags = Lens.lens (\CreateDatabase' {tags} -> tags) (\s@CreateDatabase' {} a -> s {tags = a} :: CreateDatabase) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Timestream database.
createDatabase_databaseName :: Lens.Lens' CreateDatabase Prelude.Text
createDatabase_databaseName = Lens.lens (\CreateDatabase' {databaseName} -> databaseName) (\s@CreateDatabase' {} a -> s {databaseName = a} :: CreateDatabase)

instance Core.AWSRequest CreateDatabase where
  type
    AWSResponse CreateDatabase =
      CreateDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatabaseResponse'
            Prelude.<$> (x Data..?> "Database")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatabase where
  hashWithSalt _salt CreateDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData CreateDatabase where
  rnf CreateDatabase' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToHeaders CreateDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.CreateDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatabase where
  toJSON CreateDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )

instance Data.ToPath CreateDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatabaseResponse' smart constructor.
data CreateDatabaseResponse = CreateDatabaseResponse'
  { -- | The newly created Timestream database.
    database :: Prelude.Maybe Database,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'createDatabaseResponse_database' - The newly created Timestream database.
--
-- 'httpStatus', 'createDatabaseResponse_httpStatus' - The response's http status code.
newCreateDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatabaseResponse
newCreateDatabaseResponse pHttpStatus_ =
  CreateDatabaseResponse'
    { database = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created Timestream database.
createDatabaseResponse_database :: Lens.Lens' CreateDatabaseResponse (Prelude.Maybe Database)
createDatabaseResponse_database = Lens.lens (\CreateDatabaseResponse' {database} -> database) (\s@CreateDatabaseResponse' {} a -> s {database = a} :: CreateDatabaseResponse)

-- | The response's http status code.
createDatabaseResponse_httpStatus :: Lens.Lens' CreateDatabaseResponse Prelude.Int
createDatabaseResponse_httpStatus = Lens.lens (\CreateDatabaseResponse' {httpStatus} -> httpStatus) (\s@CreateDatabaseResponse' {} a -> s {httpStatus = a} :: CreateDatabaseResponse)

instance Prelude.NFData CreateDatabaseResponse where
  rnf CreateDatabaseResponse' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf httpStatus
