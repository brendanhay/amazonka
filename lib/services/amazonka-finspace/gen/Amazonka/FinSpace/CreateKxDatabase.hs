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
-- Module      : Amazonka.FinSpace.CreateKxDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new kdb database in the environment.
module Amazonka.FinSpace.CreateKxDatabase
  ( -- * Creating a Request
    CreateKxDatabase (..),
    newCreateKxDatabase,

    -- * Request Lenses
    createKxDatabase_description,
    createKxDatabase_tags,
    createKxDatabase_environmentId,
    createKxDatabase_databaseName,
    createKxDatabase_clientToken,

    -- * Destructuring the Response
    CreateKxDatabaseResponse (..),
    newCreateKxDatabaseResponse,

    -- * Response Lenses
    createKxDatabaseResponse_createdTimestamp,
    createKxDatabaseResponse_databaseArn,
    createKxDatabaseResponse_databaseName,
    createKxDatabaseResponse_description,
    createKxDatabaseResponse_environmentId,
    createKxDatabaseResponse_lastModifiedTimestamp,
    createKxDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKxDatabase' smart constructor.
data CreateKxDatabase = CreateKxDatabase'
  { -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs to label the kdb database. You can add up to
    -- 50 tags to your kdb database
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Text,
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createKxDatabase_description' - A description of the database.
--
-- 'tags', 'createKxDatabase_tags' - A list of key-value pairs to label the kdb database. You can add up to
-- 50 tags to your kdb database
--
-- 'environmentId', 'createKxDatabase_environmentId' - A unique identifier for the kdb environment.
--
-- 'databaseName', 'createKxDatabase_databaseName' - The name of the kdb database.
--
-- 'clientToken', 'createKxDatabase_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
newCreateKxDatabase ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateKxDatabase
newCreateKxDatabase
  pEnvironmentId_
  pDatabaseName_
  pClientToken_ =
    CreateKxDatabase'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        environmentId = pEnvironmentId_,
        databaseName = pDatabaseName_,
        clientToken = pClientToken_
      }

-- | A description of the database.
createKxDatabase_description :: Lens.Lens' CreateKxDatabase (Prelude.Maybe Prelude.Text)
createKxDatabase_description = Lens.lens (\CreateKxDatabase' {description} -> description) (\s@CreateKxDatabase' {} a -> s {description = a} :: CreateKxDatabase)

-- | A list of key-value pairs to label the kdb database. You can add up to
-- 50 tags to your kdb database
createKxDatabase_tags :: Lens.Lens' CreateKxDatabase (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createKxDatabase_tags = Lens.lens (\CreateKxDatabase' {tags} -> tags) (\s@CreateKxDatabase' {} a -> s {tags = a} :: CreateKxDatabase) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the kdb environment.
createKxDatabase_environmentId :: Lens.Lens' CreateKxDatabase Prelude.Text
createKxDatabase_environmentId = Lens.lens (\CreateKxDatabase' {environmentId} -> environmentId) (\s@CreateKxDatabase' {} a -> s {environmentId = a} :: CreateKxDatabase)

-- | The name of the kdb database.
createKxDatabase_databaseName :: Lens.Lens' CreateKxDatabase Prelude.Text
createKxDatabase_databaseName = Lens.lens (\CreateKxDatabase' {databaseName} -> databaseName) (\s@CreateKxDatabase' {} a -> s {databaseName = a} :: CreateKxDatabase)

-- | A token that ensures idempotency. This token expires in 10 minutes.
createKxDatabase_clientToken :: Lens.Lens' CreateKxDatabase Prelude.Text
createKxDatabase_clientToken = Lens.lens (\CreateKxDatabase' {clientToken} -> clientToken) (\s@CreateKxDatabase' {} a -> s {clientToken = a} :: CreateKxDatabase)

instance Core.AWSRequest CreateKxDatabase where
  type
    AWSResponse CreateKxDatabase =
      CreateKxDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKxDatabaseResponse'
            Prelude.<$> (x Data..?> "createdTimestamp")
            Prelude.<*> (x Data..?> "databaseArn")
            Prelude.<*> (x Data..?> "databaseName")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKxDatabase where
  hashWithSalt _salt CreateKxDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateKxDatabase where
  rnf CreateKxDatabase' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateKxDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKxDatabase where
  toJSON CreateKxDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("databaseName" Data..= databaseName),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateKxDatabase where
  toPath CreateKxDatabase' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases"
      ]

instance Data.ToQuery CreateKxDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKxDatabaseResponse' smart constructor.
data CreateKxDatabaseResponse = CreateKxDatabaseResponse'
  { -- | The timestamp at which the database is created in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN identifier of the database.
    databaseArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The last time that the database was updated in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'createKxDatabaseResponse_createdTimestamp' - The timestamp at which the database is created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databaseArn', 'createKxDatabaseResponse_databaseArn' - The ARN identifier of the database.
--
-- 'databaseName', 'createKxDatabaseResponse_databaseName' - The name of the kdb database.
--
-- 'description', 'createKxDatabaseResponse_description' - A description of the database.
--
-- 'environmentId', 'createKxDatabaseResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'lastModifiedTimestamp', 'createKxDatabaseResponse_lastModifiedTimestamp' - The last time that the database was updated in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'httpStatus', 'createKxDatabaseResponse_httpStatus' - The response's http status code.
newCreateKxDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKxDatabaseResponse
newCreateKxDatabaseResponse pHttpStatus_ =
  CreateKxDatabaseResponse'
    { createdTimestamp =
        Prelude.Nothing,
      databaseArn = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp at which the database is created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
createKxDatabaseResponse_createdTimestamp :: Lens.Lens' CreateKxDatabaseResponse (Prelude.Maybe Prelude.UTCTime)
createKxDatabaseResponse_createdTimestamp = Lens.lens (\CreateKxDatabaseResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateKxDatabaseResponse' {} a -> s {createdTimestamp = a} :: CreateKxDatabaseResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN identifier of the database.
createKxDatabaseResponse_databaseArn :: Lens.Lens' CreateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
createKxDatabaseResponse_databaseArn = Lens.lens (\CreateKxDatabaseResponse' {databaseArn} -> databaseArn) (\s@CreateKxDatabaseResponse' {} a -> s {databaseArn = a} :: CreateKxDatabaseResponse)

-- | The name of the kdb database.
createKxDatabaseResponse_databaseName :: Lens.Lens' CreateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
createKxDatabaseResponse_databaseName = Lens.lens (\CreateKxDatabaseResponse' {databaseName} -> databaseName) (\s@CreateKxDatabaseResponse' {} a -> s {databaseName = a} :: CreateKxDatabaseResponse)

-- | A description of the database.
createKxDatabaseResponse_description :: Lens.Lens' CreateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
createKxDatabaseResponse_description = Lens.lens (\CreateKxDatabaseResponse' {description} -> description) (\s@CreateKxDatabaseResponse' {} a -> s {description = a} :: CreateKxDatabaseResponse)

-- | A unique identifier for the kdb environment.
createKxDatabaseResponse_environmentId :: Lens.Lens' CreateKxDatabaseResponse (Prelude.Maybe Prelude.Text)
createKxDatabaseResponse_environmentId = Lens.lens (\CreateKxDatabaseResponse' {environmentId} -> environmentId) (\s@CreateKxDatabaseResponse' {} a -> s {environmentId = a} :: CreateKxDatabaseResponse)

-- | The last time that the database was updated in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
createKxDatabaseResponse_lastModifiedTimestamp :: Lens.Lens' CreateKxDatabaseResponse (Prelude.Maybe Prelude.UTCTime)
createKxDatabaseResponse_lastModifiedTimestamp = Lens.lens (\CreateKxDatabaseResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@CreateKxDatabaseResponse' {} a -> s {lastModifiedTimestamp = a} :: CreateKxDatabaseResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createKxDatabaseResponse_httpStatus :: Lens.Lens' CreateKxDatabaseResponse Prelude.Int
createKxDatabaseResponse_httpStatus = Lens.lens (\CreateKxDatabaseResponse' {httpStatus} -> httpStatus) (\s@CreateKxDatabaseResponse' {} a -> s {httpStatus = a} :: CreateKxDatabaseResponse)

instance Prelude.NFData CreateKxDatabaseResponse where
  rnf CreateKxDatabaseResponse' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf databaseArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
