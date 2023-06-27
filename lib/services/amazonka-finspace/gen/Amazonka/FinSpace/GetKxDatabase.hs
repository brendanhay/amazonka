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
-- Module      : Amazonka.FinSpace.GetKxDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns database information for the specified environment ID.
module Amazonka.FinSpace.GetKxDatabase
  ( -- * Creating a Request
    GetKxDatabase (..),
    newGetKxDatabase,

    -- * Request Lenses
    getKxDatabase_environmentId,
    getKxDatabase_databaseName,

    -- * Destructuring the Response
    GetKxDatabaseResponse (..),
    newGetKxDatabaseResponse,

    -- * Response Lenses
    getKxDatabaseResponse_createdTimestamp,
    getKxDatabaseResponse_databaseArn,
    getKxDatabaseResponse_databaseName,
    getKxDatabaseResponse_description,
    getKxDatabaseResponse_environmentId,
    getKxDatabaseResponse_lastCompletedChangesetId,
    getKxDatabaseResponse_lastModifiedTimestamp,
    getKxDatabaseResponse_numBytes,
    getKxDatabaseResponse_numChangesets,
    getKxDatabaseResponse_numFiles,
    getKxDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKxDatabase' smart constructor.
data GetKxDatabase = GetKxDatabase'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'getKxDatabase_environmentId' - A unique identifier for the kdb environment.
--
-- 'databaseName', 'getKxDatabase_databaseName' - The name of the kdb database.
newGetKxDatabase ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  GetKxDatabase
newGetKxDatabase pEnvironmentId_ pDatabaseName_ =
  GetKxDatabase'
    { environmentId = pEnvironmentId_,
      databaseName = pDatabaseName_
    }

-- | A unique identifier for the kdb environment.
getKxDatabase_environmentId :: Lens.Lens' GetKxDatabase Prelude.Text
getKxDatabase_environmentId = Lens.lens (\GetKxDatabase' {environmentId} -> environmentId) (\s@GetKxDatabase' {} a -> s {environmentId = a} :: GetKxDatabase)

-- | The name of the kdb database.
getKxDatabase_databaseName :: Lens.Lens' GetKxDatabase Prelude.Text
getKxDatabase_databaseName = Lens.lens (\GetKxDatabase' {databaseName} -> databaseName) (\s@GetKxDatabase' {} a -> s {databaseName = a} :: GetKxDatabase)

instance Core.AWSRequest GetKxDatabase where
  type
    AWSResponse GetKxDatabase =
      GetKxDatabaseResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKxDatabaseResponse'
            Prelude.<$> (x Data..?> "createdTimestamp")
            Prelude.<*> (x Data..?> "databaseArn")
            Prelude.<*> (x Data..?> "databaseName")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "lastCompletedChangesetId")
            Prelude.<*> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (x Data..?> "numBytes")
            Prelude.<*> (x Data..?> "numChangesets")
            Prelude.<*> (x Data..?> "numFiles")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKxDatabase where
  hashWithSalt _salt GetKxDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData GetKxDatabase where
  rnf GetKxDatabase' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToHeaders GetKxDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKxDatabase where
  toPath GetKxDatabase' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases/",
        Data.toBS databaseName
      ]

instance Data.ToQuery GetKxDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKxDatabaseResponse' smart constructor.
data GetKxDatabaseResponse = GetKxDatabaseResponse'
  { -- | The timestamp at which the database is created in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN identifier of the database.
    databaseArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb database for which the information is retrieved.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the changeset.
    lastCompletedChangesetId :: Prelude.Maybe Prelude.Text,
    -- | The last time that the database was modified. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The total number of bytes in the database.
    numBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total number of changesets in the database.
    numChangesets :: Prelude.Maybe Prelude.Int,
    -- | The total number of files in the database.
    numFiles :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'getKxDatabaseResponse_createdTimestamp' - The timestamp at which the database is created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databaseArn', 'getKxDatabaseResponse_databaseArn' - The ARN identifier of the database.
--
-- 'databaseName', 'getKxDatabaseResponse_databaseName' - The name of the kdb database for which the information is retrieved.
--
-- 'description', 'getKxDatabaseResponse_description' - A description of the database.
--
-- 'environmentId', 'getKxDatabaseResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'lastCompletedChangesetId', 'getKxDatabaseResponse_lastCompletedChangesetId' - A unique identifier for the changeset.
--
-- 'lastModifiedTimestamp', 'getKxDatabaseResponse_lastModifiedTimestamp' - The last time that the database was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'numBytes', 'getKxDatabaseResponse_numBytes' - The total number of bytes in the database.
--
-- 'numChangesets', 'getKxDatabaseResponse_numChangesets' - The total number of changesets in the database.
--
-- 'numFiles', 'getKxDatabaseResponse_numFiles' - The total number of files in the database.
--
-- 'httpStatus', 'getKxDatabaseResponse_httpStatus' - The response's http status code.
newGetKxDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKxDatabaseResponse
newGetKxDatabaseResponse pHttpStatus_ =
  GetKxDatabaseResponse'
    { createdTimestamp =
        Prelude.Nothing,
      databaseArn = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lastCompletedChangesetId = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      numBytes = Prelude.Nothing,
      numChangesets = Prelude.Nothing,
      numFiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp at which the database is created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxDatabaseResponse_createdTimestamp :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.UTCTime)
getKxDatabaseResponse_createdTimestamp = Lens.lens (\GetKxDatabaseResponse' {createdTimestamp} -> createdTimestamp) (\s@GetKxDatabaseResponse' {} a -> s {createdTimestamp = a} :: GetKxDatabaseResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN identifier of the database.
getKxDatabaseResponse_databaseArn :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Text)
getKxDatabaseResponse_databaseArn = Lens.lens (\GetKxDatabaseResponse' {databaseArn} -> databaseArn) (\s@GetKxDatabaseResponse' {} a -> s {databaseArn = a} :: GetKxDatabaseResponse)

-- | The name of the kdb database for which the information is retrieved.
getKxDatabaseResponse_databaseName :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Text)
getKxDatabaseResponse_databaseName = Lens.lens (\GetKxDatabaseResponse' {databaseName} -> databaseName) (\s@GetKxDatabaseResponse' {} a -> s {databaseName = a} :: GetKxDatabaseResponse)

-- | A description of the database.
getKxDatabaseResponse_description :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Text)
getKxDatabaseResponse_description = Lens.lens (\GetKxDatabaseResponse' {description} -> description) (\s@GetKxDatabaseResponse' {} a -> s {description = a} :: GetKxDatabaseResponse)

-- | A unique identifier for the kdb environment.
getKxDatabaseResponse_environmentId :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Text)
getKxDatabaseResponse_environmentId = Lens.lens (\GetKxDatabaseResponse' {environmentId} -> environmentId) (\s@GetKxDatabaseResponse' {} a -> s {environmentId = a} :: GetKxDatabaseResponse)

-- | A unique identifier for the changeset.
getKxDatabaseResponse_lastCompletedChangesetId :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Text)
getKxDatabaseResponse_lastCompletedChangesetId = Lens.lens (\GetKxDatabaseResponse' {lastCompletedChangesetId} -> lastCompletedChangesetId) (\s@GetKxDatabaseResponse' {} a -> s {lastCompletedChangesetId = a} :: GetKxDatabaseResponse)

-- | The last time that the database was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxDatabaseResponse_lastModifiedTimestamp :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.UTCTime)
getKxDatabaseResponse_lastModifiedTimestamp = Lens.lens (\GetKxDatabaseResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@GetKxDatabaseResponse' {} a -> s {lastModifiedTimestamp = a} :: GetKxDatabaseResponse) Prelude.. Lens.mapping Data._Time

-- | The total number of bytes in the database.
getKxDatabaseResponse_numBytes :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Integer)
getKxDatabaseResponse_numBytes = Lens.lens (\GetKxDatabaseResponse' {numBytes} -> numBytes) (\s@GetKxDatabaseResponse' {} a -> s {numBytes = a} :: GetKxDatabaseResponse)

-- | The total number of changesets in the database.
getKxDatabaseResponse_numChangesets :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Int)
getKxDatabaseResponse_numChangesets = Lens.lens (\GetKxDatabaseResponse' {numChangesets} -> numChangesets) (\s@GetKxDatabaseResponse' {} a -> s {numChangesets = a} :: GetKxDatabaseResponse)

-- | The total number of files in the database.
getKxDatabaseResponse_numFiles :: Lens.Lens' GetKxDatabaseResponse (Prelude.Maybe Prelude.Int)
getKxDatabaseResponse_numFiles = Lens.lens (\GetKxDatabaseResponse' {numFiles} -> numFiles) (\s@GetKxDatabaseResponse' {} a -> s {numFiles = a} :: GetKxDatabaseResponse)

-- | The response's http status code.
getKxDatabaseResponse_httpStatus :: Lens.Lens' GetKxDatabaseResponse Prelude.Int
getKxDatabaseResponse_httpStatus = Lens.lens (\GetKxDatabaseResponse' {httpStatus} -> httpStatus) (\s@GetKxDatabaseResponse' {} a -> s {httpStatus = a} :: GetKxDatabaseResponse)

instance Prelude.NFData GetKxDatabaseResponse where
  rnf GetKxDatabaseResponse' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf databaseArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lastCompletedChangesetId
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf numBytes
      `Prelude.seq` Prelude.rnf numChangesets
      `Prelude.seq` Prelude.rnf numFiles
      `Prelude.seq` Prelude.rnf httpStatus
