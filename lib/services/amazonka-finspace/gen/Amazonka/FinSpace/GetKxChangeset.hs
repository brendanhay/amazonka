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
-- Module      : Amazonka.FinSpace.GetKxChangeset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a kdb changeset.
module Amazonka.FinSpace.GetKxChangeset
  ( -- * Creating a Request
    GetKxChangeset (..),
    newGetKxChangeset,

    -- * Request Lenses
    getKxChangeset_environmentId,
    getKxChangeset_databaseName,
    getKxChangeset_changesetId,

    -- * Destructuring the Response
    GetKxChangesetResponse (..),
    newGetKxChangesetResponse,

    -- * Response Lenses
    getKxChangesetResponse_activeFromTimestamp,
    getKxChangesetResponse_changeRequests,
    getKxChangesetResponse_changesetId,
    getKxChangesetResponse_createdTimestamp,
    getKxChangesetResponse_databaseName,
    getKxChangesetResponse_environmentId,
    getKxChangesetResponse_errorInfo,
    getKxChangesetResponse_lastModifiedTimestamp,
    getKxChangesetResponse_status,
    getKxChangesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKxChangeset' smart constructor.
data GetKxChangeset = GetKxChangeset'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Text,
    -- | A unique identifier of the changeset for which you want to retrieve
    -- data.
    changesetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxChangeset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'getKxChangeset_environmentId' - A unique identifier for the kdb environment.
--
-- 'databaseName', 'getKxChangeset_databaseName' - The name of the kdb database.
--
-- 'changesetId', 'getKxChangeset_changesetId' - A unique identifier of the changeset for which you want to retrieve
-- data.
newGetKxChangeset ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'changesetId'
  Prelude.Text ->
  GetKxChangeset
newGetKxChangeset
  pEnvironmentId_
  pDatabaseName_
  pChangesetId_ =
    GetKxChangeset'
      { environmentId = pEnvironmentId_,
        databaseName = pDatabaseName_,
        changesetId = pChangesetId_
      }

-- | A unique identifier for the kdb environment.
getKxChangeset_environmentId :: Lens.Lens' GetKxChangeset Prelude.Text
getKxChangeset_environmentId = Lens.lens (\GetKxChangeset' {environmentId} -> environmentId) (\s@GetKxChangeset' {} a -> s {environmentId = a} :: GetKxChangeset)

-- | The name of the kdb database.
getKxChangeset_databaseName :: Lens.Lens' GetKxChangeset Prelude.Text
getKxChangeset_databaseName = Lens.lens (\GetKxChangeset' {databaseName} -> databaseName) (\s@GetKxChangeset' {} a -> s {databaseName = a} :: GetKxChangeset)

-- | A unique identifier of the changeset for which you want to retrieve
-- data.
getKxChangeset_changesetId :: Lens.Lens' GetKxChangeset Prelude.Text
getKxChangeset_changesetId = Lens.lens (\GetKxChangeset' {changesetId} -> changesetId) (\s@GetKxChangeset' {} a -> s {changesetId = a} :: GetKxChangeset)

instance Core.AWSRequest GetKxChangeset where
  type
    AWSResponse GetKxChangeset =
      GetKxChangesetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKxChangesetResponse'
            Prelude.<$> (x Data..?> "activeFromTimestamp")
            Prelude.<*> (x Data..?> "changeRequests")
            Prelude.<*> (x Data..?> "changesetId")
            Prelude.<*> (x Data..?> "createdTimestamp")
            Prelude.<*> (x Data..?> "databaseName")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "errorInfo")
            Prelude.<*> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKxChangeset where
  hashWithSalt _salt GetKxChangeset' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` changesetId

instance Prelude.NFData GetKxChangeset where
  rnf GetKxChangeset' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf changesetId

instance Data.ToHeaders GetKxChangeset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKxChangeset where
  toPath GetKxChangeset' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases/",
        Data.toBS databaseName,
        "/changesets/",
        Data.toBS changesetId
      ]

instance Data.ToQuery GetKxChangeset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKxChangesetResponse' smart constructor.
data GetKxChangesetResponse = GetKxChangesetResponse'
  { -- | Beginning time from which the changeset is active. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    activeFromTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of change request objects that are run in order.
    changeRequests :: Prelude.Maybe (Prelude.NonEmpty ChangeRequest),
    -- | A unique identifier for the changeset.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the changeset was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the kdb database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Provides details in the event of a failed flow, including the error type
    -- and the related error message.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | The timestamp at which the changeset was updated in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Status of the changeset creation process.
    --
    -- -   Pending – Changeset creation is pending.
    --
    -- -   Processing – Changeset creation is running.
    --
    -- -   Failed – Changeset creation has failed.
    --
    -- -   Complete – Changeset creation has succeeded.
    status :: Prelude.Maybe ChangesetStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxChangesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeFromTimestamp', 'getKxChangesetResponse_activeFromTimestamp' - Beginning time from which the changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'changeRequests', 'getKxChangesetResponse_changeRequests' - A list of change request objects that are run in order.
--
-- 'changesetId', 'getKxChangesetResponse_changesetId' - A unique identifier for the changeset.
--
-- 'createdTimestamp', 'getKxChangesetResponse_createdTimestamp' - The timestamp at which the changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databaseName', 'getKxChangesetResponse_databaseName' - The name of the kdb database.
--
-- 'environmentId', 'getKxChangesetResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'errorInfo', 'getKxChangesetResponse_errorInfo' - Provides details in the event of a failed flow, including the error type
-- and the related error message.
--
-- 'lastModifiedTimestamp', 'getKxChangesetResponse_lastModifiedTimestamp' - The timestamp at which the changeset was updated in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'status', 'getKxChangesetResponse_status' - Status of the changeset creation process.
--
-- -   Pending – Changeset creation is pending.
--
-- -   Processing – Changeset creation is running.
--
-- -   Failed – Changeset creation has failed.
--
-- -   Complete – Changeset creation has succeeded.
--
-- 'httpStatus', 'getKxChangesetResponse_httpStatus' - The response's http status code.
newGetKxChangesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKxChangesetResponse
newGetKxChangesetResponse pHttpStatus_ =
  GetKxChangesetResponse'
    { activeFromTimestamp =
        Prelude.Nothing,
      changeRequests = Prelude.Nothing,
      changesetId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Beginning time from which the changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxChangesetResponse_activeFromTimestamp :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe Prelude.UTCTime)
getKxChangesetResponse_activeFromTimestamp = Lens.lens (\GetKxChangesetResponse' {activeFromTimestamp} -> activeFromTimestamp) (\s@GetKxChangesetResponse' {} a -> s {activeFromTimestamp = a} :: GetKxChangesetResponse) Prelude.. Lens.mapping Data._Time

-- | A list of change request objects that are run in order.
getKxChangesetResponse_changeRequests :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe (Prelude.NonEmpty ChangeRequest))
getKxChangesetResponse_changeRequests = Lens.lens (\GetKxChangesetResponse' {changeRequests} -> changeRequests) (\s@GetKxChangesetResponse' {} a -> s {changeRequests = a} :: GetKxChangesetResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the changeset.
getKxChangesetResponse_changesetId :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe Prelude.Text)
getKxChangesetResponse_changesetId = Lens.lens (\GetKxChangesetResponse' {changesetId} -> changesetId) (\s@GetKxChangesetResponse' {} a -> s {changesetId = a} :: GetKxChangesetResponse)

-- | The timestamp at which the changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxChangesetResponse_createdTimestamp :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe Prelude.UTCTime)
getKxChangesetResponse_createdTimestamp = Lens.lens (\GetKxChangesetResponse' {createdTimestamp} -> createdTimestamp) (\s@GetKxChangesetResponse' {} a -> s {createdTimestamp = a} :: GetKxChangesetResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the kdb database.
getKxChangesetResponse_databaseName :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe Prelude.Text)
getKxChangesetResponse_databaseName = Lens.lens (\GetKxChangesetResponse' {databaseName} -> databaseName) (\s@GetKxChangesetResponse' {} a -> s {databaseName = a} :: GetKxChangesetResponse)

-- | A unique identifier for the kdb environment.
getKxChangesetResponse_environmentId :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe Prelude.Text)
getKxChangesetResponse_environmentId = Lens.lens (\GetKxChangesetResponse' {environmentId} -> environmentId) (\s@GetKxChangesetResponse' {} a -> s {environmentId = a} :: GetKxChangesetResponse)

-- | Provides details in the event of a failed flow, including the error type
-- and the related error message.
getKxChangesetResponse_errorInfo :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe ErrorInfo)
getKxChangesetResponse_errorInfo = Lens.lens (\GetKxChangesetResponse' {errorInfo} -> errorInfo) (\s@GetKxChangesetResponse' {} a -> s {errorInfo = a} :: GetKxChangesetResponse)

-- | The timestamp at which the changeset was updated in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxChangesetResponse_lastModifiedTimestamp :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe Prelude.UTCTime)
getKxChangesetResponse_lastModifiedTimestamp = Lens.lens (\GetKxChangesetResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@GetKxChangesetResponse' {} a -> s {lastModifiedTimestamp = a} :: GetKxChangesetResponse) Prelude.. Lens.mapping Data._Time

-- | Status of the changeset creation process.
--
-- -   Pending – Changeset creation is pending.
--
-- -   Processing – Changeset creation is running.
--
-- -   Failed – Changeset creation has failed.
--
-- -   Complete – Changeset creation has succeeded.
getKxChangesetResponse_status :: Lens.Lens' GetKxChangesetResponse (Prelude.Maybe ChangesetStatus)
getKxChangesetResponse_status = Lens.lens (\GetKxChangesetResponse' {status} -> status) (\s@GetKxChangesetResponse' {} a -> s {status = a} :: GetKxChangesetResponse)

-- | The response's http status code.
getKxChangesetResponse_httpStatus :: Lens.Lens' GetKxChangesetResponse Prelude.Int
getKxChangesetResponse_httpStatus = Lens.lens (\GetKxChangesetResponse' {httpStatus} -> httpStatus) (\s@GetKxChangesetResponse' {} a -> s {httpStatus = a} :: GetKxChangesetResponse)

instance Prelude.NFData GetKxChangesetResponse where
  rnf GetKxChangesetResponse' {..} =
    Prelude.rnf activeFromTimestamp
      `Prelude.seq` Prelude.rnf changeRequests
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
