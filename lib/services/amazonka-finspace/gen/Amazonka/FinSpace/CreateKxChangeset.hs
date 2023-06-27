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
-- Module      : Amazonka.FinSpace.CreateKxChangeset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a changeset for a kdb database. A changeset allows you to add
-- and delete existing files by using an ordered list of change requests.
module Amazonka.FinSpace.CreateKxChangeset
  ( -- * Creating a Request
    CreateKxChangeset (..),
    newCreateKxChangeset,

    -- * Request Lenses
    createKxChangeset_environmentId,
    createKxChangeset_databaseName,
    createKxChangeset_changeRequests,
    createKxChangeset_clientToken,

    -- * Destructuring the Response
    CreateKxChangesetResponse (..),
    newCreateKxChangesetResponse,

    -- * Response Lenses
    createKxChangesetResponse_changeRequests,
    createKxChangesetResponse_changesetId,
    createKxChangesetResponse_createdTimestamp,
    createKxChangesetResponse_databaseName,
    createKxChangesetResponse_environmentId,
    createKxChangesetResponse_errorInfo,
    createKxChangesetResponse_lastModifiedTimestamp,
    createKxChangesetResponse_status,
    createKxChangesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKxChangeset' smart constructor.
data CreateKxChangeset = CreateKxChangeset'
  { -- | A unique identifier of the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Text,
    -- | A list of change request objects that are run in order. A change request
    -- object consists of changeType , s3Path, and a dbPath. A changeType can
    -- has the following values:
    --
    -- -   PUT – Adds or updates files in a database.
    --
    -- -   DELETE – Deletes files in a database.
    --
    -- All the change requests require a mandatory /dbPath/ attribute that
    -- defines the path within the database directory. The /s3Path/ attribute
    -- defines the s3 source file path and is required for a PUT change type.
    --
    -- Here is an example of how you can use the change request object:
    --
    -- @[ { \"changeType\": \"PUT\", \"s3Path\":\"s3:\/\/bucket\/db\/2020.01.02\/\", \"dbPath\":\"\/2020.01.02\/\"}, { \"changeType\": \"PUT\", \"s3Path\":\"s3:\/\/bucket\/db\/sym\", \"dbPath\":\"\/\"}, { \"changeType\": \"DELETE\", \"dbPath\": \"\/2020.01.01\/\"} ]@
    --
    -- In this example, the first request with /PUT/ change type allows you to
    -- add files in the given s3Path under the /2020.01.02/ partition of the
    -- database. The second request with /PUT/ change type allows you to add a
    -- single sym file at database root location. The last request with
    -- /DELETE/ change type allows you to delete the files under the
    -- /2020.01.01/ partition of the database.
    changeRequests :: Prelude.NonEmpty ChangeRequest,
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxChangeset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'createKxChangeset_environmentId' - A unique identifier of the kdb environment.
--
-- 'databaseName', 'createKxChangeset_databaseName' - The name of the kdb database.
--
-- 'changeRequests', 'createKxChangeset_changeRequests' - A list of change request objects that are run in order. A change request
-- object consists of changeType , s3Path, and a dbPath. A changeType can
-- has the following values:
--
-- -   PUT – Adds or updates files in a database.
--
-- -   DELETE – Deletes files in a database.
--
-- All the change requests require a mandatory /dbPath/ attribute that
-- defines the path within the database directory. The /s3Path/ attribute
-- defines the s3 source file path and is required for a PUT change type.
--
-- Here is an example of how you can use the change request object:
--
-- @[ { \"changeType\": \"PUT\", \"s3Path\":\"s3:\/\/bucket\/db\/2020.01.02\/\", \"dbPath\":\"\/2020.01.02\/\"}, { \"changeType\": \"PUT\", \"s3Path\":\"s3:\/\/bucket\/db\/sym\", \"dbPath\":\"\/\"}, { \"changeType\": \"DELETE\", \"dbPath\": \"\/2020.01.01\/\"} ]@
--
-- In this example, the first request with /PUT/ change type allows you to
-- add files in the given s3Path under the /2020.01.02/ partition of the
-- database. The second request with /PUT/ change type allows you to add a
-- single sym file at database root location. The last request with
-- /DELETE/ change type allows you to delete the files under the
-- /2020.01.01/ partition of the database.
--
-- 'clientToken', 'createKxChangeset_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
newCreateKxChangeset ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'changeRequests'
  Prelude.NonEmpty ChangeRequest ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateKxChangeset
newCreateKxChangeset
  pEnvironmentId_
  pDatabaseName_
  pChangeRequests_
  pClientToken_ =
    CreateKxChangeset'
      { environmentId = pEnvironmentId_,
        databaseName = pDatabaseName_,
        changeRequests =
          Lens.coerced Lens.# pChangeRequests_,
        clientToken = pClientToken_
      }

-- | A unique identifier of the kdb environment.
createKxChangeset_environmentId :: Lens.Lens' CreateKxChangeset Prelude.Text
createKxChangeset_environmentId = Lens.lens (\CreateKxChangeset' {environmentId} -> environmentId) (\s@CreateKxChangeset' {} a -> s {environmentId = a} :: CreateKxChangeset)

-- | The name of the kdb database.
createKxChangeset_databaseName :: Lens.Lens' CreateKxChangeset Prelude.Text
createKxChangeset_databaseName = Lens.lens (\CreateKxChangeset' {databaseName} -> databaseName) (\s@CreateKxChangeset' {} a -> s {databaseName = a} :: CreateKxChangeset)

-- | A list of change request objects that are run in order. A change request
-- object consists of changeType , s3Path, and a dbPath. A changeType can
-- has the following values:
--
-- -   PUT – Adds or updates files in a database.
--
-- -   DELETE – Deletes files in a database.
--
-- All the change requests require a mandatory /dbPath/ attribute that
-- defines the path within the database directory. The /s3Path/ attribute
-- defines the s3 source file path and is required for a PUT change type.
--
-- Here is an example of how you can use the change request object:
--
-- @[ { \"changeType\": \"PUT\", \"s3Path\":\"s3:\/\/bucket\/db\/2020.01.02\/\", \"dbPath\":\"\/2020.01.02\/\"}, { \"changeType\": \"PUT\", \"s3Path\":\"s3:\/\/bucket\/db\/sym\", \"dbPath\":\"\/\"}, { \"changeType\": \"DELETE\", \"dbPath\": \"\/2020.01.01\/\"} ]@
--
-- In this example, the first request with /PUT/ change type allows you to
-- add files in the given s3Path under the /2020.01.02/ partition of the
-- database. The second request with /PUT/ change type allows you to add a
-- single sym file at database root location. The last request with
-- /DELETE/ change type allows you to delete the files under the
-- /2020.01.01/ partition of the database.
createKxChangeset_changeRequests :: Lens.Lens' CreateKxChangeset (Prelude.NonEmpty ChangeRequest)
createKxChangeset_changeRequests = Lens.lens (\CreateKxChangeset' {changeRequests} -> changeRequests) (\s@CreateKxChangeset' {} a -> s {changeRequests = a} :: CreateKxChangeset) Prelude.. Lens.coerced

-- | A token that ensures idempotency. This token expires in 10 minutes.
createKxChangeset_clientToken :: Lens.Lens' CreateKxChangeset Prelude.Text
createKxChangeset_clientToken = Lens.lens (\CreateKxChangeset' {clientToken} -> clientToken) (\s@CreateKxChangeset' {} a -> s {clientToken = a} :: CreateKxChangeset)

instance Core.AWSRequest CreateKxChangeset where
  type
    AWSResponse CreateKxChangeset =
      CreateKxChangesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKxChangesetResponse'
            Prelude.<$> (x Data..?> "changeRequests")
            Prelude.<*> (x Data..?> "changesetId")
            Prelude.<*> (x Data..?> "createdTimestamp")
            Prelude.<*> (x Data..?> "databaseName")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "errorInfo")
            Prelude.<*> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKxChangeset where
  hashWithSalt _salt CreateKxChangeset' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` changeRequests
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateKxChangeset where
  rnf CreateKxChangeset' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf changeRequests
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateKxChangeset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKxChangeset where
  toJSON CreateKxChangeset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("changeRequests" Data..= changeRequests),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateKxChangeset where
  toPath CreateKxChangeset' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases/",
        Data.toBS databaseName,
        "/changesets"
      ]

instance Data.ToQuery CreateKxChangeset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKxChangesetResponse' smart constructor.
data CreateKxChangesetResponse = CreateKxChangesetResponse'
  { -- | A list of change requests.
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
    -- | The details of the error that you receive when creating a changeset. It
    -- consists of the type of error and the error message.
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
-- Create a value of 'CreateKxChangesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeRequests', 'createKxChangesetResponse_changeRequests' - A list of change requests.
--
-- 'changesetId', 'createKxChangesetResponse_changesetId' - A unique identifier for the changeset.
--
-- 'createdTimestamp', 'createKxChangesetResponse_createdTimestamp' - The timestamp at which the changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databaseName', 'createKxChangesetResponse_databaseName' - The name of the kdb database.
--
-- 'environmentId', 'createKxChangesetResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'errorInfo', 'createKxChangesetResponse_errorInfo' - The details of the error that you receive when creating a changeset. It
-- consists of the type of error and the error message.
--
-- 'lastModifiedTimestamp', 'createKxChangesetResponse_lastModifiedTimestamp' - The timestamp at which the changeset was updated in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'status', 'createKxChangesetResponse_status' - Status of the changeset creation process.
--
-- -   Pending – Changeset creation is pending.
--
-- -   Processing – Changeset creation is running.
--
-- -   Failed – Changeset creation has failed.
--
-- -   Complete – Changeset creation has succeeded.
--
-- 'httpStatus', 'createKxChangesetResponse_httpStatus' - The response's http status code.
newCreateKxChangesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKxChangesetResponse
newCreateKxChangesetResponse pHttpStatus_ =
  CreateKxChangesetResponse'
    { changeRequests =
        Prelude.Nothing,
      changesetId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of change requests.
createKxChangesetResponse_changeRequests :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe (Prelude.NonEmpty ChangeRequest))
createKxChangesetResponse_changeRequests = Lens.lens (\CreateKxChangesetResponse' {changeRequests} -> changeRequests) (\s@CreateKxChangesetResponse' {} a -> s {changeRequests = a} :: CreateKxChangesetResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the changeset.
createKxChangesetResponse_changesetId :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe Prelude.Text)
createKxChangesetResponse_changesetId = Lens.lens (\CreateKxChangesetResponse' {changesetId} -> changesetId) (\s@CreateKxChangesetResponse' {} a -> s {changesetId = a} :: CreateKxChangesetResponse)

-- | The timestamp at which the changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
createKxChangesetResponse_createdTimestamp :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe Prelude.UTCTime)
createKxChangesetResponse_createdTimestamp = Lens.lens (\CreateKxChangesetResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateKxChangesetResponse' {} a -> s {createdTimestamp = a} :: CreateKxChangesetResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the kdb database.
createKxChangesetResponse_databaseName :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe Prelude.Text)
createKxChangesetResponse_databaseName = Lens.lens (\CreateKxChangesetResponse' {databaseName} -> databaseName) (\s@CreateKxChangesetResponse' {} a -> s {databaseName = a} :: CreateKxChangesetResponse)

-- | A unique identifier for the kdb environment.
createKxChangesetResponse_environmentId :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe Prelude.Text)
createKxChangesetResponse_environmentId = Lens.lens (\CreateKxChangesetResponse' {environmentId} -> environmentId) (\s@CreateKxChangesetResponse' {} a -> s {environmentId = a} :: CreateKxChangesetResponse)

-- | The details of the error that you receive when creating a changeset. It
-- consists of the type of error and the error message.
createKxChangesetResponse_errorInfo :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe ErrorInfo)
createKxChangesetResponse_errorInfo = Lens.lens (\CreateKxChangesetResponse' {errorInfo} -> errorInfo) (\s@CreateKxChangesetResponse' {} a -> s {errorInfo = a} :: CreateKxChangesetResponse)

-- | The timestamp at which the changeset was updated in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
createKxChangesetResponse_lastModifiedTimestamp :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe Prelude.UTCTime)
createKxChangesetResponse_lastModifiedTimestamp = Lens.lens (\CreateKxChangesetResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@CreateKxChangesetResponse' {} a -> s {lastModifiedTimestamp = a} :: CreateKxChangesetResponse) Prelude.. Lens.mapping Data._Time

-- | Status of the changeset creation process.
--
-- -   Pending – Changeset creation is pending.
--
-- -   Processing – Changeset creation is running.
--
-- -   Failed – Changeset creation has failed.
--
-- -   Complete – Changeset creation has succeeded.
createKxChangesetResponse_status :: Lens.Lens' CreateKxChangesetResponse (Prelude.Maybe ChangesetStatus)
createKxChangesetResponse_status = Lens.lens (\CreateKxChangesetResponse' {status} -> status) (\s@CreateKxChangesetResponse' {} a -> s {status = a} :: CreateKxChangesetResponse)

-- | The response's http status code.
createKxChangesetResponse_httpStatus :: Lens.Lens' CreateKxChangesetResponse Prelude.Int
createKxChangesetResponse_httpStatus = Lens.lens (\CreateKxChangesetResponse' {httpStatus} -> httpStatus) (\s@CreateKxChangesetResponse' {} a -> s {httpStatus = a} :: CreateKxChangesetResponse)

instance Prelude.NFData CreateKxChangesetResponse where
  rnf CreateKxChangesetResponse' {..} =
    Prelude.rnf changeRequests
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
