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
-- Module      : Amazonka.FinSpaceData.GetChangeset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a Changeset.
module Amazonka.FinSpaceData.GetChangeset
  ( -- * Creating a Request
    GetChangeset (..),
    newGetChangeset,

    -- * Request Lenses
    getChangeset_datasetId,
    getChangeset_changesetId,

    -- * Destructuring the Response
    GetChangesetResponse (..),
    newGetChangesetResponse,

    -- * Response Lenses
    getChangesetResponse_sourceParams,
    getChangesetResponse_updatedByChangesetId,
    getChangesetResponse_changeType,
    getChangesetResponse_changesetId,
    getChangesetResponse_changesetArn,
    getChangesetResponse_formatParams,
    getChangesetResponse_activeUntilTimestamp,
    getChangesetResponse_status,
    getChangesetResponse_updatesChangesetId,
    getChangesetResponse_datasetId,
    getChangesetResponse_activeFromTimestamp,
    getChangesetResponse_createTime,
    getChangesetResponse_errorInfo,
    getChangesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe a changeset.
--
-- /See:/ 'newGetChangeset' smart constructor.
data GetChangeset = GetChangeset'
  { -- | The unique identifier for the FinSpace Dataset where the Changeset is
    -- created.
    datasetId :: Prelude.Text,
    -- | The unique identifier of the Changeset for which to get data.
    changesetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetId', 'getChangeset_datasetId' - The unique identifier for the FinSpace Dataset where the Changeset is
-- created.
--
-- 'changesetId', 'getChangeset_changesetId' - The unique identifier of the Changeset for which to get data.
newGetChangeset ::
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'changesetId'
  Prelude.Text ->
  GetChangeset
newGetChangeset pDatasetId_ pChangesetId_ =
  GetChangeset'
    { datasetId = pDatasetId_,
      changesetId = pChangesetId_
    }

-- | The unique identifier for the FinSpace Dataset where the Changeset is
-- created.
getChangeset_datasetId :: Lens.Lens' GetChangeset Prelude.Text
getChangeset_datasetId = Lens.lens (\GetChangeset' {datasetId} -> datasetId) (\s@GetChangeset' {} a -> s {datasetId = a} :: GetChangeset)

-- | The unique identifier of the Changeset for which to get data.
getChangeset_changesetId :: Lens.Lens' GetChangeset Prelude.Text
getChangeset_changesetId = Lens.lens (\GetChangeset' {changesetId} -> changesetId) (\s@GetChangeset' {} a -> s {changesetId = a} :: GetChangeset)

instance Core.AWSRequest GetChangeset where
  type AWSResponse GetChangeset = GetChangesetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangesetResponse'
            Prelude.<$> (x Data..?> "sourceParams" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "updatedByChangesetId")
            Prelude.<*> (x Data..?> "changeType")
            Prelude.<*> (x Data..?> "changesetId")
            Prelude.<*> (x Data..?> "changesetArn")
            Prelude.<*> (x Data..?> "formatParams" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "activeUntilTimestamp")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "updatesChangesetId")
            Prelude.<*> (x Data..?> "datasetId")
            Prelude.<*> (x Data..?> "activeFromTimestamp")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "errorInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChangeset where
  hashWithSalt _salt GetChangeset' {..} =
    _salt `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` changesetId

instance Prelude.NFData GetChangeset where
  rnf GetChangeset' {..} =
    Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf changesetId

instance Data.ToHeaders GetChangeset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChangeset where
  toPath GetChangeset' {..} =
    Prelude.mconcat
      [ "/datasets/",
        Data.toBS datasetId,
        "/changesetsv2/",
        Data.toBS changesetId
      ]

instance Data.ToQuery GetChangeset where
  toQuery = Prelude.const Prelude.mempty

-- | The response from a describe changeset operation
--
-- /See:/ 'newGetChangesetResponse' smart constructor.
data GetChangesetResponse = GetChangesetResponse'
  { -- | Options that define the location of the data being ingested.
    sourceParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier of the updated Changeset.
    updatedByChangesetId :: Prelude.Maybe Prelude.Text,
    -- | Type that indicates how a Changeset is applied to a Dataset.
    --
    -- -   @REPLACE@ – Changeset is considered as a replacement to all prior
    --     loaded Changesets.
    --
    -- -   @APPEND@ – Changeset is considered as an addition to the end of all
    --     prior loaded Changesets.
    --
    -- -   @MODIFY@ – Changeset is considered as a replacement to a specific
    --     prior ingested Changeset.
    changeType :: Prelude.Maybe ChangeType,
    -- | The unique identifier for a Changeset.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The ARN identifier of the Changeset.
    changesetArn :: Prelude.Maybe Prelude.Text,
    -- | Structure of the source file(s).
    formatParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Time until which the Changeset is active. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    activeUntilTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The status of Changeset creation operation.
    status :: Prelude.Maybe IngestionStatus,
    -- | The unique identifier of the Changeset that is being updated.
    updatesChangesetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace Dataset where the Changeset is
    -- created.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | Beginning time from which the Changeset is active. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    activeFromTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The timestamp at which the Changeset was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | The structure with error messages.
    errorInfo :: Prelude.Maybe ChangesetErrorInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceParams', 'getChangesetResponse_sourceParams' - Options that define the location of the data being ingested.
--
-- 'updatedByChangesetId', 'getChangesetResponse_updatedByChangesetId' - The unique identifier of the updated Changeset.
--
-- 'changeType', 'getChangesetResponse_changeType' - Type that indicates how a Changeset is applied to a Dataset.
--
-- -   @REPLACE@ – Changeset is considered as a replacement to all prior
--     loaded Changesets.
--
-- -   @APPEND@ – Changeset is considered as an addition to the end of all
--     prior loaded Changesets.
--
-- -   @MODIFY@ – Changeset is considered as a replacement to a specific
--     prior ingested Changeset.
--
-- 'changesetId', 'getChangesetResponse_changesetId' - The unique identifier for a Changeset.
--
-- 'changesetArn', 'getChangesetResponse_changesetArn' - The ARN identifier of the Changeset.
--
-- 'formatParams', 'getChangesetResponse_formatParams' - Structure of the source file(s).
--
-- 'activeUntilTimestamp', 'getChangesetResponse_activeUntilTimestamp' - Time until which the Changeset is active. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'status', 'getChangesetResponse_status' - The status of Changeset creation operation.
--
-- 'updatesChangesetId', 'getChangesetResponse_updatesChangesetId' - The unique identifier of the Changeset that is being updated.
--
-- 'datasetId', 'getChangesetResponse_datasetId' - The unique identifier for the FinSpace Dataset where the Changeset is
-- created.
--
-- 'activeFromTimestamp', 'getChangesetResponse_activeFromTimestamp' - Beginning time from which the Changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'createTime', 'getChangesetResponse_createTime' - The timestamp at which the Changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'errorInfo', 'getChangesetResponse_errorInfo' - The structure with error messages.
--
-- 'httpStatus', 'getChangesetResponse_httpStatus' - The response's http status code.
newGetChangesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChangesetResponse
newGetChangesetResponse pHttpStatus_ =
  GetChangesetResponse'
    { sourceParams =
        Prelude.Nothing,
      updatedByChangesetId = Prelude.Nothing,
      changeType = Prelude.Nothing,
      changesetId = Prelude.Nothing,
      changesetArn = Prelude.Nothing,
      formatParams = Prelude.Nothing,
      activeUntilTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      updatesChangesetId = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      activeFromTimestamp = Prelude.Nothing,
      createTime = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Options that define the location of the data being ingested.
getChangesetResponse_sourceParams :: Lens.Lens' GetChangesetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getChangesetResponse_sourceParams = Lens.lens (\GetChangesetResponse' {sourceParams} -> sourceParams) (\s@GetChangesetResponse' {} a -> s {sourceParams = a} :: GetChangesetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the updated Changeset.
getChangesetResponse_updatedByChangesetId :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Text)
getChangesetResponse_updatedByChangesetId = Lens.lens (\GetChangesetResponse' {updatedByChangesetId} -> updatedByChangesetId) (\s@GetChangesetResponse' {} a -> s {updatedByChangesetId = a} :: GetChangesetResponse)

-- | Type that indicates how a Changeset is applied to a Dataset.
--
-- -   @REPLACE@ – Changeset is considered as a replacement to all prior
--     loaded Changesets.
--
-- -   @APPEND@ – Changeset is considered as an addition to the end of all
--     prior loaded Changesets.
--
-- -   @MODIFY@ – Changeset is considered as a replacement to a specific
--     prior ingested Changeset.
getChangesetResponse_changeType :: Lens.Lens' GetChangesetResponse (Prelude.Maybe ChangeType)
getChangesetResponse_changeType = Lens.lens (\GetChangesetResponse' {changeType} -> changeType) (\s@GetChangesetResponse' {} a -> s {changeType = a} :: GetChangesetResponse)

-- | The unique identifier for a Changeset.
getChangesetResponse_changesetId :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Text)
getChangesetResponse_changesetId = Lens.lens (\GetChangesetResponse' {changesetId} -> changesetId) (\s@GetChangesetResponse' {} a -> s {changesetId = a} :: GetChangesetResponse)

-- | The ARN identifier of the Changeset.
getChangesetResponse_changesetArn :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Text)
getChangesetResponse_changesetArn = Lens.lens (\GetChangesetResponse' {changesetArn} -> changesetArn) (\s@GetChangesetResponse' {} a -> s {changesetArn = a} :: GetChangesetResponse)

-- | Structure of the source file(s).
getChangesetResponse_formatParams :: Lens.Lens' GetChangesetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getChangesetResponse_formatParams = Lens.lens (\GetChangesetResponse' {formatParams} -> formatParams) (\s@GetChangesetResponse' {} a -> s {formatParams = a} :: GetChangesetResponse) Prelude.. Lens.mapping Lens.coerced

-- | Time until which the Changeset is active. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getChangesetResponse_activeUntilTimestamp :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Integer)
getChangesetResponse_activeUntilTimestamp = Lens.lens (\GetChangesetResponse' {activeUntilTimestamp} -> activeUntilTimestamp) (\s@GetChangesetResponse' {} a -> s {activeUntilTimestamp = a} :: GetChangesetResponse)

-- | The status of Changeset creation operation.
getChangesetResponse_status :: Lens.Lens' GetChangesetResponse (Prelude.Maybe IngestionStatus)
getChangesetResponse_status = Lens.lens (\GetChangesetResponse' {status} -> status) (\s@GetChangesetResponse' {} a -> s {status = a} :: GetChangesetResponse)

-- | The unique identifier of the Changeset that is being updated.
getChangesetResponse_updatesChangesetId :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Text)
getChangesetResponse_updatesChangesetId = Lens.lens (\GetChangesetResponse' {updatesChangesetId} -> updatesChangesetId) (\s@GetChangesetResponse' {} a -> s {updatesChangesetId = a} :: GetChangesetResponse)

-- | The unique identifier for the FinSpace Dataset where the Changeset is
-- created.
getChangesetResponse_datasetId :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Text)
getChangesetResponse_datasetId = Lens.lens (\GetChangesetResponse' {datasetId} -> datasetId) (\s@GetChangesetResponse' {} a -> s {datasetId = a} :: GetChangesetResponse)

-- | Beginning time from which the Changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getChangesetResponse_activeFromTimestamp :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Integer)
getChangesetResponse_activeFromTimestamp = Lens.lens (\GetChangesetResponse' {activeFromTimestamp} -> activeFromTimestamp) (\s@GetChangesetResponse' {} a -> s {activeFromTimestamp = a} :: GetChangesetResponse)

-- | The timestamp at which the Changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getChangesetResponse_createTime :: Lens.Lens' GetChangesetResponse (Prelude.Maybe Prelude.Integer)
getChangesetResponse_createTime = Lens.lens (\GetChangesetResponse' {createTime} -> createTime) (\s@GetChangesetResponse' {} a -> s {createTime = a} :: GetChangesetResponse)

-- | The structure with error messages.
getChangesetResponse_errorInfo :: Lens.Lens' GetChangesetResponse (Prelude.Maybe ChangesetErrorInfo)
getChangesetResponse_errorInfo = Lens.lens (\GetChangesetResponse' {errorInfo} -> errorInfo) (\s@GetChangesetResponse' {} a -> s {errorInfo = a} :: GetChangesetResponse)

-- | The response's http status code.
getChangesetResponse_httpStatus :: Lens.Lens' GetChangesetResponse Prelude.Int
getChangesetResponse_httpStatus = Lens.lens (\GetChangesetResponse' {httpStatus} -> httpStatus) (\s@GetChangesetResponse' {} a -> s {httpStatus = a} :: GetChangesetResponse)

instance Prelude.NFData GetChangesetResponse where
  rnf GetChangesetResponse' {..} =
    Prelude.rnf sourceParams
      `Prelude.seq` Prelude.rnf updatedByChangesetId
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf changesetArn
      `Prelude.seq` Prelude.rnf formatParams
      `Prelude.seq` Prelude.rnf activeUntilTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatesChangesetId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf activeFromTimestamp
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf httpStatus
