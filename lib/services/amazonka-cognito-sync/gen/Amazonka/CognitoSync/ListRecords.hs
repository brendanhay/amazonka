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
-- Module      : Amazonka.CognitoSync.ListRecords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets paginated records, optionally changed after a particular sync count
-- for a dataset and identity. With Amazon Cognito Sync, each identity has
-- access only to its own data. Thus, the credentials used to make this API
-- call need to have access to the identity data.
--
-- ListRecords can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials. You should use Cognito
-- Identity credentials to make this API call.
module Amazonka.CognitoSync.ListRecords
  ( -- * Creating a Request
    ListRecords (..),
    newListRecords,

    -- * Request Lenses
    listRecords_lastSyncCount,
    listRecords_maxResults,
    listRecords_nextToken,
    listRecords_syncSessionToken,
    listRecords_identityPoolId,
    listRecords_identityId,
    listRecords_datasetName,

    -- * Destructuring the Response
    ListRecordsResponse (..),
    newListRecordsResponse,

    -- * Response Lenses
    listRecordsResponse_count,
    listRecordsResponse_datasetDeletedAfterRequestedSyncCount,
    listRecordsResponse_datasetExists,
    listRecordsResponse_datasetSyncCount,
    listRecordsResponse_lastModifiedBy,
    listRecordsResponse_mergedDatasetNames,
    listRecordsResponse_nextToken,
    listRecordsResponse_records,
    listRecordsResponse_syncSessionToken,
    listRecordsResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request for a list of records.
--
-- /See:/ 'newListRecords' smart constructor.
data ListRecords = ListRecords'
  { -- | The last server sync count for this record.
    lastSyncCount :: Prelude.Maybe Prelude.Integer,
    -- | The maximum number of results to be returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Prelude.Maybe Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Prelude.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
    -- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastSyncCount', 'listRecords_lastSyncCount' - The last server sync count for this record.
--
-- 'maxResults', 'listRecords_maxResults' - The maximum number of results to be returned.
--
-- 'nextToken', 'listRecords_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'syncSessionToken', 'listRecords_syncSessionToken' - A token containing a session ID, identity ID, and expiration.
--
-- 'identityPoolId', 'listRecords_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'identityId', 'listRecords_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'datasetName', 'listRecords_datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
newListRecords ::
  -- | 'identityPoolId'
  Prelude.Text ->
  -- | 'identityId'
  Prelude.Text ->
  -- | 'datasetName'
  Prelude.Text ->
  ListRecords
newListRecords
  pIdentityPoolId_
  pIdentityId_
  pDatasetName_ =
    ListRecords'
      { lastSyncCount = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        syncSessionToken = Prelude.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_
      }

-- | The last server sync count for this record.
listRecords_lastSyncCount :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Integer)
listRecords_lastSyncCount = Lens.lens (\ListRecords' {lastSyncCount} -> lastSyncCount) (\s@ListRecords' {} a -> s {lastSyncCount = a} :: ListRecords)

-- | The maximum number of results to be returned.
listRecords_maxResults :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Int)
listRecords_maxResults = Lens.lens (\ListRecords' {maxResults} -> maxResults) (\s@ListRecords' {} a -> s {maxResults = a} :: ListRecords)

-- | A pagination token for obtaining the next page of results.
listRecords_nextToken :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Text)
listRecords_nextToken = Lens.lens (\ListRecords' {nextToken} -> nextToken) (\s@ListRecords' {} a -> s {nextToken = a} :: ListRecords)

-- | A token containing a session ID, identity ID, and expiration.
listRecords_syncSessionToken :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Text)
listRecords_syncSessionToken = Lens.lens (\ListRecords' {syncSessionToken} -> syncSessionToken) (\s@ListRecords' {} a -> s {syncSessionToken = a} :: ListRecords)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
listRecords_identityPoolId :: Lens.Lens' ListRecords Prelude.Text
listRecords_identityPoolId = Lens.lens (\ListRecords' {identityPoolId} -> identityPoolId) (\s@ListRecords' {} a -> s {identityPoolId = a} :: ListRecords)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
listRecords_identityId :: Lens.Lens' ListRecords Prelude.Text
listRecords_identityId = Lens.lens (\ListRecords' {identityId} -> identityId) (\s@ListRecords' {} a -> s {identityId = a} :: ListRecords)

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
listRecords_datasetName :: Lens.Lens' ListRecords Prelude.Text
listRecords_datasetName = Lens.lens (\ListRecords' {datasetName} -> datasetName) (\s@ListRecords' {} a -> s {datasetName = a} :: ListRecords)

instance Core.AWSRequest ListRecords where
  type AWSResponse ListRecords = ListRecordsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecordsResponse'
            Prelude.<$> (x Data..?> "Count")
            Prelude.<*> (x Data..?> "DatasetDeletedAfterRequestedSyncCount")
            Prelude.<*> (x Data..?> "DatasetExists")
            Prelude.<*> (x Data..?> "DatasetSyncCount")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> ( x Data..?> "MergedDatasetNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Records" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SyncSessionToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecords where
  hashWithSalt _salt ListRecords' {..} =
    _salt `Prelude.hashWithSalt` lastSyncCount
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` syncSessionToken
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` datasetName

instance Prelude.NFData ListRecords where
  rnf ListRecords' {..} =
    Prelude.rnf lastSyncCount
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf syncSessionToken
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf datasetName

instance Data.ToHeaders ListRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecords where
  toPath ListRecords' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/identities/",
        Data.toBS identityId,
        "/datasets/",
        Data.toBS datasetName,
        "/records"
      ]

instance Data.ToQuery ListRecords where
  toQuery ListRecords' {..} =
    Prelude.mconcat
      [ "lastSyncCount" Data.=: lastSyncCount,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "syncSessionToken" Data.=: syncSessionToken
      ]

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'newListRecordsResponse' smart constructor.
data ListRecordsResponse = ListRecordsResponse'
  { -- | Total number of records.
    count :: Prelude.Maybe Prelude.Int,
    -- | A boolean value specifying whether to delete the dataset locally.
    datasetDeletedAfterRequestedSyncCount :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the dataset exists.
    datasetExists :: Prelude.Maybe Prelude.Bool,
    -- | Server sync count for this dataset.
    datasetSyncCount :: Prelude.Maybe Prelude.Integer,
    -- | The user\/device that made the last change to this record.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Names of merged datasets.
    mergedDatasetNames :: Prelude.Maybe [Prelude.Text],
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all records.
    records :: Prelude.Maybe [Record],
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'listRecordsResponse_count' - Total number of records.
--
-- 'datasetDeletedAfterRequestedSyncCount', 'listRecordsResponse_datasetDeletedAfterRequestedSyncCount' - A boolean value specifying whether to delete the dataset locally.
--
-- 'datasetExists', 'listRecordsResponse_datasetExists' - Indicates whether the dataset exists.
--
-- 'datasetSyncCount', 'listRecordsResponse_datasetSyncCount' - Server sync count for this dataset.
--
-- 'lastModifiedBy', 'listRecordsResponse_lastModifiedBy' - The user\/device that made the last change to this record.
--
-- 'mergedDatasetNames', 'listRecordsResponse_mergedDatasetNames' - Names of merged datasets.
--
-- 'nextToken', 'listRecordsResponse_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'records', 'listRecordsResponse_records' - A list of all records.
--
-- 'syncSessionToken', 'listRecordsResponse_syncSessionToken' - A token containing a session ID, identity ID, and expiration.
--
-- 'httpStatus', 'listRecordsResponse_httpStatus' - The response's http status code.
newListRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecordsResponse
newListRecordsResponse pHttpStatus_ =
  ListRecordsResponse'
    { count = Prelude.Nothing,
      datasetDeletedAfterRequestedSyncCount =
        Prelude.Nothing,
      datasetExists = Prelude.Nothing,
      datasetSyncCount = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      mergedDatasetNames = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      records = Prelude.Nothing,
      syncSessionToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Total number of records.
listRecordsResponse_count :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Int)
listRecordsResponse_count = Lens.lens (\ListRecordsResponse' {count} -> count) (\s@ListRecordsResponse' {} a -> s {count = a} :: ListRecordsResponse)

-- | A boolean value specifying whether to delete the dataset locally.
listRecordsResponse_datasetDeletedAfterRequestedSyncCount :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Bool)
listRecordsResponse_datasetDeletedAfterRequestedSyncCount = Lens.lens (\ListRecordsResponse' {datasetDeletedAfterRequestedSyncCount} -> datasetDeletedAfterRequestedSyncCount) (\s@ListRecordsResponse' {} a -> s {datasetDeletedAfterRequestedSyncCount = a} :: ListRecordsResponse)

-- | Indicates whether the dataset exists.
listRecordsResponse_datasetExists :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Bool)
listRecordsResponse_datasetExists = Lens.lens (\ListRecordsResponse' {datasetExists} -> datasetExists) (\s@ListRecordsResponse' {} a -> s {datasetExists = a} :: ListRecordsResponse)

-- | Server sync count for this dataset.
listRecordsResponse_datasetSyncCount :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Integer)
listRecordsResponse_datasetSyncCount = Lens.lens (\ListRecordsResponse' {datasetSyncCount} -> datasetSyncCount) (\s@ListRecordsResponse' {} a -> s {datasetSyncCount = a} :: ListRecordsResponse)

-- | The user\/device that made the last change to this record.
listRecordsResponse_lastModifiedBy :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Text)
listRecordsResponse_lastModifiedBy = Lens.lens (\ListRecordsResponse' {lastModifiedBy} -> lastModifiedBy) (\s@ListRecordsResponse' {} a -> s {lastModifiedBy = a} :: ListRecordsResponse)

-- | Names of merged datasets.
listRecordsResponse_mergedDatasetNames :: Lens.Lens' ListRecordsResponse (Prelude.Maybe [Prelude.Text])
listRecordsResponse_mergedDatasetNames = Lens.lens (\ListRecordsResponse' {mergedDatasetNames} -> mergedDatasetNames) (\s@ListRecordsResponse' {} a -> s {mergedDatasetNames = a} :: ListRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token for obtaining the next page of results.
listRecordsResponse_nextToken :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Text)
listRecordsResponse_nextToken = Lens.lens (\ListRecordsResponse' {nextToken} -> nextToken) (\s@ListRecordsResponse' {} a -> s {nextToken = a} :: ListRecordsResponse)

-- | A list of all records.
listRecordsResponse_records :: Lens.Lens' ListRecordsResponse (Prelude.Maybe [Record])
listRecordsResponse_records = Lens.lens (\ListRecordsResponse' {records} -> records) (\s@ListRecordsResponse' {} a -> s {records = a} :: ListRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token containing a session ID, identity ID, and expiration.
listRecordsResponse_syncSessionToken :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Text)
listRecordsResponse_syncSessionToken = Lens.lens (\ListRecordsResponse' {syncSessionToken} -> syncSessionToken) (\s@ListRecordsResponse' {} a -> s {syncSessionToken = a} :: ListRecordsResponse)

-- | The response's http status code.
listRecordsResponse_httpStatus :: Lens.Lens' ListRecordsResponse Prelude.Int
listRecordsResponse_httpStatus = Lens.lens (\ListRecordsResponse' {httpStatus} -> httpStatus) (\s@ListRecordsResponse' {} a -> s {httpStatus = a} :: ListRecordsResponse)

instance Prelude.NFData ListRecordsResponse where
  rnf ListRecordsResponse' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf datasetDeletedAfterRequestedSyncCount
      `Prelude.seq` Prelude.rnf datasetExists
      `Prelude.seq` Prelude.rnf datasetSyncCount
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf mergedDatasetNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf syncSessionToken
      `Prelude.seq` Prelude.rnf httpStatus
