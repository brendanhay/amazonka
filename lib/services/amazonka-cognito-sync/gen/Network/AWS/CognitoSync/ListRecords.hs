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
-- Module      : Network.AWS.CognitoSync.ListRecords
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CognitoSync.ListRecords
  ( -- * Creating a Request
    ListRecords (..),
    newListRecords,

    -- * Request Lenses
    listRecords_nextToken,
    listRecords_maxResults,
    listRecords_lastSyncCount,
    listRecords_syncSessionToken,
    listRecords_identityPoolId,
    listRecords_identityId,
    listRecords_datasetName,

    -- * Destructuring the Response
    ListRecordsResponse (..),
    newListRecordsResponse,

    -- * Response Lenses
    listRecordsResponse_nextToken,
    listRecordsResponse_datasetSyncCount,
    listRecordsResponse_records,
    listRecordsResponse_datasetDeletedAfterRequestedSyncCount,
    listRecordsResponse_count,
    listRecordsResponse_lastModifiedBy,
    listRecordsResponse_datasetExists,
    listRecordsResponse_syncSessionToken,
    listRecordsResponse_mergedDatasetNames,
    listRecordsResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for a list of records.
--
-- /See:/ 'newListRecords' smart constructor.
data ListRecords = ListRecords'
  { -- | A pagination token for obtaining the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The last server sync count for this record.
    lastSyncCount :: Prelude.Maybe Prelude.Integer,
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
-- 'nextToken', 'listRecords_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'maxResults', 'listRecords_maxResults' - The maximum number of results to be returned.
--
-- 'lastSyncCount', 'listRecords_lastSyncCount' - The last server sync count for this record.
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
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        lastSyncCount = Prelude.Nothing,
        syncSessionToken = Prelude.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_
      }

-- | A pagination token for obtaining the next page of results.
listRecords_nextToken :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Text)
listRecords_nextToken = Lens.lens (\ListRecords' {nextToken} -> nextToken) (\s@ListRecords' {} a -> s {nextToken = a} :: ListRecords)

-- | The maximum number of results to be returned.
listRecords_maxResults :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Int)
listRecords_maxResults = Lens.lens (\ListRecords' {maxResults} -> maxResults) (\s@ListRecords' {} a -> s {maxResults = a} :: ListRecords)

-- | The last server sync count for this record.
listRecords_lastSyncCount :: Lens.Lens' ListRecords (Prelude.Maybe Prelude.Integer)
listRecords_lastSyncCount = Lens.lens (\ListRecords' {lastSyncCount} -> lastSyncCount) (\s@ListRecords' {} a -> s {lastSyncCount = a} :: ListRecords)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecordsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DatasetSyncCount")
            Prelude.<*> (x Core..?> "Records" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "DatasetDeletedAfterRequestedSyncCount")
            Prelude.<*> (x Core..?> "Count")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "DatasetExists")
            Prelude.<*> (x Core..?> "SyncSessionToken")
            Prelude.<*> ( x Core..?> "MergedDatasetNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecords

instance Prelude.NFData ListRecords

instance Core.ToHeaders ListRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListRecords where
  toPath ListRecords' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/identities/",
        Core.toBS identityId,
        "/datasets/",
        Core.toBS datasetName,
        "/records"
      ]

instance Core.ToQuery ListRecords where
  toQuery ListRecords' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "lastSyncCount" Core.=: lastSyncCount,
        "syncSessionToken" Core.=: syncSessionToken
      ]

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'newListRecordsResponse' smart constructor.
data ListRecordsResponse = ListRecordsResponse'
  { -- | A pagination token for obtaining the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Server sync count for this dataset.
    datasetSyncCount :: Prelude.Maybe Prelude.Integer,
    -- | A list of all records.
    records :: Prelude.Maybe [Record],
    -- | A boolean value specifying whether to delete the dataset locally.
    datasetDeletedAfterRequestedSyncCount :: Prelude.Maybe Prelude.Bool,
    -- | Total number of records.
    count :: Prelude.Maybe Prelude.Int,
    -- | The user\/device that made the last change to this record.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the dataset exists.
    datasetExists :: Prelude.Maybe Prelude.Bool,
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Prelude.Maybe Prelude.Text,
    -- | Names of merged datasets.
    mergedDatasetNames :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'listRecordsResponse_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'datasetSyncCount', 'listRecordsResponse_datasetSyncCount' - Server sync count for this dataset.
--
-- 'records', 'listRecordsResponse_records' - A list of all records.
--
-- 'datasetDeletedAfterRequestedSyncCount', 'listRecordsResponse_datasetDeletedAfterRequestedSyncCount' - A boolean value specifying whether to delete the dataset locally.
--
-- 'count', 'listRecordsResponse_count' - Total number of records.
--
-- 'lastModifiedBy', 'listRecordsResponse_lastModifiedBy' - The user\/device that made the last change to this record.
--
-- 'datasetExists', 'listRecordsResponse_datasetExists' - Indicates whether the dataset exists.
--
-- 'syncSessionToken', 'listRecordsResponse_syncSessionToken' - A token containing a session ID, identity ID, and expiration.
--
-- 'mergedDatasetNames', 'listRecordsResponse_mergedDatasetNames' - Names of merged datasets.
--
-- 'httpStatus', 'listRecordsResponse_httpStatus' - The response's http status code.
newListRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecordsResponse
newListRecordsResponse pHttpStatus_ =
  ListRecordsResponse'
    { nextToken = Prelude.Nothing,
      datasetSyncCount = Prelude.Nothing,
      records = Prelude.Nothing,
      datasetDeletedAfterRequestedSyncCount =
        Prelude.Nothing,
      count = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      datasetExists = Prelude.Nothing,
      syncSessionToken = Prelude.Nothing,
      mergedDatasetNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for obtaining the next page of results.
listRecordsResponse_nextToken :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Text)
listRecordsResponse_nextToken = Lens.lens (\ListRecordsResponse' {nextToken} -> nextToken) (\s@ListRecordsResponse' {} a -> s {nextToken = a} :: ListRecordsResponse)

-- | Server sync count for this dataset.
listRecordsResponse_datasetSyncCount :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Integer)
listRecordsResponse_datasetSyncCount = Lens.lens (\ListRecordsResponse' {datasetSyncCount} -> datasetSyncCount) (\s@ListRecordsResponse' {} a -> s {datasetSyncCount = a} :: ListRecordsResponse)

-- | A list of all records.
listRecordsResponse_records :: Lens.Lens' ListRecordsResponse (Prelude.Maybe [Record])
listRecordsResponse_records = Lens.lens (\ListRecordsResponse' {records} -> records) (\s@ListRecordsResponse' {} a -> s {records = a} :: ListRecordsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A boolean value specifying whether to delete the dataset locally.
listRecordsResponse_datasetDeletedAfterRequestedSyncCount :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Bool)
listRecordsResponse_datasetDeletedAfterRequestedSyncCount = Lens.lens (\ListRecordsResponse' {datasetDeletedAfterRequestedSyncCount} -> datasetDeletedAfterRequestedSyncCount) (\s@ListRecordsResponse' {} a -> s {datasetDeletedAfterRequestedSyncCount = a} :: ListRecordsResponse)

-- | Total number of records.
listRecordsResponse_count :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Int)
listRecordsResponse_count = Lens.lens (\ListRecordsResponse' {count} -> count) (\s@ListRecordsResponse' {} a -> s {count = a} :: ListRecordsResponse)

-- | The user\/device that made the last change to this record.
listRecordsResponse_lastModifiedBy :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Text)
listRecordsResponse_lastModifiedBy = Lens.lens (\ListRecordsResponse' {lastModifiedBy} -> lastModifiedBy) (\s@ListRecordsResponse' {} a -> s {lastModifiedBy = a} :: ListRecordsResponse)

-- | Indicates whether the dataset exists.
listRecordsResponse_datasetExists :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Bool)
listRecordsResponse_datasetExists = Lens.lens (\ListRecordsResponse' {datasetExists} -> datasetExists) (\s@ListRecordsResponse' {} a -> s {datasetExists = a} :: ListRecordsResponse)

-- | A token containing a session ID, identity ID, and expiration.
listRecordsResponse_syncSessionToken :: Lens.Lens' ListRecordsResponse (Prelude.Maybe Prelude.Text)
listRecordsResponse_syncSessionToken = Lens.lens (\ListRecordsResponse' {syncSessionToken} -> syncSessionToken) (\s@ListRecordsResponse' {} a -> s {syncSessionToken = a} :: ListRecordsResponse)

-- | Names of merged datasets.
listRecordsResponse_mergedDatasetNames :: Lens.Lens' ListRecordsResponse (Prelude.Maybe [Prelude.Text])
listRecordsResponse_mergedDatasetNames = Lens.lens (\ListRecordsResponse' {mergedDatasetNames} -> mergedDatasetNames) (\s@ListRecordsResponse' {} a -> s {mergedDatasetNames = a} :: ListRecordsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRecordsResponse_httpStatus :: Lens.Lens' ListRecordsResponse Prelude.Int
listRecordsResponse_httpStatus = Lens.lens (\ListRecordsResponse' {httpStatus} -> httpStatus) (\s@ListRecordsResponse' {} a -> s {httpStatus = a} :: ListRecordsResponse)

instance Prelude.NFData ListRecordsResponse
