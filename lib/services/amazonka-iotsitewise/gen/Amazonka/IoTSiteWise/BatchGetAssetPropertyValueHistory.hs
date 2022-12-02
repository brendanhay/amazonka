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
-- Module      : Amazonka.IoTSiteWise.BatchGetAssetPropertyValueHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the historical values for one or more asset properties. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/query-industrial-data.html#historical-values Querying historical values>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.BatchGetAssetPropertyValueHistory
  ( -- * Creating a Request
    BatchGetAssetPropertyValueHistory (..),
    newBatchGetAssetPropertyValueHistory,

    -- * Request Lenses
    batchGetAssetPropertyValueHistory_nextToken,
    batchGetAssetPropertyValueHistory_maxResults,
    batchGetAssetPropertyValueHistory_entries,

    -- * Destructuring the Response
    BatchGetAssetPropertyValueHistoryResponse (..),
    newBatchGetAssetPropertyValueHistoryResponse,

    -- * Response Lenses
    batchGetAssetPropertyValueHistoryResponse_nextToken,
    batchGetAssetPropertyValueHistoryResponse_httpStatus,
    batchGetAssetPropertyValueHistoryResponse_errorEntries,
    batchGetAssetPropertyValueHistoryResponse_successEntries,
    batchGetAssetPropertyValueHistoryResponse_skippedEntries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetAssetPropertyValueHistory' smart constructor.
data BatchGetAssetPropertyValueHistory = BatchGetAssetPropertyValueHistory'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request. A
    -- result set is returned in the two cases, whichever occurs first.
    --
    -- -   The size of the result set is less than 1 MB.
    --
    -- -   The number of data points in the result set is less than the value
    --     of @maxResults@. The maximum value of @maxResults@ is 4000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The list of asset property historical value entries for the batch get
    -- request. You can specify up to 16 entries per request.
    entries :: [BatchGetAssetPropertyValueHistoryEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetAssetPropertyValueHistory_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'batchGetAssetPropertyValueHistory_maxResults' - The maximum number of results to return for each paginated request. A
-- result set is returned in the two cases, whichever occurs first.
--
-- -   The size of the result set is less than 1 MB.
--
-- -   The number of data points in the result set is less than the value
--     of @maxResults@. The maximum value of @maxResults@ is 4000.
--
-- 'entries', 'batchGetAssetPropertyValueHistory_entries' - The list of asset property historical value entries for the batch get
-- request. You can specify up to 16 entries per request.
newBatchGetAssetPropertyValueHistory ::
  BatchGetAssetPropertyValueHistory
newBatchGetAssetPropertyValueHistory =
  BatchGetAssetPropertyValueHistory'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      entries = Prelude.mempty
    }

-- | The token to be used for the next set of paginated results.
batchGetAssetPropertyValueHistory_nextToken :: Lens.Lens' BatchGetAssetPropertyValueHistory (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueHistory_nextToken = Lens.lens (\BatchGetAssetPropertyValueHistory' {nextToken} -> nextToken) (\s@BatchGetAssetPropertyValueHistory' {} a -> s {nextToken = a} :: BatchGetAssetPropertyValueHistory)

-- | The maximum number of results to return for each paginated request. A
-- result set is returned in the two cases, whichever occurs first.
--
-- -   The size of the result set is less than 1 MB.
--
-- -   The number of data points in the result set is less than the value
--     of @maxResults@. The maximum value of @maxResults@ is 4000.
batchGetAssetPropertyValueHistory_maxResults :: Lens.Lens' BatchGetAssetPropertyValueHistory (Prelude.Maybe Prelude.Natural)
batchGetAssetPropertyValueHistory_maxResults = Lens.lens (\BatchGetAssetPropertyValueHistory' {maxResults} -> maxResults) (\s@BatchGetAssetPropertyValueHistory' {} a -> s {maxResults = a} :: BatchGetAssetPropertyValueHistory)

-- | The list of asset property historical value entries for the batch get
-- request. You can specify up to 16 entries per request.
batchGetAssetPropertyValueHistory_entries :: Lens.Lens' BatchGetAssetPropertyValueHistory [BatchGetAssetPropertyValueHistoryEntry]
batchGetAssetPropertyValueHistory_entries = Lens.lens (\BatchGetAssetPropertyValueHistory' {entries} -> entries) (\s@BatchGetAssetPropertyValueHistory' {} a -> s {entries = a} :: BatchGetAssetPropertyValueHistory) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchGetAssetPropertyValueHistory
  where
  type
    AWSResponse BatchGetAssetPropertyValueHistory =
      BatchGetAssetPropertyValueHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetAssetPropertyValueHistoryResponse'
            Prelude.<$> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..?> "errorEntries" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "successEntries" Core..!@ Prelude.mempty)
              Prelude.<*> ( x Data..?> "skippedEntries"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueHistory
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueHistory' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` entries

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistory
  where
  rnf BatchGetAssetPropertyValueHistory' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf entries

instance
  Data.ToHeaders
    BatchGetAssetPropertyValueHistory
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    BatchGetAssetPropertyValueHistory
  where
  toJSON BatchGetAssetPropertyValueHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("entries" Data..= entries)
          ]
      )

instance
  Data.ToPath
    BatchGetAssetPropertyValueHistory
  where
  toPath = Prelude.const "/properties/batch/history"

instance
  Data.ToQuery
    BatchGetAssetPropertyValueHistory
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetAssetPropertyValueHistoryResponse' smart constructor.
data BatchGetAssetPropertyValueHistoryResponse = BatchGetAssetPropertyValueHistoryResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the errors (if any) associated with the batch request. Each
    -- error entry contains the @entryId@ of the entry that failed.
    errorEntries :: [BatchGetAssetPropertyValueHistoryErrorEntry],
    -- | A list of entries that were processed successfully by this batch
    -- request. Each success entry contains the @entryId@ of the entry that
    -- succeeded and the latest query result.
    successEntries :: [BatchGetAssetPropertyValueHistorySuccessEntry],
    -- | A list of entries that were not processed by this batch request. because
    -- these entries had been completely processed by previous paginated
    -- requests. Each skipped entry contains the @entryId@ of the entry that
    -- skipped.
    skippedEntries :: [BatchGetAssetPropertyValueHistorySkippedEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetAssetPropertyValueHistoryResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'batchGetAssetPropertyValueHistoryResponse_httpStatus' - The response's http status code.
--
-- 'errorEntries', 'batchGetAssetPropertyValueHistoryResponse_errorEntries' - A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
--
-- 'successEntries', 'batchGetAssetPropertyValueHistoryResponse_successEntries' - A list of entries that were processed successfully by this batch
-- request. Each success entry contains the @entryId@ of the entry that
-- succeeded and the latest query result.
--
-- 'skippedEntries', 'batchGetAssetPropertyValueHistoryResponse_skippedEntries' - A list of entries that were not processed by this batch request. because
-- these entries had been completely processed by previous paginated
-- requests. Each skipped entry contains the @entryId@ of the entry that
-- skipped.
newBatchGetAssetPropertyValueHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetAssetPropertyValueHistoryResponse
newBatchGetAssetPropertyValueHistoryResponse
  pHttpStatus_ =
    BatchGetAssetPropertyValueHistoryResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        errorEntries = Prelude.mempty,
        successEntries = Prelude.mempty,
        skippedEntries = Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no
-- additional results.
batchGetAssetPropertyValueHistoryResponse_nextToken :: Lens.Lens' BatchGetAssetPropertyValueHistoryResponse (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueHistoryResponse_nextToken = Lens.lens (\BatchGetAssetPropertyValueHistoryResponse' {nextToken} -> nextToken) (\s@BatchGetAssetPropertyValueHistoryResponse' {} a -> s {nextToken = a} :: BatchGetAssetPropertyValueHistoryResponse)

-- | The response's http status code.
batchGetAssetPropertyValueHistoryResponse_httpStatus :: Lens.Lens' BatchGetAssetPropertyValueHistoryResponse Prelude.Int
batchGetAssetPropertyValueHistoryResponse_httpStatus = Lens.lens (\BatchGetAssetPropertyValueHistoryResponse' {httpStatus} -> httpStatus) (\s@BatchGetAssetPropertyValueHistoryResponse' {} a -> s {httpStatus = a} :: BatchGetAssetPropertyValueHistoryResponse)

-- | A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
batchGetAssetPropertyValueHistoryResponse_errorEntries :: Lens.Lens' BatchGetAssetPropertyValueHistoryResponse [BatchGetAssetPropertyValueHistoryErrorEntry]
batchGetAssetPropertyValueHistoryResponse_errorEntries = Lens.lens (\BatchGetAssetPropertyValueHistoryResponse' {errorEntries} -> errorEntries) (\s@BatchGetAssetPropertyValueHistoryResponse' {} a -> s {errorEntries = a} :: BatchGetAssetPropertyValueHistoryResponse) Prelude.. Lens.coerced

-- | A list of entries that were processed successfully by this batch
-- request. Each success entry contains the @entryId@ of the entry that
-- succeeded and the latest query result.
batchGetAssetPropertyValueHistoryResponse_successEntries :: Lens.Lens' BatchGetAssetPropertyValueHistoryResponse [BatchGetAssetPropertyValueHistorySuccessEntry]
batchGetAssetPropertyValueHistoryResponse_successEntries = Lens.lens (\BatchGetAssetPropertyValueHistoryResponse' {successEntries} -> successEntries) (\s@BatchGetAssetPropertyValueHistoryResponse' {} a -> s {successEntries = a} :: BatchGetAssetPropertyValueHistoryResponse) Prelude.. Lens.coerced

-- | A list of entries that were not processed by this batch request. because
-- these entries had been completely processed by previous paginated
-- requests. Each skipped entry contains the @entryId@ of the entry that
-- skipped.
batchGetAssetPropertyValueHistoryResponse_skippedEntries :: Lens.Lens' BatchGetAssetPropertyValueHistoryResponse [BatchGetAssetPropertyValueHistorySkippedEntry]
batchGetAssetPropertyValueHistoryResponse_skippedEntries = Lens.lens (\BatchGetAssetPropertyValueHistoryResponse' {skippedEntries} -> skippedEntries) (\s@BatchGetAssetPropertyValueHistoryResponse' {} a -> s {skippedEntries = a} :: BatchGetAssetPropertyValueHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistoryResponse
  where
  rnf BatchGetAssetPropertyValueHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf successEntries
      `Prelude.seq` Prelude.rnf skippedEntries
