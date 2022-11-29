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
-- Module      : Amazonka.IoTSiteWise.BatchGetAssetPropertyAggregates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets aggregated values (for example, average, minimum, and maximum) for
-- one or more asset properties. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/query-industrial-data.html#aggregates Querying aggregates>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.BatchGetAssetPropertyAggregates
  ( -- * Creating a Request
    BatchGetAssetPropertyAggregates (..),
    newBatchGetAssetPropertyAggregates,

    -- * Request Lenses
    batchGetAssetPropertyAggregates_nextToken,
    batchGetAssetPropertyAggregates_maxResults,
    batchGetAssetPropertyAggregates_entries,

    -- * Destructuring the Response
    BatchGetAssetPropertyAggregatesResponse (..),
    newBatchGetAssetPropertyAggregatesResponse,

    -- * Response Lenses
    batchGetAssetPropertyAggregatesResponse_nextToken,
    batchGetAssetPropertyAggregatesResponse_httpStatus,
    batchGetAssetPropertyAggregatesResponse_errorEntries,
    batchGetAssetPropertyAggregatesResponse_successEntries,
    batchGetAssetPropertyAggregatesResponse_skippedEntries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetAssetPropertyAggregates' smart constructor.
data BatchGetAssetPropertyAggregates = BatchGetAssetPropertyAggregates'
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
    -- | The list of asset property aggregate entries for the batch get request.
    -- You can specify up to 16 entries per request.
    entries :: [BatchGetAssetPropertyAggregatesEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetAssetPropertyAggregates_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'batchGetAssetPropertyAggregates_maxResults' - The maximum number of results to return for each paginated request. A
-- result set is returned in the two cases, whichever occurs first.
--
-- -   The size of the result set is less than 1 MB.
--
-- -   The number of data points in the result set is less than the value
--     of @maxResults@. The maximum value of @maxResults@ is 4000.
--
-- 'entries', 'batchGetAssetPropertyAggregates_entries' - The list of asset property aggregate entries for the batch get request.
-- You can specify up to 16 entries per request.
newBatchGetAssetPropertyAggregates ::
  BatchGetAssetPropertyAggregates
newBatchGetAssetPropertyAggregates =
  BatchGetAssetPropertyAggregates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      entries = Prelude.mempty
    }

-- | The token to be used for the next set of paginated results.
batchGetAssetPropertyAggregates_nextToken :: Lens.Lens' BatchGetAssetPropertyAggregates (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyAggregates_nextToken = Lens.lens (\BatchGetAssetPropertyAggregates' {nextToken} -> nextToken) (\s@BatchGetAssetPropertyAggregates' {} a -> s {nextToken = a} :: BatchGetAssetPropertyAggregates)

-- | The maximum number of results to return for each paginated request. A
-- result set is returned in the two cases, whichever occurs first.
--
-- -   The size of the result set is less than 1 MB.
--
-- -   The number of data points in the result set is less than the value
--     of @maxResults@. The maximum value of @maxResults@ is 4000.
batchGetAssetPropertyAggregates_maxResults :: Lens.Lens' BatchGetAssetPropertyAggregates (Prelude.Maybe Prelude.Natural)
batchGetAssetPropertyAggregates_maxResults = Lens.lens (\BatchGetAssetPropertyAggregates' {maxResults} -> maxResults) (\s@BatchGetAssetPropertyAggregates' {} a -> s {maxResults = a} :: BatchGetAssetPropertyAggregates)

-- | The list of asset property aggregate entries for the batch get request.
-- You can specify up to 16 entries per request.
batchGetAssetPropertyAggregates_entries :: Lens.Lens' BatchGetAssetPropertyAggregates [BatchGetAssetPropertyAggregatesEntry]
batchGetAssetPropertyAggregates_entries = Lens.lens (\BatchGetAssetPropertyAggregates' {entries} -> entries) (\s@BatchGetAssetPropertyAggregates' {} a -> s {entries = a} :: BatchGetAssetPropertyAggregates) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchGetAssetPropertyAggregates
  where
  type
    AWSResponse BatchGetAssetPropertyAggregates =
      BatchGetAssetPropertyAggregatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetAssetPropertyAggregatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "errorEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "successEntries" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "skippedEntries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyAggregates
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyAggregates' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` entries

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregates
  where
  rnf BatchGetAssetPropertyAggregates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf entries

instance
  Core.ToHeaders
    BatchGetAssetPropertyAggregates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetAssetPropertyAggregates where
  toJSON BatchGetAssetPropertyAggregates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("entries" Core..= entries)
          ]
      )

instance Core.ToPath BatchGetAssetPropertyAggregates where
  toPath = Prelude.const "/properties/batch/aggregates"

instance Core.ToQuery BatchGetAssetPropertyAggregates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetAssetPropertyAggregatesResponse' smart constructor.
data BatchGetAssetPropertyAggregatesResponse = BatchGetAssetPropertyAggregatesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the errors (if any) associated with the batch request. Each
    -- error entry contains the @entryId@ of the entry that failed.
    errorEntries :: [BatchGetAssetPropertyAggregatesErrorEntry],
    -- | A list of entries that were processed successfully by this batch
    -- request. Each success entry contains the @entryId@ of the entry that
    -- succeeded and the latest query result.
    successEntries :: [BatchGetAssetPropertyAggregatesSuccessEntry],
    -- | A list of entries that were not processed by this batch request. because
    -- these entries had been completely processed by previous paginated
    -- requests. Each skipped entry contains the @entryId@ of the entry that
    -- skipped.
    skippedEntries :: [BatchGetAssetPropertyAggregatesSkippedEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetAssetPropertyAggregatesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'batchGetAssetPropertyAggregatesResponse_httpStatus' - The response's http status code.
--
-- 'errorEntries', 'batchGetAssetPropertyAggregatesResponse_errorEntries' - A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
--
-- 'successEntries', 'batchGetAssetPropertyAggregatesResponse_successEntries' - A list of entries that were processed successfully by this batch
-- request. Each success entry contains the @entryId@ of the entry that
-- succeeded and the latest query result.
--
-- 'skippedEntries', 'batchGetAssetPropertyAggregatesResponse_skippedEntries' - A list of entries that were not processed by this batch request. because
-- these entries had been completely processed by previous paginated
-- requests. Each skipped entry contains the @entryId@ of the entry that
-- skipped.
newBatchGetAssetPropertyAggregatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetAssetPropertyAggregatesResponse
newBatchGetAssetPropertyAggregatesResponse
  pHttpStatus_ =
    BatchGetAssetPropertyAggregatesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        errorEntries = Prelude.mempty,
        successEntries = Prelude.mempty,
        skippedEntries = Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no
-- additional results.
batchGetAssetPropertyAggregatesResponse_nextToken :: Lens.Lens' BatchGetAssetPropertyAggregatesResponse (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyAggregatesResponse_nextToken = Lens.lens (\BatchGetAssetPropertyAggregatesResponse' {nextToken} -> nextToken) (\s@BatchGetAssetPropertyAggregatesResponse' {} a -> s {nextToken = a} :: BatchGetAssetPropertyAggregatesResponse)

-- | The response's http status code.
batchGetAssetPropertyAggregatesResponse_httpStatus :: Lens.Lens' BatchGetAssetPropertyAggregatesResponse Prelude.Int
batchGetAssetPropertyAggregatesResponse_httpStatus = Lens.lens (\BatchGetAssetPropertyAggregatesResponse' {httpStatus} -> httpStatus) (\s@BatchGetAssetPropertyAggregatesResponse' {} a -> s {httpStatus = a} :: BatchGetAssetPropertyAggregatesResponse)

-- | A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
batchGetAssetPropertyAggregatesResponse_errorEntries :: Lens.Lens' BatchGetAssetPropertyAggregatesResponse [BatchGetAssetPropertyAggregatesErrorEntry]
batchGetAssetPropertyAggregatesResponse_errorEntries = Lens.lens (\BatchGetAssetPropertyAggregatesResponse' {errorEntries} -> errorEntries) (\s@BatchGetAssetPropertyAggregatesResponse' {} a -> s {errorEntries = a} :: BatchGetAssetPropertyAggregatesResponse) Prelude.. Lens.coerced

-- | A list of entries that were processed successfully by this batch
-- request. Each success entry contains the @entryId@ of the entry that
-- succeeded and the latest query result.
batchGetAssetPropertyAggregatesResponse_successEntries :: Lens.Lens' BatchGetAssetPropertyAggregatesResponse [BatchGetAssetPropertyAggregatesSuccessEntry]
batchGetAssetPropertyAggregatesResponse_successEntries = Lens.lens (\BatchGetAssetPropertyAggregatesResponse' {successEntries} -> successEntries) (\s@BatchGetAssetPropertyAggregatesResponse' {} a -> s {successEntries = a} :: BatchGetAssetPropertyAggregatesResponse) Prelude.. Lens.coerced

-- | A list of entries that were not processed by this batch request. because
-- these entries had been completely processed by previous paginated
-- requests. Each skipped entry contains the @entryId@ of the entry that
-- skipped.
batchGetAssetPropertyAggregatesResponse_skippedEntries :: Lens.Lens' BatchGetAssetPropertyAggregatesResponse [BatchGetAssetPropertyAggregatesSkippedEntry]
batchGetAssetPropertyAggregatesResponse_skippedEntries = Lens.lens (\BatchGetAssetPropertyAggregatesResponse' {skippedEntries} -> skippedEntries) (\s@BatchGetAssetPropertyAggregatesResponse' {} a -> s {skippedEntries = a} :: BatchGetAssetPropertyAggregatesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregatesResponse
  where
  rnf BatchGetAssetPropertyAggregatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf successEntries
      `Prelude.seq` Prelude.rnf skippedEntries
