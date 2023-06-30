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
-- Module      : Amazonka.IoTSiteWise.BatchGetAssetPropertyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current value for one or more asset properties. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/query-industrial-data.html#current-values Querying current values>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.BatchGetAssetPropertyValue
  ( -- * Creating a Request
    BatchGetAssetPropertyValue (..),
    newBatchGetAssetPropertyValue,

    -- * Request Lenses
    batchGetAssetPropertyValue_nextToken,
    batchGetAssetPropertyValue_entries,

    -- * Destructuring the Response
    BatchGetAssetPropertyValueResponse (..),
    newBatchGetAssetPropertyValueResponse,

    -- * Response Lenses
    batchGetAssetPropertyValueResponse_nextToken,
    batchGetAssetPropertyValueResponse_httpStatus,
    batchGetAssetPropertyValueResponse_errorEntries,
    batchGetAssetPropertyValueResponse_successEntries,
    batchGetAssetPropertyValueResponse_skippedEntries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetAssetPropertyValue' smart constructor.
data BatchGetAssetPropertyValue = BatchGetAssetPropertyValue'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of asset property value entries for the batch get request. You
    -- can specify up to 16 entries per request.
    entries :: [BatchGetAssetPropertyValueEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetAssetPropertyValue_nextToken' - The token to be used for the next set of paginated results.
--
-- 'entries', 'batchGetAssetPropertyValue_entries' - The list of asset property value entries for the batch get request. You
-- can specify up to 16 entries per request.
newBatchGetAssetPropertyValue ::
  BatchGetAssetPropertyValue
newBatchGetAssetPropertyValue =
  BatchGetAssetPropertyValue'
    { nextToken =
        Prelude.Nothing,
      entries = Prelude.mempty
    }

-- | The token to be used for the next set of paginated results.
batchGetAssetPropertyValue_nextToken :: Lens.Lens' BatchGetAssetPropertyValue (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValue_nextToken = Lens.lens (\BatchGetAssetPropertyValue' {nextToken} -> nextToken) (\s@BatchGetAssetPropertyValue' {} a -> s {nextToken = a} :: BatchGetAssetPropertyValue)

-- | The list of asset property value entries for the batch get request. You
-- can specify up to 16 entries per request.
batchGetAssetPropertyValue_entries :: Lens.Lens' BatchGetAssetPropertyValue [BatchGetAssetPropertyValueEntry]
batchGetAssetPropertyValue_entries = Lens.lens (\BatchGetAssetPropertyValue' {entries} -> entries) (\s@BatchGetAssetPropertyValue' {} a -> s {entries = a} :: BatchGetAssetPropertyValue) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetAssetPropertyValue where
  type
    AWSResponse BatchGetAssetPropertyValue =
      BatchGetAssetPropertyValueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetAssetPropertyValueResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "errorEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "successEntries" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "skippedEntries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchGetAssetPropertyValue where
  hashWithSalt _salt BatchGetAssetPropertyValue' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` entries

instance Prelude.NFData BatchGetAssetPropertyValue where
  rnf BatchGetAssetPropertyValue' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf entries

instance Data.ToHeaders BatchGetAssetPropertyValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetAssetPropertyValue where
  toJSON BatchGetAssetPropertyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("entries" Data..= entries)
          ]
      )

instance Data.ToPath BatchGetAssetPropertyValue where
  toPath = Prelude.const "/properties/batch/latest"

instance Data.ToQuery BatchGetAssetPropertyValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetAssetPropertyValueResponse' smart constructor.
data BatchGetAssetPropertyValueResponse = BatchGetAssetPropertyValueResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the errors (if any) associated with the batch request. Each
    -- error entry contains the @entryId@ of the entry that failed.
    errorEntries :: [BatchGetAssetPropertyValueErrorEntry],
    -- | A list of entries that were processed successfully by this batch
    -- request. Each success entry contains the @entryId@ of the entry that
    -- succeeded and the latest query result.
    successEntries :: [BatchGetAssetPropertyValueSuccessEntry],
    -- | A list of entries that were not processed by this batch request. because
    -- these entries had been completely processed by previous paginated
    -- requests. Each skipped entry contains the @entryId@ of the entry that
    -- skipped.
    skippedEntries :: [BatchGetAssetPropertyValueSkippedEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetAssetPropertyValueResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'batchGetAssetPropertyValueResponse_httpStatus' - The response's http status code.
--
-- 'errorEntries', 'batchGetAssetPropertyValueResponse_errorEntries' - A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
--
-- 'successEntries', 'batchGetAssetPropertyValueResponse_successEntries' - A list of entries that were processed successfully by this batch
-- request. Each success entry contains the @entryId@ of the entry that
-- succeeded and the latest query result.
--
-- 'skippedEntries', 'batchGetAssetPropertyValueResponse_skippedEntries' - A list of entries that were not processed by this batch request. because
-- these entries had been completely processed by previous paginated
-- requests. Each skipped entry contains the @entryId@ of the entry that
-- skipped.
newBatchGetAssetPropertyValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetAssetPropertyValueResponse
newBatchGetAssetPropertyValueResponse pHttpStatus_ =
  BatchGetAssetPropertyValueResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      errorEntries = Prelude.mempty,
      successEntries = Prelude.mempty,
      skippedEntries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
batchGetAssetPropertyValueResponse_nextToken :: Lens.Lens' BatchGetAssetPropertyValueResponse (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueResponse_nextToken = Lens.lens (\BatchGetAssetPropertyValueResponse' {nextToken} -> nextToken) (\s@BatchGetAssetPropertyValueResponse' {} a -> s {nextToken = a} :: BatchGetAssetPropertyValueResponse)

-- | The response's http status code.
batchGetAssetPropertyValueResponse_httpStatus :: Lens.Lens' BatchGetAssetPropertyValueResponse Prelude.Int
batchGetAssetPropertyValueResponse_httpStatus = Lens.lens (\BatchGetAssetPropertyValueResponse' {httpStatus} -> httpStatus) (\s@BatchGetAssetPropertyValueResponse' {} a -> s {httpStatus = a} :: BatchGetAssetPropertyValueResponse)

-- | A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
batchGetAssetPropertyValueResponse_errorEntries :: Lens.Lens' BatchGetAssetPropertyValueResponse [BatchGetAssetPropertyValueErrorEntry]
batchGetAssetPropertyValueResponse_errorEntries = Lens.lens (\BatchGetAssetPropertyValueResponse' {errorEntries} -> errorEntries) (\s@BatchGetAssetPropertyValueResponse' {} a -> s {errorEntries = a} :: BatchGetAssetPropertyValueResponse) Prelude.. Lens.coerced

-- | A list of entries that were processed successfully by this batch
-- request. Each success entry contains the @entryId@ of the entry that
-- succeeded and the latest query result.
batchGetAssetPropertyValueResponse_successEntries :: Lens.Lens' BatchGetAssetPropertyValueResponse [BatchGetAssetPropertyValueSuccessEntry]
batchGetAssetPropertyValueResponse_successEntries = Lens.lens (\BatchGetAssetPropertyValueResponse' {successEntries} -> successEntries) (\s@BatchGetAssetPropertyValueResponse' {} a -> s {successEntries = a} :: BatchGetAssetPropertyValueResponse) Prelude.. Lens.coerced

-- | A list of entries that were not processed by this batch request. because
-- these entries had been completely processed by previous paginated
-- requests. Each skipped entry contains the @entryId@ of the entry that
-- skipped.
batchGetAssetPropertyValueResponse_skippedEntries :: Lens.Lens' BatchGetAssetPropertyValueResponse [BatchGetAssetPropertyValueSkippedEntry]
batchGetAssetPropertyValueResponse_skippedEntries = Lens.lens (\BatchGetAssetPropertyValueResponse' {skippedEntries} -> skippedEntries) (\s@BatchGetAssetPropertyValueResponse' {} a -> s {skippedEntries = a} :: BatchGetAssetPropertyValueResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetAssetPropertyValueResponse
  where
  rnf BatchGetAssetPropertyValueResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf successEntries
      `Prelude.seq` Prelude.rnf skippedEntries
