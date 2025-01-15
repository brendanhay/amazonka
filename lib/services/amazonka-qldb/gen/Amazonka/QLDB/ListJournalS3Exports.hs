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
-- Module      : Amazonka.QLDB.ListJournalS3Exports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of journal export job descriptions for all ledgers that
-- are associated with the current Amazon Web Services account and Region.
--
-- This action returns a maximum of @MaxResults@ items, and is paginated so
-- that you can retrieve all the items by calling @ListJournalS3Exports@
-- multiple times.
--
-- This action does not return any expired export jobs. For more
-- information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/export-journal.request.html#export-journal.request.expiration Export job expiration>
-- in the /Amazon QLDB Developer Guide/.
module Amazonka.QLDB.ListJournalS3Exports
  ( -- * Creating a Request
    ListJournalS3Exports (..),
    newListJournalS3Exports,

    -- * Request Lenses
    listJournalS3Exports_maxResults,
    listJournalS3Exports_nextToken,

    -- * Destructuring the Response
    ListJournalS3ExportsResponse (..),
    newListJournalS3ExportsResponse,

    -- * Response Lenses
    listJournalS3ExportsResponse_journalS3Exports,
    listJournalS3ExportsResponse_nextToken,
    listJournalS3ExportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJournalS3Exports' smart constructor.
data ListJournalS3Exports = ListJournalS3Exports'
  { -- | The maximum number of results to return in a single
    -- @ListJournalS3Exports@ request. (The actual number of results returned
    -- might be fewer.)
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token, indicating that you want to retrieve the next page
    -- of results. If you received a value for @NextToken@ in the response from
    -- a previous @ListJournalS3Exports@ call, then you should use that value
    -- as input here.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJournalS3Exports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listJournalS3Exports_maxResults' - The maximum number of results to return in a single
-- @ListJournalS3Exports@ request. (The actual number of results returned
-- might be fewer.)
--
-- 'nextToken', 'listJournalS3Exports_nextToken' - A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalS3Exports@ call, then you should use that value
-- as input here.
newListJournalS3Exports ::
  ListJournalS3Exports
newListJournalS3Exports =
  ListJournalS3Exports'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single
-- @ListJournalS3Exports@ request. (The actual number of results returned
-- might be fewer.)
listJournalS3Exports_maxResults :: Lens.Lens' ListJournalS3Exports (Prelude.Maybe Prelude.Natural)
listJournalS3Exports_maxResults = Lens.lens (\ListJournalS3Exports' {maxResults} -> maxResults) (\s@ListJournalS3Exports' {} a -> s {maxResults = a} :: ListJournalS3Exports)

-- | A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalS3Exports@ call, then you should use that value
-- as input here.
listJournalS3Exports_nextToken :: Lens.Lens' ListJournalS3Exports (Prelude.Maybe Prelude.Text)
listJournalS3Exports_nextToken = Lens.lens (\ListJournalS3Exports' {nextToken} -> nextToken) (\s@ListJournalS3Exports' {} a -> s {nextToken = a} :: ListJournalS3Exports)

instance Core.AWSRequest ListJournalS3Exports where
  type
    AWSResponse ListJournalS3Exports =
      ListJournalS3ExportsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJournalS3ExportsResponse'
            Prelude.<$> ( x
                            Data..?> "JournalS3Exports"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJournalS3Exports where
  hashWithSalt _salt ListJournalS3Exports' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListJournalS3Exports where
  rnf ListJournalS3Exports' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListJournalS3Exports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListJournalS3Exports where
  toPath = Prelude.const "/journal-s3-exports"

instance Data.ToQuery ListJournalS3Exports where
  toQuery ListJournalS3Exports' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "next_token" Data.=: nextToken
      ]

-- | /See:/ 'newListJournalS3ExportsResponse' smart constructor.
data ListJournalS3ExportsResponse = ListJournalS3ExportsResponse'
  { -- | The array of journal export job descriptions for all ledgers that are
    -- associated with the current Amazon Web Services account and Region.
    journalS3Exports :: Prelude.Maybe [JournalS3ExportDescription],
    -- | -   If @NextToken@ is empty, then the last page of results has been
    --     processed and there are no more results to be retrieved.
    --
    -- -   If @NextToken@ is /not/ empty, then there are more results
    --     available. To retrieve the next page of results, use the value of
    --     @NextToken@ in a subsequent @ListJournalS3Exports@ call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJournalS3ExportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'journalS3Exports', 'listJournalS3ExportsResponse_journalS3Exports' - The array of journal export job descriptions for all ledgers that are
-- associated with the current Amazon Web Services account and Region.
--
-- 'nextToken', 'listJournalS3ExportsResponse_nextToken' - -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListJournalS3Exports@ call.
--
-- 'httpStatus', 'listJournalS3ExportsResponse_httpStatus' - The response's http status code.
newListJournalS3ExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJournalS3ExportsResponse
newListJournalS3ExportsResponse pHttpStatus_ =
  ListJournalS3ExportsResponse'
    { journalS3Exports =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of journal export job descriptions for all ledgers that are
-- associated with the current Amazon Web Services account and Region.
listJournalS3ExportsResponse_journalS3Exports :: Lens.Lens' ListJournalS3ExportsResponse (Prelude.Maybe [JournalS3ExportDescription])
listJournalS3ExportsResponse_journalS3Exports = Lens.lens (\ListJournalS3ExportsResponse' {journalS3Exports} -> journalS3Exports) (\s@ListJournalS3ExportsResponse' {} a -> s {journalS3Exports = a} :: ListJournalS3ExportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListJournalS3Exports@ call.
listJournalS3ExportsResponse_nextToken :: Lens.Lens' ListJournalS3ExportsResponse (Prelude.Maybe Prelude.Text)
listJournalS3ExportsResponse_nextToken = Lens.lens (\ListJournalS3ExportsResponse' {nextToken} -> nextToken) (\s@ListJournalS3ExportsResponse' {} a -> s {nextToken = a} :: ListJournalS3ExportsResponse)

-- | The response's http status code.
listJournalS3ExportsResponse_httpStatus :: Lens.Lens' ListJournalS3ExportsResponse Prelude.Int
listJournalS3ExportsResponse_httpStatus = Lens.lens (\ListJournalS3ExportsResponse' {httpStatus} -> httpStatus) (\s@ListJournalS3ExportsResponse' {} a -> s {httpStatus = a} :: ListJournalS3ExportsResponse)

instance Prelude.NFData ListJournalS3ExportsResponse where
  rnf ListJournalS3ExportsResponse' {..} =
    Prelude.rnf journalS3Exports `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
