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
-- Module      : Amazonka.Omics.ListReadSetImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of read set import jobs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReadSetImportJobs
  ( -- * Creating a Request
    ListReadSetImportJobs (..),
    newListReadSetImportJobs,

    -- * Request Lenses
    listReadSetImportJobs_filter,
    listReadSetImportJobs_maxResults,
    listReadSetImportJobs_nextToken,
    listReadSetImportJobs_sequenceStoreId,

    -- * Destructuring the Response
    ListReadSetImportJobsResponse (..),
    newListReadSetImportJobsResponse,

    -- * Response Lenses
    listReadSetImportJobsResponse_importJobs,
    listReadSetImportJobsResponse_nextToken,
    listReadSetImportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReadSetImportJobs' smart constructor.
data ListReadSetImportJobs = ListReadSetImportJobs'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ImportReadSetFilter,
    -- | The maximum number of jobs to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The jobs\' sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReadSetImportJobs_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReadSetImportJobs_maxResults' - The maximum number of jobs to return in one page of results.
--
-- 'nextToken', 'listReadSetImportJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'sequenceStoreId', 'listReadSetImportJobs_sequenceStoreId' - The jobs\' sequence store ID.
newListReadSetImportJobs ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  ListReadSetImportJobs
newListReadSetImportJobs pSequenceStoreId_ =
  ListReadSetImportJobs'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sequenceStoreId = pSequenceStoreId_
    }

-- | A filter to apply to the list.
listReadSetImportJobs_filter :: Lens.Lens' ListReadSetImportJobs (Prelude.Maybe ImportReadSetFilter)
listReadSetImportJobs_filter = Lens.lens (\ListReadSetImportJobs' {filter'} -> filter') (\s@ListReadSetImportJobs' {} a -> s {filter' = a} :: ListReadSetImportJobs)

-- | The maximum number of jobs to return in one page of results.
listReadSetImportJobs_maxResults :: Lens.Lens' ListReadSetImportJobs (Prelude.Maybe Prelude.Natural)
listReadSetImportJobs_maxResults = Lens.lens (\ListReadSetImportJobs' {maxResults} -> maxResults) (\s@ListReadSetImportJobs' {} a -> s {maxResults = a} :: ListReadSetImportJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReadSetImportJobs_nextToken :: Lens.Lens' ListReadSetImportJobs (Prelude.Maybe Prelude.Text)
listReadSetImportJobs_nextToken = Lens.lens (\ListReadSetImportJobs' {nextToken} -> nextToken) (\s@ListReadSetImportJobs' {} a -> s {nextToken = a} :: ListReadSetImportJobs)

-- | The jobs\' sequence store ID.
listReadSetImportJobs_sequenceStoreId :: Lens.Lens' ListReadSetImportJobs Prelude.Text
listReadSetImportJobs_sequenceStoreId = Lens.lens (\ListReadSetImportJobs' {sequenceStoreId} -> sequenceStoreId) (\s@ListReadSetImportJobs' {} a -> s {sequenceStoreId = a} :: ListReadSetImportJobs)

instance Core.AWSPager ListReadSetImportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReadSetImportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReadSetImportJobsResponse_importJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReadSetImportJobs_nextToken
          Lens..~ rs
          Lens.^? listReadSetImportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReadSetImportJobs where
  type
    AWSResponse ListReadSetImportJobs =
      ListReadSetImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadSetImportJobsResponse'
            Prelude.<$> (x Data..?> "importJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReadSetImportJobs where
  hashWithSalt _salt ListReadSetImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData ListReadSetImportJobs where
  rnf ListReadSetImportJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders ListReadSetImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReadSetImportJobs where
  toJSON ListReadSetImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReadSetImportJobs where
  toPath ListReadSetImportJobs' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/importjobs"
      ]

instance Data.ToQuery ListReadSetImportJobs where
  toQuery ListReadSetImportJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReadSetImportJobsResponse' smart constructor.
data ListReadSetImportJobsResponse = ListReadSetImportJobsResponse'
  { -- | A list of jobs.
    importJobs :: Prelude.Maybe [ImportReadSetJobItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importJobs', 'listReadSetImportJobsResponse_importJobs' - A list of jobs.
--
-- 'nextToken', 'listReadSetImportJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReadSetImportJobsResponse_httpStatus' - The response's http status code.
newListReadSetImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadSetImportJobsResponse
newListReadSetImportJobsResponse pHttpStatus_ =
  ListReadSetImportJobsResponse'
    { importJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of jobs.
listReadSetImportJobsResponse_importJobs :: Lens.Lens' ListReadSetImportJobsResponse (Prelude.Maybe [ImportReadSetJobItem])
listReadSetImportJobsResponse_importJobs = Lens.lens (\ListReadSetImportJobsResponse' {importJobs} -> importJobs) (\s@ListReadSetImportJobsResponse' {} a -> s {importJobs = a} :: ListReadSetImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listReadSetImportJobsResponse_nextToken :: Lens.Lens' ListReadSetImportJobsResponse (Prelude.Maybe Prelude.Text)
listReadSetImportJobsResponse_nextToken = Lens.lens (\ListReadSetImportJobsResponse' {nextToken} -> nextToken) (\s@ListReadSetImportJobsResponse' {} a -> s {nextToken = a} :: ListReadSetImportJobsResponse)

-- | The response's http status code.
listReadSetImportJobsResponse_httpStatus :: Lens.Lens' ListReadSetImportJobsResponse Prelude.Int
listReadSetImportJobsResponse_httpStatus = Lens.lens (\ListReadSetImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListReadSetImportJobsResponse' {} a -> s {httpStatus = a} :: ListReadSetImportJobsResponse)

instance Prelude.NFData ListReadSetImportJobsResponse where
  rnf ListReadSetImportJobsResponse' {..} =
    Prelude.rnf importJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
