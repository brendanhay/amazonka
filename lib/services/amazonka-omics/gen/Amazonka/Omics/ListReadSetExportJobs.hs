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
-- Module      : Amazonka.Omics.ListReadSetExportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of read set export jobs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReadSetExportJobs
  ( -- * Creating a Request
    ListReadSetExportJobs (..),
    newListReadSetExportJobs,

    -- * Request Lenses
    listReadSetExportJobs_filter,
    listReadSetExportJobs_maxResults,
    listReadSetExportJobs_nextToken,
    listReadSetExportJobs_sequenceStoreId,

    -- * Destructuring the Response
    ListReadSetExportJobsResponse (..),
    newListReadSetExportJobsResponse,

    -- * Response Lenses
    listReadSetExportJobsResponse_exportJobs,
    listReadSetExportJobsResponse_nextToken,
    listReadSetExportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReadSetExportJobs' smart constructor.
data ListReadSetExportJobs = ListReadSetExportJobs'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ExportReadSetFilter,
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
-- Create a value of 'ListReadSetExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReadSetExportJobs_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReadSetExportJobs_maxResults' - The maximum number of jobs to return in one page of results.
--
-- 'nextToken', 'listReadSetExportJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'sequenceStoreId', 'listReadSetExportJobs_sequenceStoreId' - The jobs\' sequence store ID.
newListReadSetExportJobs ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  ListReadSetExportJobs
newListReadSetExportJobs pSequenceStoreId_ =
  ListReadSetExportJobs'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sequenceStoreId = pSequenceStoreId_
    }

-- | A filter to apply to the list.
listReadSetExportJobs_filter :: Lens.Lens' ListReadSetExportJobs (Prelude.Maybe ExportReadSetFilter)
listReadSetExportJobs_filter = Lens.lens (\ListReadSetExportJobs' {filter'} -> filter') (\s@ListReadSetExportJobs' {} a -> s {filter' = a} :: ListReadSetExportJobs)

-- | The maximum number of jobs to return in one page of results.
listReadSetExportJobs_maxResults :: Lens.Lens' ListReadSetExportJobs (Prelude.Maybe Prelude.Natural)
listReadSetExportJobs_maxResults = Lens.lens (\ListReadSetExportJobs' {maxResults} -> maxResults) (\s@ListReadSetExportJobs' {} a -> s {maxResults = a} :: ListReadSetExportJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReadSetExportJobs_nextToken :: Lens.Lens' ListReadSetExportJobs (Prelude.Maybe Prelude.Text)
listReadSetExportJobs_nextToken = Lens.lens (\ListReadSetExportJobs' {nextToken} -> nextToken) (\s@ListReadSetExportJobs' {} a -> s {nextToken = a} :: ListReadSetExportJobs)

-- | The jobs\' sequence store ID.
listReadSetExportJobs_sequenceStoreId :: Lens.Lens' ListReadSetExportJobs Prelude.Text
listReadSetExportJobs_sequenceStoreId = Lens.lens (\ListReadSetExportJobs' {sequenceStoreId} -> sequenceStoreId) (\s@ListReadSetExportJobs' {} a -> s {sequenceStoreId = a} :: ListReadSetExportJobs)

instance Core.AWSPager ListReadSetExportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReadSetExportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReadSetExportJobsResponse_exportJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listReadSetExportJobs_nextToken
              Lens..~ rs
              Lens.^? listReadSetExportJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListReadSetExportJobs where
  type
    AWSResponse ListReadSetExportJobs =
      ListReadSetExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadSetExportJobsResponse'
            Prelude.<$> (x Data..?> "exportJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReadSetExportJobs where
  hashWithSalt _salt ListReadSetExportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData ListReadSetExportJobs where
  rnf ListReadSetExportJobs' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf sequenceStoreId

instance Data.ToHeaders ListReadSetExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReadSetExportJobs where
  toJSON ListReadSetExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReadSetExportJobs where
  toPath ListReadSetExportJobs' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/exportjobs"
      ]

instance Data.ToQuery ListReadSetExportJobs where
  toQuery ListReadSetExportJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReadSetExportJobsResponse' smart constructor.
data ListReadSetExportJobsResponse = ListReadSetExportJobsResponse'
  { -- | A list of jobs.
    exportJobs :: Prelude.Maybe [ExportReadSetJobDetail],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportJobs', 'listReadSetExportJobsResponse_exportJobs' - A list of jobs.
--
-- 'nextToken', 'listReadSetExportJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReadSetExportJobsResponse_httpStatus' - The response's http status code.
newListReadSetExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadSetExportJobsResponse
newListReadSetExportJobsResponse pHttpStatus_ =
  ListReadSetExportJobsResponse'
    { exportJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of jobs.
listReadSetExportJobsResponse_exportJobs :: Lens.Lens' ListReadSetExportJobsResponse (Prelude.Maybe [ExportReadSetJobDetail])
listReadSetExportJobsResponse_exportJobs = Lens.lens (\ListReadSetExportJobsResponse' {exportJobs} -> exportJobs) (\s@ListReadSetExportJobsResponse' {} a -> s {exportJobs = a} :: ListReadSetExportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listReadSetExportJobsResponse_nextToken :: Lens.Lens' ListReadSetExportJobsResponse (Prelude.Maybe Prelude.Text)
listReadSetExportJobsResponse_nextToken = Lens.lens (\ListReadSetExportJobsResponse' {nextToken} -> nextToken) (\s@ListReadSetExportJobsResponse' {} a -> s {nextToken = a} :: ListReadSetExportJobsResponse)

-- | The response's http status code.
listReadSetExportJobsResponse_httpStatus :: Lens.Lens' ListReadSetExportJobsResponse Prelude.Int
listReadSetExportJobsResponse_httpStatus = Lens.lens (\ListReadSetExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListReadSetExportJobsResponse' {} a -> s {httpStatus = a} :: ListReadSetExportJobsResponse)

instance Prelude.NFData ListReadSetExportJobsResponse where
  rnf ListReadSetExportJobsResponse' {..} =
    Prelude.rnf exportJobs `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
