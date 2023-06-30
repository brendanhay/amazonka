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
-- Module      : Amazonka.Omics.ListReferenceImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of reference import jobs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReferenceImportJobs
  ( -- * Creating a Request
    ListReferenceImportJobs (..),
    newListReferenceImportJobs,

    -- * Request Lenses
    listReferenceImportJobs_filter,
    listReferenceImportJobs_maxResults,
    listReferenceImportJobs_nextToken,
    listReferenceImportJobs_referenceStoreId,

    -- * Destructuring the Response
    ListReferenceImportJobsResponse (..),
    newListReferenceImportJobsResponse,

    -- * Response Lenses
    listReferenceImportJobsResponse_importJobs,
    listReferenceImportJobsResponse_nextToken,
    listReferenceImportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReferenceImportJobs' smart constructor.
data ListReferenceImportJobs = ListReferenceImportJobs'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ImportReferenceFilter,
    -- | The maximum number of jobs to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The job\'s reference store ID.
    referenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReferenceImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReferenceImportJobs_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReferenceImportJobs_maxResults' - The maximum number of jobs to return in one page of results.
--
-- 'nextToken', 'listReferenceImportJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'referenceStoreId', 'listReferenceImportJobs_referenceStoreId' - The job\'s reference store ID.
newListReferenceImportJobs ::
  -- | 'referenceStoreId'
  Prelude.Text ->
  ListReferenceImportJobs
newListReferenceImportJobs pReferenceStoreId_ =
  ListReferenceImportJobs'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      referenceStoreId = pReferenceStoreId_
    }

-- | A filter to apply to the list.
listReferenceImportJobs_filter :: Lens.Lens' ListReferenceImportJobs (Prelude.Maybe ImportReferenceFilter)
listReferenceImportJobs_filter = Lens.lens (\ListReferenceImportJobs' {filter'} -> filter') (\s@ListReferenceImportJobs' {} a -> s {filter' = a} :: ListReferenceImportJobs)

-- | The maximum number of jobs to return in one page of results.
listReferenceImportJobs_maxResults :: Lens.Lens' ListReferenceImportJobs (Prelude.Maybe Prelude.Natural)
listReferenceImportJobs_maxResults = Lens.lens (\ListReferenceImportJobs' {maxResults} -> maxResults) (\s@ListReferenceImportJobs' {} a -> s {maxResults = a} :: ListReferenceImportJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReferenceImportJobs_nextToken :: Lens.Lens' ListReferenceImportJobs (Prelude.Maybe Prelude.Text)
listReferenceImportJobs_nextToken = Lens.lens (\ListReferenceImportJobs' {nextToken} -> nextToken) (\s@ListReferenceImportJobs' {} a -> s {nextToken = a} :: ListReferenceImportJobs)

-- | The job\'s reference store ID.
listReferenceImportJobs_referenceStoreId :: Lens.Lens' ListReferenceImportJobs Prelude.Text
listReferenceImportJobs_referenceStoreId = Lens.lens (\ListReferenceImportJobs' {referenceStoreId} -> referenceStoreId) (\s@ListReferenceImportJobs' {} a -> s {referenceStoreId = a} :: ListReferenceImportJobs)

instance Core.AWSPager ListReferenceImportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReferenceImportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReferenceImportJobsResponse_importJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReferenceImportJobs_nextToken
          Lens..~ rs
          Lens.^? listReferenceImportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReferenceImportJobs where
  type
    AWSResponse ListReferenceImportJobs =
      ListReferenceImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReferenceImportJobsResponse'
            Prelude.<$> (x Data..?> "importJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReferenceImportJobs where
  hashWithSalt _salt ListReferenceImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` referenceStoreId

instance Prelude.NFData ListReferenceImportJobs where
  rnf ListReferenceImportJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf referenceStoreId

instance Data.ToHeaders ListReferenceImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReferenceImportJobs where
  toJSON ListReferenceImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReferenceImportJobs where
  toPath ListReferenceImportJobs' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/importjobs"
      ]

instance Data.ToQuery ListReferenceImportJobs where
  toQuery ListReferenceImportJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReferenceImportJobsResponse' smart constructor.
data ListReferenceImportJobsResponse = ListReferenceImportJobsResponse'
  { -- | A lis of jobs.
    importJobs :: Prelude.Maybe [ImportReferenceJobItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReferenceImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importJobs', 'listReferenceImportJobsResponse_importJobs' - A lis of jobs.
--
-- 'nextToken', 'listReferenceImportJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReferenceImportJobsResponse_httpStatus' - The response's http status code.
newListReferenceImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReferenceImportJobsResponse
newListReferenceImportJobsResponse pHttpStatus_ =
  ListReferenceImportJobsResponse'
    { importJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A lis of jobs.
listReferenceImportJobsResponse_importJobs :: Lens.Lens' ListReferenceImportJobsResponse (Prelude.Maybe [ImportReferenceJobItem])
listReferenceImportJobsResponse_importJobs = Lens.lens (\ListReferenceImportJobsResponse' {importJobs} -> importJobs) (\s@ListReferenceImportJobsResponse' {} a -> s {importJobs = a} :: ListReferenceImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listReferenceImportJobsResponse_nextToken :: Lens.Lens' ListReferenceImportJobsResponse (Prelude.Maybe Prelude.Text)
listReferenceImportJobsResponse_nextToken = Lens.lens (\ListReferenceImportJobsResponse' {nextToken} -> nextToken) (\s@ListReferenceImportJobsResponse' {} a -> s {nextToken = a} :: ListReferenceImportJobsResponse)

-- | The response's http status code.
listReferenceImportJobsResponse_httpStatus :: Lens.Lens' ListReferenceImportJobsResponse Prelude.Int
listReferenceImportJobsResponse_httpStatus = Lens.lens (\ListReferenceImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListReferenceImportJobsResponse' {} a -> s {httpStatus = a} :: ListReferenceImportJobsResponse)

instance
  Prelude.NFData
    ListReferenceImportJobsResponse
  where
  rnf ListReferenceImportJobsResponse' {..} =
    Prelude.rnf importJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
