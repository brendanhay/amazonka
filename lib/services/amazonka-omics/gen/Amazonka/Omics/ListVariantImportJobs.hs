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
-- Module      : Amazonka.Omics.ListVariantImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of variant import jobs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListVariantImportJobs
  ( -- * Creating a Request
    ListVariantImportJobs (..),
    newListVariantImportJobs,

    -- * Request Lenses
    listVariantImportJobs_filter,
    listVariantImportJobs_ids,
    listVariantImportJobs_maxResults,
    listVariantImportJobs_nextToken,

    -- * Destructuring the Response
    ListVariantImportJobsResponse (..),
    newListVariantImportJobsResponse,

    -- * Response Lenses
    listVariantImportJobsResponse_nextToken,
    listVariantImportJobsResponse_variantImportJobs,
    listVariantImportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVariantImportJobs' smart constructor.
data ListVariantImportJobs = ListVariantImportJobs'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ListVariantImportJobsFilter,
    -- | A list of job IDs.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of import jobs to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVariantImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listVariantImportJobs_filter' - A filter to apply to the list.
--
-- 'ids', 'listVariantImportJobs_ids' - A list of job IDs.
--
-- 'maxResults', 'listVariantImportJobs_maxResults' - The maximum number of import jobs to return in one page of results.
--
-- 'nextToken', 'listVariantImportJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListVariantImportJobs ::
  ListVariantImportJobs
newListVariantImportJobs =
  ListVariantImportJobs'
    { filter' = Prelude.Nothing,
      ids = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A filter to apply to the list.
listVariantImportJobs_filter :: Lens.Lens' ListVariantImportJobs (Prelude.Maybe ListVariantImportJobsFilter)
listVariantImportJobs_filter = Lens.lens (\ListVariantImportJobs' {filter'} -> filter') (\s@ListVariantImportJobs' {} a -> s {filter' = a} :: ListVariantImportJobs)

-- | A list of job IDs.
listVariantImportJobs_ids :: Lens.Lens' ListVariantImportJobs (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listVariantImportJobs_ids = Lens.lens (\ListVariantImportJobs' {ids} -> ids) (\s@ListVariantImportJobs' {} a -> s {ids = a} :: ListVariantImportJobs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of import jobs to return in one page of results.
listVariantImportJobs_maxResults :: Lens.Lens' ListVariantImportJobs (Prelude.Maybe Prelude.Natural)
listVariantImportJobs_maxResults = Lens.lens (\ListVariantImportJobs' {maxResults} -> maxResults) (\s@ListVariantImportJobs' {} a -> s {maxResults = a} :: ListVariantImportJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listVariantImportJobs_nextToken :: Lens.Lens' ListVariantImportJobs (Prelude.Maybe Prelude.Text)
listVariantImportJobs_nextToken = Lens.lens (\ListVariantImportJobs' {nextToken} -> nextToken) (\s@ListVariantImportJobs' {} a -> s {nextToken = a} :: ListVariantImportJobs)

instance Core.AWSPager ListVariantImportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVariantImportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVariantImportJobsResponse_variantImportJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listVariantImportJobs_nextToken
              Lens..~ rs
              Lens.^? listVariantImportJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListVariantImportJobs where
  type
    AWSResponse ListVariantImportJobs =
      ListVariantImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVariantImportJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "variantImportJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVariantImportJobs where
  hashWithSalt _salt ListVariantImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVariantImportJobs where
  rnf ListVariantImportJobs' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf ids `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken

instance Data.ToHeaders ListVariantImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVariantImportJobs where
  toJSON ListVariantImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("ids" Data..=) Prelude.<$> ids
          ]
      )

instance Data.ToPath ListVariantImportJobs where
  toPath = Prelude.const "/import/variants"

instance Data.ToQuery ListVariantImportJobs where
  toQuery ListVariantImportJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListVariantImportJobsResponse' smart constructor.
data ListVariantImportJobsResponse = ListVariantImportJobsResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of jobs.
    variantImportJobs :: Prelude.Maybe [VariantImportJobItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVariantImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVariantImportJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'variantImportJobs', 'listVariantImportJobsResponse_variantImportJobs' - A list of jobs.
--
-- 'httpStatus', 'listVariantImportJobsResponse_httpStatus' - The response's http status code.
newListVariantImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVariantImportJobsResponse
newListVariantImportJobsResponse pHttpStatus_ =
  ListVariantImportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      variantImportJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that\'s included if more results are available.
listVariantImportJobsResponse_nextToken :: Lens.Lens' ListVariantImportJobsResponse (Prelude.Maybe Prelude.Text)
listVariantImportJobsResponse_nextToken = Lens.lens (\ListVariantImportJobsResponse' {nextToken} -> nextToken) (\s@ListVariantImportJobsResponse' {} a -> s {nextToken = a} :: ListVariantImportJobsResponse)

-- | A list of jobs.
listVariantImportJobsResponse_variantImportJobs :: Lens.Lens' ListVariantImportJobsResponse (Prelude.Maybe [VariantImportJobItem])
listVariantImportJobsResponse_variantImportJobs = Lens.lens (\ListVariantImportJobsResponse' {variantImportJobs} -> variantImportJobs) (\s@ListVariantImportJobsResponse' {} a -> s {variantImportJobs = a} :: ListVariantImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVariantImportJobsResponse_httpStatus :: Lens.Lens' ListVariantImportJobsResponse Prelude.Int
listVariantImportJobsResponse_httpStatus = Lens.lens (\ListVariantImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListVariantImportJobsResponse' {} a -> s {httpStatus = a} :: ListVariantImportJobsResponse)

instance Prelude.NFData ListVariantImportJobsResponse where
  rnf ListVariantImportJobsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf variantImportJobs `Prelude.seq`
        Prelude.rnf httpStatus
