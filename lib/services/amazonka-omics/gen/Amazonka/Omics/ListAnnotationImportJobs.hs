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
-- Module      : Amazonka.Omics.ListAnnotationImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of annotation import jobs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListAnnotationImportJobs
  ( -- * Creating a Request
    ListAnnotationImportJobs (..),
    newListAnnotationImportJobs,

    -- * Request Lenses
    listAnnotationImportJobs_filter,
    listAnnotationImportJobs_ids,
    listAnnotationImportJobs_maxResults,
    listAnnotationImportJobs_nextToken,

    -- * Destructuring the Response
    ListAnnotationImportJobsResponse (..),
    newListAnnotationImportJobsResponse,

    -- * Response Lenses
    listAnnotationImportJobsResponse_annotationImportJobs,
    listAnnotationImportJobsResponse_nextToken,
    listAnnotationImportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnnotationImportJobs' smart constructor.
data ListAnnotationImportJobs = ListAnnotationImportJobs'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ListAnnotationImportJobsFilter,
    -- | IDs of annotation import jobs to retrieve.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of jobs to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnnotationImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listAnnotationImportJobs_filter' - A filter to apply to the list.
--
-- 'ids', 'listAnnotationImportJobs_ids' - IDs of annotation import jobs to retrieve.
--
-- 'maxResults', 'listAnnotationImportJobs_maxResults' - The maximum number of jobs to return in one page of results.
--
-- 'nextToken', 'listAnnotationImportJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListAnnotationImportJobs ::
  ListAnnotationImportJobs
newListAnnotationImportJobs =
  ListAnnotationImportJobs'
    { filter' =
        Prelude.Nothing,
      ids = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A filter to apply to the list.
listAnnotationImportJobs_filter :: Lens.Lens' ListAnnotationImportJobs (Prelude.Maybe ListAnnotationImportJobsFilter)
listAnnotationImportJobs_filter = Lens.lens (\ListAnnotationImportJobs' {filter'} -> filter') (\s@ListAnnotationImportJobs' {} a -> s {filter' = a} :: ListAnnotationImportJobs)

-- | IDs of annotation import jobs to retrieve.
listAnnotationImportJobs_ids :: Lens.Lens' ListAnnotationImportJobs (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listAnnotationImportJobs_ids = Lens.lens (\ListAnnotationImportJobs' {ids} -> ids) (\s@ListAnnotationImportJobs' {} a -> s {ids = a} :: ListAnnotationImportJobs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of jobs to return in one page of results.
listAnnotationImportJobs_maxResults :: Lens.Lens' ListAnnotationImportJobs (Prelude.Maybe Prelude.Natural)
listAnnotationImportJobs_maxResults = Lens.lens (\ListAnnotationImportJobs' {maxResults} -> maxResults) (\s@ListAnnotationImportJobs' {} a -> s {maxResults = a} :: ListAnnotationImportJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listAnnotationImportJobs_nextToken :: Lens.Lens' ListAnnotationImportJobs (Prelude.Maybe Prelude.Text)
listAnnotationImportJobs_nextToken = Lens.lens (\ListAnnotationImportJobs' {nextToken} -> nextToken) (\s@ListAnnotationImportJobs' {} a -> s {nextToken = a} :: ListAnnotationImportJobs)

instance Core.AWSPager ListAnnotationImportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAnnotationImportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAnnotationImportJobsResponse_annotationImportJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAnnotationImportJobs_nextToken
          Lens..~ rs
          Lens.^? listAnnotationImportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAnnotationImportJobs where
  type
    AWSResponse ListAnnotationImportJobs =
      ListAnnotationImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnnotationImportJobsResponse'
            Prelude.<$> ( x
                            Data..?> "annotationImportJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnnotationImportJobs where
  hashWithSalt _salt ListAnnotationImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAnnotationImportJobs where
  rnf ListAnnotationImportJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf ids
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAnnotationImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnnotationImportJobs where
  toJSON ListAnnotationImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("ids" Data..=) Prelude.<$> ids
          ]
      )

instance Data.ToPath ListAnnotationImportJobs where
  toPath = Prelude.const "/import/annotations"

instance Data.ToQuery ListAnnotationImportJobs where
  toQuery ListAnnotationImportJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAnnotationImportJobsResponse' smart constructor.
data ListAnnotationImportJobsResponse = ListAnnotationImportJobsResponse'
  { -- | A list of jobs.
    annotationImportJobs :: Prelude.Maybe [AnnotationImportJobItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnnotationImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationImportJobs', 'listAnnotationImportJobsResponse_annotationImportJobs' - A list of jobs.
--
-- 'nextToken', 'listAnnotationImportJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listAnnotationImportJobsResponse_httpStatus' - The response's http status code.
newListAnnotationImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnnotationImportJobsResponse
newListAnnotationImportJobsResponse pHttpStatus_ =
  ListAnnotationImportJobsResponse'
    { annotationImportJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of jobs.
listAnnotationImportJobsResponse_annotationImportJobs :: Lens.Lens' ListAnnotationImportJobsResponse (Prelude.Maybe [AnnotationImportJobItem])
listAnnotationImportJobsResponse_annotationImportJobs = Lens.lens (\ListAnnotationImportJobsResponse' {annotationImportJobs} -> annotationImportJobs) (\s@ListAnnotationImportJobsResponse' {} a -> s {annotationImportJobs = a} :: ListAnnotationImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listAnnotationImportJobsResponse_nextToken :: Lens.Lens' ListAnnotationImportJobsResponse (Prelude.Maybe Prelude.Text)
listAnnotationImportJobsResponse_nextToken = Lens.lens (\ListAnnotationImportJobsResponse' {nextToken} -> nextToken) (\s@ListAnnotationImportJobsResponse' {} a -> s {nextToken = a} :: ListAnnotationImportJobsResponse)

-- | The response's http status code.
listAnnotationImportJobsResponse_httpStatus :: Lens.Lens' ListAnnotationImportJobsResponse Prelude.Int
listAnnotationImportJobsResponse_httpStatus = Lens.lens (\ListAnnotationImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListAnnotationImportJobsResponse' {} a -> s {httpStatus = a} :: ListAnnotationImportJobsResponse)

instance
  Prelude.NFData
    ListAnnotationImportJobsResponse
  where
  rnf ListAnnotationImportJobsResponse' {..} =
    Prelude.rnf annotationImportJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
