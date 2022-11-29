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
-- Module      : Amazonka.Comprehend.ListDocumentClassificationJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the documentation classification jobs that you have
-- submitted.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListDocumentClassificationJobs
  ( -- * Creating a Request
    ListDocumentClassificationJobs (..),
    newListDocumentClassificationJobs,

    -- * Request Lenses
    listDocumentClassificationJobs_nextToken,
    listDocumentClassificationJobs_filter,
    listDocumentClassificationJobs_maxResults,

    -- * Destructuring the Response
    ListDocumentClassificationJobsResponse (..),
    newListDocumentClassificationJobsResponse,

    -- * Response Lenses
    listDocumentClassificationJobsResponse_nextToken,
    listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList,
    listDocumentClassificationJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDocumentClassificationJobs' smart constructor.
data ListDocumentClassificationJobs = ListDocumentClassificationJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their names,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe DocumentClassificationJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentClassificationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentClassificationJobs_nextToken' - Identifies the next page of results to return.
--
-- 'filter'', 'listDocumentClassificationJobs_filter' - Filters the jobs that are returned. You can filter jobs on their names,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listDocumentClassificationJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListDocumentClassificationJobs ::
  ListDocumentClassificationJobs
newListDocumentClassificationJobs =
  ListDocumentClassificationJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listDocumentClassificationJobs_nextToken :: Lens.Lens' ListDocumentClassificationJobs (Prelude.Maybe Prelude.Text)
listDocumentClassificationJobs_nextToken = Lens.lens (\ListDocumentClassificationJobs' {nextToken} -> nextToken) (\s@ListDocumentClassificationJobs' {} a -> s {nextToken = a} :: ListDocumentClassificationJobs)

-- | Filters the jobs that are returned. You can filter jobs on their names,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDocumentClassificationJobs_filter :: Lens.Lens' ListDocumentClassificationJobs (Prelude.Maybe DocumentClassificationJobFilter)
listDocumentClassificationJobs_filter = Lens.lens (\ListDocumentClassificationJobs' {filter'} -> filter') (\s@ListDocumentClassificationJobs' {} a -> s {filter' = a} :: ListDocumentClassificationJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDocumentClassificationJobs_maxResults :: Lens.Lens' ListDocumentClassificationJobs (Prelude.Maybe Prelude.Natural)
listDocumentClassificationJobs_maxResults = Lens.lens (\ListDocumentClassificationJobs' {maxResults} -> maxResults) (\s@ListDocumentClassificationJobs' {} a -> s {maxResults = a} :: ListDocumentClassificationJobs)

instance Core.AWSPager ListDocumentClassificationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentClassificationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDocumentClassificationJobs_nextToken
          Lens..~ rs
          Lens.^? listDocumentClassificationJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDocumentClassificationJobs
  where
  type
    AWSResponse ListDocumentClassificationJobs =
      ListDocumentClassificationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassificationJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DocumentClassificationJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDocumentClassificationJobs
  where
  hashWithSalt
    _salt
    ListDocumentClassificationJobs' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListDocumentClassificationJobs
  where
  rnf ListDocumentClassificationJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListDocumentClassificationJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListDocumentClassificationJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDocumentClassificationJobs where
  toJSON ListDocumentClassificationJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDocumentClassificationJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDocumentClassificationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentClassificationJobsResponse' smart constructor.
data ListDocumentClassificationJobsResponse = ListDocumentClassificationJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job returned.
    documentClassificationJobPropertiesList :: Prelude.Maybe [DocumentClassificationJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentClassificationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentClassificationJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'documentClassificationJobPropertiesList', 'listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList' - A list containing the properties of each job returned.
--
-- 'httpStatus', 'listDocumentClassificationJobsResponse_httpStatus' - The response's http status code.
newListDocumentClassificationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDocumentClassificationJobsResponse
newListDocumentClassificationJobsResponse
  pHttpStatus_ =
    ListDocumentClassificationJobsResponse'
      { nextToken =
          Prelude.Nothing,
        documentClassificationJobPropertiesList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifies the next page of results to return.
listDocumentClassificationJobsResponse_nextToken :: Lens.Lens' ListDocumentClassificationJobsResponse (Prelude.Maybe Prelude.Text)
listDocumentClassificationJobsResponse_nextToken = Lens.lens (\ListDocumentClassificationJobsResponse' {nextToken} -> nextToken) (\s@ListDocumentClassificationJobsResponse' {} a -> s {nextToken = a} :: ListDocumentClassificationJobsResponse)

-- | A list containing the properties of each job returned.
listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList :: Lens.Lens' ListDocumentClassificationJobsResponse (Prelude.Maybe [DocumentClassificationJobProperties])
listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList = Lens.lens (\ListDocumentClassificationJobsResponse' {documentClassificationJobPropertiesList} -> documentClassificationJobPropertiesList) (\s@ListDocumentClassificationJobsResponse' {} a -> s {documentClassificationJobPropertiesList = a} :: ListDocumentClassificationJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDocumentClassificationJobsResponse_httpStatus :: Lens.Lens' ListDocumentClassificationJobsResponse Prelude.Int
listDocumentClassificationJobsResponse_httpStatus = Lens.lens (\ListDocumentClassificationJobsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentClassificationJobsResponse' {} a -> s {httpStatus = a} :: ListDocumentClassificationJobsResponse)

instance
  Prelude.NFData
    ListDocumentClassificationJobsResponse
  where
  rnf ListDocumentClassificationJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf documentClassificationJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
