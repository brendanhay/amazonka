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
-- Module      : Network.AWS.Comprehend.ListDocumentClassificationJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the documentation classification jobs that you have
-- submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassificationJobs
  ( -- * Creating a Request
    ListDocumentClassificationJobs (..),
    newListDocumentClassificationJobs,

    -- * Request Lenses
    listDocumentClassificationJobs_nextToken,
    listDocumentClassificationJobs_maxResults,
    listDocumentClassificationJobs_filter,

    -- * Destructuring the Response
    ListDocumentClassificationJobsResponse (..),
    newListDocumentClassificationJobsResponse,

    -- * Response Lenses
    listDocumentClassificationJobsResponse_nextToken,
    listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList,
    listDocumentClassificationJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDocumentClassificationJobs' smart constructor.
data ListDocumentClassificationJobs = ListDocumentClassificationJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their names,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Core.Maybe DocumentClassificationJobFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'listDocumentClassificationJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listDocumentClassificationJobs_filter' - Filters the jobs that are returned. You can filter jobs on their names,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListDocumentClassificationJobs ::
  ListDocumentClassificationJobs
newListDocumentClassificationJobs =
  ListDocumentClassificationJobs'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Identifies the next page of results to return.
listDocumentClassificationJobs_nextToken :: Lens.Lens' ListDocumentClassificationJobs (Core.Maybe Core.Text)
listDocumentClassificationJobs_nextToken = Lens.lens (\ListDocumentClassificationJobs' {nextToken} -> nextToken) (\s@ListDocumentClassificationJobs' {} a -> s {nextToken = a} :: ListDocumentClassificationJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDocumentClassificationJobs_maxResults :: Lens.Lens' ListDocumentClassificationJobs (Core.Maybe Core.Natural)
listDocumentClassificationJobs_maxResults = Lens.lens (\ListDocumentClassificationJobs' {maxResults} -> maxResults) (\s@ListDocumentClassificationJobs' {} a -> s {maxResults = a} :: ListDocumentClassificationJobs)

-- | Filters the jobs that are returned. You can filter jobs on their names,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDocumentClassificationJobs_filter :: Lens.Lens' ListDocumentClassificationJobs (Core.Maybe DocumentClassificationJobFilter)
listDocumentClassificationJobs_filter = Lens.lens (\ListDocumentClassificationJobs' {filter'} -> filter') (\s@ListDocumentClassificationJobs' {} a -> s {filter' = a} :: ListDocumentClassificationJobs)

instance Core.AWSPager ListDocumentClassificationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentClassificationJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDocumentClassificationJobs_nextToken
          Lens..~ rs
          Lens.^? listDocumentClassificationJobsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListDocumentClassificationJobs
  where
  type
    AWSResponse ListDocumentClassificationJobs =
      ListDocumentClassificationJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassificationJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "DocumentClassificationJobPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDocumentClassificationJobs

instance Core.NFData ListDocumentClassificationJobs

instance
  Core.ToHeaders
    ListDocumentClassificationJobs
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListDocumentClassificationJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDocumentClassificationJobs where
  toJSON ListDocumentClassificationJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListDocumentClassificationJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListDocumentClassificationJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDocumentClassificationJobsResponse' smart constructor.
data ListDocumentClassificationJobsResponse = ListDocumentClassificationJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list containing the properties of each job returned.
    documentClassificationJobPropertiesList :: Core.Maybe [DocumentClassificationJobProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListDocumentClassificationJobsResponse
newListDocumentClassificationJobsResponse
  pHttpStatus_ =
    ListDocumentClassificationJobsResponse'
      { nextToken =
          Core.Nothing,
        documentClassificationJobPropertiesList =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifies the next page of results to return.
listDocumentClassificationJobsResponse_nextToken :: Lens.Lens' ListDocumentClassificationJobsResponse (Core.Maybe Core.Text)
listDocumentClassificationJobsResponse_nextToken = Lens.lens (\ListDocumentClassificationJobsResponse' {nextToken} -> nextToken) (\s@ListDocumentClassificationJobsResponse' {} a -> s {nextToken = a} :: ListDocumentClassificationJobsResponse)

-- | A list containing the properties of each job returned.
listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList :: Lens.Lens' ListDocumentClassificationJobsResponse (Core.Maybe [DocumentClassificationJobProperties])
listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList = Lens.lens (\ListDocumentClassificationJobsResponse' {documentClassificationJobPropertiesList} -> documentClassificationJobPropertiesList) (\s@ListDocumentClassificationJobsResponse' {} a -> s {documentClassificationJobPropertiesList = a} :: ListDocumentClassificationJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDocumentClassificationJobsResponse_httpStatus :: Lens.Lens' ListDocumentClassificationJobsResponse Core.Int
listDocumentClassificationJobsResponse_httpStatus = Lens.lens (\ListDocumentClassificationJobsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentClassificationJobsResponse' {} a -> s {httpStatus = a} :: ListDocumentClassificationJobsResponse)

instance
  Core.NFData
    ListDocumentClassificationJobsResponse
