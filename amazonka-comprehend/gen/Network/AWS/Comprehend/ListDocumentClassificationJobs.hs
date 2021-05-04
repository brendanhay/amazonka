{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDocumentClassificationJobs' smart constructor.
data ListDocumentClassificationJobs = ListDocumentClassificationJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their names,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe DocumentClassificationJobFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listDocumentClassificationJobs_nextToken :: Lens.Lens' ListDocumentClassificationJobs (Prelude.Maybe Prelude.Text)
listDocumentClassificationJobs_nextToken = Lens.lens (\ListDocumentClassificationJobs' {nextToken} -> nextToken) (\s@ListDocumentClassificationJobs' {} a -> s {nextToken = a} :: ListDocumentClassificationJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDocumentClassificationJobs_maxResults :: Lens.Lens' ListDocumentClassificationJobs (Prelude.Maybe Prelude.Natural)
listDocumentClassificationJobs_maxResults = Lens.lens (\ListDocumentClassificationJobs' {maxResults} -> maxResults) (\s@ListDocumentClassificationJobs' {} a -> s {maxResults = a} :: ListDocumentClassificationJobs)

-- | Filters the jobs that are returned. You can filter jobs on their names,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDocumentClassificationJobs_filter :: Lens.Lens' ListDocumentClassificationJobs (Prelude.Maybe DocumentClassificationJobFilter)
listDocumentClassificationJobs_filter = Lens.lens (\ListDocumentClassificationJobs' {filter'} -> filter') (\s@ListDocumentClassificationJobs' {} a -> s {filter' = a} :: ListDocumentClassificationJobs)

instance
  Pager.AWSPager
    ListDocumentClassificationJobs
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDocumentClassificationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDocumentClassificationJobs_nextToken
          Lens..~ rs
          Lens.^? listDocumentClassificationJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListDocumentClassificationJobs
  where
  type
    Rs ListDocumentClassificationJobs =
      ListDocumentClassificationJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassificationJobsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x
                            Prelude..?> "DocumentClassificationJobPropertiesList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDocumentClassificationJobs

instance
  Prelude.NFData
    ListDocumentClassificationJobs

instance
  Prelude.ToHeaders
    ListDocumentClassificationJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.ListDocumentClassificationJobs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    ListDocumentClassificationJobs
  where
  toJSON ListDocumentClassificationJobs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance
  Prelude.ToPath
    ListDocumentClassificationJobs
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ListDocumentClassificationJobs
  where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList = Lens.lens (\ListDocumentClassificationJobsResponse' {documentClassificationJobPropertiesList} -> documentClassificationJobPropertiesList) (\s@ListDocumentClassificationJobsResponse' {} a -> s {documentClassificationJobPropertiesList = a} :: ListDocumentClassificationJobsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDocumentClassificationJobsResponse_httpStatus :: Lens.Lens' ListDocumentClassificationJobsResponse Prelude.Int
listDocumentClassificationJobsResponse_httpStatus = Lens.lens (\ListDocumentClassificationJobsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentClassificationJobsResponse' {} a -> s {httpStatus = a} :: ListDocumentClassificationJobsResponse)

instance
  Prelude.NFData
    ListDocumentClassificationJobsResponse
