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
-- Module      : Amazonka.ElasticTranscoder.ListJobsByStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByStatus operation gets a list of jobs that have a specified
-- status. The response body contains one element for each job that
-- satisfies the search criteria.
--
-- This operation returns paginated results.
module Amazonka.ElasticTranscoder.ListJobsByStatus
  ( -- * Creating a Request
    ListJobsByStatus (..),
    newListJobsByStatus,

    -- * Request Lenses
    listJobsByStatus_ascending,
    listJobsByStatus_pageToken,
    listJobsByStatus_status,

    -- * Destructuring the Response
    ListJobsByStatusResponse (..),
    newListJobsByStatusResponse,

    -- * Response Lenses
    listJobsByStatusResponse_jobs,
    listJobsByStatusResponse_nextPageToken,
    listJobsByStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @ListJobsByStatusRequest@ structure.
--
-- /See:/ 'newListJobsByStatus' smart constructor.
data ListJobsByStatus = ListJobsByStatus'
  { -- | To list jobs in chronological order by the date and time that they were
    -- submitted, enter @true@. To list jobs in reverse chronological order,
    -- enter @false@.
    ascending :: Prelude.Maybe Prelude.Text,
    -- | When Elastic Transcoder returns more than one page of results, use
    -- @pageToken@ in subsequent @GET@ requests to get each successive page of
    -- results.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | To get information about all of the jobs associated with the current AWS
    -- account that have a given status, specify the following status:
    -- @Submitted@, @Progressing@, @Complete@, @Canceled@, or @Error@.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsByStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ascending', 'listJobsByStatus_ascending' - To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
--
-- 'pageToken', 'listJobsByStatus_pageToken' - When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
--
-- 'status', 'listJobsByStatus_status' - To get information about all of the jobs associated with the current AWS
-- account that have a given status, specify the following status:
-- @Submitted@, @Progressing@, @Complete@, @Canceled@, or @Error@.
newListJobsByStatus ::
  -- | 'status'
  Prelude.Text ->
  ListJobsByStatus
newListJobsByStatus pStatus_ =
  ListJobsByStatus'
    { ascending = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      status = pStatus_
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
listJobsByStatus_ascending :: Lens.Lens' ListJobsByStatus (Prelude.Maybe Prelude.Text)
listJobsByStatus_ascending = Lens.lens (\ListJobsByStatus' {ascending} -> ascending) (\s@ListJobsByStatus' {} a -> s {ascending = a} :: ListJobsByStatus)

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
listJobsByStatus_pageToken :: Lens.Lens' ListJobsByStatus (Prelude.Maybe Prelude.Text)
listJobsByStatus_pageToken = Lens.lens (\ListJobsByStatus' {pageToken} -> pageToken) (\s@ListJobsByStatus' {} a -> s {pageToken = a} :: ListJobsByStatus)

-- | To get information about all of the jobs associated with the current AWS
-- account that have a given status, specify the following status:
-- @Submitted@, @Progressing@, @Complete@, @Canceled@, or @Error@.
listJobsByStatus_status :: Lens.Lens' ListJobsByStatus Prelude.Text
listJobsByStatus_status = Lens.lens (\ListJobsByStatus' {status} -> status) (\s@ListJobsByStatus' {} a -> s {status = a} :: ListJobsByStatus)

instance Core.AWSPager ListJobsByStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsByStatusResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsByStatusResponse_jobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listJobsByStatus_pageToken
          Lens..~ rs
          Lens.^? listJobsByStatusResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListJobsByStatus where
  type
    AWSResponse ListJobsByStatus =
      ListJobsByStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsByStatusResponse'
            Prelude.<$> (x Data..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobsByStatus where
  hashWithSalt _salt ListJobsByStatus' {..} =
    _salt
      `Prelude.hashWithSalt` ascending
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListJobsByStatus where
  rnf ListJobsByStatus' {..} =
    Prelude.rnf ascending
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListJobsByStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListJobsByStatus where
  toPath ListJobsByStatus' {..} =
    Prelude.mconcat
      ["/2012-09-25/jobsByStatus/", Data.toBS status]

instance Data.ToQuery ListJobsByStatus where
  toQuery ListJobsByStatus' {..} =
    Prelude.mconcat
      [ "Ascending" Data.=: ascending,
        "PageToken" Data.=: pageToken
      ]

-- | The @ListJobsByStatusResponse@ structure.
--
-- /See:/ 'newListJobsByStatusResponse' smart constructor.
data ListJobsByStatusResponse = ListJobsByStatusResponse'
  { -- | An array of @Job@ objects that have the specified status.
    jobs :: Prelude.Maybe [Job],
    -- | A value that you use to access the second and subsequent pages of
    -- results, if any. When the jobs in the specified pipeline fit on one page
    -- or when you\'ve reached the last page of results, the value of
    -- @NextPageToken@ is @null@.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsByStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'listJobsByStatusResponse_jobs' - An array of @Job@ objects that have the specified status.
--
-- 'nextPageToken', 'listJobsByStatusResponse_nextPageToken' - A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
--
-- 'httpStatus', 'listJobsByStatusResponse_httpStatus' - The response's http status code.
newListJobsByStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsByStatusResponse
newListJobsByStatusResponse pHttpStatus_ =
  ListJobsByStatusResponse'
    { jobs = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Job@ objects that have the specified status.
listJobsByStatusResponse_jobs :: Lens.Lens' ListJobsByStatusResponse (Prelude.Maybe [Job])
listJobsByStatusResponse_jobs = Lens.lens (\ListJobsByStatusResponse' {jobs} -> jobs) (\s@ListJobsByStatusResponse' {} a -> s {jobs = a} :: ListJobsByStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
listJobsByStatusResponse_nextPageToken :: Lens.Lens' ListJobsByStatusResponse (Prelude.Maybe Prelude.Text)
listJobsByStatusResponse_nextPageToken = Lens.lens (\ListJobsByStatusResponse' {nextPageToken} -> nextPageToken) (\s@ListJobsByStatusResponse' {} a -> s {nextPageToken = a} :: ListJobsByStatusResponse)

-- | The response's http status code.
listJobsByStatusResponse_httpStatus :: Lens.Lens' ListJobsByStatusResponse Prelude.Int
listJobsByStatusResponse_httpStatus = Lens.lens (\ListJobsByStatusResponse' {httpStatus} -> httpStatus) (\s@ListJobsByStatusResponse' {} a -> s {httpStatus = a} :: ListJobsByStatusResponse)

instance Prelude.NFData ListJobsByStatusResponse where
  rnf ListJobsByStatusResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
