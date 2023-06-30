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
-- Module      : Amazonka.Personalize.ListBatchSegmentJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch segment jobs that have been performed off of a
-- solution version that you specify.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListBatchSegmentJobs
  ( -- * Creating a Request
    ListBatchSegmentJobs (..),
    newListBatchSegmentJobs,

    -- * Request Lenses
    listBatchSegmentJobs_maxResults,
    listBatchSegmentJobs_nextToken,
    listBatchSegmentJobs_solutionVersionArn,

    -- * Destructuring the Response
    ListBatchSegmentJobsResponse (..),
    newListBatchSegmentJobsResponse,

    -- * Response Lenses
    listBatchSegmentJobsResponse_batchSegmentJobs,
    listBatchSegmentJobsResponse_nextToken,
    listBatchSegmentJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBatchSegmentJobs' smart constructor.
data ListBatchSegmentJobs = ListBatchSegmentJobs'
  { -- | The maximum number of batch segment job results to return in each page.
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution version that the batch
    -- segment jobs used to generate batch segments.
    solutionVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchSegmentJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBatchSegmentJobs_maxResults' - The maximum number of batch segment job results to return in each page.
-- The default value is 100.
--
-- 'nextToken', 'listBatchSegmentJobs_nextToken' - The token to request the next page of results.
--
-- 'solutionVersionArn', 'listBatchSegmentJobs_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version that the batch
-- segment jobs used to generate batch segments.
newListBatchSegmentJobs ::
  ListBatchSegmentJobs
newListBatchSegmentJobs =
  ListBatchSegmentJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing
    }

-- | The maximum number of batch segment job results to return in each page.
-- The default value is 100.
listBatchSegmentJobs_maxResults :: Lens.Lens' ListBatchSegmentJobs (Prelude.Maybe Prelude.Natural)
listBatchSegmentJobs_maxResults = Lens.lens (\ListBatchSegmentJobs' {maxResults} -> maxResults) (\s@ListBatchSegmentJobs' {} a -> s {maxResults = a} :: ListBatchSegmentJobs)

-- | The token to request the next page of results.
listBatchSegmentJobs_nextToken :: Lens.Lens' ListBatchSegmentJobs (Prelude.Maybe Prelude.Text)
listBatchSegmentJobs_nextToken = Lens.lens (\ListBatchSegmentJobs' {nextToken} -> nextToken) (\s@ListBatchSegmentJobs' {} a -> s {nextToken = a} :: ListBatchSegmentJobs)

-- | The Amazon Resource Name (ARN) of the solution version that the batch
-- segment jobs used to generate batch segments.
listBatchSegmentJobs_solutionVersionArn :: Lens.Lens' ListBatchSegmentJobs (Prelude.Maybe Prelude.Text)
listBatchSegmentJobs_solutionVersionArn = Lens.lens (\ListBatchSegmentJobs' {solutionVersionArn} -> solutionVersionArn) (\s@ListBatchSegmentJobs' {} a -> s {solutionVersionArn = a} :: ListBatchSegmentJobs)

instance Core.AWSPager ListBatchSegmentJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBatchSegmentJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBatchSegmentJobsResponse_batchSegmentJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBatchSegmentJobs_nextToken
          Lens..~ rs
          Lens.^? listBatchSegmentJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBatchSegmentJobs where
  type
    AWSResponse ListBatchSegmentJobs =
      ListBatchSegmentJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBatchSegmentJobsResponse'
            Prelude.<$> ( x
                            Data..?> "batchSegmentJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBatchSegmentJobs where
  hashWithSalt _salt ListBatchSegmentJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData ListBatchSegmentJobs where
  rnf ListBatchSegmentJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf solutionVersionArn

instance Data.ToHeaders ListBatchSegmentJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListBatchSegmentJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBatchSegmentJobs where
  toJSON ListBatchSegmentJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("solutionVersionArn" Data..=)
              Prelude.<$> solutionVersionArn
          ]
      )

instance Data.ToPath ListBatchSegmentJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBatchSegmentJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBatchSegmentJobsResponse' smart constructor.
data ListBatchSegmentJobsResponse = ListBatchSegmentJobsResponse'
  { -- | A list containing information on each job that is returned.
    batchSegmentJobs :: Prelude.Maybe [BatchSegmentJobSummary],
    -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchSegmentJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSegmentJobs', 'listBatchSegmentJobsResponse_batchSegmentJobs' - A list containing information on each job that is returned.
--
-- 'nextToken', 'listBatchSegmentJobsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listBatchSegmentJobsResponse_httpStatus' - The response's http status code.
newListBatchSegmentJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBatchSegmentJobsResponse
newListBatchSegmentJobsResponse pHttpStatus_ =
  ListBatchSegmentJobsResponse'
    { batchSegmentJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing information on each job that is returned.
listBatchSegmentJobsResponse_batchSegmentJobs :: Lens.Lens' ListBatchSegmentJobsResponse (Prelude.Maybe [BatchSegmentJobSummary])
listBatchSegmentJobsResponse_batchSegmentJobs = Lens.lens (\ListBatchSegmentJobsResponse' {batchSegmentJobs} -> batchSegmentJobs) (\s@ListBatchSegmentJobsResponse' {} a -> s {batchSegmentJobs = a} :: ListBatchSegmentJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
listBatchSegmentJobsResponse_nextToken :: Lens.Lens' ListBatchSegmentJobsResponse (Prelude.Maybe Prelude.Text)
listBatchSegmentJobsResponse_nextToken = Lens.lens (\ListBatchSegmentJobsResponse' {nextToken} -> nextToken) (\s@ListBatchSegmentJobsResponse' {} a -> s {nextToken = a} :: ListBatchSegmentJobsResponse)

-- | The response's http status code.
listBatchSegmentJobsResponse_httpStatus :: Lens.Lens' ListBatchSegmentJobsResponse Prelude.Int
listBatchSegmentJobsResponse_httpStatus = Lens.lens (\ListBatchSegmentJobsResponse' {httpStatus} -> httpStatus) (\s@ListBatchSegmentJobsResponse' {} a -> s {httpStatus = a} :: ListBatchSegmentJobsResponse)

instance Prelude.NFData ListBatchSegmentJobsResponse where
  rnf ListBatchSegmentJobsResponse' {..} =
    Prelude.rnf batchSegmentJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
