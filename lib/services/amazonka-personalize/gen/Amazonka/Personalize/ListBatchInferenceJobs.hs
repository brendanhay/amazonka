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
-- Module      : Amazonka.Personalize.ListBatchInferenceJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch inference jobs that have been performed off of
-- a solution version.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListBatchInferenceJobs
  ( -- * Creating a Request
    ListBatchInferenceJobs (..),
    newListBatchInferenceJobs,

    -- * Request Lenses
    listBatchInferenceJobs_maxResults,
    listBatchInferenceJobs_nextToken,
    listBatchInferenceJobs_solutionVersionArn,

    -- * Destructuring the Response
    ListBatchInferenceJobsResponse (..),
    newListBatchInferenceJobsResponse,

    -- * Response Lenses
    listBatchInferenceJobsResponse_batchInferenceJobs,
    listBatchInferenceJobsResponse_nextToken,
    listBatchInferenceJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBatchInferenceJobs' smart constructor.
data ListBatchInferenceJobs = ListBatchInferenceJobs'
  { -- | The maximum number of batch inference job results to return in each
    -- page. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution version from which the
    -- batch inference jobs were created.
    solutionVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchInferenceJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBatchInferenceJobs_maxResults' - The maximum number of batch inference job results to return in each
-- page. The default value is 100.
--
-- 'nextToken', 'listBatchInferenceJobs_nextToken' - The token to request the next page of results.
--
-- 'solutionVersionArn', 'listBatchInferenceJobs_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version from which the
-- batch inference jobs were created.
newListBatchInferenceJobs ::
  ListBatchInferenceJobs
newListBatchInferenceJobs =
  ListBatchInferenceJobs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing
    }

-- | The maximum number of batch inference job results to return in each
-- page. The default value is 100.
listBatchInferenceJobs_maxResults :: Lens.Lens' ListBatchInferenceJobs (Prelude.Maybe Prelude.Natural)
listBatchInferenceJobs_maxResults = Lens.lens (\ListBatchInferenceJobs' {maxResults} -> maxResults) (\s@ListBatchInferenceJobs' {} a -> s {maxResults = a} :: ListBatchInferenceJobs)

-- | The token to request the next page of results.
listBatchInferenceJobs_nextToken :: Lens.Lens' ListBatchInferenceJobs (Prelude.Maybe Prelude.Text)
listBatchInferenceJobs_nextToken = Lens.lens (\ListBatchInferenceJobs' {nextToken} -> nextToken) (\s@ListBatchInferenceJobs' {} a -> s {nextToken = a} :: ListBatchInferenceJobs)

-- | The Amazon Resource Name (ARN) of the solution version from which the
-- batch inference jobs were created.
listBatchInferenceJobs_solutionVersionArn :: Lens.Lens' ListBatchInferenceJobs (Prelude.Maybe Prelude.Text)
listBatchInferenceJobs_solutionVersionArn = Lens.lens (\ListBatchInferenceJobs' {solutionVersionArn} -> solutionVersionArn) (\s@ListBatchInferenceJobs' {} a -> s {solutionVersionArn = a} :: ListBatchInferenceJobs)

instance Core.AWSPager ListBatchInferenceJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBatchInferenceJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBatchInferenceJobsResponse_batchInferenceJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBatchInferenceJobs_nextToken
          Lens..~ rs
          Lens.^? listBatchInferenceJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBatchInferenceJobs where
  type
    AWSResponse ListBatchInferenceJobs =
      ListBatchInferenceJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBatchInferenceJobsResponse'
            Prelude.<$> ( x
                            Data..?> "batchInferenceJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBatchInferenceJobs where
  hashWithSalt _salt ListBatchInferenceJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData ListBatchInferenceJobs where
  rnf ListBatchInferenceJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf solutionVersionArn

instance Data.ToHeaders ListBatchInferenceJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListBatchInferenceJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBatchInferenceJobs where
  toJSON ListBatchInferenceJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("solutionVersionArn" Data..=)
              Prelude.<$> solutionVersionArn
          ]
      )

instance Data.ToPath ListBatchInferenceJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBatchInferenceJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBatchInferenceJobsResponse' smart constructor.
data ListBatchInferenceJobsResponse = ListBatchInferenceJobsResponse'
  { -- | A list containing information on each job that is returned.
    batchInferenceJobs :: Prelude.Maybe [BatchInferenceJobSummary],
    -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchInferenceJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchInferenceJobs', 'listBatchInferenceJobsResponse_batchInferenceJobs' - A list containing information on each job that is returned.
--
-- 'nextToken', 'listBatchInferenceJobsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listBatchInferenceJobsResponse_httpStatus' - The response's http status code.
newListBatchInferenceJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBatchInferenceJobsResponse
newListBatchInferenceJobsResponse pHttpStatus_ =
  ListBatchInferenceJobsResponse'
    { batchInferenceJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing information on each job that is returned.
listBatchInferenceJobsResponse_batchInferenceJobs :: Lens.Lens' ListBatchInferenceJobsResponse (Prelude.Maybe [BatchInferenceJobSummary])
listBatchInferenceJobsResponse_batchInferenceJobs = Lens.lens (\ListBatchInferenceJobsResponse' {batchInferenceJobs} -> batchInferenceJobs) (\s@ListBatchInferenceJobsResponse' {} a -> s {batchInferenceJobs = a} :: ListBatchInferenceJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
listBatchInferenceJobsResponse_nextToken :: Lens.Lens' ListBatchInferenceJobsResponse (Prelude.Maybe Prelude.Text)
listBatchInferenceJobsResponse_nextToken = Lens.lens (\ListBatchInferenceJobsResponse' {nextToken} -> nextToken) (\s@ListBatchInferenceJobsResponse' {} a -> s {nextToken = a} :: ListBatchInferenceJobsResponse)

-- | The response's http status code.
listBatchInferenceJobsResponse_httpStatus :: Lens.Lens' ListBatchInferenceJobsResponse Prelude.Int
listBatchInferenceJobsResponse_httpStatus = Lens.lens (\ListBatchInferenceJobsResponse' {httpStatus} -> httpStatus) (\s@ListBatchInferenceJobsResponse' {} a -> s {httpStatus = a} :: ListBatchInferenceJobsResponse)

instance
  Prelude.NFData
    ListBatchInferenceJobsResponse
  where
  rnf ListBatchInferenceJobsResponse' {..} =
    Prelude.rnf batchInferenceJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
