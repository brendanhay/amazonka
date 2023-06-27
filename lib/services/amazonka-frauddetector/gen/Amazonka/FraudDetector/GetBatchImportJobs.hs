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
-- Module      : Amazonka.FraudDetector.GetBatchImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all batch import jobs or a specific job of the specified ID. This
-- is a paginated API. If you provide a null @maxResults@, this action
-- retrieves a maximum of 50 records per page. If you provide a
-- @maxResults@, the value must be between 1 and 50. To get the next page
-- results, provide the pagination token from the
-- @GetBatchImportJobsResponse@ as part of your request. A null pagination
-- token fetches the records from the beginning.
module Amazonka.FraudDetector.GetBatchImportJobs
  ( -- * Creating a Request
    GetBatchImportJobs (..),
    newGetBatchImportJobs,

    -- * Request Lenses
    getBatchImportJobs_jobId,
    getBatchImportJobs_maxResults,
    getBatchImportJobs_nextToken,

    -- * Destructuring the Response
    GetBatchImportJobsResponse (..),
    newGetBatchImportJobsResponse,

    -- * Response Lenses
    getBatchImportJobsResponse_batchImports,
    getBatchImportJobsResponse_nextToken,
    getBatchImportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBatchImportJobs' smart constructor.
data GetBatchImportJobs = GetBatchImportJobs'
  { -- | The ID of the batch import job to get.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return for request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next token from the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBatchImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getBatchImportJobs_jobId' - The ID of the batch import job to get.
--
-- 'maxResults', 'getBatchImportJobs_maxResults' - The maximum number of objects to return for request.
--
-- 'nextToken', 'getBatchImportJobs_nextToken' - The next token from the previous request.
newGetBatchImportJobs ::
  GetBatchImportJobs
newGetBatchImportJobs =
  GetBatchImportJobs'
    { jobId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ID of the batch import job to get.
getBatchImportJobs_jobId :: Lens.Lens' GetBatchImportJobs (Prelude.Maybe Prelude.Text)
getBatchImportJobs_jobId = Lens.lens (\GetBatchImportJobs' {jobId} -> jobId) (\s@GetBatchImportJobs' {} a -> s {jobId = a} :: GetBatchImportJobs)

-- | The maximum number of objects to return for request.
getBatchImportJobs_maxResults :: Lens.Lens' GetBatchImportJobs (Prelude.Maybe Prelude.Natural)
getBatchImportJobs_maxResults = Lens.lens (\GetBatchImportJobs' {maxResults} -> maxResults) (\s@GetBatchImportJobs' {} a -> s {maxResults = a} :: GetBatchImportJobs)

-- | The next token from the previous request.
getBatchImportJobs_nextToken :: Lens.Lens' GetBatchImportJobs (Prelude.Maybe Prelude.Text)
getBatchImportJobs_nextToken = Lens.lens (\GetBatchImportJobs' {nextToken} -> nextToken) (\s@GetBatchImportJobs' {} a -> s {nextToken = a} :: GetBatchImportJobs)

instance Core.AWSRequest GetBatchImportJobs where
  type
    AWSResponse GetBatchImportJobs =
      GetBatchImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBatchImportJobsResponse'
            Prelude.<$> (x Data..?> "batchImports" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBatchImportJobs where
  hashWithSalt _salt GetBatchImportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetBatchImportJobs where
  rnf GetBatchImportJobs' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetBatchImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetBatchImportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBatchImportJobs where
  toJSON GetBatchImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobId" Data..=) Prelude.<$> jobId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetBatchImportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBatchImportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBatchImportJobsResponse' smart constructor.
data GetBatchImportJobsResponse = GetBatchImportJobsResponse'
  { -- | An array containing the details of each batch import job.
    batchImports :: Prelude.Maybe [BatchImport],
    -- | The next token for the subsequent resquest.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBatchImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchImports', 'getBatchImportJobsResponse_batchImports' - An array containing the details of each batch import job.
--
-- 'nextToken', 'getBatchImportJobsResponse_nextToken' - The next token for the subsequent resquest.
--
-- 'httpStatus', 'getBatchImportJobsResponse_httpStatus' - The response's http status code.
newGetBatchImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBatchImportJobsResponse
newGetBatchImportJobsResponse pHttpStatus_ =
  GetBatchImportJobsResponse'
    { batchImports =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array containing the details of each batch import job.
getBatchImportJobsResponse_batchImports :: Lens.Lens' GetBatchImportJobsResponse (Prelude.Maybe [BatchImport])
getBatchImportJobsResponse_batchImports = Lens.lens (\GetBatchImportJobsResponse' {batchImports} -> batchImports) (\s@GetBatchImportJobsResponse' {} a -> s {batchImports = a} :: GetBatchImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token for the subsequent resquest.
getBatchImportJobsResponse_nextToken :: Lens.Lens' GetBatchImportJobsResponse (Prelude.Maybe Prelude.Text)
getBatchImportJobsResponse_nextToken = Lens.lens (\GetBatchImportJobsResponse' {nextToken} -> nextToken) (\s@GetBatchImportJobsResponse' {} a -> s {nextToken = a} :: GetBatchImportJobsResponse)

-- | The response's http status code.
getBatchImportJobsResponse_httpStatus :: Lens.Lens' GetBatchImportJobsResponse Prelude.Int
getBatchImportJobsResponse_httpStatus = Lens.lens (\GetBatchImportJobsResponse' {httpStatus} -> httpStatus) (\s@GetBatchImportJobsResponse' {} a -> s {httpStatus = a} :: GetBatchImportJobsResponse)

instance Prelude.NFData GetBatchImportJobsResponse where
  rnf GetBatchImportJobsResponse' {..} =
    Prelude.rnf batchImports
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
