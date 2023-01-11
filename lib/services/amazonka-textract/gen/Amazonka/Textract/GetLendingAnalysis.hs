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
-- Module      : Amazonka.Textract.GetLendingAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the results for an Amazon Textract asynchronous operation that
-- analyzes text in a lending document.
--
-- You start asynchronous text analysis by calling @StartLendingAnalysis@,
-- which returns a job identifier (@JobId@). When the text analysis
-- operation finishes, Amazon Textract publishes a completion status to the
-- Amazon Simple Notification Service (Amazon SNS) topic that\'s registered
-- in the initial call to @StartLendingAnalysis@.
--
-- To get the results of the text analysis operation, first check that the
-- status value published to the Amazon SNS topic is SUCCEEDED. If so, call
-- GetLendingAnalysis, and pass the job identifier (@JobId@) from the
-- initial call to @StartLendingAnalysis@.
module Amazonka.Textract.GetLendingAnalysis
  ( -- * Creating a Request
    GetLendingAnalysis (..),
    newGetLendingAnalysis,

    -- * Request Lenses
    getLendingAnalysis_maxResults,
    getLendingAnalysis_nextToken,
    getLendingAnalysis_jobId,

    -- * Destructuring the Response
    GetLendingAnalysisResponse (..),
    newGetLendingAnalysisResponse,

    -- * Response Lenses
    getLendingAnalysisResponse_analyzeLendingModelVersion,
    getLendingAnalysisResponse_documentMetadata,
    getLendingAnalysisResponse_jobStatus,
    getLendingAnalysisResponse_nextToken,
    getLendingAnalysisResponse_results,
    getLendingAnalysisResponse_statusMessage,
    getLendingAnalysisResponse_warnings,
    getLendingAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newGetLendingAnalysis' smart constructor.
data GetLendingAnalysis = GetLendingAnalysis'
  { -- | The maximum number of results to return per paginated call. The largest
    -- value that you can specify is 30. If you specify a value greater than
    -- 30, a maximum of 30 results is returned. The default value is 30.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete, Amazon Textract returns a
    -- pagination token in the response. You can use this pagination token to
    -- retrieve the next set of lending results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the lending or text-detection job. The @JobId@
    -- is returned from @StartLendingAnalysis@. A @JobId@ value is only valid
    -- for 7 days.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLendingAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getLendingAnalysis_maxResults' - The maximum number of results to return per paginated call. The largest
-- value that you can specify is 30. If you specify a value greater than
-- 30, a maximum of 30 results is returned. The default value is 30.
--
-- 'nextToken', 'getLendingAnalysis_nextToken' - If the previous response was incomplete, Amazon Textract returns a
-- pagination token in the response. You can use this pagination token to
-- retrieve the next set of lending results.
--
-- 'jobId', 'getLendingAnalysis_jobId' - A unique identifier for the lending or text-detection job. The @JobId@
-- is returned from @StartLendingAnalysis@. A @JobId@ value is only valid
-- for 7 days.
newGetLendingAnalysis ::
  -- | 'jobId'
  Prelude.Text ->
  GetLendingAnalysis
newGetLendingAnalysis pJobId_ =
  GetLendingAnalysis'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The maximum number of results to return per paginated call. The largest
-- value that you can specify is 30. If you specify a value greater than
-- 30, a maximum of 30 results is returned. The default value is 30.
getLendingAnalysis_maxResults :: Lens.Lens' GetLendingAnalysis (Prelude.Maybe Prelude.Natural)
getLendingAnalysis_maxResults = Lens.lens (\GetLendingAnalysis' {maxResults} -> maxResults) (\s@GetLendingAnalysis' {} a -> s {maxResults = a} :: GetLendingAnalysis)

-- | If the previous response was incomplete, Amazon Textract returns a
-- pagination token in the response. You can use this pagination token to
-- retrieve the next set of lending results.
getLendingAnalysis_nextToken :: Lens.Lens' GetLendingAnalysis (Prelude.Maybe Prelude.Text)
getLendingAnalysis_nextToken = Lens.lens (\GetLendingAnalysis' {nextToken} -> nextToken) (\s@GetLendingAnalysis' {} a -> s {nextToken = a} :: GetLendingAnalysis)

-- | A unique identifier for the lending or text-detection job. The @JobId@
-- is returned from @StartLendingAnalysis@. A @JobId@ value is only valid
-- for 7 days.
getLendingAnalysis_jobId :: Lens.Lens' GetLendingAnalysis Prelude.Text
getLendingAnalysis_jobId = Lens.lens (\GetLendingAnalysis' {jobId} -> jobId) (\s@GetLendingAnalysis' {} a -> s {jobId = a} :: GetLendingAnalysis)

instance Core.AWSRequest GetLendingAnalysis where
  type
    AWSResponse GetLendingAnalysis =
      GetLendingAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLendingAnalysisResponse'
            Prelude.<$> (x Data..?> "AnalyzeLendingModelVersion")
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLendingAnalysis where
  hashWithSalt _salt GetLendingAnalysis' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetLendingAnalysis where
  rnf GetLendingAnalysis' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetLendingAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.GetLendingAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLendingAnalysis where
  toJSON GetLendingAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetLendingAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLendingAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLendingAnalysisResponse' smart constructor.
data GetLendingAnalysisResponse = GetLendingAnalysisResponse'
  { -- | The current model version of the Analyze Lending API.
    analyzeLendingModelVersion :: Prelude.Maybe Prelude.Text,
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The current status of the lending analysis job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | If the response is truncated, Amazon Textract returns this token. You
    -- can use this token in the subsequent request to retrieve the next set of
    -- lending results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Holds the information returned by one of AmazonTextract\'s document
    -- analysis operations for the pinstripe.
    results :: Prelude.Maybe [LendingResult],
    -- | Returns if the lending analysis job could not be completed. Contains
    -- explanation for what error occurred.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A list of warnings that occurred during the lending analysis operation.
    warnings :: Prelude.Maybe [Warning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLendingAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzeLendingModelVersion', 'getLendingAnalysisResponse_analyzeLendingModelVersion' - The current model version of the Analyze Lending API.
--
-- 'documentMetadata', 'getLendingAnalysisResponse_documentMetadata' - Undocumented member.
--
-- 'jobStatus', 'getLendingAnalysisResponse_jobStatus' - The current status of the lending analysis job.
--
-- 'nextToken', 'getLendingAnalysisResponse_nextToken' - If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- lending results.
--
-- 'results', 'getLendingAnalysisResponse_results' - Holds the information returned by one of AmazonTextract\'s document
-- analysis operations for the pinstripe.
--
-- 'statusMessage', 'getLendingAnalysisResponse_statusMessage' - Returns if the lending analysis job could not be completed. Contains
-- explanation for what error occurred.
--
-- 'warnings', 'getLendingAnalysisResponse_warnings' - A list of warnings that occurred during the lending analysis operation.
--
-- 'httpStatus', 'getLendingAnalysisResponse_httpStatus' - The response's http status code.
newGetLendingAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLendingAnalysisResponse
newGetLendingAnalysisResponse pHttpStatus_ =
  GetLendingAnalysisResponse'
    { analyzeLendingModelVersion =
        Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      results = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current model version of the Analyze Lending API.
getLendingAnalysisResponse_analyzeLendingModelVersion :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe Prelude.Text)
getLendingAnalysisResponse_analyzeLendingModelVersion = Lens.lens (\GetLendingAnalysisResponse' {analyzeLendingModelVersion} -> analyzeLendingModelVersion) (\s@GetLendingAnalysisResponse' {} a -> s {analyzeLendingModelVersion = a} :: GetLendingAnalysisResponse)

-- | Undocumented member.
getLendingAnalysisResponse_documentMetadata :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe DocumentMetadata)
getLendingAnalysisResponse_documentMetadata = Lens.lens (\GetLendingAnalysisResponse' {documentMetadata} -> documentMetadata) (\s@GetLendingAnalysisResponse' {} a -> s {documentMetadata = a} :: GetLendingAnalysisResponse)

-- | The current status of the lending analysis job.
getLendingAnalysisResponse_jobStatus :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe JobStatus)
getLendingAnalysisResponse_jobStatus = Lens.lens (\GetLendingAnalysisResponse' {jobStatus} -> jobStatus) (\s@GetLendingAnalysisResponse' {} a -> s {jobStatus = a} :: GetLendingAnalysisResponse)

-- | If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- lending results.
getLendingAnalysisResponse_nextToken :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe Prelude.Text)
getLendingAnalysisResponse_nextToken = Lens.lens (\GetLendingAnalysisResponse' {nextToken} -> nextToken) (\s@GetLendingAnalysisResponse' {} a -> s {nextToken = a} :: GetLendingAnalysisResponse)

-- | Holds the information returned by one of AmazonTextract\'s document
-- analysis operations for the pinstripe.
getLendingAnalysisResponse_results :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe [LendingResult])
getLendingAnalysisResponse_results = Lens.lens (\GetLendingAnalysisResponse' {results} -> results) (\s@GetLendingAnalysisResponse' {} a -> s {results = a} :: GetLendingAnalysisResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns if the lending analysis job could not be completed. Contains
-- explanation for what error occurred.
getLendingAnalysisResponse_statusMessage :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe Prelude.Text)
getLendingAnalysisResponse_statusMessage = Lens.lens (\GetLendingAnalysisResponse' {statusMessage} -> statusMessage) (\s@GetLendingAnalysisResponse' {} a -> s {statusMessage = a} :: GetLendingAnalysisResponse)

-- | A list of warnings that occurred during the lending analysis operation.
getLendingAnalysisResponse_warnings :: Lens.Lens' GetLendingAnalysisResponse (Prelude.Maybe [Warning])
getLendingAnalysisResponse_warnings = Lens.lens (\GetLendingAnalysisResponse' {warnings} -> warnings) (\s@GetLendingAnalysisResponse' {} a -> s {warnings = a} :: GetLendingAnalysisResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLendingAnalysisResponse_httpStatus :: Lens.Lens' GetLendingAnalysisResponse Prelude.Int
getLendingAnalysisResponse_httpStatus = Lens.lens (\GetLendingAnalysisResponse' {httpStatus} -> httpStatus) (\s@GetLendingAnalysisResponse' {} a -> s {httpStatus = a} :: GetLendingAnalysisResponse)

instance Prelude.NFData GetLendingAnalysisResponse where
  rnf GetLendingAnalysisResponse' {..} =
    Prelude.rnf analyzeLendingModelVersion
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
