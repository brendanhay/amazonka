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
-- Module      : Amazonka.Textract.GetExpenseAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the results for an Amazon Textract asynchronous operation that
-- analyzes invoices and receipts. Amazon Textract finds contact
-- information, items purchased, and vendor name, from input invoices and
-- receipts.
--
-- You start asynchronous invoice\/receipt analysis by calling
-- StartExpenseAnalysis, which returns a job identifier (@JobId@). Upon
-- completion of the invoice\/receipt analysis, Amazon Textract publishes
-- the completion status to the Amazon Simple Notification Service (Amazon
-- SNS) topic. This topic must be registered in the initial call to
-- @StartExpenseAnalysis@. To get the results of the invoice\/receipt
-- analysis operation, first ensure that the status value published to the
-- Amazon SNS topic is @SUCCEEDED@. If so, call @GetExpenseAnalysis@, and
-- pass the job identifier (@JobId@) from the initial call to
-- @StartExpenseAnalysis@.
--
-- Use the MaxResults parameter to limit the number of blocks that are
-- returned. If there are more results than specified in @MaxResults@, the
-- value of @NextToken@ in the operation response contains a pagination
-- token for getting the next set of results. To get the next page of
-- results, call @GetExpenseAnalysis@, and populate the @NextToken@ request
-- parameter with the token value that\'s returned from the previous call
-- to @GetExpenseAnalysis@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/invoices-receipts.html Analyzing Invoices and Receipts>.
module Amazonka.Textract.GetExpenseAnalysis
  ( -- * Creating a Request
    GetExpenseAnalysis (..),
    newGetExpenseAnalysis,

    -- * Request Lenses
    getExpenseAnalysis_maxResults,
    getExpenseAnalysis_nextToken,
    getExpenseAnalysis_jobId,

    -- * Destructuring the Response
    GetExpenseAnalysisResponse (..),
    newGetExpenseAnalysisResponse,

    -- * Response Lenses
    getExpenseAnalysisResponse_analyzeExpenseModelVersion,
    getExpenseAnalysisResponse_documentMetadata,
    getExpenseAnalysisResponse_expenseDocuments,
    getExpenseAnalysisResponse_jobStatus,
    getExpenseAnalysisResponse_nextToken,
    getExpenseAnalysisResponse_statusMessage,
    getExpenseAnalysisResponse_warnings,
    getExpenseAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newGetExpenseAnalysis' smart constructor.
data GetExpenseAnalysis = GetExpenseAnalysis'
  { -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 20. If you specify a value greater than 20, a
    -- maximum of 20 results is returned. The default value is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there are more blocks
    -- to retrieve), Amazon Textract returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- blocks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the text detection job. The @JobId@ is returned
    -- from @StartExpenseAnalysis@. A @JobId@ value is only valid for 7 days.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExpenseAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getExpenseAnalysis_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 20. If you specify a value greater than 20, a
-- maximum of 20 results is returned. The default value is 20.
--
-- 'nextToken', 'getExpenseAnalysis_nextToken' - If the previous response was incomplete (because there are more blocks
-- to retrieve), Amazon Textract returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- blocks.
--
-- 'jobId', 'getExpenseAnalysis_jobId' - A unique identifier for the text detection job. The @JobId@ is returned
-- from @StartExpenseAnalysis@. A @JobId@ value is only valid for 7 days.
newGetExpenseAnalysis ::
  -- | 'jobId'
  Prelude.Text ->
  GetExpenseAnalysis
newGetExpenseAnalysis pJobId_ =
  GetExpenseAnalysis'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 20. If you specify a value greater than 20, a
-- maximum of 20 results is returned. The default value is 20.
getExpenseAnalysis_maxResults :: Lens.Lens' GetExpenseAnalysis (Prelude.Maybe Prelude.Natural)
getExpenseAnalysis_maxResults = Lens.lens (\GetExpenseAnalysis' {maxResults} -> maxResults) (\s@GetExpenseAnalysis' {} a -> s {maxResults = a} :: GetExpenseAnalysis)

-- | If the previous response was incomplete (because there are more blocks
-- to retrieve), Amazon Textract returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- blocks.
getExpenseAnalysis_nextToken :: Lens.Lens' GetExpenseAnalysis (Prelude.Maybe Prelude.Text)
getExpenseAnalysis_nextToken = Lens.lens (\GetExpenseAnalysis' {nextToken} -> nextToken) (\s@GetExpenseAnalysis' {} a -> s {nextToken = a} :: GetExpenseAnalysis)

-- | A unique identifier for the text detection job. The @JobId@ is returned
-- from @StartExpenseAnalysis@. A @JobId@ value is only valid for 7 days.
getExpenseAnalysis_jobId :: Lens.Lens' GetExpenseAnalysis Prelude.Text
getExpenseAnalysis_jobId = Lens.lens (\GetExpenseAnalysis' {jobId} -> jobId) (\s@GetExpenseAnalysis' {} a -> s {jobId = a} :: GetExpenseAnalysis)

instance Core.AWSRequest GetExpenseAnalysis where
  type
    AWSResponse GetExpenseAnalysis =
      GetExpenseAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExpenseAnalysisResponse'
            Prelude.<$> (x Data..?> "AnalyzeExpenseModelVersion")
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> ( x
                            Data..?> "ExpenseDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExpenseAnalysis where
  hashWithSalt _salt GetExpenseAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetExpenseAnalysis where
  rnf GetExpenseAnalysis' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetExpenseAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.GetExpenseAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetExpenseAnalysis where
  toJSON GetExpenseAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetExpenseAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery GetExpenseAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExpenseAnalysisResponse' smart constructor.
data GetExpenseAnalysisResponse = GetExpenseAnalysisResponse'
  { -- | The current model version of AnalyzeExpense.
    analyzeExpenseModelVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about a document that Amazon Textract processed.
    -- @DocumentMetadata@ is returned in every page of paginated responses from
    -- an Amazon Textract operation.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The expenses detected by Amazon Textract.
    expenseDocuments :: Prelude.Maybe [ExpenseDocument],
    -- | The current status of the text detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | If the response is truncated, Amazon Textract returns this token. You
    -- can use this token in the subsequent request to retrieve the next set of
    -- text-detection results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns if the detection job could not be completed. Contains
    -- explanation for what error occured.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A list of warnings that occurred during the text-detection operation for
    -- the document.
    warnings :: Prelude.Maybe [Warning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExpenseAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzeExpenseModelVersion', 'getExpenseAnalysisResponse_analyzeExpenseModelVersion' - The current model version of AnalyzeExpense.
--
-- 'documentMetadata', 'getExpenseAnalysisResponse_documentMetadata' - Information about a document that Amazon Textract processed.
-- @DocumentMetadata@ is returned in every page of paginated responses from
-- an Amazon Textract operation.
--
-- 'expenseDocuments', 'getExpenseAnalysisResponse_expenseDocuments' - The expenses detected by Amazon Textract.
--
-- 'jobStatus', 'getExpenseAnalysisResponse_jobStatus' - The current status of the text detection job.
--
-- 'nextToken', 'getExpenseAnalysisResponse_nextToken' - If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- text-detection results.
--
-- 'statusMessage', 'getExpenseAnalysisResponse_statusMessage' - Returns if the detection job could not be completed. Contains
-- explanation for what error occured.
--
-- 'warnings', 'getExpenseAnalysisResponse_warnings' - A list of warnings that occurred during the text-detection operation for
-- the document.
--
-- 'httpStatus', 'getExpenseAnalysisResponse_httpStatus' - The response's http status code.
newGetExpenseAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExpenseAnalysisResponse
newGetExpenseAnalysisResponse pHttpStatus_ =
  GetExpenseAnalysisResponse'
    { analyzeExpenseModelVersion =
        Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      expenseDocuments = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current model version of AnalyzeExpense.
getExpenseAnalysisResponse_analyzeExpenseModelVersion :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe Prelude.Text)
getExpenseAnalysisResponse_analyzeExpenseModelVersion = Lens.lens (\GetExpenseAnalysisResponse' {analyzeExpenseModelVersion} -> analyzeExpenseModelVersion) (\s@GetExpenseAnalysisResponse' {} a -> s {analyzeExpenseModelVersion = a} :: GetExpenseAnalysisResponse)

-- | Information about a document that Amazon Textract processed.
-- @DocumentMetadata@ is returned in every page of paginated responses from
-- an Amazon Textract operation.
getExpenseAnalysisResponse_documentMetadata :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe DocumentMetadata)
getExpenseAnalysisResponse_documentMetadata = Lens.lens (\GetExpenseAnalysisResponse' {documentMetadata} -> documentMetadata) (\s@GetExpenseAnalysisResponse' {} a -> s {documentMetadata = a} :: GetExpenseAnalysisResponse)

-- | The expenses detected by Amazon Textract.
getExpenseAnalysisResponse_expenseDocuments :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe [ExpenseDocument])
getExpenseAnalysisResponse_expenseDocuments = Lens.lens (\GetExpenseAnalysisResponse' {expenseDocuments} -> expenseDocuments) (\s@GetExpenseAnalysisResponse' {} a -> s {expenseDocuments = a} :: GetExpenseAnalysisResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the text detection job.
getExpenseAnalysisResponse_jobStatus :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe JobStatus)
getExpenseAnalysisResponse_jobStatus = Lens.lens (\GetExpenseAnalysisResponse' {jobStatus} -> jobStatus) (\s@GetExpenseAnalysisResponse' {} a -> s {jobStatus = a} :: GetExpenseAnalysisResponse)

-- | If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- text-detection results.
getExpenseAnalysisResponse_nextToken :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe Prelude.Text)
getExpenseAnalysisResponse_nextToken = Lens.lens (\GetExpenseAnalysisResponse' {nextToken} -> nextToken) (\s@GetExpenseAnalysisResponse' {} a -> s {nextToken = a} :: GetExpenseAnalysisResponse)

-- | Returns if the detection job could not be completed. Contains
-- explanation for what error occured.
getExpenseAnalysisResponse_statusMessage :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe Prelude.Text)
getExpenseAnalysisResponse_statusMessage = Lens.lens (\GetExpenseAnalysisResponse' {statusMessage} -> statusMessage) (\s@GetExpenseAnalysisResponse' {} a -> s {statusMessage = a} :: GetExpenseAnalysisResponse)

-- | A list of warnings that occurred during the text-detection operation for
-- the document.
getExpenseAnalysisResponse_warnings :: Lens.Lens' GetExpenseAnalysisResponse (Prelude.Maybe [Warning])
getExpenseAnalysisResponse_warnings = Lens.lens (\GetExpenseAnalysisResponse' {warnings} -> warnings) (\s@GetExpenseAnalysisResponse' {} a -> s {warnings = a} :: GetExpenseAnalysisResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getExpenseAnalysisResponse_httpStatus :: Lens.Lens' GetExpenseAnalysisResponse Prelude.Int
getExpenseAnalysisResponse_httpStatus = Lens.lens (\GetExpenseAnalysisResponse' {httpStatus} -> httpStatus) (\s@GetExpenseAnalysisResponse' {} a -> s {httpStatus = a} :: GetExpenseAnalysisResponse)

instance Prelude.NFData GetExpenseAnalysisResponse where
  rnf GetExpenseAnalysisResponse' {..} =
    Prelude.rnf analyzeExpenseModelVersion
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf expenseDocuments
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
