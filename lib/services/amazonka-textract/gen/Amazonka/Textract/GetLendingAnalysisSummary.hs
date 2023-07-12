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
-- Module      : Amazonka.Textract.GetLendingAnalysisSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summarized results for the @StartLendingAnalysis@ operation, which
-- analyzes text in a lending document. The returned summary consists of
-- information about documents grouped together by a common document type.
-- Information like detected signatures, page numbers, and split documents
-- is returned with respect to the type of grouped document.
--
-- You start asynchronous text analysis by calling @StartLendingAnalysis@,
-- which returns a job identifier (@JobId@). When the text analysis
-- operation finishes, Amazon Textract publishes a completion status to the
-- Amazon Simple Notification Service (Amazon SNS) topic that\'s registered
-- in the initial call to @StartLendingAnalysis@.
--
-- To get the results of the text analysis operation, first check that the
-- status value published to the Amazon SNS topic is SUCCEEDED. If so, call
-- @GetLendingAnalysisSummary@, and pass the job identifier (@JobId@) from
-- the initial call to @StartLendingAnalysis@.
module Amazonka.Textract.GetLendingAnalysisSummary
  ( -- * Creating a Request
    GetLendingAnalysisSummary (..),
    newGetLendingAnalysisSummary,

    -- * Request Lenses
    getLendingAnalysisSummary_jobId,

    -- * Destructuring the Response
    GetLendingAnalysisSummaryResponse (..),
    newGetLendingAnalysisSummaryResponse,

    -- * Response Lenses
    getLendingAnalysisSummaryResponse_analyzeLendingModelVersion,
    getLendingAnalysisSummaryResponse_documentMetadata,
    getLendingAnalysisSummaryResponse_jobStatus,
    getLendingAnalysisSummaryResponse_statusMessage,
    getLendingAnalysisSummaryResponse_summary,
    getLendingAnalysisSummaryResponse_warnings,
    getLendingAnalysisSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newGetLendingAnalysisSummary' smart constructor.
data GetLendingAnalysisSummary = GetLendingAnalysisSummary'
  { -- | A unique identifier for the lending or text-detection job. The @JobId@
    -- is returned from StartLendingAnalysis. A @JobId@ value is only valid for
    -- 7 days.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLendingAnalysisSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getLendingAnalysisSummary_jobId' - A unique identifier for the lending or text-detection job. The @JobId@
-- is returned from StartLendingAnalysis. A @JobId@ value is only valid for
-- 7 days.
newGetLendingAnalysisSummary ::
  -- | 'jobId'
  Prelude.Text ->
  GetLendingAnalysisSummary
newGetLendingAnalysisSummary pJobId_ =
  GetLendingAnalysisSummary' {jobId = pJobId_}

-- | A unique identifier for the lending or text-detection job. The @JobId@
-- is returned from StartLendingAnalysis. A @JobId@ value is only valid for
-- 7 days.
getLendingAnalysisSummary_jobId :: Lens.Lens' GetLendingAnalysisSummary Prelude.Text
getLendingAnalysisSummary_jobId = Lens.lens (\GetLendingAnalysisSummary' {jobId} -> jobId) (\s@GetLendingAnalysisSummary' {} a -> s {jobId = a} :: GetLendingAnalysisSummary)

instance Core.AWSRequest GetLendingAnalysisSummary where
  type
    AWSResponse GetLendingAnalysisSummary =
      GetLendingAnalysisSummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLendingAnalysisSummaryResponse'
            Prelude.<$> (x Data..?> "AnalyzeLendingModelVersion")
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "Summary")
            Prelude.<*> (x Data..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLendingAnalysisSummary where
  hashWithSalt _salt GetLendingAnalysisSummary' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetLendingAnalysisSummary where
  rnf GetLendingAnalysisSummary' {..} =
    Prelude.rnf jobId

instance Data.ToHeaders GetLendingAnalysisSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.GetLendingAnalysisSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLendingAnalysisSummary where
  toJSON GetLendingAnalysisSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath GetLendingAnalysisSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLendingAnalysisSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLendingAnalysisSummaryResponse' smart constructor.
data GetLendingAnalysisSummaryResponse = GetLendingAnalysisSummaryResponse'
  { -- | The current model version of the Analyze Lending API.
    analyzeLendingModelVersion :: Prelude.Maybe Prelude.Text,
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The current status of the lending analysis job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Returns if the lending analysis could not be completed. Contains
    -- explanation for what error occurred.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Contains summary information for documents grouped by type.
    summary :: Prelude.Maybe LendingSummary,
    -- | A list of warnings that occurred during the lending analysis operation.
    warnings :: Prelude.Maybe [Warning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLendingAnalysisSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzeLendingModelVersion', 'getLendingAnalysisSummaryResponse_analyzeLendingModelVersion' - The current model version of the Analyze Lending API.
--
-- 'documentMetadata', 'getLendingAnalysisSummaryResponse_documentMetadata' - Undocumented member.
--
-- 'jobStatus', 'getLendingAnalysisSummaryResponse_jobStatus' - The current status of the lending analysis job.
--
-- 'statusMessage', 'getLendingAnalysisSummaryResponse_statusMessage' - Returns if the lending analysis could not be completed. Contains
-- explanation for what error occurred.
--
-- 'summary', 'getLendingAnalysisSummaryResponse_summary' - Contains summary information for documents grouped by type.
--
-- 'warnings', 'getLendingAnalysisSummaryResponse_warnings' - A list of warnings that occurred during the lending analysis operation.
--
-- 'httpStatus', 'getLendingAnalysisSummaryResponse_httpStatus' - The response's http status code.
newGetLendingAnalysisSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLendingAnalysisSummaryResponse
newGetLendingAnalysisSummaryResponse pHttpStatus_ =
  GetLendingAnalysisSummaryResponse'
    { analyzeLendingModelVersion =
        Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      summary = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current model version of the Analyze Lending API.
getLendingAnalysisSummaryResponse_analyzeLendingModelVersion :: Lens.Lens' GetLendingAnalysisSummaryResponse (Prelude.Maybe Prelude.Text)
getLendingAnalysisSummaryResponse_analyzeLendingModelVersion = Lens.lens (\GetLendingAnalysisSummaryResponse' {analyzeLendingModelVersion} -> analyzeLendingModelVersion) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {analyzeLendingModelVersion = a} :: GetLendingAnalysisSummaryResponse)

-- | Undocumented member.
getLendingAnalysisSummaryResponse_documentMetadata :: Lens.Lens' GetLendingAnalysisSummaryResponse (Prelude.Maybe DocumentMetadata)
getLendingAnalysisSummaryResponse_documentMetadata = Lens.lens (\GetLendingAnalysisSummaryResponse' {documentMetadata} -> documentMetadata) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {documentMetadata = a} :: GetLendingAnalysisSummaryResponse)

-- | The current status of the lending analysis job.
getLendingAnalysisSummaryResponse_jobStatus :: Lens.Lens' GetLendingAnalysisSummaryResponse (Prelude.Maybe JobStatus)
getLendingAnalysisSummaryResponse_jobStatus = Lens.lens (\GetLendingAnalysisSummaryResponse' {jobStatus} -> jobStatus) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {jobStatus = a} :: GetLendingAnalysisSummaryResponse)

-- | Returns if the lending analysis could not be completed. Contains
-- explanation for what error occurred.
getLendingAnalysisSummaryResponse_statusMessage :: Lens.Lens' GetLendingAnalysisSummaryResponse (Prelude.Maybe Prelude.Text)
getLendingAnalysisSummaryResponse_statusMessage = Lens.lens (\GetLendingAnalysisSummaryResponse' {statusMessage} -> statusMessage) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {statusMessage = a} :: GetLendingAnalysisSummaryResponse)

-- | Contains summary information for documents grouped by type.
getLendingAnalysisSummaryResponse_summary :: Lens.Lens' GetLendingAnalysisSummaryResponse (Prelude.Maybe LendingSummary)
getLendingAnalysisSummaryResponse_summary = Lens.lens (\GetLendingAnalysisSummaryResponse' {summary} -> summary) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {summary = a} :: GetLendingAnalysisSummaryResponse)

-- | A list of warnings that occurred during the lending analysis operation.
getLendingAnalysisSummaryResponse_warnings :: Lens.Lens' GetLendingAnalysisSummaryResponse (Prelude.Maybe [Warning])
getLendingAnalysisSummaryResponse_warnings = Lens.lens (\GetLendingAnalysisSummaryResponse' {warnings} -> warnings) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {warnings = a} :: GetLendingAnalysisSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLendingAnalysisSummaryResponse_httpStatus :: Lens.Lens' GetLendingAnalysisSummaryResponse Prelude.Int
getLendingAnalysisSummaryResponse_httpStatus = Lens.lens (\GetLendingAnalysisSummaryResponse' {httpStatus} -> httpStatus) (\s@GetLendingAnalysisSummaryResponse' {} a -> s {httpStatus = a} :: GetLendingAnalysisSummaryResponse)

instance
  Prelude.NFData
    GetLendingAnalysisSummaryResponse
  where
  rnf GetLendingAnalysisSummaryResponse' {..} =
    Prelude.rnf analyzeLendingModelVersion
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
