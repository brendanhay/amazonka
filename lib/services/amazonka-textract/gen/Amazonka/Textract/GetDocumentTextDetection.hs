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
-- Module      : Amazonka.Textract.GetDocumentTextDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the results for an Amazon Textract asynchronous operation that
-- detects text in a document. Amazon Textract can detect lines of text and
-- the words that make up a line of text.
--
-- You start asynchronous text detection by calling
-- StartDocumentTextDetection, which returns a job identifier (@JobId@).
-- When the text detection operation finishes, Amazon Textract publishes a
-- completion status to the Amazon Simple Notification Service (Amazon SNS)
-- topic that\'s registered in the initial call to
-- @StartDocumentTextDetection@. To get the results of the text-detection
-- operation, first check that the status value published to the Amazon SNS
-- topic is @SUCCEEDED@. If so, call @GetDocumentTextDetection@, and pass
-- the job identifier (@JobId@) from the initial call to
-- @StartDocumentTextDetection@.
--
-- @GetDocumentTextDetection@ returns an array of Block objects.
--
-- Each document page has as an associated @Block@ of type PAGE. Each PAGE
-- @Block@ object is the parent of LINE @Block@ objects that represent the
-- lines of detected text on a page. A LINE @Block@ object is a parent for
-- each word that makes up the line. Words are represented by @Block@
-- objects of type WORD.
--
-- Use the MaxResults parameter to limit the number of blocks that are
-- returned. If there are more results than specified in @MaxResults@, the
-- value of @NextToken@ in the operation response contains a pagination
-- token for getting the next set of results. To get the next page of
-- results, call @GetDocumentTextDetection@, and populate the @NextToken@
-- request parameter with the token value that\'s returned from the
-- previous call to @GetDocumentTextDetection@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-detecting.html Document Text Detection>.
module Amazonka.Textract.GetDocumentTextDetection
  ( -- * Creating a Request
    GetDocumentTextDetection (..),
    newGetDocumentTextDetection,

    -- * Request Lenses
    getDocumentTextDetection_nextToken,
    getDocumentTextDetection_maxResults,
    getDocumentTextDetection_jobId,

    -- * Destructuring the Response
    GetDocumentTextDetectionResponse (..),
    newGetDocumentTextDetectionResponse,

    -- * Response Lenses
    getDocumentTextDetectionResponse_nextToken,
    getDocumentTextDetectionResponse_jobStatus,
    getDocumentTextDetectionResponse_documentMetadata,
    getDocumentTextDetectionResponse_warnings,
    getDocumentTextDetectionResponse_detectDocumentTextModelVersion,
    getDocumentTextDetectionResponse_statusMessage,
    getDocumentTextDetectionResponse_blocks,
    getDocumentTextDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newGetDocumentTextDetection' smart constructor.
data GetDocumentTextDetection = GetDocumentTextDetection'
  { -- | If the previous response was incomplete (because there are more blocks
    -- to retrieve), Amazon Textract returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- blocks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 1,000. If you specify a value greater than
    -- 1,000, a maximum of 1,000 results is returned. The default value is
    -- 1,000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the text detection job. The @JobId@ is returned
    -- from @StartDocumentTextDetection@. A @JobId@ value is only valid for 7
    -- days.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentTextDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDocumentTextDetection_nextToken' - If the previous response was incomplete (because there are more blocks
-- to retrieve), Amazon Textract returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- blocks.
--
-- 'maxResults', 'getDocumentTextDetection_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 1,000. If you specify a value greater than
-- 1,000, a maximum of 1,000 results is returned. The default value is
-- 1,000.
--
-- 'jobId', 'getDocumentTextDetection_jobId' - A unique identifier for the text detection job. The @JobId@ is returned
-- from @StartDocumentTextDetection@. A @JobId@ value is only valid for 7
-- days.
newGetDocumentTextDetection ::
  -- | 'jobId'
  Prelude.Text ->
  GetDocumentTextDetection
newGetDocumentTextDetection pJobId_ =
  GetDocumentTextDetection'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more blocks
-- to retrieve), Amazon Textract returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- blocks.
getDocumentTextDetection_nextToken :: Lens.Lens' GetDocumentTextDetection (Prelude.Maybe Prelude.Text)
getDocumentTextDetection_nextToken = Lens.lens (\GetDocumentTextDetection' {nextToken} -> nextToken) (\s@GetDocumentTextDetection' {} a -> s {nextToken = a} :: GetDocumentTextDetection)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 1,000. If you specify a value greater than
-- 1,000, a maximum of 1,000 results is returned. The default value is
-- 1,000.
getDocumentTextDetection_maxResults :: Lens.Lens' GetDocumentTextDetection (Prelude.Maybe Prelude.Natural)
getDocumentTextDetection_maxResults = Lens.lens (\GetDocumentTextDetection' {maxResults} -> maxResults) (\s@GetDocumentTextDetection' {} a -> s {maxResults = a} :: GetDocumentTextDetection)

-- | A unique identifier for the text detection job. The @JobId@ is returned
-- from @StartDocumentTextDetection@. A @JobId@ value is only valid for 7
-- days.
getDocumentTextDetection_jobId :: Lens.Lens' GetDocumentTextDetection Prelude.Text
getDocumentTextDetection_jobId = Lens.lens (\GetDocumentTextDetection' {jobId} -> jobId) (\s@GetDocumentTextDetection' {} a -> s {jobId = a} :: GetDocumentTextDetection)

instance Core.AWSRequest GetDocumentTextDetection where
  type
    AWSResponse GetDocumentTextDetection =
      GetDocumentTextDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentTextDetectionResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "DocumentMetadata")
            Prelude.<*> (x Core..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "DetectDocumentTextModelVersion")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (x Core..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentTextDetection where
  hashWithSalt _salt GetDocumentTextDetection' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetDocumentTextDetection where
  rnf GetDocumentTextDetection' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobId

instance Core.ToHeaders GetDocumentTextDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Textract.GetDocumentTextDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDocumentTextDetection where
  toJSON GetDocumentTextDetection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath GetDocumentTextDetection where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDocumentTextDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDocumentTextDetectionResponse' smart constructor.
data GetDocumentTextDetectionResponse = GetDocumentTextDetectionResponse'
  { -- | If the response is truncated, Amazon Textract returns this token. You
    -- can use this token in the subsequent request to retrieve the next set of
    -- text-detection results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the text detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Information about a document that Amazon Textract processed.
    -- @DocumentMetadata@ is returned in every page of paginated responses from
    -- an Amazon Textract video operation.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | A list of warnings that occurred during the text-detection operation for
    -- the document.
    warnings :: Prelude.Maybe [Warning],
    detectDocumentTextModelVersion :: Prelude.Maybe Prelude.Text,
    -- | Returns if the detection job could not be completed. Contains
    -- explanation for what error occured.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The results of the text-detection operation.
    blocks :: Prelude.Maybe [Block],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentTextDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDocumentTextDetectionResponse_nextToken' - If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- text-detection results.
--
-- 'jobStatus', 'getDocumentTextDetectionResponse_jobStatus' - The current status of the text detection job.
--
-- 'documentMetadata', 'getDocumentTextDetectionResponse_documentMetadata' - Information about a document that Amazon Textract processed.
-- @DocumentMetadata@ is returned in every page of paginated responses from
-- an Amazon Textract video operation.
--
-- 'warnings', 'getDocumentTextDetectionResponse_warnings' - A list of warnings that occurred during the text-detection operation for
-- the document.
--
-- 'detectDocumentTextModelVersion', 'getDocumentTextDetectionResponse_detectDocumentTextModelVersion' -
--
-- 'statusMessage', 'getDocumentTextDetectionResponse_statusMessage' - Returns if the detection job could not be completed. Contains
-- explanation for what error occured.
--
-- 'blocks', 'getDocumentTextDetectionResponse_blocks' - The results of the text-detection operation.
--
-- 'httpStatus', 'getDocumentTextDetectionResponse_httpStatus' - The response's http status code.
newGetDocumentTextDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentTextDetectionResponse
newGetDocumentTextDetectionResponse pHttpStatus_ =
  GetDocumentTextDetectionResponse'
    { nextToken =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      warnings = Prelude.Nothing,
      detectDocumentTextModelVersion =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      blocks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- text-detection results.
getDocumentTextDetectionResponse_nextToken :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe Prelude.Text)
getDocumentTextDetectionResponse_nextToken = Lens.lens (\GetDocumentTextDetectionResponse' {nextToken} -> nextToken) (\s@GetDocumentTextDetectionResponse' {} a -> s {nextToken = a} :: GetDocumentTextDetectionResponse)

-- | The current status of the text detection job.
getDocumentTextDetectionResponse_jobStatus :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe JobStatus)
getDocumentTextDetectionResponse_jobStatus = Lens.lens (\GetDocumentTextDetectionResponse' {jobStatus} -> jobStatus) (\s@GetDocumentTextDetectionResponse' {} a -> s {jobStatus = a} :: GetDocumentTextDetectionResponse)

-- | Information about a document that Amazon Textract processed.
-- @DocumentMetadata@ is returned in every page of paginated responses from
-- an Amazon Textract video operation.
getDocumentTextDetectionResponse_documentMetadata :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe DocumentMetadata)
getDocumentTextDetectionResponse_documentMetadata = Lens.lens (\GetDocumentTextDetectionResponse' {documentMetadata} -> documentMetadata) (\s@GetDocumentTextDetectionResponse' {} a -> s {documentMetadata = a} :: GetDocumentTextDetectionResponse)

-- | A list of warnings that occurred during the text-detection operation for
-- the document.
getDocumentTextDetectionResponse_warnings :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe [Warning])
getDocumentTextDetectionResponse_warnings = Lens.lens (\GetDocumentTextDetectionResponse' {warnings} -> warnings) (\s@GetDocumentTextDetectionResponse' {} a -> s {warnings = a} :: GetDocumentTextDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- |
getDocumentTextDetectionResponse_detectDocumentTextModelVersion :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe Prelude.Text)
getDocumentTextDetectionResponse_detectDocumentTextModelVersion = Lens.lens (\GetDocumentTextDetectionResponse' {detectDocumentTextModelVersion} -> detectDocumentTextModelVersion) (\s@GetDocumentTextDetectionResponse' {} a -> s {detectDocumentTextModelVersion = a} :: GetDocumentTextDetectionResponse)

-- | Returns if the detection job could not be completed. Contains
-- explanation for what error occured.
getDocumentTextDetectionResponse_statusMessage :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe Prelude.Text)
getDocumentTextDetectionResponse_statusMessage = Lens.lens (\GetDocumentTextDetectionResponse' {statusMessage} -> statusMessage) (\s@GetDocumentTextDetectionResponse' {} a -> s {statusMessage = a} :: GetDocumentTextDetectionResponse)

-- | The results of the text-detection operation.
getDocumentTextDetectionResponse_blocks :: Lens.Lens' GetDocumentTextDetectionResponse (Prelude.Maybe [Block])
getDocumentTextDetectionResponse_blocks = Lens.lens (\GetDocumentTextDetectionResponse' {blocks} -> blocks) (\s@GetDocumentTextDetectionResponse' {} a -> s {blocks = a} :: GetDocumentTextDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDocumentTextDetectionResponse_httpStatus :: Lens.Lens' GetDocumentTextDetectionResponse Prelude.Int
getDocumentTextDetectionResponse_httpStatus = Lens.lens (\GetDocumentTextDetectionResponse' {httpStatus} -> httpStatus) (\s@GetDocumentTextDetectionResponse' {} a -> s {httpStatus = a} :: GetDocumentTextDetectionResponse)

instance
  Prelude.NFData
    GetDocumentTextDetectionResponse
  where
  rnf GetDocumentTextDetectionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf detectDocumentTextModelVersion
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf blocks
      `Prelude.seq` Prelude.rnf httpStatus
