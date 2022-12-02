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
-- Module      : Amazonka.Rekognition.GetTextDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the text detection results of a Amazon Rekognition Video analysis
-- started by StartTextDetection.
--
-- Text detection with Amazon Rekognition Video is an asynchronous
-- operation. You start text detection by calling StartTextDetection which
-- returns a job identifier (@JobId@) When the text detection operation
-- finishes, Amazon Rekognition publishes a completion status to the Amazon
-- Simple Notification Service topic registered in the initial call to
-- @StartTextDetection@. To get the results of the text detection
-- operation, first check that the status value published to the Amazon SNS
-- topic is @SUCCEEDED@. if so, call @GetTextDetection@ and pass the job
-- identifier (@JobId@) from the initial call of @StartLabelDetection@.
--
-- @GetTextDetection@ returns an array of detected text (@TextDetections@)
-- sorted by the time the text was detected, up to 50 words per frame of
-- video.
--
-- Each element of the array includes the detected text, the precentage
-- confidence in the acuracy of the detected text, the time the text was
-- detected, bounding box information for where the text was located, and
-- unique identifiers for words and their lines.
--
-- Use MaxResults parameter to limit the number of text detections
-- returned. If there are more results than specified in @MaxResults@, the
-- value of @NextToken@ in the operation response contains a pagination
-- token for getting the next set of results. To get the next page of
-- results, call @GetTextDetection@ and populate the @NextToken@ request
-- parameter with the token value returned from the previous call to
-- @GetTextDetection@.
module Amazonka.Rekognition.GetTextDetection
  ( -- * Creating a Request
    GetTextDetection (..),
    newGetTextDetection,

    -- * Request Lenses
    getTextDetection_nextToken,
    getTextDetection_maxResults,
    getTextDetection_jobId,

    -- * Destructuring the Response
    GetTextDetectionResponse (..),
    newGetTextDetectionResponse,

    -- * Response Lenses
    getTextDetectionResponse_nextToken,
    getTextDetectionResponse_jobStatus,
    getTextDetectionResponse_textDetections,
    getTextDetectionResponse_videoMetadata,
    getTextDetectionResponse_textModelVersion,
    getTextDetectionResponse_statusMessage,
    getTextDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTextDetection' smart constructor.
data GetTextDetection = GetTextDetection'
  { -- | If the previous response was incomplete (because there are more labels
    -- to retrieve), Amazon Rekognition Video returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- text.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Job identifier for the text detection operation for which you want
    -- results returned. You get the job identifer from an initial call to
    -- @StartTextDetection@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTextDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTextDetection_nextToken' - If the previous response was incomplete (because there are more labels
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- text.
--
-- 'maxResults', 'getTextDetection_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000.
--
-- 'jobId', 'getTextDetection_jobId' - Job identifier for the text detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartTextDetection@.
newGetTextDetection ::
  -- | 'jobId'
  Prelude.Text ->
  GetTextDetection
newGetTextDetection pJobId_ =
  GetTextDetection'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more labels
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- text.
getTextDetection_nextToken :: Lens.Lens' GetTextDetection (Prelude.Maybe Prelude.Text)
getTextDetection_nextToken = Lens.lens (\GetTextDetection' {nextToken} -> nextToken) (\s@GetTextDetection' {} a -> s {nextToken = a} :: GetTextDetection)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000.
getTextDetection_maxResults :: Lens.Lens' GetTextDetection (Prelude.Maybe Prelude.Natural)
getTextDetection_maxResults = Lens.lens (\GetTextDetection' {maxResults} -> maxResults) (\s@GetTextDetection' {} a -> s {maxResults = a} :: GetTextDetection)

-- | Job identifier for the text detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartTextDetection@.
getTextDetection_jobId :: Lens.Lens' GetTextDetection Prelude.Text
getTextDetection_jobId = Lens.lens (\GetTextDetection' {jobId} -> jobId) (\s@GetTextDetection' {} a -> s {jobId = a} :: GetTextDetection)

instance Core.AWSRequest GetTextDetection where
  type
    AWSResponse GetTextDetection =
      GetTextDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTextDetectionResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "TextDetections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VideoMetadata")
            Prelude.<*> (x Data..?> "TextModelVersion")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTextDetection where
  hashWithSalt _salt GetTextDetection' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetTextDetection where
  rnf GetTextDetection' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetTextDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.GetTextDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTextDetection where
  toJSON GetTextDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetTextDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTextDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTextDetectionResponse' smart constructor.
data GetTextDetectionResponse = GetTextDetectionResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of text.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Current status of the text detection job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | An array of text detected in the video. Each element contains the
    -- detected text, the time in milliseconds from the start of the video that
    -- the text was detected, and where it was detected on the screen.
    textDetections :: Prelude.Maybe [TextDetectionResult],
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | Version number of the text detection model that was used to detect text.
    textModelVersion :: Prelude.Maybe Prelude.Text,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTextDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTextDetectionResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of text.
--
-- 'jobStatus', 'getTextDetectionResponse_jobStatus' - Current status of the text detection job.
--
-- 'textDetections', 'getTextDetectionResponse_textDetections' - An array of text detected in the video. Each element contains the
-- detected text, the time in milliseconds from the start of the video that
-- the text was detected, and where it was detected on the screen.
--
-- 'videoMetadata', 'getTextDetectionResponse_videoMetadata' - Undocumented member.
--
-- 'textModelVersion', 'getTextDetectionResponse_textModelVersion' - Version number of the text detection model that was used to detect text.
--
-- 'statusMessage', 'getTextDetectionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'httpStatus', 'getTextDetectionResponse_httpStatus' - The response's http status code.
newGetTextDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTextDetectionResponse
newGetTextDetectionResponse pHttpStatus_ =
  GetTextDetectionResponse'
    { nextToken =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      textDetections = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      textModelVersion = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of text.
getTextDetectionResponse_nextToken :: Lens.Lens' GetTextDetectionResponse (Prelude.Maybe Prelude.Text)
getTextDetectionResponse_nextToken = Lens.lens (\GetTextDetectionResponse' {nextToken} -> nextToken) (\s@GetTextDetectionResponse' {} a -> s {nextToken = a} :: GetTextDetectionResponse)

-- | Current status of the text detection job.
getTextDetectionResponse_jobStatus :: Lens.Lens' GetTextDetectionResponse (Prelude.Maybe VideoJobStatus)
getTextDetectionResponse_jobStatus = Lens.lens (\GetTextDetectionResponse' {jobStatus} -> jobStatus) (\s@GetTextDetectionResponse' {} a -> s {jobStatus = a} :: GetTextDetectionResponse)

-- | An array of text detected in the video. Each element contains the
-- detected text, the time in milliseconds from the start of the video that
-- the text was detected, and where it was detected on the screen.
getTextDetectionResponse_textDetections :: Lens.Lens' GetTextDetectionResponse (Prelude.Maybe [TextDetectionResult])
getTextDetectionResponse_textDetections = Lens.lens (\GetTextDetectionResponse' {textDetections} -> textDetections) (\s@GetTextDetectionResponse' {} a -> s {textDetections = a} :: GetTextDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getTextDetectionResponse_videoMetadata :: Lens.Lens' GetTextDetectionResponse (Prelude.Maybe VideoMetadata)
getTextDetectionResponse_videoMetadata = Lens.lens (\GetTextDetectionResponse' {videoMetadata} -> videoMetadata) (\s@GetTextDetectionResponse' {} a -> s {videoMetadata = a} :: GetTextDetectionResponse)

-- | Version number of the text detection model that was used to detect text.
getTextDetectionResponse_textModelVersion :: Lens.Lens' GetTextDetectionResponse (Prelude.Maybe Prelude.Text)
getTextDetectionResponse_textModelVersion = Lens.lens (\GetTextDetectionResponse' {textModelVersion} -> textModelVersion) (\s@GetTextDetectionResponse' {} a -> s {textModelVersion = a} :: GetTextDetectionResponse)

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getTextDetectionResponse_statusMessage :: Lens.Lens' GetTextDetectionResponse (Prelude.Maybe Prelude.Text)
getTextDetectionResponse_statusMessage = Lens.lens (\GetTextDetectionResponse' {statusMessage} -> statusMessage) (\s@GetTextDetectionResponse' {} a -> s {statusMessage = a} :: GetTextDetectionResponse)

-- | The response's http status code.
getTextDetectionResponse_httpStatus :: Lens.Lens' GetTextDetectionResponse Prelude.Int
getTextDetectionResponse_httpStatus = Lens.lens (\GetTextDetectionResponse' {httpStatus} -> httpStatus) (\s@GetTextDetectionResponse' {} a -> s {httpStatus = a} :: GetTextDetectionResponse)

instance Prelude.NFData GetTextDetectionResponse where
  rnf GetTextDetectionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf textDetections
      `Prelude.seq` Prelude.rnf videoMetadata
      `Prelude.seq` Prelude.rnf textModelVersion
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
