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
-- Module      : Amazonka.Rekognition.GetCelebrityRecognition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the celebrity recognition results for a Amazon Rekognition Video
-- analysis started by StartCelebrityRecognition.
--
-- Celebrity recognition in a video is an asynchronous operation. Analysis
-- is started by a call to StartCelebrityRecognition which returns a job
-- identifier (@JobId@).
--
-- When the celebrity recognition operation finishes, Amazon Rekognition
-- Video publishes a completion status to the Amazon Simple Notification
-- Service topic registered in the initial call to
-- @StartCelebrityRecognition@. To get the results of the celebrity
-- recognition analysis, first check that the status value published to the
-- Amazon SNS topic is @SUCCEEDED@. If so, call @GetCelebrityDetection@ and
-- pass the job identifier (@JobId@) from the initial call to
-- @StartCelebrityDetection@.
--
-- For more information, see Working With Stored Videos in the Amazon
-- Rekognition Developer Guide.
--
-- @GetCelebrityRecognition@ returns detected celebrities and the time(s)
-- they are detected in an array (@Celebrities@) of CelebrityRecognition
-- objects. Each @CelebrityRecognition@ contains information about the
-- celebrity in a CelebrityDetail object and the time, @Timestamp@, the
-- celebrity was detected. This CelebrityDetail object stores information
-- about the detected celebrity\'s face attributes, a face bounding box,
-- known gender, the celebrity\'s name, and a confidence estimate.
--
-- @GetCelebrityRecognition@ only returns the default facial attributes
-- (@BoundingBox@, @Confidence@, @Landmarks@, @Pose@, and @Quality@). The
-- @BoundingBox@ field only applies to the detected face instance. The
-- other facial attributes listed in the @Face@ object of the following
-- response syntax are not returned. For more information, see FaceDetail
-- in the Amazon Rekognition Developer Guide.
--
-- By default, the @Celebrities@ array is sorted by time (milliseconds from
-- the start of the video). You can also sort the array by celebrity by
-- specifying the value @ID@ in the @SortBy@ input parameter.
--
-- The @CelebrityDetail@ object includes the celebrity identifer and
-- additional information urls. If you don\'t store the additional
-- information urls, you can get them later by calling GetCelebrityInfo
-- with the celebrity identifer.
--
-- No information is returned for faces not recognized as celebrities.
--
-- Use MaxResults parameter to limit the number of labels returned. If
-- there are more results than specified in @MaxResults@, the value of
-- @NextToken@ in the operation response contains a pagination token for
-- getting the next set of results. To get the next page of results, call
-- @GetCelebrityDetection@ and populate the @NextToken@ request parameter
-- with the token value returned from the previous call to
-- @GetCelebrityRecognition@.
module Amazonka.Rekognition.GetCelebrityRecognition
  ( -- * Creating a Request
    GetCelebrityRecognition (..),
    newGetCelebrityRecognition,

    -- * Request Lenses
    getCelebrityRecognition_maxResults,
    getCelebrityRecognition_nextToken,
    getCelebrityRecognition_sortBy,
    getCelebrityRecognition_jobId,

    -- * Destructuring the Response
    GetCelebrityRecognitionResponse (..),
    newGetCelebrityRecognitionResponse,

    -- * Response Lenses
    getCelebrityRecognitionResponse_celebrities,
    getCelebrityRecognitionResponse_jobStatus,
    getCelebrityRecognitionResponse_nextToken,
    getCelebrityRecognitionResponse_statusMessage,
    getCelebrityRecognitionResponse_videoMetadata,
    getCelebrityRecognitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCelebrityRecognition' smart constructor.
data GetCelebrityRecognition = GetCelebrityRecognition'
  { -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more
    -- recognized celebrities to retrieve), Amazon Rekognition Video returns a
    -- pagination token in the response. You can use this pagination token to
    -- retrieve the next set of celebrities.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort to use for celebrities returned in @Celebrities@ field. Specify
    -- @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by
    -- the time the celebrity was recognized.
    sortBy :: Prelude.Maybe CelebrityRecognitionSortBy,
    -- | Job identifier for the required celebrity recognition analysis. You can
    -- get the job identifer from a call to @StartCelebrityRecognition@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCelebrityRecognition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCelebrityRecognition_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
--
-- 'nextToken', 'getCelebrityRecognition_nextToken' - If the previous response was incomplete (because there is more
-- recognized celebrities to retrieve), Amazon Rekognition Video returns a
-- pagination token in the response. You can use this pagination token to
-- retrieve the next set of celebrities.
--
-- 'sortBy', 'getCelebrityRecognition_sortBy' - Sort to use for celebrities returned in @Celebrities@ field. Specify
-- @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by
-- the time the celebrity was recognized.
--
-- 'jobId', 'getCelebrityRecognition_jobId' - Job identifier for the required celebrity recognition analysis. You can
-- get the job identifer from a call to @StartCelebrityRecognition@.
newGetCelebrityRecognition ::
  -- | 'jobId'
  Prelude.Text ->
  GetCelebrityRecognition
newGetCelebrityRecognition pJobId_ =
  GetCelebrityRecognition'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getCelebrityRecognition_maxResults :: Lens.Lens' GetCelebrityRecognition (Prelude.Maybe Prelude.Natural)
getCelebrityRecognition_maxResults = Lens.lens (\GetCelebrityRecognition' {maxResults} -> maxResults) (\s@GetCelebrityRecognition' {} a -> s {maxResults = a} :: GetCelebrityRecognition)

-- | If the previous response was incomplete (because there is more
-- recognized celebrities to retrieve), Amazon Rekognition Video returns a
-- pagination token in the response. You can use this pagination token to
-- retrieve the next set of celebrities.
getCelebrityRecognition_nextToken :: Lens.Lens' GetCelebrityRecognition (Prelude.Maybe Prelude.Text)
getCelebrityRecognition_nextToken = Lens.lens (\GetCelebrityRecognition' {nextToken} -> nextToken) (\s@GetCelebrityRecognition' {} a -> s {nextToken = a} :: GetCelebrityRecognition)

-- | Sort to use for celebrities returned in @Celebrities@ field. Specify
-- @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by
-- the time the celebrity was recognized.
getCelebrityRecognition_sortBy :: Lens.Lens' GetCelebrityRecognition (Prelude.Maybe CelebrityRecognitionSortBy)
getCelebrityRecognition_sortBy = Lens.lens (\GetCelebrityRecognition' {sortBy} -> sortBy) (\s@GetCelebrityRecognition' {} a -> s {sortBy = a} :: GetCelebrityRecognition)

-- | Job identifier for the required celebrity recognition analysis. You can
-- get the job identifer from a call to @StartCelebrityRecognition@.
getCelebrityRecognition_jobId :: Lens.Lens' GetCelebrityRecognition Prelude.Text
getCelebrityRecognition_jobId = Lens.lens (\GetCelebrityRecognition' {jobId} -> jobId) (\s@GetCelebrityRecognition' {} a -> s {jobId = a} :: GetCelebrityRecognition)

instance Core.AWSRequest GetCelebrityRecognition where
  type
    AWSResponse GetCelebrityRecognition =
      GetCelebrityRecognitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCelebrityRecognitionResponse'
            Prelude.<$> (x Data..?> "Celebrities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "VideoMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCelebrityRecognition where
  hashWithSalt _salt GetCelebrityRecognition' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetCelebrityRecognition where
  rnf GetCelebrityRecognition' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetCelebrityRecognition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.GetCelebrityRecognition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCelebrityRecognition where
  toJSON GetCelebrityRecognition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetCelebrityRecognition where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCelebrityRecognition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCelebrityRecognitionResponse' smart constructor.
data GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse'
  { -- | Array of celebrities recognized in the video.
    celebrities :: Prelude.Maybe [CelebrityRecognition],
    -- | The current status of the celebrity recognition job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of celebrities.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition Video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCelebrityRecognitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'celebrities', 'getCelebrityRecognitionResponse_celebrities' - Array of celebrities recognized in the video.
--
-- 'jobStatus', 'getCelebrityRecognitionResponse_jobStatus' - The current status of the celebrity recognition job.
--
-- 'nextToken', 'getCelebrityRecognitionResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of celebrities.
--
-- 'statusMessage', 'getCelebrityRecognitionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'videoMetadata', 'getCelebrityRecognitionResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
--
-- 'httpStatus', 'getCelebrityRecognitionResponse_httpStatus' - The response's http status code.
newGetCelebrityRecognitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCelebrityRecognitionResponse
newGetCelebrityRecognitionResponse pHttpStatus_ =
  GetCelebrityRecognitionResponse'
    { celebrities =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Array of celebrities recognized in the video.
getCelebrityRecognitionResponse_celebrities :: Lens.Lens' GetCelebrityRecognitionResponse (Prelude.Maybe [CelebrityRecognition])
getCelebrityRecognitionResponse_celebrities = Lens.lens (\GetCelebrityRecognitionResponse' {celebrities} -> celebrities) (\s@GetCelebrityRecognitionResponse' {} a -> s {celebrities = a} :: GetCelebrityRecognitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the celebrity recognition job.
getCelebrityRecognitionResponse_jobStatus :: Lens.Lens' GetCelebrityRecognitionResponse (Prelude.Maybe VideoJobStatus)
getCelebrityRecognitionResponse_jobStatus = Lens.lens (\GetCelebrityRecognitionResponse' {jobStatus} -> jobStatus) (\s@GetCelebrityRecognitionResponse' {} a -> s {jobStatus = a} :: GetCelebrityRecognitionResponse)

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of celebrities.
getCelebrityRecognitionResponse_nextToken :: Lens.Lens' GetCelebrityRecognitionResponse (Prelude.Maybe Prelude.Text)
getCelebrityRecognitionResponse_nextToken = Lens.lens (\GetCelebrityRecognitionResponse' {nextToken} -> nextToken) (\s@GetCelebrityRecognitionResponse' {} a -> s {nextToken = a} :: GetCelebrityRecognitionResponse)

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getCelebrityRecognitionResponse_statusMessage :: Lens.Lens' GetCelebrityRecognitionResponse (Prelude.Maybe Prelude.Text)
getCelebrityRecognitionResponse_statusMessage = Lens.lens (\GetCelebrityRecognitionResponse' {statusMessage} -> statusMessage) (\s@GetCelebrityRecognitionResponse' {} a -> s {statusMessage = a} :: GetCelebrityRecognitionResponse)

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
getCelebrityRecognitionResponse_videoMetadata :: Lens.Lens' GetCelebrityRecognitionResponse (Prelude.Maybe VideoMetadata)
getCelebrityRecognitionResponse_videoMetadata = Lens.lens (\GetCelebrityRecognitionResponse' {videoMetadata} -> videoMetadata) (\s@GetCelebrityRecognitionResponse' {} a -> s {videoMetadata = a} :: GetCelebrityRecognitionResponse)

-- | The response's http status code.
getCelebrityRecognitionResponse_httpStatus :: Lens.Lens' GetCelebrityRecognitionResponse Prelude.Int
getCelebrityRecognitionResponse_httpStatus = Lens.lens (\GetCelebrityRecognitionResponse' {httpStatus} -> httpStatus) (\s@GetCelebrityRecognitionResponse' {} a -> s {httpStatus = a} :: GetCelebrityRecognitionResponse)

instance
  Prelude.NFData
    GetCelebrityRecognitionResponse
  where
  rnf GetCelebrityRecognitionResponse' {..} =
    Prelude.rnf celebrities
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf videoMetadata
      `Prelude.seq` Prelude.rnf httpStatus
