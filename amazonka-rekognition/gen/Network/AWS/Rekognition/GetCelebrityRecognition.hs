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
-- Module      : Network.AWS.Rekognition.GetCelebrityRecognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the celebrity recognition results for a Amazon Rekognition Video
-- analysis started by StartCelebrityRecognition.
--
-- Celebrity recognition in a video is an asynchronous operation. Analysis
-- is started by a call to StartCelebrityRecognition which returns a job
-- identifier (@JobId@). When the celebrity recognition operation finishes,
-- Amazon Rekognition Video publishes a completion status to the Amazon
-- Simple Notification Service topic registered in the initial call to
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
-- celebrity was detected.
--
-- @GetCelebrityRecognition@ only returns the default facial attributes
-- (@BoundingBox@, @Confidence@, @Landmarks@, @Pose@, and @Quality@). The
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
module Network.AWS.Rekognition.GetCelebrityRecognition
  ( -- * Creating a Request
    GetCelebrityRecognition (..),
    newGetCelebrityRecognition,

    -- * Request Lenses
    getCelebrityRecognition_nextToken,
    getCelebrityRecognition_maxResults,
    getCelebrityRecognition_sortBy,
    getCelebrityRecognition_jobId,

    -- * Destructuring the Response
    GetCelebrityRecognitionResponse (..),
    newGetCelebrityRecognitionResponse,

    -- * Response Lenses
    getCelebrityRecognitionResponse_statusMessage,
    getCelebrityRecognitionResponse_videoMetadata,
    getCelebrityRecognitionResponse_nextToken,
    getCelebrityRecognitionResponse_jobStatus,
    getCelebrityRecognitionResponse_celebrities,
    getCelebrityRecognitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCelebrityRecognition' smart constructor.
data GetCelebrityRecognition = GetCelebrityRecognition'
  { -- | If the previous response was incomplete (because there is more
    -- recognized celebrities to retrieve), Amazon Rekognition Video returns a
    -- pagination token in the response. You can use this pagination token to
    -- retrieve the next set of celebrities.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | Sort to use for celebrities returned in @Celebrities@ field. Specify
    -- @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by
    -- the time the celebrity was recognized.
    sortBy :: Core.Maybe CelebrityRecognitionSortBy,
    -- | Job identifier for the required celebrity recognition analysis. You can
    -- get the job identifer from a call to @StartCelebrityRecognition@.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCelebrityRecognition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCelebrityRecognition_nextToken' - If the previous response was incomplete (because there is more
-- recognized celebrities to retrieve), Amazon Rekognition Video returns a
-- pagination token in the response. You can use this pagination token to
-- retrieve the next set of celebrities.
--
-- 'maxResults', 'getCelebrityRecognition_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
--
-- 'sortBy', 'getCelebrityRecognition_sortBy' - Sort to use for celebrities returned in @Celebrities@ field. Specify
-- @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by
-- the time the celebrity was recognized.
--
-- 'jobId', 'getCelebrityRecognition_jobId' - Job identifier for the required celebrity recognition analysis. You can
-- get the job identifer from a call to @StartCelebrityRecognition@.
newGetCelebrityRecognition ::
  -- | 'jobId'
  Core.Text ->
  GetCelebrityRecognition
newGetCelebrityRecognition pJobId_ =
  GetCelebrityRecognition'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there is more
-- recognized celebrities to retrieve), Amazon Rekognition Video returns a
-- pagination token in the response. You can use this pagination token to
-- retrieve the next set of celebrities.
getCelebrityRecognition_nextToken :: Lens.Lens' GetCelebrityRecognition (Core.Maybe Core.Text)
getCelebrityRecognition_nextToken = Lens.lens (\GetCelebrityRecognition' {nextToken} -> nextToken) (\s@GetCelebrityRecognition' {} a -> s {nextToken = a} :: GetCelebrityRecognition)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getCelebrityRecognition_maxResults :: Lens.Lens' GetCelebrityRecognition (Core.Maybe Core.Natural)
getCelebrityRecognition_maxResults = Lens.lens (\GetCelebrityRecognition' {maxResults} -> maxResults) (\s@GetCelebrityRecognition' {} a -> s {maxResults = a} :: GetCelebrityRecognition)

-- | Sort to use for celebrities returned in @Celebrities@ field. Specify
-- @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by
-- the time the celebrity was recognized.
getCelebrityRecognition_sortBy :: Lens.Lens' GetCelebrityRecognition (Core.Maybe CelebrityRecognitionSortBy)
getCelebrityRecognition_sortBy = Lens.lens (\GetCelebrityRecognition' {sortBy} -> sortBy) (\s@GetCelebrityRecognition' {} a -> s {sortBy = a} :: GetCelebrityRecognition)

-- | Job identifier for the required celebrity recognition analysis. You can
-- get the job identifer from a call to @StartCelebrityRecognition@.
getCelebrityRecognition_jobId :: Lens.Lens' GetCelebrityRecognition Core.Text
getCelebrityRecognition_jobId = Lens.lens (\GetCelebrityRecognition' {jobId} -> jobId) (\s@GetCelebrityRecognition' {} a -> s {jobId = a} :: GetCelebrityRecognition)

instance Core.AWSRequest GetCelebrityRecognition where
  type
    AWSResponse GetCelebrityRecognition =
      GetCelebrityRecognitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCelebrityRecognitionResponse'
            Core.<$> (x Core..?> "StatusMessage")
            Core.<*> (x Core..?> "VideoMetadata")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "Celebrities" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCelebrityRecognition

instance Core.NFData GetCelebrityRecognition

instance Core.ToHeaders GetCelebrityRecognition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.GetCelebrityRecognition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCelebrityRecognition where
  toJSON GetCelebrityRecognition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy,
            Core.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath GetCelebrityRecognition where
  toPath = Core.const "/"

instance Core.ToQuery GetCelebrityRecognition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCelebrityRecognitionResponse' smart constructor.
data GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse'
  { -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Core.Maybe Core.Text,
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition Video operation.
    videoMetadata :: Core.Maybe VideoMetadata,
    -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of celebrities.
    nextToken :: Core.Maybe Core.Text,
    -- | The current status of the celebrity recognition job.
    jobStatus :: Core.Maybe VideoJobStatus,
    -- | Array of celebrities recognized in the video.
    celebrities :: Core.Maybe [CelebrityRecognition],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCelebrityRecognitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'getCelebrityRecognitionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'videoMetadata', 'getCelebrityRecognitionResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
--
-- 'nextToken', 'getCelebrityRecognitionResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of celebrities.
--
-- 'jobStatus', 'getCelebrityRecognitionResponse_jobStatus' - The current status of the celebrity recognition job.
--
-- 'celebrities', 'getCelebrityRecognitionResponse_celebrities' - Array of celebrities recognized in the video.
--
-- 'httpStatus', 'getCelebrityRecognitionResponse_httpStatus' - The response's http status code.
newGetCelebrityRecognitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCelebrityRecognitionResponse
newGetCelebrityRecognitionResponse pHttpStatus_ =
  GetCelebrityRecognitionResponse'
    { statusMessage =
        Core.Nothing,
      videoMetadata = Core.Nothing,
      nextToken = Core.Nothing,
      jobStatus = Core.Nothing,
      celebrities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getCelebrityRecognitionResponse_statusMessage :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe Core.Text)
getCelebrityRecognitionResponse_statusMessage = Lens.lens (\GetCelebrityRecognitionResponse' {statusMessage} -> statusMessage) (\s@GetCelebrityRecognitionResponse' {} a -> s {statusMessage = a} :: GetCelebrityRecognitionResponse)

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
getCelebrityRecognitionResponse_videoMetadata :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe VideoMetadata)
getCelebrityRecognitionResponse_videoMetadata = Lens.lens (\GetCelebrityRecognitionResponse' {videoMetadata} -> videoMetadata) (\s@GetCelebrityRecognitionResponse' {} a -> s {videoMetadata = a} :: GetCelebrityRecognitionResponse)

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of celebrities.
getCelebrityRecognitionResponse_nextToken :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe Core.Text)
getCelebrityRecognitionResponse_nextToken = Lens.lens (\GetCelebrityRecognitionResponse' {nextToken} -> nextToken) (\s@GetCelebrityRecognitionResponse' {} a -> s {nextToken = a} :: GetCelebrityRecognitionResponse)

-- | The current status of the celebrity recognition job.
getCelebrityRecognitionResponse_jobStatus :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe VideoJobStatus)
getCelebrityRecognitionResponse_jobStatus = Lens.lens (\GetCelebrityRecognitionResponse' {jobStatus} -> jobStatus) (\s@GetCelebrityRecognitionResponse' {} a -> s {jobStatus = a} :: GetCelebrityRecognitionResponse)

-- | Array of celebrities recognized in the video.
getCelebrityRecognitionResponse_celebrities :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe [CelebrityRecognition])
getCelebrityRecognitionResponse_celebrities = Lens.lens (\GetCelebrityRecognitionResponse' {celebrities} -> celebrities) (\s@GetCelebrityRecognitionResponse' {} a -> s {celebrities = a} :: GetCelebrityRecognitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCelebrityRecognitionResponse_httpStatus :: Lens.Lens' GetCelebrityRecognitionResponse Core.Int
getCelebrityRecognitionResponse_httpStatus = Lens.lens (\GetCelebrityRecognitionResponse' {httpStatus} -> httpStatus) (\s@GetCelebrityRecognitionResponse' {} a -> s {httpStatus = a} :: GetCelebrityRecognitionResponse)

instance Core.NFData GetCelebrityRecognitionResponse
