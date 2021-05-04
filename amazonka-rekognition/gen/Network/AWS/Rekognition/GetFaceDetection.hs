{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.GetFaceDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets face detection results for a Amazon Rekognition Video analysis
-- started by StartFaceDetection.
--
-- Face detection with Amazon Rekognition Video is an asynchronous
-- operation. You start face detection by calling StartFaceDetection which
-- returns a job identifier (@JobId@). When the face detection operation
-- finishes, Amazon Rekognition Video publishes a completion status to the
-- Amazon Simple Notification Service topic registered in the initial call
-- to @StartFaceDetection@. To get the results of the face detection
-- operation, first check that the status value published to the Amazon SNS
-- topic is @SUCCEEDED@. If so, call GetFaceDetection and pass the job
-- identifier (@JobId@) from the initial call to @StartFaceDetection@.
--
-- @GetFaceDetection@ returns an array of detected faces (@Faces@) sorted
-- by the time the faces were detected.
--
-- Use MaxResults parameter to limit the number of labels returned. If
-- there are more results than specified in @MaxResults@, the value of
-- @NextToken@ in the operation response contains a pagination token for
-- getting the next set of results. To get the next page of results, call
-- @GetFaceDetection@ and populate the @NextToken@ request parameter with
-- the token value returned from the previous call to @GetFaceDetection@.
module Network.AWS.Rekognition.GetFaceDetection
  ( -- * Creating a Request
    GetFaceDetection (..),
    newGetFaceDetection,

    -- * Request Lenses
    getFaceDetection_nextToken,
    getFaceDetection_maxResults,
    getFaceDetection_jobId,

    -- * Destructuring the Response
    GetFaceDetectionResponse (..),
    newGetFaceDetectionResponse,

    -- * Response Lenses
    getFaceDetectionResponse_statusMessage,
    getFaceDetectionResponse_videoMetadata,
    getFaceDetectionResponse_nextToken,
    getFaceDetectionResponse_jobStatus,
    getFaceDetectionResponse_faces,
    getFaceDetectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFaceDetection' smart constructor.
data GetFaceDetection = GetFaceDetection'
  { -- | If the previous response was incomplete (because there are more faces to
    -- retrieve), Amazon Rekognition Video returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- faces.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Unique identifier for the face detection job. The @JobId@ is returned
    -- from @StartFaceDetection@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFaceDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFaceDetection_nextToken' - If the previous response was incomplete (because there are more faces to
-- retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- faces.
--
-- 'maxResults', 'getFaceDetection_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
--
-- 'jobId', 'getFaceDetection_jobId' - Unique identifier for the face detection job. The @JobId@ is returned
-- from @StartFaceDetection@.
newGetFaceDetection ::
  -- | 'jobId'
  Prelude.Text ->
  GetFaceDetection
newGetFaceDetection pJobId_ =
  GetFaceDetection'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more faces to
-- retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- faces.
getFaceDetection_nextToken :: Lens.Lens' GetFaceDetection (Prelude.Maybe Prelude.Text)
getFaceDetection_nextToken = Lens.lens (\GetFaceDetection' {nextToken} -> nextToken) (\s@GetFaceDetection' {} a -> s {nextToken = a} :: GetFaceDetection)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getFaceDetection_maxResults :: Lens.Lens' GetFaceDetection (Prelude.Maybe Prelude.Natural)
getFaceDetection_maxResults = Lens.lens (\GetFaceDetection' {maxResults} -> maxResults) (\s@GetFaceDetection' {} a -> s {maxResults = a} :: GetFaceDetection)

-- | Unique identifier for the face detection job. The @JobId@ is returned
-- from @StartFaceDetection@.
getFaceDetection_jobId :: Lens.Lens' GetFaceDetection Prelude.Text
getFaceDetection_jobId = Lens.lens (\GetFaceDetection' {jobId} -> jobId) (\s@GetFaceDetection' {} a -> s {jobId = a} :: GetFaceDetection)

instance Prelude.AWSRequest GetFaceDetection where
  type Rs GetFaceDetection = GetFaceDetectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFaceDetectionResponse'
            Prelude.<$> (x Prelude..?> "StatusMessage")
            Prelude.<*> (x Prelude..?> "VideoMetadata")
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "JobStatus")
            Prelude.<*> (x Prelude..?> "Faces" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFaceDetection

instance Prelude.NFData GetFaceDetection

instance Prelude.ToHeaders GetFaceDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.GetFaceDetection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetFaceDetection where
  toJSON GetFaceDetection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just ("JobId" Prelude..= jobId)
          ]
      )

instance Prelude.ToPath GetFaceDetection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetFaceDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFaceDetectionResponse' smart constructor.
data GetFaceDetectionResponse = GetFaceDetectionResponse'
  { -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | If the response is truncated, Amazon Rekognition returns this token that
    -- you can use in the subsequent request to retrieve the next set of faces.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the face detection job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | An array of faces detected in the video. Each element contains a
    -- detected face\'s details and the time, in milliseconds from the start of
    -- the video, the face was detected.
    faces :: Prelude.Maybe [FaceDetection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFaceDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'getFaceDetectionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'videoMetadata', 'getFaceDetectionResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
--
-- 'nextToken', 'getFaceDetectionResponse_nextToken' - If the response is truncated, Amazon Rekognition returns this token that
-- you can use in the subsequent request to retrieve the next set of faces.
--
-- 'jobStatus', 'getFaceDetectionResponse_jobStatus' - The current status of the face detection job.
--
-- 'faces', 'getFaceDetectionResponse_faces' - An array of faces detected in the video. Each element contains a
-- detected face\'s details and the time, in milliseconds from the start of
-- the video, the face was detected.
--
-- 'httpStatus', 'getFaceDetectionResponse_httpStatus' - The response's http status code.
newGetFaceDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFaceDetectionResponse
newGetFaceDetectionResponse pHttpStatus_ =
  GetFaceDetectionResponse'
    { statusMessage =
        Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      faces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getFaceDetectionResponse_statusMessage :: Lens.Lens' GetFaceDetectionResponse (Prelude.Maybe Prelude.Text)
getFaceDetectionResponse_statusMessage = Lens.lens (\GetFaceDetectionResponse' {statusMessage} -> statusMessage) (\s@GetFaceDetectionResponse' {} a -> s {statusMessage = a} :: GetFaceDetectionResponse)

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
getFaceDetectionResponse_videoMetadata :: Lens.Lens' GetFaceDetectionResponse (Prelude.Maybe VideoMetadata)
getFaceDetectionResponse_videoMetadata = Lens.lens (\GetFaceDetectionResponse' {videoMetadata} -> videoMetadata) (\s@GetFaceDetectionResponse' {} a -> s {videoMetadata = a} :: GetFaceDetectionResponse)

-- | If the response is truncated, Amazon Rekognition returns this token that
-- you can use in the subsequent request to retrieve the next set of faces.
getFaceDetectionResponse_nextToken :: Lens.Lens' GetFaceDetectionResponse (Prelude.Maybe Prelude.Text)
getFaceDetectionResponse_nextToken = Lens.lens (\GetFaceDetectionResponse' {nextToken} -> nextToken) (\s@GetFaceDetectionResponse' {} a -> s {nextToken = a} :: GetFaceDetectionResponse)

-- | The current status of the face detection job.
getFaceDetectionResponse_jobStatus :: Lens.Lens' GetFaceDetectionResponse (Prelude.Maybe VideoJobStatus)
getFaceDetectionResponse_jobStatus = Lens.lens (\GetFaceDetectionResponse' {jobStatus} -> jobStatus) (\s@GetFaceDetectionResponse' {} a -> s {jobStatus = a} :: GetFaceDetectionResponse)

-- | An array of faces detected in the video. Each element contains a
-- detected face\'s details and the time, in milliseconds from the start of
-- the video, the face was detected.
getFaceDetectionResponse_faces :: Lens.Lens' GetFaceDetectionResponse (Prelude.Maybe [FaceDetection])
getFaceDetectionResponse_faces = Lens.lens (\GetFaceDetectionResponse' {faces} -> faces) (\s@GetFaceDetectionResponse' {} a -> s {faces = a} :: GetFaceDetectionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getFaceDetectionResponse_httpStatus :: Lens.Lens' GetFaceDetectionResponse Prelude.Int
getFaceDetectionResponse_httpStatus = Lens.lens (\GetFaceDetectionResponse' {httpStatus} -> httpStatus) (\s@GetFaceDetectionResponse' {} a -> s {httpStatus = a} :: GetFaceDetectionResponse)

instance Prelude.NFData GetFaceDetectionResponse
