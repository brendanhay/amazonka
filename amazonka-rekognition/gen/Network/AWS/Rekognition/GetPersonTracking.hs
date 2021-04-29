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
-- Module      : Network.AWS.Rekognition.GetPersonTracking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the path tracking results of a Amazon Rekognition Video analysis
-- started by StartPersonTracking.
--
-- The person path tracking operation is started by a call to
-- @StartPersonTracking@ which returns a job identifier (@JobId@). When the
-- operation finishes, Amazon Rekognition Video publishes a completion
-- status to the Amazon Simple Notification Service topic registered in the
-- initial call to @StartPersonTracking@.
--
-- To get the results of the person path tracking operation, first check
-- that the status value published to the Amazon SNS topic is @SUCCEEDED@.
-- If so, call GetPersonTracking and pass the job identifier (@JobId@) from
-- the initial call to @StartPersonTracking@.
--
-- @GetPersonTracking@ returns an array, @Persons@, of tracked persons and
-- the time(s) their paths were tracked in the video.
--
-- @GetPersonTracking@ only returns the default facial attributes
-- (@BoundingBox@, @Confidence@, @Landmarks@, @Pose@, and @Quality@). The
-- other facial attributes listed in the @Face@ object of the following
-- response syntax are not returned.
--
-- For more information, see FaceDetail in the Amazon Rekognition Developer
-- Guide.
--
-- By default, the array is sorted by the time(s) a person\'s path is
-- tracked in the video. You can sort by tracked persons by specifying
-- @INDEX@ for the @SortBy@ input parameter.
--
-- Use the @MaxResults@ parameter to limit the number of items returned. If
-- there are more results than specified in @MaxResults@, the value of
-- @NextToken@ in the operation response contains a pagination token for
-- getting the next set of results. To get the next page of results, call
-- @GetPersonTracking@ and populate the @NextToken@ request parameter with
-- the token value returned from the previous call to @GetPersonTracking@.
module Network.AWS.Rekognition.GetPersonTracking
  ( -- * Creating a Request
    GetPersonTracking (..),
    newGetPersonTracking,

    -- * Request Lenses
    getPersonTracking_nextToken,
    getPersonTracking_maxResults,
    getPersonTracking_sortBy,
    getPersonTracking_jobId,

    -- * Destructuring the Response
    GetPersonTrackingResponse (..),
    newGetPersonTrackingResponse,

    -- * Response Lenses
    getPersonTrackingResponse_statusMessage,
    getPersonTrackingResponse_videoMetadata,
    getPersonTrackingResponse_nextToken,
    getPersonTrackingResponse_jobStatus,
    getPersonTrackingResponse_persons,
    getPersonTrackingResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPersonTracking' smart constructor.
data GetPersonTracking = GetPersonTracking'
  { -- | If the previous response was incomplete (because there are more persons
    -- to retrieve), Amazon Rekognition Video returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- persons.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort
    -- array elements by the time persons are detected. Use @INDEX@ to sort by
    -- the tracked persons. If you sort by @INDEX@, the array elements for each
    -- person are sorted by detection confidence. The default sort is by
    -- @TIMESTAMP@.
    sortBy :: Prelude.Maybe PersonTrackingSortBy,
    -- | The identifier for a job that tracks persons in a video. You get the
    -- @JobId@ from a call to @StartPersonTracking@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetPersonTracking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPersonTracking_nextToken' - If the previous response was incomplete (because there are more persons
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- persons.
--
-- 'maxResults', 'getPersonTracking_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
--
-- 'sortBy', 'getPersonTracking_sortBy' - Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort
-- array elements by the time persons are detected. Use @INDEX@ to sort by
-- the tracked persons. If you sort by @INDEX@, the array elements for each
-- person are sorted by detection confidence. The default sort is by
-- @TIMESTAMP@.
--
-- 'jobId', 'getPersonTracking_jobId' - The identifier for a job that tracks persons in a video. You get the
-- @JobId@ from a call to @StartPersonTracking@.
newGetPersonTracking ::
  -- | 'jobId'
  Prelude.Text ->
  GetPersonTracking
newGetPersonTracking pJobId_ =
  GetPersonTracking'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more persons
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- persons.
getPersonTracking_nextToken :: Lens.Lens' GetPersonTracking (Prelude.Maybe Prelude.Text)
getPersonTracking_nextToken = Lens.lens (\GetPersonTracking' {nextToken} -> nextToken) (\s@GetPersonTracking' {} a -> s {nextToken = a} :: GetPersonTracking)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getPersonTracking_maxResults :: Lens.Lens' GetPersonTracking (Prelude.Maybe Prelude.Natural)
getPersonTracking_maxResults = Lens.lens (\GetPersonTracking' {maxResults} -> maxResults) (\s@GetPersonTracking' {} a -> s {maxResults = a} :: GetPersonTracking)

-- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort
-- array elements by the time persons are detected. Use @INDEX@ to sort by
-- the tracked persons. If you sort by @INDEX@, the array elements for each
-- person are sorted by detection confidence. The default sort is by
-- @TIMESTAMP@.
getPersonTracking_sortBy :: Lens.Lens' GetPersonTracking (Prelude.Maybe PersonTrackingSortBy)
getPersonTracking_sortBy = Lens.lens (\GetPersonTracking' {sortBy} -> sortBy) (\s@GetPersonTracking' {} a -> s {sortBy = a} :: GetPersonTracking)

-- | The identifier for a job that tracks persons in a video. You get the
-- @JobId@ from a call to @StartPersonTracking@.
getPersonTracking_jobId :: Lens.Lens' GetPersonTracking Prelude.Text
getPersonTracking_jobId = Lens.lens (\GetPersonTracking' {jobId} -> jobId) (\s@GetPersonTracking' {} a -> s {jobId = a} :: GetPersonTracking)

instance Prelude.AWSRequest GetPersonTracking where
  type Rs GetPersonTracking = GetPersonTrackingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPersonTrackingResponse'
            Prelude.<$> (x Prelude..?> "StatusMessage")
            Prelude.<*> (x Prelude..?> "VideoMetadata")
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "JobStatus")
            Prelude.<*> (x Prelude..?> "Persons" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPersonTracking

instance Prelude.NFData GetPersonTracking

instance Prelude.ToHeaders GetPersonTracking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.GetPersonTracking" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetPersonTracking where
  toJSON GetPersonTracking' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SortBy" Prelude..=) Prelude.<$> sortBy,
            Prelude.Just ("JobId" Prelude..= jobId)
          ]
      )

instance Prelude.ToPath GetPersonTracking where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetPersonTracking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPersonTrackingResponse' smart constructor.
data GetPersonTrackingResponse = GetPersonTrackingResponse'
  { -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition Video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of persons.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the person tracking job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | An array of the persons detected in the video and the time(s) their path
    -- was tracked throughout the video. An array element will exist for each
    -- time a person\'s path is tracked.
    persons :: Prelude.Maybe [PersonDetection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetPersonTrackingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'getPersonTrackingResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'videoMetadata', 'getPersonTrackingResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
--
-- 'nextToken', 'getPersonTrackingResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of persons.
--
-- 'jobStatus', 'getPersonTrackingResponse_jobStatus' - The current status of the person tracking job.
--
-- 'persons', 'getPersonTrackingResponse_persons' - An array of the persons detected in the video and the time(s) their path
-- was tracked throughout the video. An array element will exist for each
-- time a person\'s path is tracked.
--
-- 'httpStatus', 'getPersonTrackingResponse_httpStatus' - The response's http status code.
newGetPersonTrackingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPersonTrackingResponse
newGetPersonTrackingResponse pHttpStatus_ =
  GetPersonTrackingResponse'
    { statusMessage =
        Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      persons = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getPersonTrackingResponse_statusMessage :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe Prelude.Text)
getPersonTrackingResponse_statusMessage = Lens.lens (\GetPersonTrackingResponse' {statusMessage} -> statusMessage) (\s@GetPersonTrackingResponse' {} a -> s {statusMessage = a} :: GetPersonTrackingResponse)

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
getPersonTrackingResponse_videoMetadata :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe VideoMetadata)
getPersonTrackingResponse_videoMetadata = Lens.lens (\GetPersonTrackingResponse' {videoMetadata} -> videoMetadata) (\s@GetPersonTrackingResponse' {} a -> s {videoMetadata = a} :: GetPersonTrackingResponse)

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of persons.
getPersonTrackingResponse_nextToken :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe Prelude.Text)
getPersonTrackingResponse_nextToken = Lens.lens (\GetPersonTrackingResponse' {nextToken} -> nextToken) (\s@GetPersonTrackingResponse' {} a -> s {nextToken = a} :: GetPersonTrackingResponse)

-- | The current status of the person tracking job.
getPersonTrackingResponse_jobStatus :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe VideoJobStatus)
getPersonTrackingResponse_jobStatus = Lens.lens (\GetPersonTrackingResponse' {jobStatus} -> jobStatus) (\s@GetPersonTrackingResponse' {} a -> s {jobStatus = a} :: GetPersonTrackingResponse)

-- | An array of the persons detected in the video and the time(s) their path
-- was tracked throughout the video. An array element will exist for each
-- time a person\'s path is tracked.
getPersonTrackingResponse_persons :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe [PersonDetection])
getPersonTrackingResponse_persons = Lens.lens (\GetPersonTrackingResponse' {persons} -> persons) (\s@GetPersonTrackingResponse' {} a -> s {persons = a} :: GetPersonTrackingResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getPersonTrackingResponse_httpStatus :: Lens.Lens' GetPersonTrackingResponse Prelude.Int
getPersonTrackingResponse_httpStatus = Lens.lens (\GetPersonTrackingResponse' {httpStatus} -> httpStatus) (\s@GetPersonTrackingResponse' {} a -> s {httpStatus = a} :: GetPersonTrackingResponse)

instance Prelude.NFData GetPersonTrackingResponse
