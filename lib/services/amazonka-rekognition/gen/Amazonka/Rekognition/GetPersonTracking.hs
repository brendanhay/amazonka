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
-- Module      : Amazonka.Rekognition.GetPersonTracking
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Rekognition.GetPersonTracking
  ( -- * Creating a Request
    GetPersonTracking (..),
    newGetPersonTracking,

    -- * Request Lenses
    getPersonTracking_nextToken,
    getPersonTracking_sortBy,
    getPersonTracking_maxResults,
    getPersonTracking_jobId,

    -- * Destructuring the Response
    GetPersonTrackingResponse (..),
    newGetPersonTrackingResponse,

    -- * Response Lenses
    getPersonTrackingResponse_nextToken,
    getPersonTrackingResponse_jobStatus,
    getPersonTrackingResponse_videoMetadata,
    getPersonTrackingResponse_persons,
    getPersonTrackingResponse_statusMessage,
    getPersonTrackingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPersonTracking' smart constructor.
data GetPersonTracking = GetPersonTracking'
  { -- | If the previous response was incomplete (because there are more persons
    -- to retrieve), Amazon Rekognition Video returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- persons.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort
    -- array elements by the time persons are detected. Use @INDEX@ to sort by
    -- the tracked persons. If you sort by @INDEX@, the array elements for each
    -- person are sorted by detection confidence. The default sort is by
    -- @TIMESTAMP@.
    sortBy :: Prelude.Maybe PersonTrackingSortBy,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for a job that tracks persons in a video. You get the
    -- @JobId@ from a call to @StartPersonTracking@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'sortBy', 'getPersonTracking_sortBy' - Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort
-- array elements by the time persons are detected. Use @INDEX@ to sort by
-- the tracked persons. If you sort by @INDEX@, the array elements for each
-- person are sorted by detection confidence. The default sort is by
-- @TIMESTAMP@.
--
-- 'maxResults', 'getPersonTracking_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
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
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more persons
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- persons.
getPersonTracking_nextToken :: Lens.Lens' GetPersonTracking (Prelude.Maybe Prelude.Text)
getPersonTracking_nextToken = Lens.lens (\GetPersonTracking' {nextToken} -> nextToken) (\s@GetPersonTracking' {} a -> s {nextToken = a} :: GetPersonTracking)

-- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort
-- array elements by the time persons are detected. Use @INDEX@ to sort by
-- the tracked persons. If you sort by @INDEX@, the array elements for each
-- person are sorted by detection confidence. The default sort is by
-- @TIMESTAMP@.
getPersonTracking_sortBy :: Lens.Lens' GetPersonTracking (Prelude.Maybe PersonTrackingSortBy)
getPersonTracking_sortBy = Lens.lens (\GetPersonTracking' {sortBy} -> sortBy) (\s@GetPersonTracking' {} a -> s {sortBy = a} :: GetPersonTracking)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getPersonTracking_maxResults :: Lens.Lens' GetPersonTracking (Prelude.Maybe Prelude.Natural)
getPersonTracking_maxResults = Lens.lens (\GetPersonTracking' {maxResults} -> maxResults) (\s@GetPersonTracking' {} a -> s {maxResults = a} :: GetPersonTracking)

-- | The identifier for a job that tracks persons in a video. You get the
-- @JobId@ from a call to @StartPersonTracking@.
getPersonTracking_jobId :: Lens.Lens' GetPersonTracking Prelude.Text
getPersonTracking_jobId = Lens.lens (\GetPersonTracking' {jobId} -> jobId) (\s@GetPersonTracking' {} a -> s {jobId = a} :: GetPersonTracking)

instance Core.AWSRequest GetPersonTracking where
  type
    AWSResponse GetPersonTracking =
      GetPersonTrackingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPersonTrackingResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "VideoMetadata")
            Prelude.<*> (x Core..?> "Persons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPersonTracking where
  hashWithSalt _salt GetPersonTracking' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetPersonTracking where
  rnf GetPersonTracking' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobId

instance Core.ToHeaders GetPersonTracking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.GetPersonTracking" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPersonTracking where
  toJSON GetPersonTracking' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath GetPersonTracking where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPersonTracking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPersonTrackingResponse' smart constructor.
data GetPersonTrackingResponse = GetPersonTrackingResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of persons.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the person tracking job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition Video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | An array of the persons detected in the video and the time(s) their path
    -- was tracked throughout the video. An array element will exist for each
    -- time a person\'s path is tracked.
    persons :: Prelude.Maybe [PersonDetection],
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPersonTrackingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPersonTrackingResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of persons.
--
-- 'jobStatus', 'getPersonTrackingResponse_jobStatus' - The current status of the person tracking job.
--
-- 'videoMetadata', 'getPersonTrackingResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
--
-- 'persons', 'getPersonTrackingResponse_persons' - An array of the persons detected in the video and the time(s) their path
-- was tracked throughout the video. An array element will exist for each
-- time a person\'s path is tracked.
--
-- 'statusMessage', 'getPersonTrackingResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'httpStatus', 'getPersonTrackingResponse_httpStatus' - The response's http status code.
newGetPersonTrackingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPersonTrackingResponse
newGetPersonTrackingResponse pHttpStatus_ =
  GetPersonTrackingResponse'
    { nextToken =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      persons = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of persons.
getPersonTrackingResponse_nextToken :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe Prelude.Text)
getPersonTrackingResponse_nextToken = Lens.lens (\GetPersonTrackingResponse' {nextToken} -> nextToken) (\s@GetPersonTrackingResponse' {} a -> s {nextToken = a} :: GetPersonTrackingResponse)

-- | The current status of the person tracking job.
getPersonTrackingResponse_jobStatus :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe VideoJobStatus)
getPersonTrackingResponse_jobStatus = Lens.lens (\GetPersonTrackingResponse' {jobStatus} -> jobStatus) (\s@GetPersonTrackingResponse' {} a -> s {jobStatus = a} :: GetPersonTrackingResponse)

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
getPersonTrackingResponse_videoMetadata :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe VideoMetadata)
getPersonTrackingResponse_videoMetadata = Lens.lens (\GetPersonTrackingResponse' {videoMetadata} -> videoMetadata) (\s@GetPersonTrackingResponse' {} a -> s {videoMetadata = a} :: GetPersonTrackingResponse)

-- | An array of the persons detected in the video and the time(s) their path
-- was tracked throughout the video. An array element will exist for each
-- time a person\'s path is tracked.
getPersonTrackingResponse_persons :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe [PersonDetection])
getPersonTrackingResponse_persons = Lens.lens (\GetPersonTrackingResponse' {persons} -> persons) (\s@GetPersonTrackingResponse' {} a -> s {persons = a} :: GetPersonTrackingResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getPersonTrackingResponse_statusMessage :: Lens.Lens' GetPersonTrackingResponse (Prelude.Maybe Prelude.Text)
getPersonTrackingResponse_statusMessage = Lens.lens (\GetPersonTrackingResponse' {statusMessage} -> statusMessage) (\s@GetPersonTrackingResponse' {} a -> s {statusMessage = a} :: GetPersonTrackingResponse)

-- | The response's http status code.
getPersonTrackingResponse_httpStatus :: Lens.Lens' GetPersonTrackingResponse Prelude.Int
getPersonTrackingResponse_httpStatus = Lens.lens (\GetPersonTrackingResponse' {httpStatus} -> httpStatus) (\s@GetPersonTrackingResponse' {} a -> s {httpStatus = a} :: GetPersonTrackingResponse)

instance Prelude.NFData GetPersonTrackingResponse where
  rnf GetPersonTrackingResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf videoMetadata
      `Prelude.seq` Prelude.rnf persons
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
