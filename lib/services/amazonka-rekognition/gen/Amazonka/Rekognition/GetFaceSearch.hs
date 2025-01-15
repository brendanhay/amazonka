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
-- Module      : Amazonka.Rekognition.GetFaceSearch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the face search results for Amazon Rekognition Video face search
-- started by StartFaceSearch. The search returns faces in a collection
-- that match the faces of persons detected in a video. It also includes
-- the time(s) that faces are matched in the video.
--
-- Face search in a video is an asynchronous operation. You start face
-- search by calling to StartFaceSearch which returns a job identifier
-- (@JobId@). When the search operation finishes, Amazon Rekognition Video
-- publishes a completion status to the Amazon Simple Notification Service
-- topic registered in the initial call to @StartFaceSearch@. To get the
-- search results, first check that the status value published to the
-- Amazon SNS topic is @SUCCEEDED@. If so, call @GetFaceSearch@ and pass
-- the job identifier (@JobId@) from the initial call to @StartFaceSearch@.
--
-- For more information, see Searching Faces in a Collection in the Amazon
-- Rekognition Developer Guide.
--
-- The search results are retured in an array, @Persons@, of PersonMatch
-- objects. Each@PersonMatch@ element contains details about the matching
-- faces in the input collection, person information (facial attributes,
-- bounding boxes, and person identifer) for the matched person, and the
-- time the person was matched in the video.
--
-- @GetFaceSearch@ only returns the default facial attributes
-- (@BoundingBox@, @Confidence@, @Landmarks@, @Pose@, and @Quality@). The
-- other facial attributes listed in the @Face@ object of the following
-- response syntax are not returned. For more information, see FaceDetail
-- in the Amazon Rekognition Developer Guide.
--
-- By default, the @Persons@ array is sorted by the time, in milliseconds
-- from the start of the video, persons are matched. You can also sort by
-- persons by specifying @INDEX@ for the @SORTBY@ input parameter.
module Amazonka.Rekognition.GetFaceSearch
  ( -- * Creating a Request
    GetFaceSearch (..),
    newGetFaceSearch,

    -- * Request Lenses
    getFaceSearch_maxResults,
    getFaceSearch_nextToken,
    getFaceSearch_sortBy,
    getFaceSearch_jobId,

    -- * Destructuring the Response
    GetFaceSearchResponse (..),
    newGetFaceSearchResponse,

    -- * Response Lenses
    getFaceSearchResponse_jobStatus,
    getFaceSearchResponse_nextToken,
    getFaceSearchResponse_persons,
    getFaceSearchResponse_statusMessage,
    getFaceSearchResponse_videoMetadata,
    getFaceSearchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFaceSearch' smart constructor.
data GetFaceSearch = GetFaceSearch'
  { -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more search
    -- results to retrieve), Amazon Rekognition Video returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of search results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group
    -- faces by the time that they are recognized. Use @INDEX@ to sort by
    -- recognized faces.
    sortBy :: Prelude.Maybe FaceSearchSortBy,
    -- | The job identifer for the search request. You get the job identifier
    -- from an initial call to @StartFaceSearch@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFaceSearch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getFaceSearch_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
--
-- 'nextToken', 'getFaceSearch_nextToken' - If the previous response was incomplete (because there is more search
-- results to retrieve), Amazon Rekognition Video returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of search results.
--
-- 'sortBy', 'getFaceSearch_sortBy' - Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group
-- faces by the time that they are recognized. Use @INDEX@ to sort by
-- recognized faces.
--
-- 'jobId', 'getFaceSearch_jobId' - The job identifer for the search request. You get the job identifier
-- from an initial call to @StartFaceSearch@.
newGetFaceSearch ::
  -- | 'jobId'
  Prelude.Text ->
  GetFaceSearch
newGetFaceSearch pJobId_ =
  GetFaceSearch'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getFaceSearch_maxResults :: Lens.Lens' GetFaceSearch (Prelude.Maybe Prelude.Natural)
getFaceSearch_maxResults = Lens.lens (\GetFaceSearch' {maxResults} -> maxResults) (\s@GetFaceSearch' {} a -> s {maxResults = a} :: GetFaceSearch)

-- | If the previous response was incomplete (because there is more search
-- results to retrieve), Amazon Rekognition Video returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of search results.
getFaceSearch_nextToken :: Lens.Lens' GetFaceSearch (Prelude.Maybe Prelude.Text)
getFaceSearch_nextToken = Lens.lens (\GetFaceSearch' {nextToken} -> nextToken) (\s@GetFaceSearch' {} a -> s {nextToken = a} :: GetFaceSearch)

-- | Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group
-- faces by the time that they are recognized. Use @INDEX@ to sort by
-- recognized faces.
getFaceSearch_sortBy :: Lens.Lens' GetFaceSearch (Prelude.Maybe FaceSearchSortBy)
getFaceSearch_sortBy = Lens.lens (\GetFaceSearch' {sortBy} -> sortBy) (\s@GetFaceSearch' {} a -> s {sortBy = a} :: GetFaceSearch)

-- | The job identifer for the search request. You get the job identifier
-- from an initial call to @StartFaceSearch@.
getFaceSearch_jobId :: Lens.Lens' GetFaceSearch Prelude.Text
getFaceSearch_jobId = Lens.lens (\GetFaceSearch' {jobId} -> jobId) (\s@GetFaceSearch' {} a -> s {jobId = a} :: GetFaceSearch)

instance Core.AWSRequest GetFaceSearch where
  type
    AWSResponse GetFaceSearch =
      GetFaceSearchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFaceSearchResponse'
            Prelude.<$> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Persons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "VideoMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFaceSearch where
  hashWithSalt _salt GetFaceSearch' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetFaceSearch where
  rnf GetFaceSearch' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf sortBy `Prelude.seq`
          Prelude.rnf jobId

instance Data.ToHeaders GetFaceSearch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.GetFaceSearch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFaceSearch where
  toJSON GetFaceSearch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetFaceSearch where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFaceSearch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFaceSearchResponse' smart constructor.
data GetFaceSearchResponse = GetFaceSearchResponse'
  { -- | The current status of the face search job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of search results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of persons, PersonMatch, in the video whose face(s) match the
    -- face(s) in an Amazon Rekognition collection. It also includes time
    -- information for when persons are matched in the video. You specify the
    -- input collection in an initial call to @StartFaceSearch@. Each @Persons@
    -- element includes a time the person was matched, face match details
    -- (@FaceMatches@) for matching faces in the collection, and person
    -- information (@Person@) for the matched person.
    persons :: Prelude.Maybe [PersonMatch],
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about a video that Amazon Rekognition analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition Video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFaceSearchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'getFaceSearchResponse_jobStatus' - The current status of the face search job.
--
-- 'nextToken', 'getFaceSearchResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of search results.
--
-- 'persons', 'getFaceSearchResponse_persons' - An array of persons, PersonMatch, in the video whose face(s) match the
-- face(s) in an Amazon Rekognition collection. It also includes time
-- information for when persons are matched in the video. You specify the
-- input collection in an initial call to @StartFaceSearch@. Each @Persons@
-- element includes a time the person was matched, face match details
-- (@FaceMatches@) for matching faces in the collection, and person
-- information (@Person@) for the matched person.
--
-- 'statusMessage', 'getFaceSearchResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'videoMetadata', 'getFaceSearchResponse_videoMetadata' - Information about a video that Amazon Rekognition analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
--
-- 'httpStatus', 'getFaceSearchResponse_httpStatus' - The response's http status code.
newGetFaceSearchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFaceSearchResponse
newGetFaceSearchResponse pHttpStatus_ =
  GetFaceSearchResponse'
    { jobStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      persons = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the face search job.
getFaceSearchResponse_jobStatus :: Lens.Lens' GetFaceSearchResponse (Prelude.Maybe VideoJobStatus)
getFaceSearchResponse_jobStatus = Lens.lens (\GetFaceSearchResponse' {jobStatus} -> jobStatus) (\s@GetFaceSearchResponse' {} a -> s {jobStatus = a} :: GetFaceSearchResponse)

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of search results.
getFaceSearchResponse_nextToken :: Lens.Lens' GetFaceSearchResponse (Prelude.Maybe Prelude.Text)
getFaceSearchResponse_nextToken = Lens.lens (\GetFaceSearchResponse' {nextToken} -> nextToken) (\s@GetFaceSearchResponse' {} a -> s {nextToken = a} :: GetFaceSearchResponse)

-- | An array of persons, PersonMatch, in the video whose face(s) match the
-- face(s) in an Amazon Rekognition collection. It also includes time
-- information for when persons are matched in the video. You specify the
-- input collection in an initial call to @StartFaceSearch@. Each @Persons@
-- element includes a time the person was matched, face match details
-- (@FaceMatches@) for matching faces in the collection, and person
-- information (@Person@) for the matched person.
getFaceSearchResponse_persons :: Lens.Lens' GetFaceSearchResponse (Prelude.Maybe [PersonMatch])
getFaceSearchResponse_persons = Lens.lens (\GetFaceSearchResponse' {persons} -> persons) (\s@GetFaceSearchResponse' {} a -> s {persons = a} :: GetFaceSearchResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getFaceSearchResponse_statusMessage :: Lens.Lens' GetFaceSearchResponse (Prelude.Maybe Prelude.Text)
getFaceSearchResponse_statusMessage = Lens.lens (\GetFaceSearchResponse' {statusMessage} -> statusMessage) (\s@GetFaceSearchResponse' {} a -> s {statusMessage = a} :: GetFaceSearchResponse)

-- | Information about a video that Amazon Rekognition analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition Video operation.
getFaceSearchResponse_videoMetadata :: Lens.Lens' GetFaceSearchResponse (Prelude.Maybe VideoMetadata)
getFaceSearchResponse_videoMetadata = Lens.lens (\GetFaceSearchResponse' {videoMetadata} -> videoMetadata) (\s@GetFaceSearchResponse' {} a -> s {videoMetadata = a} :: GetFaceSearchResponse)

-- | The response's http status code.
getFaceSearchResponse_httpStatus :: Lens.Lens' GetFaceSearchResponse Prelude.Int
getFaceSearchResponse_httpStatus = Lens.lens (\GetFaceSearchResponse' {httpStatus} -> httpStatus) (\s@GetFaceSearchResponse' {} a -> s {httpStatus = a} :: GetFaceSearchResponse)

instance Prelude.NFData GetFaceSearchResponse where
  rnf GetFaceSearchResponse' {..} =
    Prelude.rnf jobStatus `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf persons `Prelude.seq`
          Prelude.rnf statusMessage `Prelude.seq`
            Prelude.rnf videoMetadata `Prelude.seq`
              Prelude.rnf httpStatus
