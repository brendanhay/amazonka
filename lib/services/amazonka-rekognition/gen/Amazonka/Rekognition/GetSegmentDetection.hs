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
-- Module      : Amazonka.Rekognition.GetSegmentDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the segment detection results of a Amazon Rekognition Video
-- analysis started by StartSegmentDetection.
--
-- Segment detection with Amazon Rekognition Video is an asynchronous
-- operation. You start segment detection by calling StartSegmentDetection
-- which returns a job identifier (@JobId@). When the segment detection
-- operation finishes, Amazon Rekognition publishes a completion status to
-- the Amazon Simple Notification Service topic registered in the initial
-- call to @StartSegmentDetection@. To get the results of the segment
-- detection operation, first check that the status value published to the
-- Amazon SNS topic is @SUCCEEDED@. if so, call @GetSegmentDetection@ and
-- pass the job identifier (@JobId@) from the initial call of
-- @StartSegmentDetection@.
--
-- @GetSegmentDetection@ returns detected segments in an array (@Segments@)
-- of SegmentDetection objects. @Segments@ is sorted by the segment types
-- specified in the @SegmentTypes@ input parameter of
-- @StartSegmentDetection@. Each element of the array includes the detected
-- segment, the precentage confidence in the acuracy of the detected
-- segment, the type of the segment, and the frame in which the segment was
-- detected.
--
-- Use @SelectedSegmentTypes@ to find out the type of segment detection
-- requested in the call to @StartSegmentDetection@.
--
-- Use the @MaxResults@ parameter to limit the number of segment detections
-- returned. If there are more results than specified in @MaxResults@, the
-- value of @NextToken@ in the operation response contains a pagination
-- token for getting the next set of results. To get the next page of
-- results, call @GetSegmentDetection@ and populate the @NextToken@ request
-- parameter with the token value returned from the previous call to
-- @GetSegmentDetection@.
--
-- For more information, see Detecting video segments in stored video in
-- the Amazon Rekognition Developer Guide.
module Amazonka.Rekognition.GetSegmentDetection
  ( -- * Creating a Request
    GetSegmentDetection (..),
    newGetSegmentDetection,

    -- * Request Lenses
    getSegmentDetection_maxResults,
    getSegmentDetection_nextToken,
    getSegmentDetection_jobId,

    -- * Destructuring the Response
    GetSegmentDetectionResponse (..),
    newGetSegmentDetectionResponse,

    -- * Response Lenses
    getSegmentDetectionResponse_audioMetadata,
    getSegmentDetectionResponse_jobStatus,
    getSegmentDetectionResponse_nextToken,
    getSegmentDetectionResponse_segments,
    getSegmentDetectionResponse_selectedSegmentTypes,
    getSegmentDetectionResponse_statusMessage,
    getSegmentDetectionResponse_videoMetadata,
    getSegmentDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSegmentDetection' smart constructor.
data GetSegmentDetection = GetSegmentDetection'
  { -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of text.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Job identifier for the text detection operation for which you want
    -- results returned. You get the job identifer from an initial call to
    -- @StartSegmentDetection@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getSegmentDetection_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000.
--
-- 'nextToken', 'getSegmentDetection_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of text.
--
-- 'jobId', 'getSegmentDetection_jobId' - Job identifier for the text detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartSegmentDetection@.
newGetSegmentDetection ::
  -- | 'jobId'
  Prelude.Text ->
  GetSegmentDetection
newGetSegmentDetection pJobId_ =
  GetSegmentDetection'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000.
getSegmentDetection_maxResults :: Lens.Lens' GetSegmentDetection (Prelude.Maybe Prelude.Natural)
getSegmentDetection_maxResults = Lens.lens (\GetSegmentDetection' {maxResults} -> maxResults) (\s@GetSegmentDetection' {} a -> s {maxResults = a} :: GetSegmentDetection)

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of text.
getSegmentDetection_nextToken :: Lens.Lens' GetSegmentDetection (Prelude.Maybe Prelude.Text)
getSegmentDetection_nextToken = Lens.lens (\GetSegmentDetection' {nextToken} -> nextToken) (\s@GetSegmentDetection' {} a -> s {nextToken = a} :: GetSegmentDetection)

-- | Job identifier for the text detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartSegmentDetection@.
getSegmentDetection_jobId :: Lens.Lens' GetSegmentDetection Prelude.Text
getSegmentDetection_jobId = Lens.lens (\GetSegmentDetection' {jobId} -> jobId) (\s@GetSegmentDetection' {} a -> s {jobId = a} :: GetSegmentDetection)

instance Core.AWSRequest GetSegmentDetection where
  type
    AWSResponse GetSegmentDetection =
      GetSegmentDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentDetectionResponse'
            Prelude.<$> (x Data..?> "AudioMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Segments" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "SelectedSegmentTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "VideoMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSegmentDetection where
  hashWithSalt _salt GetSegmentDetection' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetSegmentDetection where
  rnf GetSegmentDetection' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetSegmentDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.GetSegmentDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSegmentDetection where
  toJSON GetSegmentDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetSegmentDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSegmentDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSegmentDetectionResponse' smart constructor.
data GetSegmentDetectionResponse = GetSegmentDetectionResponse'
  { -- | An array of objects. There can be multiple audio streams. Each
    -- @AudioMetadata@ object contains metadata for a single audio stream.
    -- Audio information in an @AudioMetadata@ objects includes the audio
    -- codec, the number of audio channels, the duration of the audio stream,
    -- and the sample rate. Audio metadata is returned in each page of
    -- information returned by @GetSegmentDetection@.
    audioMetadata :: Prelude.Maybe [AudioMetadata],
    -- | Current status of the segment detection job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | If the previous response was incomplete (because there are more labels
    -- to retrieve), Amazon Rekognition Video returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- text.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of segments detected in a video. The array is sorted by the
    -- segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@
    -- input parameter of @StartSegmentDetection@. Within each segment type the
    -- array is sorted by timestamp values.
    segments :: Prelude.Maybe [SegmentDetection],
    -- | An array containing the segment types requested in the call to
    -- @StartSegmentDetection@.
    selectedSegmentTypes :: Prelude.Maybe [SegmentTypeInfo],
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Currently, Amazon Rekognition Video returns a single object in the
    -- @VideoMetadata@ array. The object contains information about the video
    -- stream in the input file that Amazon Rekognition Video chose to analyze.
    -- The @VideoMetadata@ object includes the video codec, video format and
    -- other information. Video metadata is returned in each page of
    -- information returned by @GetSegmentDetection@.
    videoMetadata :: Prelude.Maybe [VideoMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegmentDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioMetadata', 'getSegmentDetectionResponse_audioMetadata' - An array of objects. There can be multiple audio streams. Each
-- @AudioMetadata@ object contains metadata for a single audio stream.
-- Audio information in an @AudioMetadata@ objects includes the audio
-- codec, the number of audio channels, the duration of the audio stream,
-- and the sample rate. Audio metadata is returned in each page of
-- information returned by @GetSegmentDetection@.
--
-- 'jobStatus', 'getSegmentDetectionResponse_jobStatus' - Current status of the segment detection job.
--
-- 'nextToken', 'getSegmentDetectionResponse_nextToken' - If the previous response was incomplete (because there are more labels
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- text.
--
-- 'segments', 'getSegmentDetectionResponse_segments' - An array of segments detected in a video. The array is sorted by the
-- segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@
-- input parameter of @StartSegmentDetection@. Within each segment type the
-- array is sorted by timestamp values.
--
-- 'selectedSegmentTypes', 'getSegmentDetectionResponse_selectedSegmentTypes' - An array containing the segment types requested in the call to
-- @StartSegmentDetection@.
--
-- 'statusMessage', 'getSegmentDetectionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'videoMetadata', 'getSegmentDetectionResponse_videoMetadata' - Currently, Amazon Rekognition Video returns a single object in the
-- @VideoMetadata@ array. The object contains information about the video
-- stream in the input file that Amazon Rekognition Video chose to analyze.
-- The @VideoMetadata@ object includes the video codec, video format and
-- other information. Video metadata is returned in each page of
-- information returned by @GetSegmentDetection@.
--
-- 'httpStatus', 'getSegmentDetectionResponse_httpStatus' - The response's http status code.
newGetSegmentDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSegmentDetectionResponse
newGetSegmentDetectionResponse pHttpStatus_ =
  GetSegmentDetectionResponse'
    { audioMetadata =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      segments = Prelude.Nothing,
      selectedSegmentTypes = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects. There can be multiple audio streams. Each
-- @AudioMetadata@ object contains metadata for a single audio stream.
-- Audio information in an @AudioMetadata@ objects includes the audio
-- codec, the number of audio channels, the duration of the audio stream,
-- and the sample rate. Audio metadata is returned in each page of
-- information returned by @GetSegmentDetection@.
getSegmentDetectionResponse_audioMetadata :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe [AudioMetadata])
getSegmentDetectionResponse_audioMetadata = Lens.lens (\GetSegmentDetectionResponse' {audioMetadata} -> audioMetadata) (\s@GetSegmentDetectionResponse' {} a -> s {audioMetadata = a} :: GetSegmentDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Current status of the segment detection job.
getSegmentDetectionResponse_jobStatus :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe VideoJobStatus)
getSegmentDetectionResponse_jobStatus = Lens.lens (\GetSegmentDetectionResponse' {jobStatus} -> jobStatus) (\s@GetSegmentDetectionResponse' {} a -> s {jobStatus = a} :: GetSegmentDetectionResponse)

-- | If the previous response was incomplete (because there are more labels
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- text.
getSegmentDetectionResponse_nextToken :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe Prelude.Text)
getSegmentDetectionResponse_nextToken = Lens.lens (\GetSegmentDetectionResponse' {nextToken} -> nextToken) (\s@GetSegmentDetectionResponse' {} a -> s {nextToken = a} :: GetSegmentDetectionResponse)

-- | An array of segments detected in a video. The array is sorted by the
-- segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@
-- input parameter of @StartSegmentDetection@. Within each segment type the
-- array is sorted by timestamp values.
getSegmentDetectionResponse_segments :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe [SegmentDetection])
getSegmentDetectionResponse_segments = Lens.lens (\GetSegmentDetectionResponse' {segments} -> segments) (\s@GetSegmentDetectionResponse' {} a -> s {segments = a} :: GetSegmentDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array containing the segment types requested in the call to
-- @StartSegmentDetection@.
getSegmentDetectionResponse_selectedSegmentTypes :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe [SegmentTypeInfo])
getSegmentDetectionResponse_selectedSegmentTypes = Lens.lens (\GetSegmentDetectionResponse' {selectedSegmentTypes} -> selectedSegmentTypes) (\s@GetSegmentDetectionResponse' {} a -> s {selectedSegmentTypes = a} :: GetSegmentDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getSegmentDetectionResponse_statusMessage :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe Prelude.Text)
getSegmentDetectionResponse_statusMessage = Lens.lens (\GetSegmentDetectionResponse' {statusMessage} -> statusMessage) (\s@GetSegmentDetectionResponse' {} a -> s {statusMessage = a} :: GetSegmentDetectionResponse)

-- | Currently, Amazon Rekognition Video returns a single object in the
-- @VideoMetadata@ array. The object contains information about the video
-- stream in the input file that Amazon Rekognition Video chose to analyze.
-- The @VideoMetadata@ object includes the video codec, video format and
-- other information. Video metadata is returned in each page of
-- information returned by @GetSegmentDetection@.
getSegmentDetectionResponse_videoMetadata :: Lens.Lens' GetSegmentDetectionResponse (Prelude.Maybe [VideoMetadata])
getSegmentDetectionResponse_videoMetadata = Lens.lens (\GetSegmentDetectionResponse' {videoMetadata} -> videoMetadata) (\s@GetSegmentDetectionResponse' {} a -> s {videoMetadata = a} :: GetSegmentDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSegmentDetectionResponse_httpStatus :: Lens.Lens' GetSegmentDetectionResponse Prelude.Int
getSegmentDetectionResponse_httpStatus = Lens.lens (\GetSegmentDetectionResponse' {httpStatus} -> httpStatus) (\s@GetSegmentDetectionResponse' {} a -> s {httpStatus = a} :: GetSegmentDetectionResponse)

instance Prelude.NFData GetSegmentDetectionResponse where
  rnf GetSegmentDetectionResponse' {..} =
    Prelude.rnf audioMetadata
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf segments
      `Prelude.seq` Prelude.rnf selectedSegmentTypes
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf videoMetadata
      `Prelude.seq` Prelude.rnf httpStatus
