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
-- Module      : Amazonka.Rekognition.GetLabelDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the label detection results of a Amazon Rekognition Video analysis
-- started by StartLabelDetection.
--
-- The label detection operation is started by a call to
-- StartLabelDetection which returns a job identifier (@JobId@). When the
-- label detection operation finishes, Amazon Rekognition publishes a
-- completion status to the Amazon Simple Notification Service topic
-- registered in the initial call to @StartlabelDetection@. To get the
-- results of the label detection operation, first check that the status
-- value published to the Amazon SNS topic is @SUCCEEDED@. If so, call
-- GetLabelDetection and pass the job identifier (@JobId@) from the initial
-- call to @StartLabelDetection@.
--
-- @GetLabelDetection@ returns an array of detected labels (@Labels@)
-- sorted by the time the labels were detected. You can also sort by the
-- label name by specifying @NAME@ for the @SortBy@ input parameter.
--
-- The labels returned include the label name, the percentage confidence in
-- the accuracy of the detected label, and the time the label was detected
-- in the video.
--
-- The returned labels also include bounding box information for common
-- objects, a hierarchical taxonomy of detected labels, and the version of
-- the label model used for detection.
--
-- Use MaxResults parameter to limit the number of labels returned. If
-- there are more results than specified in @MaxResults@, the value of
-- @NextToken@ in the operation response contains a pagination token for
-- getting the next set of results. To get the next page of results, call
-- @GetlabelDetection@ and populate the @NextToken@ request parameter with
-- the token value returned from the previous call to @GetLabelDetection@.
module Amazonka.Rekognition.GetLabelDetection
  ( -- * Creating a Request
    GetLabelDetection (..),
    newGetLabelDetection,

    -- * Request Lenses
    getLabelDetection_nextToken,
    getLabelDetection_sortBy,
    getLabelDetection_maxResults,
    getLabelDetection_jobId,

    -- * Destructuring the Response
    GetLabelDetectionResponse (..),
    newGetLabelDetectionResponse,

    -- * Response Lenses
    getLabelDetectionResponse_nextToken,
    getLabelDetectionResponse_jobStatus,
    getLabelDetectionResponse_labelModelVersion,
    getLabelDetectionResponse_labels,
    getLabelDetectionResponse_videoMetadata,
    getLabelDetectionResponse_statusMessage,
    getLabelDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLabelDetection' smart constructor.
data GetLabelDetection = GetLabelDetection'
  { -- | If the previous response was incomplete (because there are more labels
    -- to retrieve), Amazon Rekognition Video returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- labels.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort
    -- array elements by the time labels are detected. Use @NAME@ to
    -- alphabetically group elements for a label together. Within each label
    -- group, the array element are sorted by detection confidence. The default
    -- sort is by @TIMESTAMP@.
    sortBy :: Prelude.Maybe LabelDetectionSortBy,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Job identifier for the label detection operation for which you want
    -- results returned. You get the job identifer from an initial call to
    -- @StartlabelDetection@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLabelDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getLabelDetection_nextToken' - If the previous response was incomplete (because there are more labels
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- labels.
--
-- 'sortBy', 'getLabelDetection_sortBy' - Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort
-- array elements by the time labels are detected. Use @NAME@ to
-- alphabetically group elements for a label together. Within each label
-- group, the array element are sorted by detection confidence. The default
-- sort is by @TIMESTAMP@.
--
-- 'maxResults', 'getLabelDetection_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
--
-- 'jobId', 'getLabelDetection_jobId' - Job identifier for the label detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartlabelDetection@.
newGetLabelDetection ::
  -- | 'jobId'
  Prelude.Text ->
  GetLabelDetection
newGetLabelDetection pJobId_ =
  GetLabelDetection'
    { nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more labels
-- to retrieve), Amazon Rekognition Video returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- labels.
getLabelDetection_nextToken :: Lens.Lens' GetLabelDetection (Prelude.Maybe Prelude.Text)
getLabelDetection_nextToken = Lens.lens (\GetLabelDetection' {nextToken} -> nextToken) (\s@GetLabelDetection' {} a -> s {nextToken = a} :: GetLabelDetection)

-- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort
-- array elements by the time labels are detected. Use @NAME@ to
-- alphabetically group elements for a label together. Within each label
-- group, the array element are sorted by detection confidence. The default
-- sort is by @TIMESTAMP@.
getLabelDetection_sortBy :: Lens.Lens' GetLabelDetection (Prelude.Maybe LabelDetectionSortBy)
getLabelDetection_sortBy = Lens.lens (\GetLabelDetection' {sortBy} -> sortBy) (\s@GetLabelDetection' {} a -> s {sortBy = a} :: GetLabelDetection)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getLabelDetection_maxResults :: Lens.Lens' GetLabelDetection (Prelude.Maybe Prelude.Natural)
getLabelDetection_maxResults = Lens.lens (\GetLabelDetection' {maxResults} -> maxResults) (\s@GetLabelDetection' {} a -> s {maxResults = a} :: GetLabelDetection)

-- | Job identifier for the label detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartlabelDetection@.
getLabelDetection_jobId :: Lens.Lens' GetLabelDetection Prelude.Text
getLabelDetection_jobId = Lens.lens (\GetLabelDetection' {jobId} -> jobId) (\s@GetLabelDetection' {} a -> s {jobId = a} :: GetLabelDetection)

instance Core.AWSRequest GetLabelDetection where
  type
    AWSResponse GetLabelDetection =
      GetLabelDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLabelDetectionResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "LabelModelVersion")
            Prelude.<*> (x Core..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "VideoMetadata")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLabelDetection where
  hashWithSalt _salt GetLabelDetection' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetLabelDetection where
  rnf GetLabelDetection' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobId

instance Core.ToHeaders GetLabelDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.GetLabelDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLabelDetection where
  toJSON GetLabelDetection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath GetLabelDetection where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLabelDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLabelDetectionResponse' smart constructor.
data GetLabelDetectionResponse = GetLabelDetectionResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of labels.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the label detection job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | Version number of the label detection model that was used to detect
    -- labels.
    labelModelVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of labels detected in the video. Each element contains the
    -- detected label and the time, in milliseconds from the start of the
    -- video, that the label was detected.
    labels :: Prelude.Maybe [LabelDetection],
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLabelDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getLabelDetectionResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of labels.
--
-- 'jobStatus', 'getLabelDetectionResponse_jobStatus' - The current status of the label detection job.
--
-- 'labelModelVersion', 'getLabelDetectionResponse_labelModelVersion' - Version number of the label detection model that was used to detect
-- labels.
--
-- 'labels', 'getLabelDetectionResponse_labels' - An array of labels detected in the video. Each element contains the
-- detected label and the time, in milliseconds from the start of the
-- video, that the label was detected.
--
-- 'videoMetadata', 'getLabelDetectionResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
--
-- 'statusMessage', 'getLabelDetectionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'httpStatus', 'getLabelDetectionResponse_httpStatus' - The response's http status code.
newGetLabelDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLabelDetectionResponse
newGetLabelDetectionResponse pHttpStatus_ =
  GetLabelDetectionResponse'
    { nextToken =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      labelModelVersion = Prelude.Nothing,
      labels = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of labels.
getLabelDetectionResponse_nextToken :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_nextToken = Lens.lens (\GetLabelDetectionResponse' {nextToken} -> nextToken) (\s@GetLabelDetectionResponse' {} a -> s {nextToken = a} :: GetLabelDetectionResponse)

-- | The current status of the label detection job.
getLabelDetectionResponse_jobStatus :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe VideoJobStatus)
getLabelDetectionResponse_jobStatus = Lens.lens (\GetLabelDetectionResponse' {jobStatus} -> jobStatus) (\s@GetLabelDetectionResponse' {} a -> s {jobStatus = a} :: GetLabelDetectionResponse)

-- | Version number of the label detection model that was used to detect
-- labels.
getLabelDetectionResponse_labelModelVersion :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_labelModelVersion = Lens.lens (\GetLabelDetectionResponse' {labelModelVersion} -> labelModelVersion) (\s@GetLabelDetectionResponse' {} a -> s {labelModelVersion = a} :: GetLabelDetectionResponse)

-- | An array of labels detected in the video. Each element contains the
-- detected label and the time, in milliseconds from the start of the
-- video, that the label was detected.
getLabelDetectionResponse_labels :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe [LabelDetection])
getLabelDetectionResponse_labels = Lens.lens (\GetLabelDetectionResponse' {labels} -> labels) (\s@GetLabelDetectionResponse' {} a -> s {labels = a} :: GetLabelDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
getLabelDetectionResponse_videoMetadata :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe VideoMetadata)
getLabelDetectionResponse_videoMetadata = Lens.lens (\GetLabelDetectionResponse' {videoMetadata} -> videoMetadata) (\s@GetLabelDetectionResponse' {} a -> s {videoMetadata = a} :: GetLabelDetectionResponse)

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getLabelDetectionResponse_statusMessage :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_statusMessage = Lens.lens (\GetLabelDetectionResponse' {statusMessage} -> statusMessage) (\s@GetLabelDetectionResponse' {} a -> s {statusMessage = a} :: GetLabelDetectionResponse)

-- | The response's http status code.
getLabelDetectionResponse_httpStatus :: Lens.Lens' GetLabelDetectionResponse Prelude.Int
getLabelDetectionResponse_httpStatus = Lens.lens (\GetLabelDetectionResponse' {httpStatus} -> httpStatus) (\s@GetLabelDetectionResponse' {} a -> s {httpStatus = a} :: GetLabelDetectionResponse)

instance Prelude.NFData GetLabelDetectionResponse where
  rnf GetLabelDetectionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf labelModelVersion
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf videoMetadata
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
