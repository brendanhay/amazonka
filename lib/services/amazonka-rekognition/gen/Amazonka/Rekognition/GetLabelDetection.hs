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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- registered in the initial call to @StartlabelDetection@.
--
-- To get the results of the label detection operation, first check that
-- the status value published to the Amazon SNS topic is @SUCCEEDED@. If
-- so, call GetLabelDetection and pass the job identifier (@JobId@) from
-- the initial call to @StartLabelDetection@.
--
-- @GetLabelDetection@ returns an array of detected labels (@Labels@)
-- sorted by the time the labels were detected. You can also sort by the
-- label name by specifying @NAME@ for the @SortBy@ input parameter. If
-- there is no @NAME@ specified, the default sort is by timestamp.
--
-- You can select how results are aggregated by using the @AggregateBy@
-- input parameter. The default aggregation method is @TIMESTAMPS@. You can
-- also aggregate by @SEGMENTS@, which aggregates all instances of labels
-- detected in a given segment.
--
-- The returned Labels array may include the following attributes:
--
-- -   Name - The name of the detected label.
--
-- -   Confidence - The level of confidence in the label assigned to a
--     detected object.
--
-- -   Parents - The ancestor labels for a detected label.
--     GetLabelDetection returns a hierarchical taxonomy of detected
--     labels. For example, a detected car might be assigned the label car.
--     The label car has two parent labels: Vehicle (its parent) and
--     Transportation (its grandparent). The response includes the all
--     ancestors for a label, where every ancestor is a unique label. In
--     the previous example, Car, Vehicle, and Transportation are returned
--     as unique labels in the response.
--
-- -   Aliases - Possible Aliases for the label.
--
-- -   Categories - The label categories that the detected label belongs
--     to.
--
-- -   BoundingBox — Bounding boxes are described for all instances of
--     detected common object labels, returned in an array of Instance
--     objects. An Instance object contains a BoundingBox object,
--     describing the location of the label on the input image. It also
--     includes the confidence for the accuracy of the detected bounding
--     box.
--
-- -   Timestamp - Time, in milliseconds from the start of the video, that
--     the label was detected. For aggregation by @SEGMENTS@, the
--     @StartTimestampMillis@, @EndTimestampMillis@, and @DurationMillis@
--     structures are what define a segment. Although the “Timestamp”
--     structure is still returned with each label, its value is set to be
--     the same as @StartTimestampMillis@.
--
-- Timestamp and Bounding box information are returned for detected
-- Instances, only if aggregation is done by @TIMESTAMPS@. If aggregating
-- by @SEGMENTS@, information about detected instances isn’t returned.
--
-- The version of the label model used for the detection is also returned.
--
-- __Note @DominantColors@ isn\'t returned for @Instances@, although it is
-- shown as part of the response in the sample seen below.__
--
-- Use @MaxResults@ parameter to limit the number of labels returned. If
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
    getLabelDetection_aggregateBy,
    getLabelDetection_maxResults,
    getLabelDetection_nextToken,
    getLabelDetection_sortBy,
    getLabelDetection_jobId,

    -- * Destructuring the Response
    GetLabelDetectionResponse (..),
    newGetLabelDetectionResponse,

    -- * Response Lenses
    getLabelDetectionResponse_getRequestMetadata,
    getLabelDetectionResponse_jobId,
    getLabelDetectionResponse_jobStatus,
    getLabelDetectionResponse_jobTag,
    getLabelDetectionResponse_labelModelVersion,
    getLabelDetectionResponse_labels,
    getLabelDetectionResponse_nextToken,
    getLabelDetectionResponse_statusMessage,
    getLabelDetectionResponse_video,
    getLabelDetectionResponse_videoMetadata,
    getLabelDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLabelDetection' smart constructor.
data GetLabelDetection = GetLabelDetection'
  { -- | Defines how to aggregate the returned results. Results can be aggregated
    -- by timestamps or segments.
    aggregateBy :: Prelude.Maybe LabelDetectionAggregateBy,
    -- | Maximum number of results to return per paginated call. The largest
    -- value you can specify is 1000. If you specify a value greater than 1000,
    -- a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there are more labels
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
-- 'aggregateBy', 'getLabelDetection_aggregateBy' - Defines how to aggregate the returned results. Results can be aggregated
-- by timestamps or segments.
--
-- 'maxResults', 'getLabelDetection_maxResults' - Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
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
-- 'jobId', 'getLabelDetection_jobId' - Job identifier for the label detection operation for which you want
-- results returned. You get the job identifer from an initial call to
-- @StartlabelDetection@.
newGetLabelDetection ::
  -- | 'jobId'
  Prelude.Text ->
  GetLabelDetection
newGetLabelDetection pJobId_ =
  GetLabelDetection'
    { aggregateBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Defines how to aggregate the returned results. Results can be aggregated
-- by timestamps or segments.
getLabelDetection_aggregateBy :: Lens.Lens' GetLabelDetection (Prelude.Maybe LabelDetectionAggregateBy)
getLabelDetection_aggregateBy = Lens.lens (\GetLabelDetection' {aggregateBy} -> aggregateBy) (\s@GetLabelDetection' {} a -> s {aggregateBy = a} :: GetLabelDetection)

-- | Maximum number of results to return per paginated call. The largest
-- value you can specify is 1000. If you specify a value greater than 1000,
-- a maximum of 1000 results is returned. The default value is 1000.
getLabelDetection_maxResults :: Lens.Lens' GetLabelDetection (Prelude.Maybe Prelude.Natural)
getLabelDetection_maxResults = Lens.lens (\GetLabelDetection' {maxResults} -> maxResults) (\s@GetLabelDetection' {} a -> s {maxResults = a} :: GetLabelDetection)

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
            Prelude.<$> (x Data..?> "GetRequestMetadata")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "JobTag")
            Prelude.<*> (x Data..?> "LabelModelVersion")
            Prelude.<*> (x Data..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "Video")
            Prelude.<*> (x Data..?> "VideoMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLabelDetection where
  hashWithSalt _salt GetLabelDetection' {..} =
    _salt
      `Prelude.hashWithSalt` aggregateBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetLabelDetection where
  rnf GetLabelDetection' {..} =
    Prelude.rnf aggregateBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetLabelDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.GetLabelDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLabelDetection where
  toJSON GetLabelDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AggregateBy" Data..=) Prelude.<$> aggregateBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath GetLabelDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLabelDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLabelDetectionResponse' smart constructor.
data GetLabelDetectionResponse = GetLabelDetectionResponse'
  { -- | Information about the paramters used when getting a response. Includes
    -- information on aggregation and sorting methods.
    getRequestMetadata :: Prelude.Maybe GetLabelDetectionRequestMetadata,
    -- | Job identifier for the label detection operation for which you want to
    -- obtain results. The job identifer is returned by an initial call to
    -- StartLabelDetection.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the label detection job.
    jobStatus :: Prelude.Maybe VideoJobStatus,
    -- | A job identifier specified in the call to StartLabelDetection and
    -- returned in the job completion notification sent to your Amazon Simple
    -- Notification Service topic.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | Version number of the label detection model that was used to detect
    -- labels.
    labelModelVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of labels detected in the video. Each element contains the
    -- detected label and the time, in milliseconds from the start of the
    -- video, that the label was detected.
    labels :: Prelude.Maybe [LabelDetection],
    -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of labels.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    video :: Prelude.Maybe Video,
    -- | Information about a video that Amazon Rekognition Video analyzed.
    -- @Videometadata@ is returned in every page of paginated responses from a
    -- Amazon Rekognition video operation.
    videoMetadata :: Prelude.Maybe VideoMetadata,
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
-- 'getRequestMetadata', 'getLabelDetectionResponse_getRequestMetadata' - Information about the paramters used when getting a response. Includes
-- information on aggregation and sorting methods.
--
-- 'jobId', 'getLabelDetectionResponse_jobId' - Job identifier for the label detection operation for which you want to
-- obtain results. The job identifer is returned by an initial call to
-- StartLabelDetection.
--
-- 'jobStatus', 'getLabelDetectionResponse_jobStatus' - The current status of the label detection job.
--
-- 'jobTag', 'getLabelDetectionResponse_jobTag' - A job identifier specified in the call to StartLabelDetection and
-- returned in the job completion notification sent to your Amazon Simple
-- Notification Service topic.
--
-- 'labelModelVersion', 'getLabelDetectionResponse_labelModelVersion' - Version number of the label detection model that was used to detect
-- labels.
--
-- 'labels', 'getLabelDetectionResponse_labels' - An array of labels detected in the video. Each element contains the
-- detected label and the time, in milliseconds from the start of the
-- video, that the label was detected.
--
-- 'nextToken', 'getLabelDetectionResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of labels.
--
-- 'statusMessage', 'getLabelDetectionResponse_statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- 'video', 'getLabelDetectionResponse_video' - Undocumented member.
--
-- 'videoMetadata', 'getLabelDetectionResponse_videoMetadata' - Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
--
-- 'httpStatus', 'getLabelDetectionResponse_httpStatus' - The response's http status code.
newGetLabelDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLabelDetectionResponse
newGetLabelDetectionResponse pHttpStatus_ =
  GetLabelDetectionResponse'
    { getRequestMetadata =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      labelModelVersion = Prelude.Nothing,
      labels = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      video = Prelude.Nothing,
      videoMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the paramters used when getting a response. Includes
-- information on aggregation and sorting methods.
getLabelDetectionResponse_getRequestMetadata :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe GetLabelDetectionRequestMetadata)
getLabelDetectionResponse_getRequestMetadata = Lens.lens (\GetLabelDetectionResponse' {getRequestMetadata} -> getRequestMetadata) (\s@GetLabelDetectionResponse' {} a -> s {getRequestMetadata = a} :: GetLabelDetectionResponse)

-- | Job identifier for the label detection operation for which you want to
-- obtain results. The job identifer is returned by an initial call to
-- StartLabelDetection.
getLabelDetectionResponse_jobId :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_jobId = Lens.lens (\GetLabelDetectionResponse' {jobId} -> jobId) (\s@GetLabelDetectionResponse' {} a -> s {jobId = a} :: GetLabelDetectionResponse)

-- | The current status of the label detection job.
getLabelDetectionResponse_jobStatus :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe VideoJobStatus)
getLabelDetectionResponse_jobStatus = Lens.lens (\GetLabelDetectionResponse' {jobStatus} -> jobStatus) (\s@GetLabelDetectionResponse' {} a -> s {jobStatus = a} :: GetLabelDetectionResponse)

-- | A job identifier specified in the call to StartLabelDetection and
-- returned in the job completion notification sent to your Amazon Simple
-- Notification Service topic.
getLabelDetectionResponse_jobTag :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_jobTag = Lens.lens (\GetLabelDetectionResponse' {jobTag} -> jobTag) (\s@GetLabelDetectionResponse' {} a -> s {jobTag = a} :: GetLabelDetectionResponse)

-- | Version number of the label detection model that was used to detect
-- labels.
getLabelDetectionResponse_labelModelVersion :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_labelModelVersion = Lens.lens (\GetLabelDetectionResponse' {labelModelVersion} -> labelModelVersion) (\s@GetLabelDetectionResponse' {} a -> s {labelModelVersion = a} :: GetLabelDetectionResponse)

-- | An array of labels detected in the video. Each element contains the
-- detected label and the time, in milliseconds from the start of the
-- video, that the label was detected.
getLabelDetectionResponse_labels :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe [LabelDetection])
getLabelDetectionResponse_labels = Lens.lens (\GetLabelDetectionResponse' {labels} -> labels) (\s@GetLabelDetectionResponse' {} a -> s {labels = a} :: GetLabelDetectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of labels.
getLabelDetectionResponse_nextToken :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_nextToken = Lens.lens (\GetLabelDetectionResponse' {nextToken} -> nextToken) (\s@GetLabelDetectionResponse' {} a -> s {nextToken = a} :: GetLabelDetectionResponse)

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
getLabelDetectionResponse_statusMessage :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Prelude.Text)
getLabelDetectionResponse_statusMessage = Lens.lens (\GetLabelDetectionResponse' {statusMessage} -> statusMessage) (\s@GetLabelDetectionResponse' {} a -> s {statusMessage = a} :: GetLabelDetectionResponse)

-- | Undocumented member.
getLabelDetectionResponse_video :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe Video)
getLabelDetectionResponse_video = Lens.lens (\GetLabelDetectionResponse' {video} -> video) (\s@GetLabelDetectionResponse' {} a -> s {video = a} :: GetLabelDetectionResponse)

-- | Information about a video that Amazon Rekognition Video analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
getLabelDetectionResponse_videoMetadata :: Lens.Lens' GetLabelDetectionResponse (Prelude.Maybe VideoMetadata)
getLabelDetectionResponse_videoMetadata = Lens.lens (\GetLabelDetectionResponse' {videoMetadata} -> videoMetadata) (\s@GetLabelDetectionResponse' {} a -> s {videoMetadata = a} :: GetLabelDetectionResponse)

-- | The response's http status code.
getLabelDetectionResponse_httpStatus :: Lens.Lens' GetLabelDetectionResponse Prelude.Int
getLabelDetectionResponse_httpStatus = Lens.lens (\GetLabelDetectionResponse' {httpStatus} -> httpStatus) (\s@GetLabelDetectionResponse' {} a -> s {httpStatus = a} :: GetLabelDetectionResponse)

instance Prelude.NFData GetLabelDetectionResponse where
  rnf GetLabelDetectionResponse' {..} =
    Prelude.rnf getRequestMetadata
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf labelModelVersion
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf video
      `Prelude.seq` Prelude.rnf videoMetadata
      `Prelude.seq` Prelude.rnf httpStatus
