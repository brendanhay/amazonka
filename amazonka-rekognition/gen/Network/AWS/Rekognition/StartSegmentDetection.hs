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
-- Module      : Network.AWS.Rekognition.StartSegmentDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of segment detection in a stored video.
--
-- Amazon Rekognition Video can detect segments in a video stored in an
-- Amazon S3 bucket. Use Video to specify the bucket name and the filename
-- of the video. @StartSegmentDetection@ returns a job identifier (@JobId@)
-- which you use to get the results of the operation. When segment
-- detection is finished, Amazon Rekognition Video publishes a completion
-- status to the Amazon Simple Notification Service topic that you specify
-- in @NotificationChannel@.
--
-- You can use the @Filters@ (StartSegmentDetectionFilters) input parameter
-- to specify the minimum detection confidence returned in the response.
-- Within @Filters@, use @ShotFilter@ (StartShotDetectionFilter) to filter
-- detected shots. Use @TechnicalCueFilter@
-- (StartTechnicalCueDetectionFilter) to filter technical cues.
--
-- To get the results of the segment detection operation, first check that
-- the status value published to the Amazon SNS topic is @SUCCEEDED@. if
-- so, call GetSegmentDetection and pass the job identifier (@JobId@) from
-- the initial call to @StartSegmentDetection@.
--
-- For more information, see Detecting Video Segments in Stored Video in
-- the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartSegmentDetection
  ( -- * Creating a Request
    StartSegmentDetection (..),
    newStartSegmentDetection,

    -- * Request Lenses
    startSegmentDetection_notificationChannel,
    startSegmentDetection_filters,
    startSegmentDetection_clientRequestToken,
    startSegmentDetection_jobTag,
    startSegmentDetection_video,
    startSegmentDetection_segmentTypes,

    -- * Destructuring the Response
    StartSegmentDetectionResponse (..),
    newStartSegmentDetectionResponse,

    -- * Response Lenses
    startSegmentDetectionResponse_jobId,
    startSegmentDetectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartSegmentDetection' smart constructor.
data StartSegmentDetection = StartSegmentDetection'
  { -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
    -- Video to publish the completion status of the segment detection
    -- operation.
    notificationChannel :: Core.Maybe NotificationChannel,
    -- | Filters for technical cue or shot detection.
    filters :: Core.Maybe StartSegmentDetectionFilters,
    -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartSegmentDetection@ requests, the same @JobId@
    -- is returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Core.Maybe Core.Text,
    video :: Video,
    -- | An array of segment types to detect in the video. Valid values are
    -- TECHNICAL_CUE and SHOT.
    segmentTypes :: Core.NonEmpty SegmentType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSegmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationChannel', 'startSegmentDetection_notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the segment detection
-- operation.
--
-- 'filters', 'startSegmentDetection_filters' - Filters for technical cue or shot detection.
--
-- 'clientRequestToken', 'startSegmentDetection_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartSegmentDetection@ requests, the same @JobId@
-- is returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'jobTag', 'startSegmentDetection_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'video', 'startSegmentDetection_video' - Undocumented member.
--
-- 'segmentTypes', 'startSegmentDetection_segmentTypes' - An array of segment types to detect in the video. Valid values are
-- TECHNICAL_CUE and SHOT.
newStartSegmentDetection ::
  -- | 'video'
  Video ->
  -- | 'segmentTypes'
  Core.NonEmpty SegmentType ->
  StartSegmentDetection
newStartSegmentDetection pVideo_ pSegmentTypes_ =
  StartSegmentDetection'
    { notificationChannel =
        Core.Nothing,
      filters = Core.Nothing,
      clientRequestToken = Core.Nothing,
      jobTag = Core.Nothing,
      video = pVideo_,
      segmentTypes = Lens._Coerce Lens.# pSegmentTypes_
    }

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the segment detection
-- operation.
startSegmentDetection_notificationChannel :: Lens.Lens' StartSegmentDetection (Core.Maybe NotificationChannel)
startSegmentDetection_notificationChannel = Lens.lens (\StartSegmentDetection' {notificationChannel} -> notificationChannel) (\s@StartSegmentDetection' {} a -> s {notificationChannel = a} :: StartSegmentDetection)

-- | Filters for technical cue or shot detection.
startSegmentDetection_filters :: Lens.Lens' StartSegmentDetection (Core.Maybe StartSegmentDetectionFilters)
startSegmentDetection_filters = Lens.lens (\StartSegmentDetection' {filters} -> filters) (\s@StartSegmentDetection' {} a -> s {filters = a} :: StartSegmentDetection)

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartSegmentDetection@ requests, the same @JobId@
-- is returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startSegmentDetection_clientRequestToken :: Lens.Lens' StartSegmentDetection (Core.Maybe Core.Text)
startSegmentDetection_clientRequestToken = Lens.lens (\StartSegmentDetection' {clientRequestToken} -> clientRequestToken) (\s@StartSegmentDetection' {} a -> s {clientRequestToken = a} :: StartSegmentDetection)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startSegmentDetection_jobTag :: Lens.Lens' StartSegmentDetection (Core.Maybe Core.Text)
startSegmentDetection_jobTag = Lens.lens (\StartSegmentDetection' {jobTag} -> jobTag) (\s@StartSegmentDetection' {} a -> s {jobTag = a} :: StartSegmentDetection)

-- | Undocumented member.
startSegmentDetection_video :: Lens.Lens' StartSegmentDetection Video
startSegmentDetection_video = Lens.lens (\StartSegmentDetection' {video} -> video) (\s@StartSegmentDetection' {} a -> s {video = a} :: StartSegmentDetection)

-- | An array of segment types to detect in the video. Valid values are
-- TECHNICAL_CUE and SHOT.
startSegmentDetection_segmentTypes :: Lens.Lens' StartSegmentDetection (Core.NonEmpty SegmentType)
startSegmentDetection_segmentTypes = Lens.lens (\StartSegmentDetection' {segmentTypes} -> segmentTypes) (\s@StartSegmentDetection' {} a -> s {segmentTypes = a} :: StartSegmentDetection) Core.. Lens._Coerce

instance Core.AWSRequest StartSegmentDetection where
  type
    AWSResponse StartSegmentDetection =
      StartSegmentDetectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSegmentDetectionResponse'
            Core.<$> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartSegmentDetection

instance Core.NFData StartSegmentDetection

instance Core.ToHeaders StartSegmentDetection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StartSegmentDetection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartSegmentDetection where
  toJSON StartSegmentDetection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationChannel" Core..=)
              Core.<$> notificationChannel,
            ("Filters" Core..=) Core.<$> filters,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("JobTag" Core..=) Core.<$> jobTag,
            Core.Just ("Video" Core..= video),
            Core.Just ("SegmentTypes" Core..= segmentTypes)
          ]
      )

instance Core.ToPath StartSegmentDetection where
  toPath = Core.const "/"

instance Core.ToQuery StartSegmentDetection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartSegmentDetectionResponse' smart constructor.
data StartSegmentDetectionResponse = StartSegmentDetectionResponse'
  { -- | Unique identifier for the segment detection job. The @JobId@ is returned
    -- from @StartSegmentDetection@.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSegmentDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startSegmentDetectionResponse_jobId' - Unique identifier for the segment detection job. The @JobId@ is returned
-- from @StartSegmentDetection@.
--
-- 'httpStatus', 'startSegmentDetectionResponse_httpStatus' - The response's http status code.
newStartSegmentDetectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartSegmentDetectionResponse
newStartSegmentDetectionResponse pHttpStatus_ =
  StartSegmentDetectionResponse'
    { jobId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier for the segment detection job. The @JobId@ is returned
-- from @StartSegmentDetection@.
startSegmentDetectionResponse_jobId :: Lens.Lens' StartSegmentDetectionResponse (Core.Maybe Core.Text)
startSegmentDetectionResponse_jobId = Lens.lens (\StartSegmentDetectionResponse' {jobId} -> jobId) (\s@StartSegmentDetectionResponse' {} a -> s {jobId = a} :: StartSegmentDetectionResponse)

-- | The response's http status code.
startSegmentDetectionResponse_httpStatus :: Lens.Lens' StartSegmentDetectionResponse Core.Int
startSegmentDetectionResponse_httpStatus = Lens.lens (\StartSegmentDetectionResponse' {httpStatus} -> httpStatus) (\s@StartSegmentDetectionResponse' {} a -> s {httpStatus = a} :: StartSegmentDetectionResponse)

instance Core.NFData StartSegmentDetectionResponse
