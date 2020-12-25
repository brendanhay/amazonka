{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartSegmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of segment detection in a stored video.
--
-- Amazon Rekognition Video can detect segments in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartSegmentDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When segment detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- You can use the @Filters@ ('StartSegmentDetectionFilters' ) input parameter to specify the minimum detection confidence returned in the response. Within @Filters@ , use @ShotFilter@ ('StartShotDetectionFilter' ) to filter detected shots. Use @TechnicalCueFilter@ ('StartTechnicalCueDetectionFilter' ) to filter technical cues.
-- To get the results of the segment detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call 'GetSegmentDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartSegmentDetection@ .
-- For more information, see Detecting Video Segments in Stored Video in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartSegmentDetection
  ( -- * Creating a request
    StartSegmentDetection (..),
    mkStartSegmentDetection,

    -- ** Request lenses
    ssdVideo,
    ssdSegmentTypes,
    ssdClientRequestToken,
    ssdFilters,
    ssdJobTag,
    ssdNotificationChannel,

    -- * Destructuring the response
    StartSegmentDetectionResponse (..),
    mkStartSegmentDetectionResponse,

    -- ** Response lenses
    ssdrrsJobId,
    ssdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSegmentDetection' smart constructor.
data StartSegmentDetection = StartSegmentDetection'
  { video :: Types.Video,
    -- | An array of segment types to detect in the video. Valid values are TECHNICAL_CUE and SHOT.
    segmentTypes :: Core.NonEmpty Types.SegmentType,
    -- | Idempotent token used to identify the start request. If you use the same token with multiple @StartSegmentDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | Filters for technical cue or shot detection.
    filters :: Core.Maybe Types.StartSegmentDetectionFilters,
    -- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
    jobTag :: Core.Maybe Types.JobTag,
    -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the segment detection operation.
    notificationChannel :: Core.Maybe Types.NotificationChannel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSegmentDetection' value with any optional fields omitted.
mkStartSegmentDetection ::
  -- | 'video'
  Types.Video ->
  -- | 'segmentTypes'
  Core.NonEmpty Types.SegmentType ->
  StartSegmentDetection
mkStartSegmentDetection video segmentTypes =
  StartSegmentDetection'
    { video,
      segmentTypes,
      clientRequestToken = Core.Nothing,
      filters = Core.Nothing,
      jobTag = Core.Nothing,
      notificationChannel = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdVideo :: Lens.Lens' StartSegmentDetection Types.Video
ssdVideo = Lens.field @"video"
{-# DEPRECATED ssdVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | An array of segment types to detect in the video. Valid values are TECHNICAL_CUE and SHOT.
--
-- /Note:/ Consider using 'segmentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentTypes :: Lens.Lens' StartSegmentDetection (Core.NonEmpty Types.SegmentType)
ssdSegmentTypes = Lens.field @"segmentTypes"
{-# DEPRECATED ssdSegmentTypes "Use generic-lens or generic-optics with 'segmentTypes' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartSegmentDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdClientRequestToken :: Lens.Lens' StartSegmentDetection (Core.Maybe Types.ClientRequestToken)
ssdClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED ssdClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Filters for technical cue or shot detection.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdFilters :: Lens.Lens' StartSegmentDetection (Core.Maybe Types.StartSegmentDetectionFilters)
ssdFilters = Lens.field @"filters"
{-# DEPRECATED ssdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdJobTag :: Lens.Lens' StartSegmentDetection (Core.Maybe Types.JobTag)
ssdJobTag = Lens.field @"jobTag"
{-# DEPRECATED ssdJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the segment detection operation.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdNotificationChannel :: Lens.Lens' StartSegmentDetection (Core.Maybe Types.NotificationChannel)
ssdNotificationChannel = Lens.field @"notificationChannel"
{-# DEPRECATED ssdNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

instance Core.FromJSON StartSegmentDetection where
  toJSON StartSegmentDetection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Video" Core..= video),
            Core.Just ("SegmentTypes" Core..= segmentTypes),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("Filters" Core..=) Core.<$> filters,
            ("JobTag" Core..=) Core.<$> jobTag,
            ("NotificationChannel" Core..=) Core.<$> notificationChannel
          ]
      )

instance Core.AWSRequest StartSegmentDetection where
  type Rs StartSegmentDetection = StartSegmentDetectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.StartSegmentDetection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSegmentDetectionResponse'
            Core.<$> (x Core..:? "JobId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSegmentDetectionResponse' smart constructor.
data StartSegmentDetectionResponse = StartSegmentDetectionResponse'
  { -- | Unique identifier for the segment detection job. The @JobId@ is returned from @StartSegmentDetection@ .
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSegmentDetectionResponse' value with any optional fields omitted.
mkStartSegmentDetectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartSegmentDetectionResponse
mkStartSegmentDetectionResponse responseStatus =
  StartSegmentDetectionResponse'
    { jobId = Core.Nothing,
      responseStatus
    }

-- | Unique identifier for the segment detection job. The @JobId@ is returned from @StartSegmentDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdrrsJobId :: Lens.Lens' StartSegmentDetectionResponse (Core.Maybe Types.JobId)
ssdrrsJobId = Lens.field @"jobId"
{-# DEPRECATED ssdrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdrrsResponseStatus :: Lens.Lens' StartSegmentDetectionResponse Core.Int
ssdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
