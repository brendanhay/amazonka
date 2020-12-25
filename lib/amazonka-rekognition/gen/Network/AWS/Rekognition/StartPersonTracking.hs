{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartPersonTracking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous tracking of a person's path in a stored video.
--
-- Amazon Rekognition Video can track the path of people in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartPersonTracking@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the person detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetPersonTracking' and pass the job identifier (@JobId@ ) from the initial call to @StartPersonTracking@ .
module Network.AWS.Rekognition.StartPersonTracking
  ( -- * Creating a request
    StartPersonTracking (..),
    mkStartPersonTracking,

    -- ** Request lenses
    sptVideo,
    sptClientRequestToken,
    sptJobTag,
    sptNotificationChannel,

    -- * Destructuring the response
    StartPersonTrackingResponse (..),
    mkStartPersonTrackingResponse,

    -- ** Response lenses
    sptrrsJobId,
    sptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartPersonTracking' smart constructor.
data StartPersonTracking = StartPersonTracking'
  { -- | The video in which you want to detect people. The video must be stored in an Amazon S3 bucket.
    video :: Types.Video,
    -- | Idempotent token used to identify the start request. If you use the same token with multiple @StartPersonTracking@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
    jobTag :: Core.Maybe Types.JobTag,
    -- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the people detection operation to.
    notificationChannel :: Core.Maybe Types.NotificationChannel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPersonTracking' value with any optional fields omitted.
mkStartPersonTracking ::
  -- | 'video'
  Types.Video ->
  StartPersonTracking
mkStartPersonTracking video =
  StartPersonTracking'
    { video,
      clientRequestToken = Core.Nothing,
      jobTag = Core.Nothing,
      notificationChannel = Core.Nothing
    }

-- | The video in which you want to detect people. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptVideo :: Lens.Lens' StartPersonTracking Types.Video
sptVideo = Lens.field @"video"
{-# DEPRECATED sptVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartPersonTracking@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptClientRequestToken :: Lens.Lens' StartPersonTracking (Core.Maybe Types.ClientRequestToken)
sptClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED sptClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptJobTag :: Lens.Lens' StartPersonTracking (Core.Maybe Types.JobTag)
sptJobTag = Lens.field @"jobTag"
{-# DEPRECATED sptJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the people detection operation to.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptNotificationChannel :: Lens.Lens' StartPersonTracking (Core.Maybe Types.NotificationChannel)
sptNotificationChannel = Lens.field @"notificationChannel"
{-# DEPRECATED sptNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

instance Core.FromJSON StartPersonTracking where
  toJSON StartPersonTracking {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Video" Core..= video),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("JobTag" Core..=) Core.<$> jobTag,
            ("NotificationChannel" Core..=) Core.<$> notificationChannel
          ]
      )

instance Core.AWSRequest StartPersonTracking where
  type Rs StartPersonTracking = StartPersonTrackingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.StartPersonTracking")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPersonTrackingResponse'
            Core.<$> (x Core..:? "JobId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartPersonTrackingResponse' smart constructor.
data StartPersonTrackingResponse = StartPersonTrackingResponse'
  { -- | The identifier for the person detection job. Use @JobId@ to identify the job in a subsequent call to @GetPersonTracking@ .
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPersonTrackingResponse' value with any optional fields omitted.
mkStartPersonTrackingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartPersonTrackingResponse
mkStartPersonTrackingResponse responseStatus =
  StartPersonTrackingResponse'
    { jobId = Core.Nothing,
      responseStatus
    }

-- | The identifier for the person detection job. Use @JobId@ to identify the job in a subsequent call to @GetPersonTracking@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptrrsJobId :: Lens.Lens' StartPersonTrackingResponse (Core.Maybe Types.JobId)
sptrrsJobId = Lens.field @"jobId"
{-# DEPRECATED sptrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptrrsResponseStatus :: Lens.Lens' StartPersonTrackingResponse Core.Int
sptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
