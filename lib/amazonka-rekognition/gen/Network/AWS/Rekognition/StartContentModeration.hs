{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartContentModeration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of unsafe content in a stored video.
--
-- Amazon Rekognition Video can moderate content in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartContentModeration@ returns a job identifier (@JobId@ ) which you use to get the results of the analysis. When unsafe content analysis is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the unsafe content analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetContentModeration' and pass the job identifier (@JobId@ ) from the initial call to @StartContentModeration@ .
-- For more information, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartContentModeration
  ( -- * Creating a request
    StartContentModeration (..),
    mkStartContentModeration,

    -- ** Request lenses
    scmVideo,
    scmClientRequestToken,
    scmJobTag,
    scmMinConfidence,
    scmNotificationChannel,

    -- * Destructuring the response
    StartContentModerationResponse (..),
    mkStartContentModerationResponse,

    -- ** Response lenses
    scmrrsJobId,
    scmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartContentModeration' smart constructor.
data StartContentModeration = StartContentModeration'
  { -- | The video in which you want to detect unsafe content. The video must be stored in an Amazon S3 bucket.
    video :: Types.Video,
    -- | Idempotent token used to identify the start request. If you use the same token with multiple @StartContentModeration@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
    jobTag :: Core.Maybe Types.JobTag,
    -- | Specifies the minimum confidence that Amazon Rekognition must have in order to return a moderated content label. Confidence represents how certain Amazon Rekognition is that the moderated content is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition doesn't return any moderated content labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , @GetContentModeration@ returns labels with confidence values greater than or equal to 50 percent.
    minConfidence :: Core.Maybe Core.Double,
    -- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to publish the completion status of the unsafe content analysis to.
    notificationChannel :: Core.Maybe Types.NotificationChannel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartContentModeration' value with any optional fields omitted.
mkStartContentModeration ::
  -- | 'video'
  Types.Video ->
  StartContentModeration
mkStartContentModeration video =
  StartContentModeration'
    { video,
      clientRequestToken = Core.Nothing,
      jobTag = Core.Nothing,
      minConfidence = Core.Nothing,
      notificationChannel = Core.Nothing
    }

-- | The video in which you want to detect unsafe content. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmVideo :: Lens.Lens' StartContentModeration Types.Video
scmVideo = Lens.field @"video"
{-# DEPRECATED scmVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartContentModeration@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmClientRequestToken :: Lens.Lens' StartContentModeration (Core.Maybe Types.ClientRequestToken)
scmClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED scmClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmJobTag :: Lens.Lens' StartContentModeration (Core.Maybe Types.JobTag)
scmJobTag = Lens.field @"jobTag"
{-# DEPRECATED scmJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | Specifies the minimum confidence that Amazon Rekognition must have in order to return a moderated content label. Confidence represents how certain Amazon Rekognition is that the moderated content is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition doesn't return any moderated content labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , @GetContentModeration@ returns labels with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmMinConfidence :: Lens.Lens' StartContentModeration (Core.Maybe Core.Double)
scmMinConfidence = Lens.field @"minConfidence"
{-# DEPRECATED scmMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

-- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to publish the completion status of the unsafe content analysis to.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmNotificationChannel :: Lens.Lens' StartContentModeration (Core.Maybe Types.NotificationChannel)
scmNotificationChannel = Lens.field @"notificationChannel"
{-# DEPRECATED scmNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

instance Core.FromJSON StartContentModeration where
  toJSON StartContentModeration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Video" Core..= video),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("JobTag" Core..=) Core.<$> jobTag,
            ("MinConfidence" Core..=) Core.<$> minConfidence,
            ("NotificationChannel" Core..=) Core.<$> notificationChannel
          ]
      )

instance Core.AWSRequest StartContentModeration where
  type Rs StartContentModeration = StartContentModerationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.StartContentModeration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContentModerationResponse'
            Core.<$> (x Core..:? "JobId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartContentModerationResponse' smart constructor.
data StartContentModerationResponse = StartContentModerationResponse'
  { -- | The identifier for the unsafe content analysis job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartContentModerationResponse' value with any optional fields omitted.
mkStartContentModerationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartContentModerationResponse
mkStartContentModerationResponse responseStatus =
  StartContentModerationResponse'
    { jobId = Core.Nothing,
      responseStatus
    }

-- | The identifier for the unsafe content analysis job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmrrsJobId :: Lens.Lens' StartContentModerationResponse (Core.Maybe Types.JobId)
scmrrsJobId = Lens.field @"jobId"
{-# DEPRECATED scmrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmrrsResponseStatus :: Lens.Lens' StartContentModerationResponse Core.Int
scmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
