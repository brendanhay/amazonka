{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartLabelDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of labels in a stored video.
--
-- Amazon Rekognition Video can detect labels in a video. Labels are instances of real-world entities. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; concepts like landscape, evening, and nature; and activities like a person getting out of a car or a person skiing.
-- The video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartLabelDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetLabelDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartLabelDetection@ .
--
module Network.AWS.Rekognition.StartLabelDetection
    (
    -- * Creating a request
      StartLabelDetection (..)
    , mkStartLabelDetection
    -- ** Request lenses
    , sldVideo
    , sldClientRequestToken
    , sldJobTag
    , sldMinConfidence
    , sldNotificationChannel

    -- * Destructuring the response
    , StartLabelDetectionResponse (..)
    , mkStartLabelDetectionResponse
    -- ** Response lenses
    , sldrrsJobId
    , sldrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartLabelDetection' smart constructor.
data StartLabelDetection = StartLabelDetection'
  { video :: Types.Video
    -- ^ The video in which you want to detect labels. The video must be stored in an Amazon S3 bucket.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ Idempotent token used to identify the start request. If you use the same token with multiple @StartLabelDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once. 
  , jobTag :: Core.Maybe Types.JobTag
    -- ^ An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
  , minConfidence :: Core.Maybe Core.Double
    -- ^ Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected label. Confidence represents how certain Amazon Rekognition is that a label is correctly identified.0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
  , notificationChannel :: Core.Maybe Types.NotificationChannel
    -- ^ The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the label detection operation to. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLabelDetection' value with any optional fields omitted.
mkStartLabelDetection
    :: Types.Video -- ^ 'video'
    -> StartLabelDetection
mkStartLabelDetection video
  = StartLabelDetection'{video, clientRequestToken = Core.Nothing,
                         jobTag = Core.Nothing, minConfidence = Core.Nothing,
                         notificationChannel = Core.Nothing}

-- | The video in which you want to detect labels. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldVideo :: Lens.Lens' StartLabelDetection Types.Video
sldVideo = Lens.field @"video"
{-# INLINEABLE sldVideo #-}
{-# DEPRECATED video "Use generic-lens or generic-optics with 'video' instead"  #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartLabelDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldClientRequestToken :: Lens.Lens' StartLabelDetection (Core.Maybe Types.ClientRequestToken)
sldClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE sldClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldJobTag :: Lens.Lens' StartLabelDetection (Core.Maybe Types.JobTag)
sldJobTag = Lens.field @"jobTag"
{-# INLINEABLE sldJobTag #-}
{-# DEPRECATED jobTag "Use generic-lens or generic-optics with 'jobTag' instead"  #-}

-- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected label. Confidence represents how certain Amazon Rekognition is that a label is correctly identified.0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldMinConfidence :: Lens.Lens' StartLabelDetection (Core.Maybe Core.Double)
sldMinConfidence = Lens.field @"minConfidence"
{-# INLINEABLE sldMinConfidence #-}
{-# DEPRECATED minConfidence "Use generic-lens or generic-optics with 'minConfidence' instead"  #-}

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the label detection operation to. 
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldNotificationChannel :: Lens.Lens' StartLabelDetection (Core.Maybe Types.NotificationChannel)
sldNotificationChannel = Lens.field @"notificationChannel"
{-# INLINEABLE sldNotificationChannel #-}
{-# DEPRECATED notificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead"  #-}

instance Core.ToQuery StartLabelDetection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartLabelDetection where
        toHeaders StartLabelDetection{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.StartLabelDetection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartLabelDetection where
        toJSON StartLabelDetection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Video" Core..= video),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("JobTag" Core..=) Core.<$> jobTag,
                  ("MinConfidence" Core..=) Core.<$> minConfidence,
                  ("NotificationChannel" Core..=) Core.<$> notificationChannel])

instance Core.AWSRequest StartLabelDetection where
        type Rs StartLabelDetection = StartLabelDetectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartLabelDetectionResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartLabelDetectionResponse' smart constructor.
data StartLabelDetectionResponse = StartLabelDetectionResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier for the label detection job. Use @JobId@ to identify the job in a subsequent call to @GetLabelDetection@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLabelDetectionResponse' value with any optional fields omitted.
mkStartLabelDetectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartLabelDetectionResponse
mkStartLabelDetectionResponse responseStatus
  = StartLabelDetectionResponse'{jobId = Core.Nothing,
                                 responseStatus}

-- | The identifier for the label detection job. Use @JobId@ to identify the job in a subsequent call to @GetLabelDetection@ . 
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldrrsJobId :: Lens.Lens' StartLabelDetectionResponse (Core.Maybe Types.JobId)
sldrrsJobId = Lens.field @"jobId"
{-# INLINEABLE sldrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldrrsResponseStatus :: Lens.Lens' StartLabelDetectionResponse Core.Int
sldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
