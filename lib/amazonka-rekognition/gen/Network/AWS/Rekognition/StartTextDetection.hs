{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartTextDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of text in a stored video.
--
-- Amazon Rekognition Video can detect text in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartTextDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When text detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the text detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call 'GetTextDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartTextDetection@ . 
module Network.AWS.Rekognition.StartTextDetection
    (
    -- * Creating a request
      StartTextDetection (..)
    , mkStartTextDetection
    -- ** Request lenses
    , stdVideo
    , stdClientRequestToken
    , stdFilters
    , stdJobTag
    , stdNotificationChannel

    -- * Destructuring the response
    , StartTextDetectionResponse (..)
    , mkStartTextDetectionResponse
    -- ** Response lenses
    , stdrrsJobId
    , stdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartTextDetection' smart constructor.
data StartTextDetection = StartTextDetection'
  { video :: Types.Video
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
  , filters :: Core.Maybe Types.StartTextDetectionFilters
    -- ^ Optional parameters that let you set criteria the text must meet to be included in your response.
  , jobTag :: Core.Maybe Types.JobTag
    -- ^ An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
  , notificationChannel :: Core.Maybe Types.NotificationChannel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTextDetection' value with any optional fields omitted.
mkStartTextDetection
    :: Types.Video -- ^ 'video'
    -> StartTextDetection
mkStartTextDetection video
  = StartTextDetection'{video, clientRequestToken = Core.Nothing,
                        filters = Core.Nothing, jobTag = Core.Nothing,
                        notificationChannel = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdVideo :: Lens.Lens' StartTextDetection Types.Video
stdVideo = Lens.field @"video"
{-# INLINEABLE stdVideo #-}
{-# DEPRECATED video "Use generic-lens or generic-optics with 'video' instead"  #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdClientRequestToken :: Lens.Lens' StartTextDetection (Core.Maybe Types.ClientRequestToken)
stdClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE stdClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Optional parameters that let you set criteria the text must meet to be included in your response.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFilters :: Lens.Lens' StartTextDetection (Core.Maybe Types.StartTextDetectionFilters)
stdFilters = Lens.field @"filters"
{-# INLINEABLE stdFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdJobTag :: Lens.Lens' StartTextDetection (Core.Maybe Types.JobTag)
stdJobTag = Lens.field @"jobTag"
{-# INLINEABLE stdJobTag #-}
{-# DEPRECATED jobTag "Use generic-lens or generic-optics with 'jobTag' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdNotificationChannel :: Lens.Lens' StartTextDetection (Core.Maybe Types.NotificationChannel)
stdNotificationChannel = Lens.field @"notificationChannel"
{-# INLINEABLE stdNotificationChannel #-}
{-# DEPRECATED notificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead"  #-}

instance Core.ToQuery StartTextDetection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartTextDetection where
        toHeaders StartTextDetection{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.StartTextDetection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartTextDetection where
        toJSON StartTextDetection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Video" Core..= video),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Filters" Core..=) Core.<$> filters,
                  ("JobTag" Core..=) Core.<$> jobTag,
                  ("NotificationChannel" Core..=) Core.<$> notificationChannel])

instance Core.AWSRequest StartTextDetection where
        type Rs StartTextDetection = StartTextDetectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartTextDetectionResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartTextDetectionResponse' smart constructor.
data StartTextDetectionResponse = StartTextDetectionResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTextDetectionResponse' value with any optional fields omitted.
mkStartTextDetectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartTextDetectionResponse
mkStartTextDetectionResponse responseStatus
  = StartTextDetectionResponse'{jobId = Core.Nothing, responseStatus}

-- | Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdrrsJobId :: Lens.Lens' StartTextDetectionResponse (Core.Maybe Types.JobId)
stdrrsJobId = Lens.field @"jobId"
{-# INLINEABLE stdrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdrrsResponseStatus :: Lens.Lens' StartTextDetectionResponse Core.Int
stdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
