{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartFaceSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous search for faces in a collection that match the faces of persons detected in a stored video.
--
-- The video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartFaceSearch@ returns a job identifier (@JobId@ ) which you use to get the search results once the search has completed. When searching is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the search results, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetFaceSearch' and pass the job identifier (@JobId@ ) from the initial call to @StartFaceSearch@ . For more information, see 'procedure-person-search-videos' .
module Network.AWS.Rekognition.StartFaceSearch
    (
    -- * Creating a request
      StartFaceSearch (..)
    , mkStartFaceSearch
    -- ** Request lenses
    , sfsVideo
    , sfsCollectionId
    , sfsClientRequestToken
    , sfsFaceMatchThreshold
    , sfsJobTag
    , sfsNotificationChannel

    -- * Destructuring the response
    , StartFaceSearchResponse (..)
    , mkStartFaceSearchResponse
    -- ** Response lenses
    , sfsrrsJobId
    , sfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartFaceSearch' smart constructor.
data StartFaceSearch = StartFaceSearch'
  { video :: Types.Video
    -- ^ The video you want to search. The video must be stored in an Amazon S3 bucket. 
  , collectionId :: Types.CollectionId
    -- ^ ID of the collection that contains the faces you want to search for.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceSearch@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once. 
  , faceMatchThreshold :: Core.Maybe Core.Double
    -- ^ The minimum confidence in the person match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
  , jobTag :: Core.Maybe Types.JobTag
    -- ^ An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
  , notificationChannel :: Core.Maybe Types.NotificationChannel
    -- ^ The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the search. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartFaceSearch' value with any optional fields omitted.
mkStartFaceSearch
    :: Types.Video -- ^ 'video'
    -> Types.CollectionId -- ^ 'collectionId'
    -> StartFaceSearch
mkStartFaceSearch video collectionId
  = StartFaceSearch'{video, collectionId,
                     clientRequestToken = Core.Nothing,
                     faceMatchThreshold = Core.Nothing, jobTag = Core.Nothing,
                     notificationChannel = Core.Nothing}

-- | The video you want to search. The video must be stored in an Amazon S3 bucket. 
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsVideo :: Lens.Lens' StartFaceSearch Types.Video
sfsVideo = Lens.field @"video"
{-# INLINEABLE sfsVideo #-}
{-# DEPRECATED video "Use generic-lens or generic-optics with 'video' instead"  #-}

-- | ID of the collection that contains the faces you want to search for.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsCollectionId :: Lens.Lens' StartFaceSearch Types.CollectionId
sfsCollectionId = Lens.field @"collectionId"
{-# INLINEABLE sfsCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceSearch@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsClientRequestToken :: Lens.Lens' StartFaceSearch (Core.Maybe Types.ClientRequestToken)
sfsClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE sfsClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The minimum confidence in the person match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsFaceMatchThreshold :: Lens.Lens' StartFaceSearch (Core.Maybe Core.Double)
sfsFaceMatchThreshold = Lens.field @"faceMatchThreshold"
{-# INLINEABLE sfsFaceMatchThreshold #-}
{-# DEPRECATED faceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead"  #-}

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsJobTag :: Lens.Lens' StartFaceSearch (Core.Maybe Types.JobTag)
sfsJobTag = Lens.field @"jobTag"
{-# INLINEABLE sfsJobTag #-}
{-# DEPRECATED jobTag "Use generic-lens or generic-optics with 'jobTag' instead"  #-}

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the search. 
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsNotificationChannel :: Lens.Lens' StartFaceSearch (Core.Maybe Types.NotificationChannel)
sfsNotificationChannel = Lens.field @"notificationChannel"
{-# INLINEABLE sfsNotificationChannel #-}
{-# DEPRECATED notificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead"  #-}

instance Core.ToQuery StartFaceSearch where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartFaceSearch where
        toHeaders StartFaceSearch{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.StartFaceSearch")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartFaceSearch where
        toJSON StartFaceSearch{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Video" Core..= video),
                  Core.Just ("CollectionId" Core..= collectionId),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("FaceMatchThreshold" Core..=) Core.<$> faceMatchThreshold,
                  ("JobTag" Core..=) Core.<$> jobTag,
                  ("NotificationChannel" Core..=) Core.<$> notificationChannel])

instance Core.AWSRequest StartFaceSearch where
        type Rs StartFaceSearch = StartFaceSearchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartFaceSearchResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartFaceSearchResponse' smart constructor.
data StartFaceSearchResponse = StartFaceSearchResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier for the search job. Use @JobId@ to identify the job in a subsequent call to @GetFaceSearch@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartFaceSearchResponse' value with any optional fields omitted.
mkStartFaceSearchResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartFaceSearchResponse
mkStartFaceSearchResponse responseStatus
  = StartFaceSearchResponse'{jobId = Core.Nothing, responseStatus}

-- | The identifier for the search job. Use @JobId@ to identify the job in a subsequent call to @GetFaceSearch@ . 
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsrrsJobId :: Lens.Lens' StartFaceSearchResponse (Core.Maybe Types.JobId)
sfsrrsJobId = Lens.field @"jobId"
{-# INLINEABLE sfsrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsrrsResponseStatus :: Lens.Lens' StartFaceSearchResponse Core.Int
sfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
