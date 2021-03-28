{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectModerationLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects unsafe content in a specified JPEG or PNG format image. Use @DetectModerationLabels@ to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.
--
-- To filter images, use the labels returned by @DetectModerationLabels@ to determine which types of content are appropriate.
-- For information about moderation labels, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. 
module Network.AWS.Rekognition.DetectModerationLabels
    (
    -- * Creating a request
      DetectModerationLabels (..)
    , mkDetectModerationLabels
    -- ** Request lenses
    , dmlImage
    , dmlHumanLoopConfig
    , dmlMinConfidence

    -- * Destructuring the response
    , DetectModerationLabelsResponse (..)
    , mkDetectModerationLabelsResponse
    -- ** Response lenses
    , dmlrrsHumanLoopActivationOutput
    , dmlrrsModerationLabels
    , dmlrrsModerationModelVersion
    , dmlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectModerationLabels' smart constructor.
data DetectModerationLabels = DetectModerationLabels'
  { image :: Types.Image
    -- ^ The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported. 
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
  , humanLoopConfig :: Core.Maybe Types.HumanLoopConfig
    -- ^ Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
  , minConfidence :: Core.Maybe Core.Double
    -- ^ Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectModerationLabels' value with any optional fields omitted.
mkDetectModerationLabels
    :: Types.Image -- ^ 'image'
    -> DetectModerationLabels
mkDetectModerationLabels image
  = DetectModerationLabels'{image, humanLoopConfig = Core.Nothing,
                            minConfidence = Core.Nothing}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported. 
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlImage :: Lens.Lens' DetectModerationLabels Types.Image
dmlImage = Lens.field @"image"
{-# INLINEABLE dmlImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
--
-- /Note:/ Consider using 'humanLoopConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlHumanLoopConfig :: Lens.Lens' DetectModerationLabels (Core.Maybe Types.HumanLoopConfig)
dmlHumanLoopConfig = Lens.field @"humanLoopConfig"
{-# INLINEABLE dmlHumanLoopConfig #-}
{-# DEPRECATED humanLoopConfig "Use generic-lens or generic-optics with 'humanLoopConfig' instead"  #-}

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlMinConfidence :: Lens.Lens' DetectModerationLabels (Core.Maybe Core.Double)
dmlMinConfidence = Lens.field @"minConfidence"
{-# INLINEABLE dmlMinConfidence #-}
{-# DEPRECATED minConfidence "Use generic-lens or generic-optics with 'minConfidence' instead"  #-}

instance Core.ToQuery DetectModerationLabels where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectModerationLabels where
        toHeaders DetectModerationLabels{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.DetectModerationLabels")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectModerationLabels where
        toJSON DetectModerationLabels{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Image" Core..= image),
                  ("HumanLoopConfig" Core..=) Core.<$> humanLoopConfig,
                  ("MinConfidence" Core..=) Core.<$> minConfidence])

instance Core.AWSRequest DetectModerationLabels where
        type Rs DetectModerationLabels = DetectModerationLabelsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectModerationLabelsResponse' Core.<$>
                   (x Core..:? "HumanLoopActivationOutput") Core.<*>
                     x Core..:? "ModerationLabels"
                     Core.<*> x Core..:? "ModerationModelVersion"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectModerationLabelsResponse' smart constructor.
data DetectModerationLabelsResponse = DetectModerationLabelsResponse'
  { humanLoopActivationOutput :: Core.Maybe Types.HumanLoopActivationOutput
    -- ^ Shows the results of the human in the loop evaluation.
  , moderationLabels :: Core.Maybe [Types.ModerationLabel]
    -- ^ Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
  , moderationModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the moderation detection model that was used to detect unsafe content.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectModerationLabelsResponse' value with any optional fields omitted.
mkDetectModerationLabelsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectModerationLabelsResponse
mkDetectModerationLabelsResponse responseStatus
  = DetectModerationLabelsResponse'{humanLoopActivationOutput =
                                      Core.Nothing,
                                    moderationLabels = Core.Nothing,
                                    moderationModelVersion = Core.Nothing, responseStatus}

-- | Shows the results of the human in the loop evaluation.
--
-- /Note:/ Consider using 'humanLoopActivationOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrrsHumanLoopActivationOutput :: Lens.Lens' DetectModerationLabelsResponse (Core.Maybe Types.HumanLoopActivationOutput)
dmlrrsHumanLoopActivationOutput = Lens.field @"humanLoopActivationOutput"
{-# INLINEABLE dmlrrsHumanLoopActivationOutput #-}
{-# DEPRECATED humanLoopActivationOutput "Use generic-lens or generic-optics with 'humanLoopActivationOutput' instead"  #-}

-- | Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
--
-- /Note:/ Consider using 'moderationLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrrsModerationLabels :: Lens.Lens' DetectModerationLabelsResponse (Core.Maybe [Types.ModerationLabel])
dmlrrsModerationLabels = Lens.field @"moderationLabels"
{-# INLINEABLE dmlrrsModerationLabels #-}
{-# DEPRECATED moderationLabels "Use generic-lens or generic-optics with 'moderationLabels' instead"  #-}

-- | Version number of the moderation detection model that was used to detect unsafe content.
--
-- /Note:/ Consider using 'moderationModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrrsModerationModelVersion :: Lens.Lens' DetectModerationLabelsResponse (Core.Maybe Core.Text)
dmlrrsModerationModelVersion = Lens.field @"moderationModelVersion"
{-# INLINEABLE dmlrrsModerationModelVersion #-}
{-# DEPRECATED moderationModelVersion "Use generic-lens or generic-optics with 'moderationModelVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlrrsResponseStatus :: Lens.Lens' DetectModerationLabelsResponse Core.Int
dmlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
