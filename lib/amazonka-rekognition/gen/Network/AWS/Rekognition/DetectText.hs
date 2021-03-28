{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectText
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects text in the input image and converts it into machine-readable text.
--
-- Pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, you must pass it as a reference to an image in an Amazon S3 bucket. For the AWS CLI, passing image bytes is not supported. The image must be either a .png or .jpeg formatted file. 
-- The @DetectText@ operation returns text in an array of 'TextDetection' elements, @TextDetections@ . Each @TextDetection@ element provides information about a single word or line of text that was detected in the image. 
-- A word is one or more ISO basic latin script characters that are not separated by spaces. @DetectText@ can detect up to 50 words in an image.
-- A line is a string of equally spaced words. A line isn't necessarily a complete sentence. For example, a driver's license number is detected as a line. A line ends when there is no aligned text after it. Also, a line ends when there is a large gap between words, relative to the length of the words. This means, depending on the gap between words, Amazon Rekognition may detect multiple lines in text aligned in the same direction. Periods don't represent the end of a line. If a sentence spans multiple lines, the @DetectText@ operation returns multiple lines.
-- To determine whether a @TextDetection@ element is a line of text or a word, use the @TextDetection@ object @Type@ field. 
-- To be detected, text must be within +/- 90 degrees orientation of the horizontal axis.
-- For more information, see DetectText in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.DetectText
    (
    -- * Creating a request
      DetectText (..)
    , mkDetectText
    -- ** Request lenses
    , dtImage
    , dtFilters

    -- * Destructuring the response
    , DetectTextResponse (..)
    , mkDetectTextResponse
    -- ** Response lenses
    , dtrrsTextDetections
    , dtrrsTextModelVersion
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectText' smart constructor.
data DetectText = DetectText'
  { image :: Types.Image
    -- ^ The input image as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes. 
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
  , filters :: Core.Maybe Types.DetectTextFilters
    -- ^ Optional parameters that let you set the criteria that the text must meet to be included in your response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectText' value with any optional fields omitted.
mkDetectText
    :: Types.Image -- ^ 'image'
    -> DetectText
mkDetectText image = DetectText'{image, filters = Core.Nothing}

-- | The input image as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes. 
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtImage :: Lens.Lens' DetectText Types.Image
dtImage = Lens.field @"image"
{-# INLINEABLE dtImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | Optional parameters that let you set the criteria that the text must meet to be included in your response.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DetectText (Core.Maybe Types.DetectTextFilters)
dtFilters = Lens.field @"filters"
{-# INLINEABLE dtFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

instance Core.ToQuery DetectText where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectText where
        toHeaders DetectText{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.DetectText")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectText where
        toJSON DetectText{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Image" Core..= image),
                  ("Filters" Core..=) Core.<$> filters])

instance Core.AWSRequest DetectText where
        type Rs DetectText = DetectTextResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectTextResponse' Core.<$>
                   (x Core..:? "TextDetections") Core.<*>
                     x Core..:? "TextModelVersion"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectTextResponse' smart constructor.
data DetectTextResponse = DetectTextResponse'
  { textDetections :: Core.Maybe [Types.TextDetection]
    -- ^ An array of text that was detected in the input image.
  , textModelVersion :: Core.Maybe Core.Text
    -- ^ The model version used to detect text.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectTextResponse' value with any optional fields omitted.
mkDetectTextResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectTextResponse
mkDetectTextResponse responseStatus
  = DetectTextResponse'{textDetections = Core.Nothing,
                        textModelVersion = Core.Nothing, responseStatus}

-- | An array of text that was detected in the input image.
--
-- /Note:/ Consider using 'textDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTextDetections :: Lens.Lens' DetectTextResponse (Core.Maybe [Types.TextDetection])
dtrrsTextDetections = Lens.field @"textDetections"
{-# INLINEABLE dtrrsTextDetections #-}
{-# DEPRECATED textDetections "Use generic-lens or generic-optics with 'textDetections' instead"  #-}

-- | The model version used to detect text.
--
-- /Note:/ Consider using 'textModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTextModelVersion :: Lens.Lens' DetectTextResponse (Core.Maybe Core.Text)
dtrrsTextModelVersion = Lens.field @"textModelVersion"
{-# INLINEABLE dtrrsTextModelVersion #-}
{-# DEPRECATED textModelVersion "Use generic-lens or generic-optics with 'textModelVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DetectTextResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
