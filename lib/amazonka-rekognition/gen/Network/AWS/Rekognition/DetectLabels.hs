{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects instances of real-world entities within an image (JPEG or PNG) provided as input. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; and concepts like landscape, evening, and nature. 
--
-- For an example, see Analyzing Images Stored in an Amazon S3 Bucket in the Amazon Rekognition Developer Guide.
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. 
-- For each object, scene, and concept the API returns one or more labels. Each label provides the object name, and the level of confidence that the image contains the object. For example, suppose the input image has a lighthouse, the sea, and a rock. The response includes all three labels, one for each object. 
-- @{Name: lighthouse, Confidence: 98.4629}@ 
-- @{Name: rock,Confidence: 79.2097}@ 
-- @{Name: sea,Confidence: 75.061}@ 
-- In the preceding example, the operation returns one label for each of the three objects. The operation can also return multiple labels for the same object in the image. For example, if the input image shows a flower (for example, a tulip), the operation might return the following three labels. 
-- @{Name: flower,Confidence: 99.0562}@ 
-- @{Name: plant,Confidence: 99.0562}@ 
-- @{Name: tulip,Confidence: 99.0562}@ 
-- In this example, the detection algorithm more precisely identifies the flower as a tulip.
-- In response, the API returns an array of labels. In addition, the response also includes the orientation correction. Optionally, you can specify @MinConfidence@ to control the confidence threshold for the labels returned. The default is 55%. You can also add the @MaxLabels@ parameter to limit the number of labels returned. 
-- @DetectLabels@ returns bounding boxes for instances of common object labels in an array of 'Instance' objects. An @Instance@ object contains a 'BoundingBox' object, for the location of the label on the image. It also includes the confidence by which the bounding box was detected.
-- @DetectLabels@ also returns a hierarchical taxonomy of detected labels. For example, a detected car might be assigned the label /car/ . The label /car/ has two parent labels: /Vehicle/ (its parent) and /Transportation/ (its grandparent). The response returns the entire list of ancestors for a label. Each ancestor is a unique label in the response. In the previous example, /Car/ , /Vehicle/ , and /Transportation/ are returned as unique labels in the response. 
-- This is a stateless API operation. That is, the operation does not persist any data.
-- This operation requires permissions to perform the @rekognition:DetectLabels@ action. 
module Network.AWS.Rekognition.DetectLabels
    (
    -- * Creating a request
      DetectLabels (..)
    , mkDetectLabels
    -- ** Request lenses
    , dlImage
    , dlMaxLabels
    , dlMinConfidence

    -- * Destructuring the response
    , DetectLabelsResponse (..)
    , mkDetectLabelsResponse
    -- ** Response lenses
    , dlrrsLabelModelVersion
    , dlrrsLabels
    , dlrrsOrientationCorrection
    , dlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectLabels' smart constructor.
data DetectLabels = DetectLabels'
  { image :: Types.Image
    -- ^ The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. Images stored in an S3 Bucket do not need to be base64-encoded.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
  , maxLabels :: Core.Maybe Core.Natural
    -- ^ Maximum number of labels you want the service to return in the response. The service returns the specified number of highest confidence labels. 
  , minConfidence :: Core.Maybe Core.Double
    -- ^ Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with confidence lower than this specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a confidence values greater than or equal to 55 percent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectLabels' value with any optional fields omitted.
mkDetectLabels
    :: Types.Image -- ^ 'image'
    -> DetectLabels
mkDetectLabels image
  = DetectLabels'{image, maxLabels = Core.Nothing,
                  minConfidence = Core.Nothing}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. Images stored in an S3 Bucket do not need to be base64-encoded.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlImage :: Lens.Lens' DetectLabels Types.Image
dlImage = Lens.field @"image"
{-# INLINEABLE dlImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | Maximum number of labels you want the service to return in the response. The service returns the specified number of highest confidence labels. 
--
-- /Note:/ Consider using 'maxLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMaxLabels :: Lens.Lens' DetectLabels (Core.Maybe Core.Natural)
dlMaxLabels = Lens.field @"maxLabels"
{-# INLINEABLE dlMaxLabels #-}
{-# DEPRECATED maxLabels "Use generic-lens or generic-optics with 'maxLabels' instead"  #-}

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with confidence lower than this specified value.
--
-- If @MinConfidence@ is not specified, the operation returns labels with a confidence values greater than or equal to 55 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMinConfidence :: Lens.Lens' DetectLabels (Core.Maybe Core.Double)
dlMinConfidence = Lens.field @"minConfidence"
{-# INLINEABLE dlMinConfidence #-}
{-# DEPRECATED minConfidence "Use generic-lens or generic-optics with 'minConfidence' instead"  #-}

instance Core.ToQuery DetectLabels where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectLabels where
        toHeaders DetectLabels{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.DetectLabels")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectLabels where
        toJSON DetectLabels{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Image" Core..= image),
                  ("MaxLabels" Core..=) Core.<$> maxLabels,
                  ("MinConfidence" Core..=) Core.<$> minConfidence])

instance Core.AWSRequest DetectLabels where
        type Rs DetectLabels = DetectLabelsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectLabelsResponse' Core.<$>
                   (x Core..:? "LabelModelVersion") Core.<*> x Core..:? "Labels"
                     Core.<*> x Core..:? "OrientationCorrection"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectLabelsResponse' smart constructor.
data DetectLabelsResponse = DetectLabelsResponse'
  { labelModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the label detection model that was used to detect labels.
  , labels :: Core.Maybe [Types.Label]
    -- ^ An array of labels for the real-world objects detected. 
  , orientationCorrection :: Core.Maybe Types.OrientationCorrection
    -- ^ The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectLabelsResponse' value with any optional fields omitted.
mkDetectLabelsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectLabelsResponse
mkDetectLabelsResponse responseStatus
  = DetectLabelsResponse'{labelModelVersion = Core.Nothing,
                          labels = Core.Nothing, orientationCorrection = Core.Nothing,
                          responseStatus}

-- | Version number of the label detection model that was used to detect labels.
--
-- /Note:/ Consider using 'labelModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLabelModelVersion :: Lens.Lens' DetectLabelsResponse (Core.Maybe Core.Text)
dlrrsLabelModelVersion = Lens.field @"labelModelVersion"
{-# INLINEABLE dlrrsLabelModelVersion #-}
{-# DEPRECATED labelModelVersion "Use generic-lens or generic-optics with 'labelModelVersion' instead"  #-}

-- | An array of labels for the real-world objects detected. 
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLabels :: Lens.Lens' DetectLabelsResponse (Core.Maybe [Types.Label])
dlrrsLabels = Lens.field @"labels"
{-# INLINEABLE dlrrsLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated. 
--
-- /Note:/ Consider using 'orientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsOrientationCorrection :: Lens.Lens' DetectLabelsResponse (Core.Maybe Types.OrientationCorrection)
dlrrsOrientationCorrection = Lens.field @"orientationCorrection"
{-# INLINEABLE dlrrsOrientationCorrection #-}
{-# DEPRECATED orientationCorrection "Use generic-lens or generic-optics with 'orientationCorrection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DetectLabelsResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
