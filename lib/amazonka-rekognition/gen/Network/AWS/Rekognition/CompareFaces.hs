{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CompareFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Compares a face in the /source/ input image with each of the 100 largest faces detected in the /target/ input image.
--
-- You pass the input and target images either as base64-encoded image bytes or as references to images in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes isn't supported. The image must be formatted as a PNG or JPEG file.
-- In response, the operation returns an array of face matches ordered by similarity score in descending order. For each face match, the response provides a bounding box of the face, facial landmarks, pose details (pitch, role, and yaw), quality (brightness and sharpness), and confidence value (indicating the level of confidence that the bounding box contains a face). The response also provides a similarity score, which indicates how closely the faces match.
-- @CompareFaces@ also returns an array of faces that don't match the source image. For each face, it returns a bounding box, confidence value, landmarks, pose details, and quality. The response also returns information about the face in the source image, including the bounding box of the face and confidence value.
-- The @QualityFilter@ input parameter allows you to filter out detected faces that don’t meet a required quality bar. The quality bar is based on a variety of common use cases. Use @QualityFilter@ to set the quality bar by specifying @LOW@ , @MEDIUM@ , or @HIGH@ . If you do not want to filter detected faces, specify @NONE@ . The default value is @NONE@ .
-- If the image doesn't contain Exif metadata, @CompareFaces@ returns orientation information for the source and target images. Use these values to display the images with the correct image orientation.
-- If no faces are detected in the source or target images, @CompareFaces@ returns an @InvalidParameterException@ error.
-- For an example, see Comparing Faces in Images in the Amazon Rekognition Developer Guide.
-- This operation requires permissions to perform the @rekognition:CompareFaces@ action.
module Network.AWS.Rekognition.CompareFaces
  ( -- * Creating a request
    CompareFaces (..),
    mkCompareFaces,

    -- ** Request lenses
    cfSourceImage,
    cfTargetImage,
    cfQualityFilter,
    cfSimilarityThreshold,

    -- * Destructuring the response
    CompareFacesResponse (..),
    mkCompareFacesResponse,

    -- ** Response lenses
    cfrrsFaceMatches,
    cfrrsSourceImageFace,
    cfrrsSourceImageOrientationCorrection,
    cfrrsTargetImageOrientationCorrection,
    cfrrsUnmatchedFaces,
    cfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCompareFaces' smart constructor.
data CompareFaces = CompareFaces'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    sourceImage :: Types.Image,
    -- | The target image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    targetImage :: Types.Image,
    -- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't compared. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
    --
    -- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
    qualityFilter :: Core.Maybe Types.QualityFilter,
    -- | The minimum level of confidence in the face matches that a match must meet to be included in the @FaceMatches@ array.
    similarityThreshold :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompareFaces' value with any optional fields omitted.
mkCompareFaces ::
  -- | 'sourceImage'
  Types.Image ->
  -- | 'targetImage'
  Types.Image ->
  CompareFaces
mkCompareFaces sourceImage targetImage =
  CompareFaces'
    { sourceImage,
      targetImage,
      qualityFilter = Core.Nothing,
      similarityThreshold = Core.Nothing
    }

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'sourceImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSourceImage :: Lens.Lens' CompareFaces Types.Image
cfSourceImage = Lens.field @"sourceImage"
{-# DEPRECATED cfSourceImage "Use generic-lens or generic-optics with 'sourceImage' instead." #-}

-- | The target image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'targetImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTargetImage :: Lens.Lens' CompareFaces Types.Image
cfTargetImage = Lens.field @"targetImage"
{-# DEPRECATED cfTargetImage "Use generic-lens or generic-optics with 'targetImage' instead." #-}

-- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't compared. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
--
-- /Note:/ Consider using 'qualityFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfQualityFilter :: Lens.Lens' CompareFaces (Core.Maybe Types.QualityFilter)
cfQualityFilter = Lens.field @"qualityFilter"
{-# DEPRECATED cfQualityFilter "Use generic-lens or generic-optics with 'qualityFilter' instead." #-}

-- | The minimum level of confidence in the face matches that a match must meet to be included in the @FaceMatches@ array.
--
-- /Note:/ Consider using 'similarityThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSimilarityThreshold :: Lens.Lens' CompareFaces (Core.Maybe Core.Double)
cfSimilarityThreshold = Lens.field @"similarityThreshold"
{-# DEPRECATED cfSimilarityThreshold "Use generic-lens or generic-optics with 'similarityThreshold' instead." #-}

instance Core.FromJSON CompareFaces where
  toJSON CompareFaces {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceImage" Core..= sourceImage),
            Core.Just ("TargetImage" Core..= targetImage),
            ("QualityFilter" Core..=) Core.<$> qualityFilter,
            ("SimilarityThreshold" Core..=) Core.<$> similarityThreshold
          ]
      )

instance Core.AWSRequest CompareFaces where
  type Rs CompareFaces = CompareFacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.CompareFaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CompareFacesResponse'
            Core.<$> (x Core..:? "FaceMatches")
            Core.<*> (x Core..:? "SourceImageFace")
            Core.<*> (x Core..:? "SourceImageOrientationCorrection")
            Core.<*> (x Core..:? "TargetImageOrientationCorrection")
            Core.<*> (x Core..:? "UnmatchedFaces")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCompareFacesResponse' smart constructor.
data CompareFacesResponse = CompareFacesResponse'
  { -- | An array of faces in the target image that match the source image face. Each @CompareFacesMatch@ object provides the bounding box, the confidence level that the bounding box contains a face, and the similarity score for the face in the bounding box and the face in the source image.
    faceMatches :: Core.Maybe [Types.CompareFacesMatch],
    -- | The face in the source image that was used for comparison.
    sourceImageFace :: Core.Maybe Types.ComparedSourceImageFace,
    -- | The value of @SourceImageOrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
    -- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
    sourceImageOrientationCorrection :: Core.Maybe Types.OrientationCorrection,
    -- | The value of @TargetImageOrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
    -- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
    targetImageOrientationCorrection :: Core.Maybe Types.OrientationCorrection,
    -- | An array of faces in the target image that did not match the source image face.
    unmatchedFaces :: Core.Maybe [Types.ComparedFace],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompareFacesResponse' value with any optional fields omitted.
mkCompareFacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CompareFacesResponse
mkCompareFacesResponse responseStatus =
  CompareFacesResponse'
    { faceMatches = Core.Nothing,
      sourceImageFace = Core.Nothing,
      sourceImageOrientationCorrection = Core.Nothing,
      targetImageOrientationCorrection = Core.Nothing,
      unmatchedFaces = Core.Nothing,
      responseStatus
    }

-- | An array of faces in the target image that match the source image face. Each @CompareFacesMatch@ object provides the bounding box, the confidence level that the bounding box contains a face, and the similarity score for the face in the bounding box and the face in the source image.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsFaceMatches :: Lens.Lens' CompareFacesResponse (Core.Maybe [Types.CompareFacesMatch])
cfrrsFaceMatches = Lens.field @"faceMatches"
{-# DEPRECATED cfrrsFaceMatches "Use generic-lens or generic-optics with 'faceMatches' instead." #-}

-- | The face in the source image that was used for comparison.
--
-- /Note:/ Consider using 'sourceImageFace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsSourceImageFace :: Lens.Lens' CompareFacesResponse (Core.Maybe Types.ComparedSourceImageFace)
cfrrsSourceImageFace = Lens.field @"sourceImageFace"
{-# DEPRECATED cfrrsSourceImageFace "Use generic-lens or generic-optics with 'sourceImageFace' instead." #-}

-- | The value of @SourceImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'sourceImageOrientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsSourceImageOrientationCorrection :: Lens.Lens' CompareFacesResponse (Core.Maybe Types.OrientationCorrection)
cfrrsSourceImageOrientationCorrection = Lens.field @"sourceImageOrientationCorrection"
{-# DEPRECATED cfrrsSourceImageOrientationCorrection "Use generic-lens or generic-optics with 'sourceImageOrientationCorrection' instead." #-}

-- | The value of @TargetImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'targetImageOrientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsTargetImageOrientationCorrection :: Lens.Lens' CompareFacesResponse (Core.Maybe Types.OrientationCorrection)
cfrrsTargetImageOrientationCorrection = Lens.field @"targetImageOrientationCorrection"
{-# DEPRECATED cfrrsTargetImageOrientationCorrection "Use generic-lens or generic-optics with 'targetImageOrientationCorrection' instead." #-}

-- | An array of faces in the target image that did not match the source image face.
--
-- /Note:/ Consider using 'unmatchedFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsUnmatchedFaces :: Lens.Lens' CompareFacesResponse (Core.Maybe [Types.ComparedFace])
cfrrsUnmatchedFaces = Lens.field @"unmatchedFaces"
{-# DEPRECATED cfrrsUnmatchedFaces "Use generic-lens or generic-optics with 'unmatchedFaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CompareFacesResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
