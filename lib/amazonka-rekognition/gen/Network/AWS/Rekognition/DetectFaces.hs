{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces within an image that is provided as input.
--
-- @DetectFaces@ detects the 100 largest faces in the image. For each face detected, the operation returns face details. These details include a bounding box of the face, a confidence value (that the bounding box contains a face), and a fixed set of attributes such as facial landmarks (for example, coordinates of eye and mouth), presence of beard, sunglasses, and so on.
-- The face-detection algorithm is most effective on frontal faces. For non-frontal or obscured faces, the algorithm might not detect the faces or might detect faces with lower confidence.
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
-- This operation requires permissions to perform the @rekognition:DetectFaces@ action.
module Network.AWS.Rekognition.DetectFaces
  ( -- * Creating a request
    DetectFaces (..),
    mkDetectFaces,

    -- ** Request lenses
    dfImage,
    dfAttributes,

    -- * Destructuring the response
    DetectFacesResponse (..),
    mkDetectFacesResponse,

    -- ** Response lenses
    dfrrsFaceDetails,
    dfrrsOrientationCorrection,
    dfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectFaces' smart constructor.
data DetectFaces = DetectFaces'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    image :: Types.Image,
    -- | An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete.
    --
    -- If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
    attributes :: Core.Maybe [Types.Attribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectFaces' value with any optional fields omitted.
mkDetectFaces ::
  -- | 'image'
  Types.Image ->
  DetectFaces
mkDetectFaces image =
  DetectFaces' {image, attributes = Core.Nothing}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfImage :: Lens.Lens' DetectFaces Types.Image
dfImage = Lens.field @"image"
{-# DEPRECATED dfImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete.
--
-- If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAttributes :: Lens.Lens' DetectFaces (Core.Maybe [Types.Attribute])
dfAttributes = Lens.field @"attributes"
{-# DEPRECATED dfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON DetectFaces where
  toJSON DetectFaces {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Image" Core..= image),
            ("Attributes" Core..=) Core.<$> attributes
          ]
      )

instance Core.AWSRequest DetectFaces where
  type Rs DetectFaces = DetectFacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.DetectFaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectFacesResponse'
            Core.<$> (x Core..:? "FaceDetails")
            Core.<*> (x Core..:? "OrientationCorrection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectFacesResponse' smart constructor.
data DetectFacesResponse = DetectFacesResponse'
  { -- | Details of each face found in the image.
    faceDetails :: Core.Maybe [Types.FaceDetail],
    -- | The value of @OrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
    -- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
    orientationCorrection :: Core.Maybe Types.OrientationCorrection,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectFacesResponse' value with any optional fields omitted.
mkDetectFacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectFacesResponse
mkDetectFacesResponse responseStatus =
  DetectFacesResponse'
    { faceDetails = Core.Nothing,
      orientationCorrection = Core.Nothing,
      responseStatus
    }

-- | Details of each face found in the image.
--
-- /Note:/ Consider using 'faceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsFaceDetails :: Lens.Lens' DetectFacesResponse (Core.Maybe [Types.FaceDetail])
dfrrsFaceDetails = Lens.field @"faceDetails"
{-# DEPRECATED dfrrsFaceDetails "Use generic-lens or generic-optics with 'faceDetails' instead." #-}

-- | The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'orientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsOrientationCorrection :: Lens.Lens' DetectFacesResponse (Core.Maybe Types.OrientationCorrection)
dfrrsOrientationCorrection = Lens.field @"orientationCorrection"
{-# DEPRECATED dfrrsOrientationCorrection "Use generic-lens or generic-optics with 'orientationCorrection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DetectFacesResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
