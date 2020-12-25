{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.RecognizeCelebrities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of celebrities recognized in the input image. For more information, see Recognizing Celebrities in the Amazon Rekognition Developer Guide.
--
-- @RecognizeCelebrities@ returns the 64 largest faces in the image. It lists recognized celebrities in the @CelebrityFaces@ array and unrecognized faces in the @UnrecognizedFaces@ array. @RecognizeCelebrities@ doesn't return celebrities whose faces aren't among the largest 64 faces in the image.
-- For each celebrity recognized, @RecognizeCelebrities@ returns a @Celebrity@ object. The @Celebrity@ object contains the celebrity name, ID, URL links to additional information, match confidence, and a @ComparedFace@ object that you can use to locate the celebrity's face on the image.
-- Amazon Rekognition doesn't retain information about which images a celebrity has been recognized in. Your application must store this information and use the @Celebrity@ ID property as a unique identifier for the celebrity. If you don't store the celebrity name or additional information URLs returned by @RecognizeCelebrities@ , you will need the ID to identify the celebrity in a call to the 'GetCelebrityInfo' operation.
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
-- For an example, see Recognizing Celebrities in an Image in the Amazon Rekognition Developer Guide.
-- This operation requires permissions to perform the @rekognition:RecognizeCelebrities@ operation.
module Network.AWS.Rekognition.RecognizeCelebrities
  ( -- * Creating a request
    RecognizeCelebrities (..),
    mkRecognizeCelebrities,

    -- ** Request lenses
    rcImage,

    -- * Destructuring the response
    RecognizeCelebritiesResponse (..),
    mkRecognizeCelebritiesResponse,

    -- ** Response lenses
    rcrrsCelebrityFaces,
    rcrrsOrientationCorrection,
    rcrrsUnrecognizedFaces,
    rcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRecognizeCelebrities' smart constructor.
newtype RecognizeCelebrities = RecognizeCelebrities'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    image :: Types.Image
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RecognizeCelebrities' value with any optional fields omitted.
mkRecognizeCelebrities ::
  -- | 'image'
  Types.Image ->
  RecognizeCelebrities
mkRecognizeCelebrities image = RecognizeCelebrities' {image}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcImage :: Lens.Lens' RecognizeCelebrities Types.Image
rcImage = Lens.field @"image"
{-# DEPRECATED rcImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Core.FromJSON RecognizeCelebrities where
  toJSON RecognizeCelebrities {..} =
    Core.object (Core.catMaybes [Core.Just ("Image" Core..= image)])

instance Core.AWSRequest RecognizeCelebrities where
  type Rs RecognizeCelebrities = RecognizeCelebritiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.RecognizeCelebrities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RecognizeCelebritiesResponse'
            Core.<$> (x Core..:? "CelebrityFaces")
            Core.<*> (x Core..:? "OrientationCorrection")
            Core.<*> (x Core..:? "UnrecognizedFaces")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRecognizeCelebritiesResponse' smart constructor.
data RecognizeCelebritiesResponse = RecognizeCelebritiesResponse'
  { -- | Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 64 celebrities in an image.
    celebrityFaces :: Core.Maybe [Types.Celebrity],
    -- | The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
    orientationCorrection :: Core.Maybe Types.OrientationCorrection,
    -- | Details about each unrecognized face in the image.
    unrecognizedFaces :: Core.Maybe [Types.ComparedFace],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecognizeCelebritiesResponse' value with any optional fields omitted.
mkRecognizeCelebritiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RecognizeCelebritiesResponse
mkRecognizeCelebritiesResponse responseStatus =
  RecognizeCelebritiesResponse'
    { celebrityFaces = Core.Nothing,
      orientationCorrection = Core.Nothing,
      unrecognizedFaces = Core.Nothing,
      responseStatus
    }

-- | Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 64 celebrities in an image.
--
-- /Note:/ Consider using 'celebrityFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCelebrityFaces :: Lens.Lens' RecognizeCelebritiesResponse (Core.Maybe [Types.Celebrity])
rcrrsCelebrityFaces = Lens.field @"celebrityFaces"
{-# DEPRECATED rcrrsCelebrityFaces "Use generic-lens or generic-optics with 'celebrityFaces' instead." #-}

-- | The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
--
-- /Note:/ Consider using 'orientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsOrientationCorrection :: Lens.Lens' RecognizeCelebritiesResponse (Core.Maybe Types.OrientationCorrection)
rcrrsOrientationCorrection = Lens.field @"orientationCorrection"
{-# DEPRECATED rcrrsOrientationCorrection "Use generic-lens or generic-optics with 'orientationCorrection' instead." #-}

-- | Details about each unrecognized face in the image.
--
-- /Note:/ Consider using 'unrecognizedFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsUnrecognizedFaces :: Lens.Lens' RecognizeCelebritiesResponse (Core.Maybe [Types.ComparedFace])
rcrrsUnrecognizedFaces = Lens.field @"unrecognizedFaces"
{-# DEPRECATED rcrrsUnrecognizedFaces "Use generic-lens or generic-optics with 'unrecognizedFaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' RecognizeCelebritiesResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
