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
    rcrsCelebrityFaces,
    rcrsOrientationCorrection,
    rcrsUnrecognizedFaces,
    rcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRecognizeCelebrities' smart constructor.
newtype RecognizeCelebrities = RecognizeCelebrities'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    image :: Image
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecognizeCelebrities' with the minimum fields required to make a request.
--
-- * 'image' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
mkRecognizeCelebrities ::
  -- | 'image'
  Image ->
  RecognizeCelebrities
mkRecognizeCelebrities pImage_ =
  RecognizeCelebrities' {image = pImage_}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcImage :: Lens.Lens' RecognizeCelebrities Image
rcImage = Lens.lens (image :: RecognizeCelebrities -> Image) (\s a -> s {image = a} :: RecognizeCelebrities)
{-# DEPRECATED rcImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.AWSRequest RecognizeCelebrities where
  type Rs RecognizeCelebrities = RecognizeCelebritiesResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          RecognizeCelebritiesResponse'
            Lude.<$> (x Lude..?> "CelebrityFaces" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "OrientationCorrection")
            Lude.<*> (x Lude..?> "UnrecognizedFaces" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RecognizeCelebrities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.RecognizeCelebrities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RecognizeCelebrities where
  toJSON RecognizeCelebrities' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Image" Lude..= image)])

instance Lude.ToPath RecognizeCelebrities where
  toPath = Lude.const "/"

instance Lude.ToQuery RecognizeCelebrities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRecognizeCelebritiesResponse' smart constructor.
data RecognizeCelebritiesResponse = RecognizeCelebritiesResponse'
  { -- | Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 64 celebrities in an image.
    celebrityFaces :: Lude.Maybe [Celebrity],
    -- | The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
    orientationCorrection :: Lude.Maybe OrientationCorrection,
    -- | Details about each unrecognized face in the image.
    unrecognizedFaces :: Lude.Maybe [ComparedFace],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecognizeCelebritiesResponse' with the minimum fields required to make a request.
--
-- * 'celebrityFaces' - Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 64 celebrities in an image.
-- * 'orientationCorrection' - The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
-- * 'unrecognizedFaces' - Details about each unrecognized face in the image.
-- * 'responseStatus' - The response status code.
mkRecognizeCelebritiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RecognizeCelebritiesResponse
mkRecognizeCelebritiesResponse pResponseStatus_ =
  RecognizeCelebritiesResponse'
    { celebrityFaces = Lude.Nothing,
      orientationCorrection = Lude.Nothing,
      unrecognizedFaces = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 64 celebrities in an image.
--
-- /Note:/ Consider using 'celebrityFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCelebrityFaces :: Lens.Lens' RecognizeCelebritiesResponse (Lude.Maybe [Celebrity])
rcrsCelebrityFaces = Lens.lens (celebrityFaces :: RecognizeCelebritiesResponse -> Lude.Maybe [Celebrity]) (\s a -> s {celebrityFaces = a} :: RecognizeCelebritiesResponse)
{-# DEPRECATED rcrsCelebrityFaces "Use generic-lens or generic-optics with 'celebrityFaces' instead." #-}

-- | The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
--
-- /Note:/ Consider using 'orientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsOrientationCorrection :: Lens.Lens' RecognizeCelebritiesResponse (Lude.Maybe OrientationCorrection)
rcrsOrientationCorrection = Lens.lens (orientationCorrection :: RecognizeCelebritiesResponse -> Lude.Maybe OrientationCorrection) (\s a -> s {orientationCorrection = a} :: RecognizeCelebritiesResponse)
{-# DEPRECATED rcrsOrientationCorrection "Use generic-lens or generic-optics with 'orientationCorrection' instead." #-}

-- | Details about each unrecognized face in the image.
--
-- /Note:/ Consider using 'unrecognizedFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsUnrecognizedFaces :: Lens.Lens' RecognizeCelebritiesResponse (Lude.Maybe [ComparedFace])
rcrsUnrecognizedFaces = Lens.lens (unrecognizedFaces :: RecognizeCelebritiesResponse -> Lude.Maybe [ComparedFace]) (\s a -> s {unrecognizedFaces = a} :: RecognizeCelebritiesResponse)
{-# DEPRECATED rcrsUnrecognizedFaces "Use generic-lens or generic-optics with 'unrecognizedFaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' RecognizeCelebritiesResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: RecognizeCelebritiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RecognizeCelebritiesResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
