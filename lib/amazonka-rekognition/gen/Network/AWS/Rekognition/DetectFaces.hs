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
    dfrsOrientationCorrection,
    dfrsFaceDetails,
    dfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectFaces' smart constructor.
data DetectFaces = DetectFaces'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    image :: Image,
    -- | An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete.
    --
    -- If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
    attributes :: Lude.Maybe [Attribute]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectFaces' with the minimum fields required to make a request.
--
-- * 'image' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
-- * 'attributes' - An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete.
--
-- If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
mkDetectFaces ::
  -- | 'image'
  Image ->
  DetectFaces
mkDetectFaces pImage_ =
  DetectFaces' {image = pImage_, attributes = Lude.Nothing}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfImage :: Lens.Lens' DetectFaces Image
dfImage = Lens.lens (image :: DetectFaces -> Image) (\s a -> s {image = a} :: DetectFaces)
{-# DEPRECATED dfImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete.
--
-- If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAttributes :: Lens.Lens' DetectFaces (Lude.Maybe [Attribute])
dfAttributes = Lens.lens (attributes :: DetectFaces -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: DetectFaces)
{-# DEPRECATED dfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest DetectFaces where
  type Rs DetectFaces = DetectFacesResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectFacesResponse'
            Lude.<$> (x Lude..?> "OrientationCorrection")
            Lude.<*> (x Lude..?> "FaceDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectFaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DetectFaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectFaces where
  toJSON DetectFaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Image" Lude..= image),
            ("Attributes" Lude..=) Lude.<$> attributes
          ]
      )

instance Lude.ToPath DetectFaces where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectFaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectFacesResponse' smart constructor.
data DetectFacesResponse = DetectFacesResponse'
  { -- | The value of @OrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
    -- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
    orientationCorrection :: Lude.Maybe OrientationCorrection,
    -- | Details of each face found in the image.
    faceDetails :: Lude.Maybe [FaceDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectFacesResponse' with the minimum fields required to make a request.
--
-- * 'orientationCorrection' - The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
-- * 'faceDetails' - Details of each face found in the image.
-- * 'responseStatus' - The response status code.
mkDetectFacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectFacesResponse
mkDetectFacesResponse pResponseStatus_ =
  DetectFacesResponse'
    { orientationCorrection = Lude.Nothing,
      faceDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The value of @OrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'orientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsOrientationCorrection :: Lens.Lens' DetectFacesResponse (Lude.Maybe OrientationCorrection)
dfrsOrientationCorrection = Lens.lens (orientationCorrection :: DetectFacesResponse -> Lude.Maybe OrientationCorrection) (\s a -> s {orientationCorrection = a} :: DetectFacesResponse)
{-# DEPRECATED dfrsOrientationCorrection "Use generic-lens or generic-optics with 'orientationCorrection' instead." #-}

-- | Details of each face found in the image.
--
-- /Note:/ Consider using 'faceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsFaceDetails :: Lens.Lens' DetectFacesResponse (Lude.Maybe [FaceDetail])
dfrsFaceDetails = Lens.lens (faceDetails :: DetectFacesResponse -> Lude.Maybe [FaceDetail]) (\s a -> s {faceDetails = a} :: DetectFacesResponse)
{-# DEPRECATED dfrsFaceDetails "Use generic-lens or generic-optics with 'faceDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DetectFacesResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DetectFacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectFacesResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
