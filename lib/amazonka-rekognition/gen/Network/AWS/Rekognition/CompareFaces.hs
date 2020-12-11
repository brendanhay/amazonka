{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cfQualityFilter,
    cfSimilarityThreshold,
    cfSourceImage,
    cfTargetImage,

    -- * Destructuring the response
    CompareFacesResponse (..),
    mkCompareFacesResponse,

    -- ** Response lenses
    cfrsFaceMatches,
    cfrsUnmatchedFaces,
    cfrsTargetImageOrientationCorrection,
    cfrsSourceImageOrientationCorrection,
    cfrsSourceImageFace,
    cfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCompareFaces' smart constructor.
data CompareFaces = CompareFaces'
  { qualityFilter ::
      Lude.Maybe QualityFilter,
    similarityThreshold :: Lude.Maybe Lude.Double,
    sourceImage :: Image,
    targetImage :: Image
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompareFaces' with the minimum fields required to make a request.
--
-- * 'qualityFilter' - A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't compared. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
-- * 'similarityThreshold' - The minimum level of confidence in the face matches that a match must meet to be included in the @FaceMatches@ array.
-- * 'sourceImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
-- * 'targetImage' - The target image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
mkCompareFaces ::
  -- | 'sourceImage'
  Image ->
  -- | 'targetImage'
  Image ->
  CompareFaces
mkCompareFaces pSourceImage_ pTargetImage_ =
  CompareFaces'
    { qualityFilter = Lude.Nothing,
      similarityThreshold = Lude.Nothing,
      sourceImage = pSourceImage_,
      targetImage = pTargetImage_
    }

-- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't compared. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
--
-- /Note:/ Consider using 'qualityFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfQualityFilter :: Lens.Lens' CompareFaces (Lude.Maybe QualityFilter)
cfQualityFilter = Lens.lens (qualityFilter :: CompareFaces -> Lude.Maybe QualityFilter) (\s a -> s {qualityFilter = a} :: CompareFaces)
{-# DEPRECATED cfQualityFilter "Use generic-lens or generic-optics with 'qualityFilter' instead." #-}

-- | The minimum level of confidence in the face matches that a match must meet to be included in the @FaceMatches@ array.
--
-- /Note:/ Consider using 'similarityThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSimilarityThreshold :: Lens.Lens' CompareFaces (Lude.Maybe Lude.Double)
cfSimilarityThreshold = Lens.lens (similarityThreshold :: CompareFaces -> Lude.Maybe Lude.Double) (\s a -> s {similarityThreshold = a} :: CompareFaces)
{-# DEPRECATED cfSimilarityThreshold "Use generic-lens or generic-optics with 'similarityThreshold' instead." #-}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'sourceImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSourceImage :: Lens.Lens' CompareFaces Image
cfSourceImage = Lens.lens (sourceImage :: CompareFaces -> Image) (\s a -> s {sourceImage = a} :: CompareFaces)
{-# DEPRECATED cfSourceImage "Use generic-lens or generic-optics with 'sourceImage' instead." #-}

-- | The target image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'targetImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTargetImage :: Lens.Lens' CompareFaces Image
cfTargetImage = Lens.lens (targetImage :: CompareFaces -> Image) (\s a -> s {targetImage = a} :: CompareFaces)
{-# DEPRECATED cfTargetImage "Use generic-lens or generic-optics with 'targetImage' instead." #-}

instance Lude.AWSRequest CompareFaces where
  type Rs CompareFaces = CompareFacesResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          CompareFacesResponse'
            Lude.<$> (x Lude..?> "FaceMatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UnmatchedFaces" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TargetImageOrientationCorrection")
            Lude.<*> (x Lude..?> "SourceImageOrientationCorrection")
            Lude.<*> (x Lude..?> "SourceImageFace")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CompareFaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.CompareFaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CompareFaces where
  toJSON CompareFaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QualityFilter" Lude..=) Lude.<$> qualityFilter,
            ("SimilarityThreshold" Lude..=) Lude.<$> similarityThreshold,
            Lude.Just ("SourceImage" Lude..= sourceImage),
            Lude.Just ("TargetImage" Lude..= targetImage)
          ]
      )

instance Lude.ToPath CompareFaces where
  toPath = Lude.const "/"

instance Lude.ToQuery CompareFaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCompareFacesResponse' smart constructor.
data CompareFacesResponse = CompareFacesResponse'
  { faceMatches ::
      Lude.Maybe [CompareFacesMatch],
    unmatchedFaces :: Lude.Maybe [ComparedFace],
    targetImageOrientationCorrection ::
      Lude.Maybe OrientationCorrection,
    sourceImageOrientationCorrection ::
      Lude.Maybe OrientationCorrection,
    sourceImageFace ::
      Lude.Maybe ComparedSourceImageFace,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompareFacesResponse' with the minimum fields required to make a request.
--
-- * 'faceMatches' - An array of faces in the target image that match the source image face. Each @CompareFacesMatch@ object provides the bounding box, the confidence level that the bounding box contains a face, and the similarity score for the face in the bounding box and the face in the source image.
-- * 'responseStatus' - The response status code.
-- * 'sourceImageFace' - The face in the source image that was used for comparison.
-- * 'sourceImageOrientationCorrection' - The value of @SourceImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
-- * 'targetImageOrientationCorrection' - The value of @TargetImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
-- * 'unmatchedFaces' - An array of faces in the target image that did not match the source image face.
mkCompareFacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CompareFacesResponse
mkCompareFacesResponse pResponseStatus_ =
  CompareFacesResponse'
    { faceMatches = Lude.Nothing,
      unmatchedFaces = Lude.Nothing,
      targetImageOrientationCorrection = Lude.Nothing,
      sourceImageOrientationCorrection = Lude.Nothing,
      sourceImageFace = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of faces in the target image that match the source image face. Each @CompareFacesMatch@ object provides the bounding box, the confidence level that the bounding box contains a face, and the similarity score for the face in the bounding box and the face in the source image.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsFaceMatches :: Lens.Lens' CompareFacesResponse (Lude.Maybe [CompareFacesMatch])
cfrsFaceMatches = Lens.lens (faceMatches :: CompareFacesResponse -> Lude.Maybe [CompareFacesMatch]) (\s a -> s {faceMatches = a} :: CompareFacesResponse)
{-# DEPRECATED cfrsFaceMatches "Use generic-lens or generic-optics with 'faceMatches' instead." #-}

-- | An array of faces in the target image that did not match the source image face.
--
-- /Note:/ Consider using 'unmatchedFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsUnmatchedFaces :: Lens.Lens' CompareFacesResponse (Lude.Maybe [ComparedFace])
cfrsUnmatchedFaces = Lens.lens (unmatchedFaces :: CompareFacesResponse -> Lude.Maybe [ComparedFace]) (\s a -> s {unmatchedFaces = a} :: CompareFacesResponse)
{-# DEPRECATED cfrsUnmatchedFaces "Use generic-lens or generic-optics with 'unmatchedFaces' instead." #-}

-- | The value of @TargetImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'targetImageOrientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsTargetImageOrientationCorrection :: Lens.Lens' CompareFacesResponse (Lude.Maybe OrientationCorrection)
cfrsTargetImageOrientationCorrection = Lens.lens (targetImageOrientationCorrection :: CompareFacesResponse -> Lude.Maybe OrientationCorrection) (\s a -> s {targetImageOrientationCorrection = a} :: CompareFacesResponse)
{-# DEPRECATED cfrsTargetImageOrientationCorrection "Use generic-lens or generic-optics with 'targetImageOrientationCorrection' instead." #-}

-- | The value of @SourceImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata.
-- Amazon Rekognition doesn’t perform image correction for images in .png format and .jpeg images without orientation information in the image Exif metadata. The bounding box coordinates aren't translated and represent the object locations before the image is rotated.
--
-- /Note:/ Consider using 'sourceImageOrientationCorrection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsSourceImageOrientationCorrection :: Lens.Lens' CompareFacesResponse (Lude.Maybe OrientationCorrection)
cfrsSourceImageOrientationCorrection = Lens.lens (sourceImageOrientationCorrection :: CompareFacesResponse -> Lude.Maybe OrientationCorrection) (\s a -> s {sourceImageOrientationCorrection = a} :: CompareFacesResponse)
{-# DEPRECATED cfrsSourceImageOrientationCorrection "Use generic-lens or generic-optics with 'sourceImageOrientationCorrection' instead." #-}

-- | The face in the source image that was used for comparison.
--
-- /Note:/ Consider using 'sourceImageFace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsSourceImageFace :: Lens.Lens' CompareFacesResponse (Lude.Maybe ComparedSourceImageFace)
cfrsSourceImageFace = Lens.lens (sourceImageFace :: CompareFacesResponse -> Lude.Maybe ComparedSourceImageFace) (\s a -> s {sourceImageFace = a} :: CompareFacesResponse)
{-# DEPRECATED cfrsSourceImageFace "Use generic-lens or generic-optics with 'sourceImageFace' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CompareFacesResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CompareFacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CompareFacesResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
