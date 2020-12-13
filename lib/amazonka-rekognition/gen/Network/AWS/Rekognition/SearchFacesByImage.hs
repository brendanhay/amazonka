{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.SearchFacesByImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given input image, first detects the largest face in the image, and then searches the specified collection for matching faces. The operation compares the features of the input face with faces in the specified collection.
--
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
-- The response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match found. Along with the metadata, the response also includes a @similarity@ indicating how similar the face is to the input face. In the response, the operation also returns the bounding box (and a confidence level that the bounding box contains a face) of the face that Amazon Rekognition used for the input image.
-- For an example, Searching for a Face Using an Image in the Amazon Rekognition Developer Guide.
-- The @QualityFilter@ input parameter allows you to filter out detected faces that don’t meet a required quality bar. The quality bar is based on a variety of common use cases. Use @QualityFilter@ to set the quality bar for filtering by specifying @LOW@ , @MEDIUM@ , or @HIGH@ . If you do not want to filter detected faces, specify @NONE@ . The default value is @NONE@ .
-- This operation requires permissions to perform the @rekognition:SearchFacesByImage@ action.
module Network.AWS.Rekognition.SearchFacesByImage
  ( -- * Creating a request
    SearchFacesByImage (..),
    mkSearchFacesByImage,

    -- ** Request lenses
    sfbiImage,
    sfbiQualityFilter,
    sfbiFaceMatchThreshold,
    sfbiCollectionId,
    sfbiMaxFaces,

    -- * Destructuring the response
    SearchFacesByImageResponse (..),
    mkSearchFacesByImageResponse,

    -- ** Response lenses
    sfbirsFaceMatches,
    sfbirsFaceModelVersion,
    sfbirsSearchedFaceBoundingBox,
    sfbirsSearchedFaceConfidence,
    sfbirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchFacesByImage' smart constructor.
data SearchFacesByImage = SearchFacesByImage'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
    image :: Image,
    -- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't searched for in the collection. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
    --
    -- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
    qualityFilter :: Lude.Maybe QualityFilter,
    -- | (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
    faceMatchThreshold :: Lude.Maybe Lude.Double,
    -- | ID of the collection to search.
    collectionId :: Lude.Text,
    -- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
    maxFaces :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchFacesByImage' with the minimum fields required to make a request.
--
-- * 'image' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
-- * 'qualityFilter' - A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't searched for in the collection. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
-- * 'faceMatchThreshold' - (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
-- * 'collectionId' - ID of the collection to search.
-- * 'maxFaces' - Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
mkSearchFacesByImage ::
  -- | 'image'
  Image ->
  -- | 'collectionId'
  Lude.Text ->
  SearchFacesByImage
mkSearchFacesByImage pImage_ pCollectionId_ =
  SearchFacesByImage'
    { image = pImage_,
      qualityFilter = Lude.Nothing,
      faceMatchThreshold = Lude.Nothing,
      collectionId = pCollectionId_,
      maxFaces = Lude.Nothing
    }

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiImage :: Lens.Lens' SearchFacesByImage Image
sfbiImage = Lens.lens (image :: SearchFacesByImage -> Image) (\s a -> s {image = a} :: SearchFacesByImage)
{-# DEPRECATED sfbiImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't searched for in the collection. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ .
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
--
-- /Note:/ Consider using 'qualityFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiQualityFilter :: Lens.Lens' SearchFacesByImage (Lude.Maybe QualityFilter)
sfbiQualityFilter = Lens.lens (qualityFilter :: SearchFacesByImage -> Lude.Maybe QualityFilter) (\s a -> s {qualityFilter = a} :: SearchFacesByImage)
{-# DEPRECATED sfbiQualityFilter "Use generic-lens or generic-optics with 'qualityFilter' instead." #-}

-- | (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiFaceMatchThreshold :: Lens.Lens' SearchFacesByImage (Lude.Maybe Lude.Double)
sfbiFaceMatchThreshold = Lens.lens (faceMatchThreshold :: SearchFacesByImage -> Lude.Maybe Lude.Double) (\s a -> s {faceMatchThreshold = a} :: SearchFacesByImage)
{-# DEPRECATED sfbiFaceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead." #-}

-- | ID of the collection to search.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiCollectionId :: Lens.Lens' SearchFacesByImage Lude.Text
sfbiCollectionId = Lens.lens (collectionId :: SearchFacesByImage -> Lude.Text) (\s a -> s {collectionId = a} :: SearchFacesByImage)
{-# DEPRECATED sfbiCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

-- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
--
-- /Note:/ Consider using 'maxFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiMaxFaces :: Lens.Lens' SearchFacesByImage (Lude.Maybe Lude.Natural)
sfbiMaxFaces = Lens.lens (maxFaces :: SearchFacesByImage -> Lude.Maybe Lude.Natural) (\s a -> s {maxFaces = a} :: SearchFacesByImage)
{-# DEPRECATED sfbiMaxFaces "Use generic-lens or generic-optics with 'maxFaces' instead." #-}

instance Lude.AWSRequest SearchFacesByImage where
  type Rs SearchFacesByImage = SearchFacesByImageResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchFacesByImageResponse'
            Lude.<$> (x Lude..?> "FaceMatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "FaceModelVersion")
            Lude.<*> (x Lude..?> "SearchedFaceBoundingBox")
            Lude.<*> (x Lude..?> "SearchedFaceConfidence")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchFacesByImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.SearchFacesByImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchFacesByImage where
  toJSON SearchFacesByImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Image" Lude..= image),
            ("QualityFilter" Lude..=) Lude.<$> qualityFilter,
            ("FaceMatchThreshold" Lude..=) Lude.<$> faceMatchThreshold,
            Lude.Just ("CollectionId" Lude..= collectionId),
            ("MaxFaces" Lude..=) Lude.<$> maxFaces
          ]
      )

instance Lude.ToPath SearchFacesByImage where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchFacesByImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchFacesByImageResponse' smart constructor.
data SearchFacesByImageResponse = SearchFacesByImageResponse'
  { -- | An array of faces that match the input face, along with the confidence in the match.
    faceMatches :: Lude.Maybe [FaceMatch],
    -- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
    faceModelVersion :: Lude.Maybe Lude.Text,
    -- | The bounding box around the face in the input image that Amazon Rekognition used for the search.
    searchedFaceBoundingBox :: Lude.Maybe BoundingBox,
    -- | The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
    searchedFaceConfidence :: Lude.Maybe Lude.Double,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchFacesByImageResponse' with the minimum fields required to make a request.
--
-- * 'faceMatches' - An array of faces that match the input face, along with the confidence in the match.
-- * 'faceModelVersion' - Version number of the face detection model associated with the input collection (@CollectionId@ ).
-- * 'searchedFaceBoundingBox' - The bounding box around the face in the input image that Amazon Rekognition used for the search.
-- * 'searchedFaceConfidence' - The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
-- * 'responseStatus' - The response status code.
mkSearchFacesByImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchFacesByImageResponse
mkSearchFacesByImageResponse pResponseStatus_ =
  SearchFacesByImageResponse'
    { faceMatches = Lude.Nothing,
      faceModelVersion = Lude.Nothing,
      searchedFaceBoundingBox = Lude.Nothing,
      searchedFaceConfidence = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of faces that match the input face, along with the confidence in the match.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirsFaceMatches :: Lens.Lens' SearchFacesByImageResponse (Lude.Maybe [FaceMatch])
sfbirsFaceMatches = Lens.lens (faceMatches :: SearchFacesByImageResponse -> Lude.Maybe [FaceMatch]) (\s a -> s {faceMatches = a} :: SearchFacesByImageResponse)
{-# DEPRECATED sfbirsFaceMatches "Use generic-lens or generic-optics with 'faceMatches' instead." #-}

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirsFaceModelVersion :: Lens.Lens' SearchFacesByImageResponse (Lude.Maybe Lude.Text)
sfbirsFaceModelVersion = Lens.lens (faceModelVersion :: SearchFacesByImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {faceModelVersion = a} :: SearchFacesByImageResponse)
{-# DEPRECATED sfbirsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | The bounding box around the face in the input image that Amazon Rekognition used for the search.
--
-- /Note:/ Consider using 'searchedFaceBoundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirsSearchedFaceBoundingBox :: Lens.Lens' SearchFacesByImageResponse (Lude.Maybe BoundingBox)
sfbirsSearchedFaceBoundingBox = Lens.lens (searchedFaceBoundingBox :: SearchFacesByImageResponse -> Lude.Maybe BoundingBox) (\s a -> s {searchedFaceBoundingBox = a} :: SearchFacesByImageResponse)
{-# DEPRECATED sfbirsSearchedFaceBoundingBox "Use generic-lens or generic-optics with 'searchedFaceBoundingBox' instead." #-}

-- | The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
--
-- /Note:/ Consider using 'searchedFaceConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirsSearchedFaceConfidence :: Lens.Lens' SearchFacesByImageResponse (Lude.Maybe Lude.Double)
sfbirsSearchedFaceConfidence = Lens.lens (searchedFaceConfidence :: SearchFacesByImageResponse -> Lude.Maybe Lude.Double) (\s a -> s {searchedFaceConfidence = a} :: SearchFacesByImageResponse)
{-# DEPRECATED sfbirsSearchedFaceConfidence "Use generic-lens or generic-optics with 'searchedFaceConfidence' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirsResponseStatus :: Lens.Lens' SearchFacesByImageResponse Lude.Int
sfbirsResponseStatus = Lens.lens (responseStatus :: SearchFacesByImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchFacesByImageResponse)
{-# DEPRECATED sfbirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
