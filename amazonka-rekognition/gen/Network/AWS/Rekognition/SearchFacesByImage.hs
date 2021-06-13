{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.SearchFacesByImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given input image, first detects the largest face in the image,
-- and then searches the specified collection for matching faces. The
-- operation compares the features of the input face with faces in the
-- specified collection.
--
-- To search for all faces in an input image, you might first call the
-- IndexFaces operation, and then use the face IDs returned in subsequent
-- calls to the SearchFaces operation.
--
-- You can also call the @DetectFaces@ operation and use the bounding boxes
-- in the response to make face crops, which then you can pass in to the
-- @SearchFacesByImage@ operation.
--
-- You pass the input image either as base64-encoded image bytes or as a
-- reference to an image in an Amazon S3 bucket. If you use the AWS CLI to
-- call Amazon Rekognition operations, passing image bytes is not
-- supported. The image must be either a PNG or JPEG formatted file.
--
-- The response returns an array of faces that match, ordered by similarity
-- score with the highest similarity first. More specifically, it is an
-- array of metadata for each face match found. Along with the metadata,
-- the response also includes a @similarity@ indicating how similar the
-- face is to the input face. In the response, the operation also returns
-- the bounding box (and a confidence level that the bounding box contains
-- a face) of the face that Amazon Rekognition used for the input image.
--
-- For an example, Searching for a Face Using an Image in the Amazon
-- Rekognition Developer Guide.
--
-- The @QualityFilter@ input parameter allows you to filter out detected
-- faces that don’t meet a required quality bar. The quality bar is based
-- on a variety of common use cases. Use @QualityFilter@ to set the quality
-- bar for filtering by specifying @LOW@, @MEDIUM@, or @HIGH@. If you do
-- not want to filter detected faces, specify @NONE@. The default value is
-- @NONE@.
--
-- To use quality filtering, you need a collection associated with version
-- 3 of the face model or higher. To get the version of the face model
-- associated with a collection, call DescribeCollection.
--
-- This operation requires permissions to perform the
-- @rekognition:SearchFacesByImage@ action.
module Network.AWS.Rekognition.SearchFacesByImage
  ( -- * Creating a Request
    SearchFacesByImage (..),
    newSearchFacesByImage,

    -- * Request Lenses
    searchFacesByImage_qualityFilter,
    searchFacesByImage_maxFaces,
    searchFacesByImage_faceMatchThreshold,
    searchFacesByImage_collectionId,
    searchFacesByImage_image,

    -- * Destructuring the Response
    SearchFacesByImageResponse (..),
    newSearchFacesByImageResponse,

    -- * Response Lenses
    searchFacesByImageResponse_faceModelVersion,
    searchFacesByImageResponse_faceMatches,
    searchFacesByImageResponse_searchedFaceBoundingBox,
    searchFacesByImageResponse_searchedFaceConfidence,
    searchFacesByImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchFacesByImage' smart constructor.
data SearchFacesByImage = SearchFacesByImage'
  { -- | A filter that specifies a quality bar for how much filtering is done to
    -- identify faces. Filtered faces aren\'t searched for in the collection.
    -- If you specify @AUTO@, Amazon Rekognition chooses the quality bar. If
    -- you specify @LOW@, @MEDIUM@, or @HIGH@, filtering removes all faces that
    -- don’t meet the chosen quality bar. The quality bar is based on a variety
    -- of common use cases. Low-quality detections can occur for a number of
    -- reasons. Some examples are an object that\'s misidentified as a face, a
    -- face that\'s too blurry, or a face with a pose that\'s too extreme to
    -- use. If you specify @NONE@, no filtering is performed. The default value
    -- is @NONE@.
    --
    -- To use quality filtering, the collection you are using must be
    -- associated with version 3 of the face model or higher.
    qualityFilter :: Prelude.Maybe QualityFilter,
    -- | Maximum number of faces to return. The operation returns the maximum
    -- number of faces with the highest confidence in the match.
    maxFaces :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) Specifies the minimum confidence in the face match to return.
    -- For example, don\'t return any matches where confidence in matches is
    -- less than 70%. The default value is 80%.
    faceMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | ID of the collection to search.
    collectionId :: Prelude.Text,
    -- | The input image as base64-encoded bytes or an S3 object. If you use the
    -- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
    -- image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not
    -- need to base64-encode image bytes passed using the @Bytes@ field. For
    -- more information, see Images in the Amazon Rekognition developer guide.
    image :: Image
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFacesByImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualityFilter', 'searchFacesByImage_qualityFilter' - A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t searched for in the collection.
-- If you specify @AUTO@, Amazon Rekognition chooses the quality bar. If
-- you specify @LOW@, @MEDIUM@, or @HIGH@, filtering removes all faces that
-- don’t meet the chosen quality bar. The quality bar is based on a variety
-- of common use cases. Low-quality detections can occur for a number of
-- reasons. Some examples are an object that\'s misidentified as a face, a
-- face that\'s too blurry, or a face with a pose that\'s too extreme to
-- use. If you specify @NONE@, no filtering is performed. The default value
-- is @NONE@.
--
-- To use quality filtering, the collection you are using must be
-- associated with version 3 of the face model or higher.
--
-- 'maxFaces', 'searchFacesByImage_maxFaces' - Maximum number of faces to return. The operation returns the maximum
-- number of faces with the highest confidence in the match.
--
-- 'faceMatchThreshold', 'searchFacesByImage_faceMatchThreshold' - (Optional) Specifies the minimum confidence in the face match to return.
-- For example, don\'t return any matches where confidence in matches is
-- less than 70%. The default value is 80%.
--
-- 'collectionId', 'searchFacesByImage_collectionId' - ID of the collection to search.
--
-- 'image', 'searchFacesByImage_image' - The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
newSearchFacesByImage ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'image'
  Image ->
  SearchFacesByImage
newSearchFacesByImage pCollectionId_ pImage_ =
  SearchFacesByImage'
    { qualityFilter =
        Prelude.Nothing,
      maxFaces = Prelude.Nothing,
      faceMatchThreshold = Prelude.Nothing,
      collectionId = pCollectionId_,
      image = pImage_
    }

-- | A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t searched for in the collection.
-- If you specify @AUTO@, Amazon Rekognition chooses the quality bar. If
-- you specify @LOW@, @MEDIUM@, or @HIGH@, filtering removes all faces that
-- don’t meet the chosen quality bar. The quality bar is based on a variety
-- of common use cases. Low-quality detections can occur for a number of
-- reasons. Some examples are an object that\'s misidentified as a face, a
-- face that\'s too blurry, or a face with a pose that\'s too extreme to
-- use. If you specify @NONE@, no filtering is performed. The default value
-- is @NONE@.
--
-- To use quality filtering, the collection you are using must be
-- associated with version 3 of the face model or higher.
searchFacesByImage_qualityFilter :: Lens.Lens' SearchFacesByImage (Prelude.Maybe QualityFilter)
searchFacesByImage_qualityFilter = Lens.lens (\SearchFacesByImage' {qualityFilter} -> qualityFilter) (\s@SearchFacesByImage' {} a -> s {qualityFilter = a} :: SearchFacesByImage)

-- | Maximum number of faces to return. The operation returns the maximum
-- number of faces with the highest confidence in the match.
searchFacesByImage_maxFaces :: Lens.Lens' SearchFacesByImage (Prelude.Maybe Prelude.Natural)
searchFacesByImage_maxFaces = Lens.lens (\SearchFacesByImage' {maxFaces} -> maxFaces) (\s@SearchFacesByImage' {} a -> s {maxFaces = a} :: SearchFacesByImage)

-- | (Optional) Specifies the minimum confidence in the face match to return.
-- For example, don\'t return any matches where confidence in matches is
-- less than 70%. The default value is 80%.
searchFacesByImage_faceMatchThreshold :: Lens.Lens' SearchFacesByImage (Prelude.Maybe Prelude.Double)
searchFacesByImage_faceMatchThreshold = Lens.lens (\SearchFacesByImage' {faceMatchThreshold} -> faceMatchThreshold) (\s@SearchFacesByImage' {} a -> s {faceMatchThreshold = a} :: SearchFacesByImage)

-- | ID of the collection to search.
searchFacesByImage_collectionId :: Lens.Lens' SearchFacesByImage Prelude.Text
searchFacesByImage_collectionId = Lens.lens (\SearchFacesByImage' {collectionId} -> collectionId) (\s@SearchFacesByImage' {} a -> s {collectionId = a} :: SearchFacesByImage)

-- | The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
searchFacesByImage_image :: Lens.Lens' SearchFacesByImage Image
searchFacesByImage_image = Lens.lens (\SearchFacesByImage' {image} -> image) (\s@SearchFacesByImage' {} a -> s {image = a} :: SearchFacesByImage)

instance Core.AWSRequest SearchFacesByImage where
  type
    AWSResponse SearchFacesByImage =
      SearchFacesByImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFacesByImageResponse'
            Prelude.<$> (x Core..?> "FaceModelVersion")
            Prelude.<*> (x Core..?> "FaceMatches" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "SearchedFaceBoundingBox")
            Prelude.<*> (x Core..?> "SearchedFaceConfidence")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchFacesByImage

instance Prelude.NFData SearchFacesByImage

instance Core.ToHeaders SearchFacesByImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.SearchFacesByImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchFacesByImage where
  toJSON SearchFacesByImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QualityFilter" Core..=) Prelude.<$> qualityFilter,
            ("MaxFaces" Core..=) Prelude.<$> maxFaces,
            ("FaceMatchThreshold" Core..=)
              Prelude.<$> faceMatchThreshold,
            Prelude.Just ("CollectionId" Core..= collectionId),
            Prelude.Just ("Image" Core..= image)
          ]
      )

instance Core.ToPath SearchFacesByImage where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchFacesByImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchFacesByImageResponse' smart constructor.
data SearchFacesByImageResponse = SearchFacesByImageResponse'
  { -- | Version number of the face detection model associated with the input
    -- collection (@CollectionId@).
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of faces that match the input face, along with the confidence
    -- in the match.
    faceMatches :: Prelude.Maybe [FaceMatch],
    -- | The bounding box around the face in the input image that Amazon
    -- Rekognition used for the search.
    searchedFaceBoundingBox :: Prelude.Maybe BoundingBox,
    -- | The level of confidence that the @searchedFaceBoundingBox@, contains a
    -- face.
    searchedFaceConfidence :: Prelude.Maybe Prelude.Double,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFacesByImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersion', 'searchFacesByImageResponse_faceModelVersion' - Version number of the face detection model associated with the input
-- collection (@CollectionId@).
--
-- 'faceMatches', 'searchFacesByImageResponse_faceMatches' - An array of faces that match the input face, along with the confidence
-- in the match.
--
-- 'searchedFaceBoundingBox', 'searchFacesByImageResponse_searchedFaceBoundingBox' - The bounding box around the face in the input image that Amazon
-- Rekognition used for the search.
--
-- 'searchedFaceConfidence', 'searchFacesByImageResponse_searchedFaceConfidence' - The level of confidence that the @searchedFaceBoundingBox@, contains a
-- face.
--
-- 'httpStatus', 'searchFacesByImageResponse_httpStatus' - The response's http status code.
newSearchFacesByImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchFacesByImageResponse
newSearchFacesByImageResponse pHttpStatus_ =
  SearchFacesByImageResponse'
    { faceModelVersion =
        Prelude.Nothing,
      faceMatches = Prelude.Nothing,
      searchedFaceBoundingBox = Prelude.Nothing,
      searchedFaceConfidence = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the face detection model associated with the input
-- collection (@CollectionId@).
searchFacesByImageResponse_faceModelVersion :: Lens.Lens' SearchFacesByImageResponse (Prelude.Maybe Prelude.Text)
searchFacesByImageResponse_faceModelVersion = Lens.lens (\SearchFacesByImageResponse' {faceModelVersion} -> faceModelVersion) (\s@SearchFacesByImageResponse' {} a -> s {faceModelVersion = a} :: SearchFacesByImageResponse)

-- | An array of faces that match the input face, along with the confidence
-- in the match.
searchFacesByImageResponse_faceMatches :: Lens.Lens' SearchFacesByImageResponse (Prelude.Maybe [FaceMatch])
searchFacesByImageResponse_faceMatches = Lens.lens (\SearchFacesByImageResponse' {faceMatches} -> faceMatches) (\s@SearchFacesByImageResponse' {} a -> s {faceMatches = a} :: SearchFacesByImageResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The bounding box around the face in the input image that Amazon
-- Rekognition used for the search.
searchFacesByImageResponse_searchedFaceBoundingBox :: Lens.Lens' SearchFacesByImageResponse (Prelude.Maybe BoundingBox)
searchFacesByImageResponse_searchedFaceBoundingBox = Lens.lens (\SearchFacesByImageResponse' {searchedFaceBoundingBox} -> searchedFaceBoundingBox) (\s@SearchFacesByImageResponse' {} a -> s {searchedFaceBoundingBox = a} :: SearchFacesByImageResponse)

-- | The level of confidence that the @searchedFaceBoundingBox@, contains a
-- face.
searchFacesByImageResponse_searchedFaceConfidence :: Lens.Lens' SearchFacesByImageResponse (Prelude.Maybe Prelude.Double)
searchFacesByImageResponse_searchedFaceConfidence = Lens.lens (\SearchFacesByImageResponse' {searchedFaceConfidence} -> searchedFaceConfidence) (\s@SearchFacesByImageResponse' {} a -> s {searchedFaceConfidence = a} :: SearchFacesByImageResponse)

-- | The response's http status code.
searchFacesByImageResponse_httpStatus :: Lens.Lens' SearchFacesByImageResponse Prelude.Int
searchFacesByImageResponse_httpStatus = Lens.lens (\SearchFacesByImageResponse' {httpStatus} -> httpStatus) (\s@SearchFacesByImageResponse' {} a -> s {httpStatus = a} :: SearchFacesByImageResponse)

instance Prelude.NFData SearchFacesByImageResponse
