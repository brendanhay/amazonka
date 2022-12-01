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
-- Module      : Amazonka.Rekognition.IndexFaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces in the input image and adds them to the specified
-- collection.
--
-- Amazon Rekognition doesn\'t save the actual faces that are detected.
-- Instead, the underlying detection algorithm first detects the faces in
-- the input image. For each face, the algorithm extracts facial features
-- into a feature vector, and stores it in the backend database. Amazon
-- Rekognition uses feature vectors when it performs face match and search
-- operations using the SearchFaces and SearchFacesByImage operations.
--
-- For more information, see Adding faces to a collection in the Amazon
-- Rekognition Developer Guide.
--
-- To get the number of faces in a collection, call DescribeCollection.
--
-- If you\'re using version 1.0 of the face detection model, @IndexFaces@
-- indexes the 15 largest faces in the input image. Later versions of the
-- face detection model index the 100 largest faces in the input image.
--
-- If you\'re using version 4 or later of the face model, image orientation
-- information is not returned in the @OrientationCorrection@ field.
--
-- To determine which version of the model you\'re using, call
-- DescribeCollection and supply the collection ID. You can also get the
-- model version from the value of @FaceModelVersion@ in the response from
-- @IndexFaces@
--
-- For more information, see Model Versioning in the Amazon Rekognition
-- Developer Guide.
--
-- If you provide the optional @ExternalImageId@ for the input image you
-- provided, Amazon Rekognition associates this ID with all faces that it
-- detects. When you call the ListFaces operation, the response returns the
-- external ID. You can use this external image ID to create a client-side
-- index to associate the faces with each image. You can then use the index
-- to find all faces in an image.
--
-- You can specify the maximum number of faces to index with the @MaxFaces@
-- input parameter. This is useful when you want to index the largest faces
-- in an image and don\'t want to index smaller faces, such as those
-- belonging to people standing in the background.
--
-- The @QualityFilter@ input parameter allows you to filter out detected
-- faces that don’t meet a required quality bar. The quality bar is based
-- on a variety of common use cases. By default, @IndexFaces@ chooses the
-- quality bar that\'s used to filter faces. You can also explicitly choose
-- the quality bar. Use @QualityFilter@, to set the quality bar by
-- specifying @LOW@, @MEDIUM@, or @HIGH@. If you do not want to filter
-- detected faces, specify @NONE@.
--
-- To use quality filtering, you need a collection associated with version
-- 3 of the face model or higher. To get the version of the face model
-- associated with a collection, call DescribeCollection.
--
-- Information about faces detected in an image, but not indexed, is
-- returned in an array of UnindexedFace objects, @UnindexedFaces@. Faces
-- aren\'t indexed for reasons such as:
--
-- -   The number of faces detected exceeds the value of the @MaxFaces@
--     request parameter.
--
-- -   The face is too small compared to the image dimensions.
--
-- -   The face is too blurry.
--
-- -   The image is too dark.
--
-- -   The face has an extreme pose.
--
-- -   The face doesn’t have enough detail to be suitable for face search.
--
-- In response, the @IndexFaces@ operation returns an array of metadata for
-- all detected faces, @FaceRecords@. This includes:
--
-- -   The bounding box, @BoundingBox@, of the detected face.
--
-- -   A confidence value, @Confidence@, which indicates the confidence
--     that the bounding box contains a face.
--
-- -   A face ID, @FaceId@, assigned by the service for each face that\'s
--     detected and stored.
--
-- -   An image ID, @ImageId@, assigned by the service for the input image.
--
-- If you request all facial attributes (by using the @detectionAttributes@
-- parameter), Amazon Rekognition returns detailed facial attributes, such
-- as facial landmarks (for example, location of eye and mouth) and other
-- facial attributes. If you provide the same image, specify the same
-- collection, and use the same external ID in the @IndexFaces@ operation,
-- Amazon Rekognition doesn\'t save duplicate face metadata.
--
-- The input image is passed either as base64-encoded image bytes, or as a
-- reference to an image in an Amazon S3 bucket. If you use the AWS CLI to
-- call Amazon Rekognition operations, passing image bytes isn\'t
-- supported. The image must be formatted as a PNG or JPEG file.
--
-- This operation requires permissions to perform the
-- @rekognition:IndexFaces@ action.
module Amazonka.Rekognition.IndexFaces
  ( -- * Creating a Request
    IndexFaces (..),
    newIndexFaces,

    -- * Request Lenses
    indexFaces_qualityFilter,
    indexFaces_detectionAttributes,
    indexFaces_externalImageId,
    indexFaces_maxFaces,
    indexFaces_collectionId,
    indexFaces_image,

    -- * Destructuring the Response
    IndexFacesResponse (..),
    newIndexFacesResponse,

    -- * Response Lenses
    indexFacesResponse_unindexedFaces,
    indexFacesResponse_faceRecords,
    indexFacesResponse_orientationCorrection,
    indexFacesResponse_faceModelVersion,
    indexFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newIndexFaces' smart constructor.
data IndexFaces = IndexFaces'
  { -- | A filter that specifies a quality bar for how much filtering is done to
    -- identify faces. Filtered faces aren\'t indexed. If you specify @AUTO@,
    -- Amazon Rekognition chooses the quality bar. If you specify @LOW@,
    -- @MEDIUM@, or @HIGH@, filtering removes all faces that don’t meet the
    -- chosen quality bar. The default value is @AUTO@. The quality bar is
    -- based on a variety of common use cases. Low-quality detections can occur
    -- for a number of reasons. Some examples are an object that\'s
    -- misidentified as a face, a face that\'s too blurry, or a face with a
    -- pose that\'s too extreme to use. If you specify @NONE@, no filtering is
    -- performed.
    --
    -- To use quality filtering, the collection you are using must be
    -- associated with version 3 of the face model or higher.
    qualityFilter :: Prelude.Maybe QualityFilter,
    -- | An array of facial attributes that you want to be returned. This can be
    -- the default list of attributes or all attributes. If you don\'t specify
    -- a value for @Attributes@ or if you specify @[\"DEFAULT\"]@, the API
    -- returns the following subset of facial attributes: @BoundingBox@,
    -- @Confidence@, @Pose@, @Quality@, and @Landmarks@. If you provide
    -- @[\"ALL\"]@, all facial attributes are returned, but the operation takes
    -- longer to complete.
    --
    -- If you provide both, @[\"ALL\", \"DEFAULT\"]@, the service uses a
    -- logical AND operator to determine which attributes to return (in this
    -- case, all attributes).
    detectionAttributes :: Prelude.Maybe [Attribute],
    -- | The ID you want to assign to all the faces detected in the image.
    externalImageId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of faces to index. The value of @MaxFaces@ must be
    -- greater than or equal to 1. @IndexFaces@ returns no more than 100
    -- detected faces in an image, even if you specify a larger value for
    -- @MaxFaces@.
    --
    -- If @IndexFaces@ detects more faces than the value of @MaxFaces@, the
    -- faces with the lowest quality are filtered out first. If there are still
    -- more faces than the value of @MaxFaces@, the faces with the smallest
    -- bounding boxes are filtered out (up to the number that\'s needed to
    -- satisfy the value of @MaxFaces@). Information about the unindexed faces
    -- is available in the @UnindexedFaces@ array.
    --
    -- The faces that are returned by @IndexFaces@ are sorted by the largest
    -- face bounding box size to the smallest size, in descending order.
    --
    -- @MaxFaces@ can be used with a collection associated with any version of
    -- the face model.
    maxFaces :: Prelude.Maybe Prelude.Natural,
    -- | The ID of an existing collection to which you want to add the faces that
    -- are detected in the input images.
    collectionId :: Prelude.Text,
    -- | The input image as base64-encoded bytes or an S3 object. If you use the
    -- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
    -- image bytes isn\'t supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not
    -- need to base64-encode image bytes passed using the @Bytes@ field. For
    -- more information, see Images in the Amazon Rekognition developer guide.
    image :: Image
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualityFilter', 'indexFaces_qualityFilter' - A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t indexed. If you specify @AUTO@,
-- Amazon Rekognition chooses the quality bar. If you specify @LOW@,
-- @MEDIUM@, or @HIGH@, filtering removes all faces that don’t meet the
-- chosen quality bar. The default value is @AUTO@. The quality bar is
-- based on a variety of common use cases. Low-quality detections can occur
-- for a number of reasons. Some examples are an object that\'s
-- misidentified as a face, a face that\'s too blurry, or a face with a
-- pose that\'s too extreme to use. If you specify @NONE@, no filtering is
-- performed.
--
-- To use quality filtering, the collection you are using must be
-- associated with version 3 of the face model or higher.
--
-- 'detectionAttributes', 'indexFaces_detectionAttributes' - An array of facial attributes that you want to be returned. This can be
-- the default list of attributes or all attributes. If you don\'t specify
-- a value for @Attributes@ or if you specify @[\"DEFAULT\"]@, the API
-- returns the following subset of facial attributes: @BoundingBox@,
-- @Confidence@, @Pose@, @Quality@, and @Landmarks@. If you provide
-- @[\"ALL\"]@, all facial attributes are returned, but the operation takes
-- longer to complete.
--
-- If you provide both, @[\"ALL\", \"DEFAULT\"]@, the service uses a
-- logical AND operator to determine which attributes to return (in this
-- case, all attributes).
--
-- 'externalImageId', 'indexFaces_externalImageId' - The ID you want to assign to all the faces detected in the image.
--
-- 'maxFaces', 'indexFaces_maxFaces' - The maximum number of faces to index. The value of @MaxFaces@ must be
-- greater than or equal to 1. @IndexFaces@ returns no more than 100
-- detected faces in an image, even if you specify a larger value for
-- @MaxFaces@.
--
-- If @IndexFaces@ detects more faces than the value of @MaxFaces@, the
-- faces with the lowest quality are filtered out first. If there are still
-- more faces than the value of @MaxFaces@, the faces with the smallest
-- bounding boxes are filtered out (up to the number that\'s needed to
-- satisfy the value of @MaxFaces@). Information about the unindexed faces
-- is available in the @UnindexedFaces@ array.
--
-- The faces that are returned by @IndexFaces@ are sorted by the largest
-- face bounding box size to the smallest size, in descending order.
--
-- @MaxFaces@ can be used with a collection associated with any version of
-- the face model.
--
-- 'collectionId', 'indexFaces_collectionId' - The ID of an existing collection to which you want to add the faces that
-- are detected in the input images.
--
-- 'image', 'indexFaces_image' - The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes isn\'t supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
newIndexFaces ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'image'
  Image ->
  IndexFaces
newIndexFaces pCollectionId_ pImage_ =
  IndexFaces'
    { qualityFilter = Prelude.Nothing,
      detectionAttributes = Prelude.Nothing,
      externalImageId = Prelude.Nothing,
      maxFaces = Prelude.Nothing,
      collectionId = pCollectionId_,
      image = pImage_
    }

-- | A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t indexed. If you specify @AUTO@,
-- Amazon Rekognition chooses the quality bar. If you specify @LOW@,
-- @MEDIUM@, or @HIGH@, filtering removes all faces that don’t meet the
-- chosen quality bar. The default value is @AUTO@. The quality bar is
-- based on a variety of common use cases. Low-quality detections can occur
-- for a number of reasons. Some examples are an object that\'s
-- misidentified as a face, a face that\'s too blurry, or a face with a
-- pose that\'s too extreme to use. If you specify @NONE@, no filtering is
-- performed.
--
-- To use quality filtering, the collection you are using must be
-- associated with version 3 of the face model or higher.
indexFaces_qualityFilter :: Lens.Lens' IndexFaces (Prelude.Maybe QualityFilter)
indexFaces_qualityFilter = Lens.lens (\IndexFaces' {qualityFilter} -> qualityFilter) (\s@IndexFaces' {} a -> s {qualityFilter = a} :: IndexFaces)

-- | An array of facial attributes that you want to be returned. This can be
-- the default list of attributes or all attributes. If you don\'t specify
-- a value for @Attributes@ or if you specify @[\"DEFAULT\"]@, the API
-- returns the following subset of facial attributes: @BoundingBox@,
-- @Confidence@, @Pose@, @Quality@, and @Landmarks@. If you provide
-- @[\"ALL\"]@, all facial attributes are returned, but the operation takes
-- longer to complete.
--
-- If you provide both, @[\"ALL\", \"DEFAULT\"]@, the service uses a
-- logical AND operator to determine which attributes to return (in this
-- case, all attributes).
indexFaces_detectionAttributes :: Lens.Lens' IndexFaces (Prelude.Maybe [Attribute])
indexFaces_detectionAttributes = Lens.lens (\IndexFaces' {detectionAttributes} -> detectionAttributes) (\s@IndexFaces' {} a -> s {detectionAttributes = a} :: IndexFaces) Prelude.. Lens.mapping Lens.coerced

-- | The ID you want to assign to all the faces detected in the image.
indexFaces_externalImageId :: Lens.Lens' IndexFaces (Prelude.Maybe Prelude.Text)
indexFaces_externalImageId = Lens.lens (\IndexFaces' {externalImageId} -> externalImageId) (\s@IndexFaces' {} a -> s {externalImageId = a} :: IndexFaces)

-- | The maximum number of faces to index. The value of @MaxFaces@ must be
-- greater than or equal to 1. @IndexFaces@ returns no more than 100
-- detected faces in an image, even if you specify a larger value for
-- @MaxFaces@.
--
-- If @IndexFaces@ detects more faces than the value of @MaxFaces@, the
-- faces with the lowest quality are filtered out first. If there are still
-- more faces than the value of @MaxFaces@, the faces with the smallest
-- bounding boxes are filtered out (up to the number that\'s needed to
-- satisfy the value of @MaxFaces@). Information about the unindexed faces
-- is available in the @UnindexedFaces@ array.
--
-- The faces that are returned by @IndexFaces@ are sorted by the largest
-- face bounding box size to the smallest size, in descending order.
--
-- @MaxFaces@ can be used with a collection associated with any version of
-- the face model.
indexFaces_maxFaces :: Lens.Lens' IndexFaces (Prelude.Maybe Prelude.Natural)
indexFaces_maxFaces = Lens.lens (\IndexFaces' {maxFaces} -> maxFaces) (\s@IndexFaces' {} a -> s {maxFaces = a} :: IndexFaces)

-- | The ID of an existing collection to which you want to add the faces that
-- are detected in the input images.
indexFaces_collectionId :: Lens.Lens' IndexFaces Prelude.Text
indexFaces_collectionId = Lens.lens (\IndexFaces' {collectionId} -> collectionId) (\s@IndexFaces' {} a -> s {collectionId = a} :: IndexFaces)

-- | The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes isn\'t supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
indexFaces_image :: Lens.Lens' IndexFaces Image
indexFaces_image = Lens.lens (\IndexFaces' {image} -> image) (\s@IndexFaces' {} a -> s {image = a} :: IndexFaces)

instance Core.AWSRequest IndexFaces where
  type AWSResponse IndexFaces = IndexFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          IndexFacesResponse'
            Prelude.<$> (x Core..?> "UnindexedFaces" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "FaceRecords" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "OrientationCorrection")
            Prelude.<*> (x Core..?> "FaceModelVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IndexFaces where
  hashWithSalt _salt IndexFaces' {..} =
    _salt `Prelude.hashWithSalt` qualityFilter
      `Prelude.hashWithSalt` detectionAttributes
      `Prelude.hashWithSalt` externalImageId
      `Prelude.hashWithSalt` maxFaces
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` image

instance Prelude.NFData IndexFaces where
  rnf IndexFaces' {..} =
    Prelude.rnf qualityFilter
      `Prelude.seq` Prelude.rnf detectionAttributes
      `Prelude.seq` Prelude.rnf externalImageId
      `Prelude.seq` Prelude.rnf maxFaces
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf image

instance Core.ToHeaders IndexFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.IndexFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON IndexFaces where
  toJSON IndexFaces' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QualityFilter" Core..=) Prelude.<$> qualityFilter,
            ("DetectionAttributes" Core..=)
              Prelude.<$> detectionAttributes,
            ("ExternalImageId" Core..=)
              Prelude.<$> externalImageId,
            ("MaxFaces" Core..=) Prelude.<$> maxFaces,
            Prelude.Just ("CollectionId" Core..= collectionId),
            Prelude.Just ("Image" Core..= image)
          ]
      )

instance Core.ToPath IndexFaces where
  toPath = Prelude.const "/"

instance Core.ToQuery IndexFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIndexFacesResponse' smart constructor.
data IndexFacesResponse = IndexFacesResponse'
  { -- | An array of faces that were detected in the image but weren\'t indexed.
    -- They weren\'t indexed because the quality filter identified them as low
    -- quality, or the @MaxFaces@ request parameter filtered them out. To use
    -- the quality filter, you specify the @QualityFilter@ request parameter.
    unindexedFaces :: Prelude.Maybe [UnindexedFace],
    -- | An array of faces detected and added to the collection. For more
    -- information, see Searching Faces in a Collection in the Amazon
    -- Rekognition Developer Guide.
    faceRecords :: Prelude.Maybe [FaceRecord],
    -- | If your collection is associated with a face detection model that\'s
    -- later than version 3.0, the value of @OrientationCorrection@ is always
    -- null and no orientation information is returned.
    --
    -- If your collection is associated with a face detection model that\'s
    -- version 3.0 or earlier, the following applies:
    --
    -- -   If the input image is in .jpeg format, it might contain exchangeable
    --     image file format (Exif) metadata that includes the image\'s
    --     orientation. Amazon Rekognition uses this orientation information to
    --     perform image correction - the bounding box coordinates are
    --     translated to represent object locations after the orientation
    --     information in the Exif metadata is used to correct the image
    --     orientation. Images in .png format don\'t contain Exif metadata. The
    --     value of @OrientationCorrection@ is null.
    --
    -- -   If the image doesn\'t contain orientation information in its Exif
    --     metadata, Amazon Rekognition returns an estimated orientation
    --     (ROTATE_0, ROTATE_90, ROTATE_180, ROTATE_270). Amazon Rekognition
    --     doesn’t perform image correction for images. The bounding box
    --     coordinates aren\'t translated and represent the object locations
    --     before the image is rotated.
    --
    -- Bounding box information is returned in the @FaceRecords@ array. You can
    -- get the version of the face detection model by calling
    -- DescribeCollection.
    orientationCorrection :: Prelude.Maybe OrientationCorrection,
    -- | The version number of the face detection model that\'s associated with
    -- the input collection (@CollectionId@).
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unindexedFaces', 'indexFacesResponse_unindexedFaces' - An array of faces that were detected in the image but weren\'t indexed.
-- They weren\'t indexed because the quality filter identified them as low
-- quality, or the @MaxFaces@ request parameter filtered them out. To use
-- the quality filter, you specify the @QualityFilter@ request parameter.
--
-- 'faceRecords', 'indexFacesResponse_faceRecords' - An array of faces detected and added to the collection. For more
-- information, see Searching Faces in a Collection in the Amazon
-- Rekognition Developer Guide.
--
-- 'orientationCorrection', 'indexFacesResponse_orientationCorrection' - If your collection is associated with a face detection model that\'s
-- later than version 3.0, the value of @OrientationCorrection@ is always
-- null and no orientation information is returned.
--
-- If your collection is associated with a face detection model that\'s
-- version 3.0 or earlier, the following applies:
--
-- -   If the input image is in .jpeg format, it might contain exchangeable
--     image file format (Exif) metadata that includes the image\'s
--     orientation. Amazon Rekognition uses this orientation information to
--     perform image correction - the bounding box coordinates are
--     translated to represent object locations after the orientation
--     information in the Exif metadata is used to correct the image
--     orientation. Images in .png format don\'t contain Exif metadata. The
--     value of @OrientationCorrection@ is null.
--
-- -   If the image doesn\'t contain orientation information in its Exif
--     metadata, Amazon Rekognition returns an estimated orientation
--     (ROTATE_0, ROTATE_90, ROTATE_180, ROTATE_270). Amazon Rekognition
--     doesn’t perform image correction for images. The bounding box
--     coordinates aren\'t translated and represent the object locations
--     before the image is rotated.
--
-- Bounding box information is returned in the @FaceRecords@ array. You can
-- get the version of the face detection model by calling
-- DescribeCollection.
--
-- 'faceModelVersion', 'indexFacesResponse_faceModelVersion' - The version number of the face detection model that\'s associated with
-- the input collection (@CollectionId@).
--
-- 'httpStatus', 'indexFacesResponse_httpStatus' - The response's http status code.
newIndexFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  IndexFacesResponse
newIndexFacesResponse pHttpStatus_ =
  IndexFacesResponse'
    { unindexedFaces =
        Prelude.Nothing,
      faceRecords = Prelude.Nothing,
      orientationCorrection = Prelude.Nothing,
      faceModelVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of faces that were detected in the image but weren\'t indexed.
-- They weren\'t indexed because the quality filter identified them as low
-- quality, or the @MaxFaces@ request parameter filtered them out. To use
-- the quality filter, you specify the @QualityFilter@ request parameter.
indexFacesResponse_unindexedFaces :: Lens.Lens' IndexFacesResponse (Prelude.Maybe [UnindexedFace])
indexFacesResponse_unindexedFaces = Lens.lens (\IndexFacesResponse' {unindexedFaces} -> unindexedFaces) (\s@IndexFacesResponse' {} a -> s {unindexedFaces = a} :: IndexFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of faces detected and added to the collection. For more
-- information, see Searching Faces in a Collection in the Amazon
-- Rekognition Developer Guide.
indexFacesResponse_faceRecords :: Lens.Lens' IndexFacesResponse (Prelude.Maybe [FaceRecord])
indexFacesResponse_faceRecords = Lens.lens (\IndexFacesResponse' {faceRecords} -> faceRecords) (\s@IndexFacesResponse' {} a -> s {faceRecords = a} :: IndexFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If your collection is associated with a face detection model that\'s
-- later than version 3.0, the value of @OrientationCorrection@ is always
-- null and no orientation information is returned.
--
-- If your collection is associated with a face detection model that\'s
-- version 3.0 or earlier, the following applies:
--
-- -   If the input image is in .jpeg format, it might contain exchangeable
--     image file format (Exif) metadata that includes the image\'s
--     orientation. Amazon Rekognition uses this orientation information to
--     perform image correction - the bounding box coordinates are
--     translated to represent object locations after the orientation
--     information in the Exif metadata is used to correct the image
--     orientation. Images in .png format don\'t contain Exif metadata. The
--     value of @OrientationCorrection@ is null.
--
-- -   If the image doesn\'t contain orientation information in its Exif
--     metadata, Amazon Rekognition returns an estimated orientation
--     (ROTATE_0, ROTATE_90, ROTATE_180, ROTATE_270). Amazon Rekognition
--     doesn’t perform image correction for images. The bounding box
--     coordinates aren\'t translated and represent the object locations
--     before the image is rotated.
--
-- Bounding box information is returned in the @FaceRecords@ array. You can
-- get the version of the face detection model by calling
-- DescribeCollection.
indexFacesResponse_orientationCorrection :: Lens.Lens' IndexFacesResponse (Prelude.Maybe OrientationCorrection)
indexFacesResponse_orientationCorrection = Lens.lens (\IndexFacesResponse' {orientationCorrection} -> orientationCorrection) (\s@IndexFacesResponse' {} a -> s {orientationCorrection = a} :: IndexFacesResponse)

-- | The version number of the face detection model that\'s associated with
-- the input collection (@CollectionId@).
indexFacesResponse_faceModelVersion :: Lens.Lens' IndexFacesResponse (Prelude.Maybe Prelude.Text)
indexFacesResponse_faceModelVersion = Lens.lens (\IndexFacesResponse' {faceModelVersion} -> faceModelVersion) (\s@IndexFacesResponse' {} a -> s {faceModelVersion = a} :: IndexFacesResponse)

-- | The response's http status code.
indexFacesResponse_httpStatus :: Lens.Lens' IndexFacesResponse Prelude.Int
indexFacesResponse_httpStatus = Lens.lens (\IndexFacesResponse' {httpStatus} -> httpStatus) (\s@IndexFacesResponse' {} a -> s {httpStatus = a} :: IndexFacesResponse)

instance Prelude.NFData IndexFacesResponse where
  rnf IndexFacesResponse' {..} =
    Prelude.rnf unindexedFaces
      `Prelude.seq` Prelude.rnf faceRecords
      `Prelude.seq` Prelude.rnf orientationCorrection
      `Prelude.seq` Prelude.rnf faceModelVersion
      `Prelude.seq` Prelude.rnf httpStatus
