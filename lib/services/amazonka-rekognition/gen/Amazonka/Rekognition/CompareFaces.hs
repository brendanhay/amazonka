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
-- Module      : Amazonka.Rekognition.CompareFaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Compares a face in the /source/ input image with each of the 100 largest
-- faces detected in the /target/ input image.
--
-- If the source image contains multiple faces, the service detects the
-- largest face and compares it with each face detected in the target
-- image.
--
-- CompareFaces uses machine learning algorithms, which are probabilistic.
-- A false negative is an incorrect prediction that a face in the target
-- image has a low similarity confidence score when compared to the face in
-- the source image. To reduce the probability of false negatives, we
-- recommend that you compare the target image against multiple source
-- images. If you plan to use @CompareFaces@ to make a decision that
-- impacts an individual\'s rights, privacy, or access to services, we
-- recommend that you pass the result to a human for review and further
-- validation before taking action.
--
-- You pass the input and target images either as base64-encoded image
-- bytes or as references to images in an Amazon S3 bucket. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing image bytes
-- isn\'t supported. The image must be formatted as a PNG or JPEG file.
--
-- In response, the operation returns an array of face matches ordered by
-- similarity score in descending order. For each face match, the response
-- provides a bounding box of the face, facial landmarks, pose details
-- (pitch, roll, and yaw), quality (brightness and sharpness), and
-- confidence value (indicating the level of confidence that the bounding
-- box contains a face). The response also provides a similarity score,
-- which indicates how closely the faces match.
--
-- By default, only faces with a similarity score of greater than or equal
-- to 80% are returned in the response. You can change this value by
-- specifying the @SimilarityThreshold@ parameter.
--
-- @CompareFaces@ also returns an array of faces that don\'t match the
-- source image. For each face, it returns a bounding box, confidence
-- value, landmarks, pose details, and quality. The response also returns
-- information about the face in the source image, including the bounding
-- box of the face and confidence value.
--
-- The @QualityFilter@ input parameter allows you to filter out detected
-- faces that don’t meet a required quality bar. The quality bar is based
-- on a variety of common use cases. Use @QualityFilter@ to set the quality
-- bar by specifying @LOW@, @MEDIUM@, or @HIGH@. If you do not want to
-- filter detected faces, specify @NONE@. The default value is @NONE@.
--
-- If the image doesn\'t contain Exif metadata, @CompareFaces@ returns
-- orientation information for the source and target images. Use these
-- values to display the images with the correct image orientation.
--
-- If no faces are detected in the source or target images, @CompareFaces@
-- returns an @InvalidParameterException@ error.
--
-- This is a stateless API operation. That is, data returned by this
-- operation doesn\'t persist.
--
-- For an example, see Comparing Faces in Images in the Amazon Rekognition
-- Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:CompareFaces@ action.
module Amazonka.Rekognition.CompareFaces
  ( -- * Creating a Request
    CompareFaces (..),
    newCompareFaces,

    -- * Request Lenses
    compareFaces_qualityFilter,
    compareFaces_similarityThreshold,
    compareFaces_sourceImage,
    compareFaces_targetImage,

    -- * Destructuring the Response
    CompareFacesResponse (..),
    newCompareFacesResponse,

    -- * Response Lenses
    compareFacesResponse_faceMatches,
    compareFacesResponse_sourceImageFace,
    compareFacesResponse_sourceImageOrientationCorrection,
    compareFacesResponse_targetImageOrientationCorrection,
    compareFacesResponse_unmatchedFaces,
    compareFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCompareFaces' smart constructor.
data CompareFaces = CompareFaces'
  { -- | A filter that specifies a quality bar for how much filtering is done to
    -- identify faces. Filtered faces aren\'t compared. If you specify @AUTO@,
    -- Amazon Rekognition chooses the quality bar. If you specify @LOW@,
    -- @MEDIUM@, or @HIGH@, filtering removes all faces that don’t meet the
    -- chosen quality bar. The quality bar is based on a variety of common use
    -- cases. Low-quality detections can occur for a number of reasons. Some
    -- examples are an object that\'s misidentified as a face, a face that\'s
    -- too blurry, or a face with a pose that\'s too extreme to use. If you
    -- specify @NONE@, no filtering is performed. The default value is @NONE@.
    --
    -- To use quality filtering, the collection you are using must be
    -- associated with version 3 of the face model or higher.
    qualityFilter :: Prelude.Maybe QualityFilter,
    -- | The minimum level of confidence in the face matches that a match must
    -- meet to be included in the @FaceMatches@ array.
    similarityThreshold :: Prelude.Maybe Prelude.Double,
    -- | The input image as base64-encoded bytes or an S3 object. If you use the
    -- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
    -- image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not
    -- need to base64-encode image bytes passed using the @Bytes@ field. For
    -- more information, see Images in the Amazon Rekognition developer guide.
    sourceImage :: Image,
    -- | The target image as base64-encoded bytes or an S3 object. If you use the
    -- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
    -- image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not
    -- need to base64-encode image bytes passed using the @Bytes@ field. For
    -- more information, see Images in the Amazon Rekognition developer guide.
    targetImage :: Image
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompareFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualityFilter', 'compareFaces_qualityFilter' - A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t compared. If you specify @AUTO@,
-- Amazon Rekognition chooses the quality bar. If you specify @LOW@,
-- @MEDIUM@, or @HIGH@, filtering removes all faces that don’t meet the
-- chosen quality bar. The quality bar is based on a variety of common use
-- cases. Low-quality detections can occur for a number of reasons. Some
-- examples are an object that\'s misidentified as a face, a face that\'s
-- too blurry, or a face with a pose that\'s too extreme to use. If you
-- specify @NONE@, no filtering is performed. The default value is @NONE@.
--
-- To use quality filtering, the collection you are using must be
-- associated with version 3 of the face model or higher.
--
-- 'similarityThreshold', 'compareFaces_similarityThreshold' - The minimum level of confidence in the face matches that a match must
-- meet to be included in the @FaceMatches@ array.
--
-- 'sourceImage', 'compareFaces_sourceImage' - The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
--
-- 'targetImage', 'compareFaces_targetImage' - The target image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
newCompareFaces ::
  -- | 'sourceImage'
  Image ->
  -- | 'targetImage'
  Image ->
  CompareFaces
newCompareFaces pSourceImage_ pTargetImage_ =
  CompareFaces'
    { qualityFilter = Prelude.Nothing,
      similarityThreshold = Prelude.Nothing,
      sourceImage = pSourceImage_,
      targetImage = pTargetImage_
    }

-- | A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t compared. If you specify @AUTO@,
-- Amazon Rekognition chooses the quality bar. If you specify @LOW@,
-- @MEDIUM@, or @HIGH@, filtering removes all faces that don’t meet the
-- chosen quality bar. The quality bar is based on a variety of common use
-- cases. Low-quality detections can occur for a number of reasons. Some
-- examples are an object that\'s misidentified as a face, a face that\'s
-- too blurry, or a face with a pose that\'s too extreme to use. If you
-- specify @NONE@, no filtering is performed. The default value is @NONE@.
--
-- To use quality filtering, the collection you are using must be
-- associated with version 3 of the face model or higher.
compareFaces_qualityFilter :: Lens.Lens' CompareFaces (Prelude.Maybe QualityFilter)
compareFaces_qualityFilter = Lens.lens (\CompareFaces' {qualityFilter} -> qualityFilter) (\s@CompareFaces' {} a -> s {qualityFilter = a} :: CompareFaces)

-- | The minimum level of confidence in the face matches that a match must
-- meet to be included in the @FaceMatches@ array.
compareFaces_similarityThreshold :: Lens.Lens' CompareFaces (Prelude.Maybe Prelude.Double)
compareFaces_similarityThreshold = Lens.lens (\CompareFaces' {similarityThreshold} -> similarityThreshold) (\s@CompareFaces' {} a -> s {similarityThreshold = a} :: CompareFaces)

-- | The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
compareFaces_sourceImage :: Lens.Lens' CompareFaces Image
compareFaces_sourceImage = Lens.lens (\CompareFaces' {sourceImage} -> sourceImage) (\s@CompareFaces' {} a -> s {sourceImage = a} :: CompareFaces)

-- | The target image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
compareFaces_targetImage :: Lens.Lens' CompareFaces Image
compareFaces_targetImage = Lens.lens (\CompareFaces' {targetImage} -> targetImage) (\s@CompareFaces' {} a -> s {targetImage = a} :: CompareFaces)

instance Core.AWSRequest CompareFaces where
  type AWSResponse CompareFaces = CompareFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CompareFacesResponse'
            Prelude.<$> (x Data..?> "FaceMatches" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SourceImageFace")
            Prelude.<*> (x Data..?> "SourceImageOrientationCorrection")
            Prelude.<*> (x Data..?> "TargetImageOrientationCorrection")
            Prelude.<*> (x Data..?> "UnmatchedFaces" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CompareFaces where
  hashWithSalt _salt CompareFaces' {..} =
    _salt
      `Prelude.hashWithSalt` qualityFilter
      `Prelude.hashWithSalt` similarityThreshold
      `Prelude.hashWithSalt` sourceImage
      `Prelude.hashWithSalt` targetImage

instance Prelude.NFData CompareFaces where
  rnf CompareFaces' {..} =
    Prelude.rnf qualityFilter
      `Prelude.seq` Prelude.rnf similarityThreshold
      `Prelude.seq` Prelude.rnf sourceImage
      `Prelude.seq` Prelude.rnf targetImage

instance Data.ToHeaders CompareFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CompareFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CompareFaces where
  toJSON CompareFaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("QualityFilter" Data..=) Prelude.<$> qualityFilter,
            ("SimilarityThreshold" Data..=)
              Prelude.<$> similarityThreshold,
            Prelude.Just ("SourceImage" Data..= sourceImage),
            Prelude.Just ("TargetImage" Data..= targetImage)
          ]
      )

instance Data.ToPath CompareFaces where
  toPath = Prelude.const "/"

instance Data.ToQuery CompareFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCompareFacesResponse' smart constructor.
data CompareFacesResponse = CompareFacesResponse'
  { -- | An array of faces in the target image that match the source image face.
    -- Each @CompareFacesMatch@ object provides the bounding box, the
    -- confidence level that the bounding box contains a face, and the
    -- similarity score for the face in the bounding box and the face in the
    -- source image.
    faceMatches :: Prelude.Maybe [CompareFacesMatch],
    -- | The face in the source image that was used for comparison.
    sourceImageFace :: Prelude.Maybe ComparedSourceImageFace,
    -- | The value of @SourceImageOrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable
    -- image file format (Exif) metadata that includes the image\'s
    -- orientation. Amazon Rekognition uses this orientation information to
    -- perform image correction. The bounding box coordinates are translated to
    -- represent object locations after the orientation information in the Exif
    -- metadata is used to correct the image orientation. Images in .png format
    -- don\'t contain Exif metadata.
    --
    -- Amazon Rekognition doesn’t perform image correction for images in .png
    -- format and .jpeg images without orientation information in the image
    -- Exif metadata. The bounding box coordinates aren\'t translated and
    -- represent the object locations before the image is rotated.
    sourceImageOrientationCorrection :: Prelude.Maybe OrientationCorrection,
    -- | The value of @TargetImageOrientationCorrection@ is always null.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable
    -- image file format (Exif) metadata that includes the image\'s
    -- orientation. Amazon Rekognition uses this orientation information to
    -- perform image correction. The bounding box coordinates are translated to
    -- represent object locations after the orientation information in the Exif
    -- metadata is used to correct the image orientation. Images in .png format
    -- don\'t contain Exif metadata.
    --
    -- Amazon Rekognition doesn’t perform image correction for images in .png
    -- format and .jpeg images without orientation information in the image
    -- Exif metadata. The bounding box coordinates aren\'t translated and
    -- represent the object locations before the image is rotated.
    targetImageOrientationCorrection :: Prelude.Maybe OrientationCorrection,
    -- | An array of faces in the target image that did not match the source
    -- image face.
    unmatchedFaces :: Prelude.Maybe [ComparedFace],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompareFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceMatches', 'compareFacesResponse_faceMatches' - An array of faces in the target image that match the source image face.
-- Each @CompareFacesMatch@ object provides the bounding box, the
-- confidence level that the bounding box contains a face, and the
-- similarity score for the face in the bounding box and the face in the
-- source image.
--
-- 'sourceImageFace', 'compareFacesResponse_sourceImageFace' - The face in the source image that was used for comparison.
--
-- 'sourceImageOrientationCorrection', 'compareFacesResponse_sourceImageOrientationCorrection' - The value of @SourceImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image file format (Exif) metadata that includes the image\'s
-- orientation. Amazon Rekognition uses this orientation information to
-- perform image correction. The bounding box coordinates are translated to
-- represent object locations after the orientation information in the Exif
-- metadata is used to correct the image orientation. Images in .png format
-- don\'t contain Exif metadata.
--
-- Amazon Rekognition doesn’t perform image correction for images in .png
-- format and .jpeg images without orientation information in the image
-- Exif metadata. The bounding box coordinates aren\'t translated and
-- represent the object locations before the image is rotated.
--
-- 'targetImageOrientationCorrection', 'compareFacesResponse_targetImageOrientationCorrection' - The value of @TargetImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image file format (Exif) metadata that includes the image\'s
-- orientation. Amazon Rekognition uses this orientation information to
-- perform image correction. The bounding box coordinates are translated to
-- represent object locations after the orientation information in the Exif
-- metadata is used to correct the image orientation. Images in .png format
-- don\'t contain Exif metadata.
--
-- Amazon Rekognition doesn’t perform image correction for images in .png
-- format and .jpeg images without orientation information in the image
-- Exif metadata. The bounding box coordinates aren\'t translated and
-- represent the object locations before the image is rotated.
--
-- 'unmatchedFaces', 'compareFacesResponse_unmatchedFaces' - An array of faces in the target image that did not match the source
-- image face.
--
-- 'httpStatus', 'compareFacesResponse_httpStatus' - The response's http status code.
newCompareFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CompareFacesResponse
newCompareFacesResponse pHttpStatus_ =
  CompareFacesResponse'
    { faceMatches =
        Prelude.Nothing,
      sourceImageFace = Prelude.Nothing,
      sourceImageOrientationCorrection = Prelude.Nothing,
      targetImageOrientationCorrection = Prelude.Nothing,
      unmatchedFaces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of faces in the target image that match the source image face.
-- Each @CompareFacesMatch@ object provides the bounding box, the
-- confidence level that the bounding box contains a face, and the
-- similarity score for the face in the bounding box and the face in the
-- source image.
compareFacesResponse_faceMatches :: Lens.Lens' CompareFacesResponse (Prelude.Maybe [CompareFacesMatch])
compareFacesResponse_faceMatches = Lens.lens (\CompareFacesResponse' {faceMatches} -> faceMatches) (\s@CompareFacesResponse' {} a -> s {faceMatches = a} :: CompareFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The face in the source image that was used for comparison.
compareFacesResponse_sourceImageFace :: Lens.Lens' CompareFacesResponse (Prelude.Maybe ComparedSourceImageFace)
compareFacesResponse_sourceImageFace = Lens.lens (\CompareFacesResponse' {sourceImageFace} -> sourceImageFace) (\s@CompareFacesResponse' {} a -> s {sourceImageFace = a} :: CompareFacesResponse)

-- | The value of @SourceImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image file format (Exif) metadata that includes the image\'s
-- orientation. Amazon Rekognition uses this orientation information to
-- perform image correction. The bounding box coordinates are translated to
-- represent object locations after the orientation information in the Exif
-- metadata is used to correct the image orientation. Images in .png format
-- don\'t contain Exif metadata.
--
-- Amazon Rekognition doesn’t perform image correction for images in .png
-- format and .jpeg images without orientation information in the image
-- Exif metadata. The bounding box coordinates aren\'t translated and
-- represent the object locations before the image is rotated.
compareFacesResponse_sourceImageOrientationCorrection :: Lens.Lens' CompareFacesResponse (Prelude.Maybe OrientationCorrection)
compareFacesResponse_sourceImageOrientationCorrection = Lens.lens (\CompareFacesResponse' {sourceImageOrientationCorrection} -> sourceImageOrientationCorrection) (\s@CompareFacesResponse' {} a -> s {sourceImageOrientationCorrection = a} :: CompareFacesResponse)

-- | The value of @TargetImageOrientationCorrection@ is always null.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image file format (Exif) metadata that includes the image\'s
-- orientation. Amazon Rekognition uses this orientation information to
-- perform image correction. The bounding box coordinates are translated to
-- represent object locations after the orientation information in the Exif
-- metadata is used to correct the image orientation. Images in .png format
-- don\'t contain Exif metadata.
--
-- Amazon Rekognition doesn’t perform image correction for images in .png
-- format and .jpeg images without orientation information in the image
-- Exif metadata. The bounding box coordinates aren\'t translated and
-- represent the object locations before the image is rotated.
compareFacesResponse_targetImageOrientationCorrection :: Lens.Lens' CompareFacesResponse (Prelude.Maybe OrientationCorrection)
compareFacesResponse_targetImageOrientationCorrection = Lens.lens (\CompareFacesResponse' {targetImageOrientationCorrection} -> targetImageOrientationCorrection) (\s@CompareFacesResponse' {} a -> s {targetImageOrientationCorrection = a} :: CompareFacesResponse)

-- | An array of faces in the target image that did not match the source
-- image face.
compareFacesResponse_unmatchedFaces :: Lens.Lens' CompareFacesResponse (Prelude.Maybe [ComparedFace])
compareFacesResponse_unmatchedFaces = Lens.lens (\CompareFacesResponse' {unmatchedFaces} -> unmatchedFaces) (\s@CompareFacesResponse' {} a -> s {unmatchedFaces = a} :: CompareFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
compareFacesResponse_httpStatus :: Lens.Lens' CompareFacesResponse Prelude.Int
compareFacesResponse_httpStatus = Lens.lens (\CompareFacesResponse' {httpStatus} -> httpStatus) (\s@CompareFacesResponse' {} a -> s {httpStatus = a} :: CompareFacesResponse)

instance Prelude.NFData CompareFacesResponse where
  rnf CompareFacesResponse' {..} =
    Prelude.rnf faceMatches
      `Prelude.seq` Prelude.rnf sourceImageFace
      `Prelude.seq` Prelude.rnf sourceImageOrientationCorrection
      `Prelude.seq` Prelude.rnf targetImageOrientationCorrection
      `Prelude.seq` Prelude.rnf unmatchedFaces
      `Prelude.seq` Prelude.rnf httpStatus
