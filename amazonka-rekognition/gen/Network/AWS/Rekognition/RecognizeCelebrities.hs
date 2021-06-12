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
-- Module      : Network.AWS.Rekognition.RecognizeCelebrities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of celebrities recognized in the input image. For more
-- information, see Recognizing Celebrities in the Amazon Rekognition
-- Developer Guide.
--
-- @RecognizeCelebrities@ returns the 64 largest faces in the image. It
-- lists recognized celebrities in the @CelebrityFaces@ array and
-- unrecognized faces in the @UnrecognizedFaces@ array.
-- @RecognizeCelebrities@ doesn\'t return celebrities whose faces aren\'t
-- among the largest 64 faces in the image.
--
-- For each celebrity recognized, @RecognizeCelebrities@ returns a
-- @Celebrity@ object. The @Celebrity@ object contains the celebrity name,
-- ID, URL links to additional information, match confidence, and a
-- @ComparedFace@ object that you can use to locate the celebrity\'s face
-- on the image.
--
-- Amazon Rekognition doesn\'t retain information about which images a
-- celebrity has been recognized in. Your application must store this
-- information and use the @Celebrity@ ID property as a unique identifier
-- for the celebrity. If you don\'t store the celebrity name or additional
-- information URLs returned by @RecognizeCelebrities@, you will need the
-- ID to identify the celebrity in a call to the GetCelebrityInfo
-- operation.
--
-- You pass the input image either as base64-encoded image bytes or as a
-- reference to an image in an Amazon S3 bucket. If you use the AWS CLI to
-- call Amazon Rekognition operations, passing image bytes is not
-- supported. The image must be either a PNG or JPEG formatted file.
--
-- For an example, see Recognizing Celebrities in an Image in the Amazon
-- Rekognition Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:RecognizeCelebrities@ operation.
module Network.AWS.Rekognition.RecognizeCelebrities
  ( -- * Creating a Request
    RecognizeCelebrities (..),
    newRecognizeCelebrities,

    -- * Request Lenses
    recognizeCelebrities_image,

    -- * Destructuring the Response
    RecognizeCelebritiesResponse (..),
    newRecognizeCelebritiesResponse,

    -- * Response Lenses
    recognizeCelebritiesResponse_unrecognizedFaces,
    recognizeCelebritiesResponse_celebrityFaces,
    recognizeCelebritiesResponse_orientationCorrection,
    recognizeCelebritiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRecognizeCelebrities' smart constructor.
data RecognizeCelebrities = RecognizeCelebrities'
  { -- | The input image as base64-encoded bytes or an S3 object. If you use the
    -- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
    -- image bytes is not supported.
    --
    -- If you are using an AWS SDK to call Amazon Rekognition, you might not
    -- need to base64-encode image bytes passed using the @Bytes@ field. For
    -- more information, see Images in the Amazon Rekognition developer guide.
    image :: Image
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecognizeCelebrities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'image', 'recognizeCelebrities_image' - The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
newRecognizeCelebrities ::
  -- | 'image'
  Image ->
  RecognizeCelebrities
newRecognizeCelebrities pImage_ =
  RecognizeCelebrities' {image = pImage_}

-- | The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
recognizeCelebrities_image :: Lens.Lens' RecognizeCelebrities Image
recognizeCelebrities_image = Lens.lens (\RecognizeCelebrities' {image} -> image) (\s@RecognizeCelebrities' {} a -> s {image = a} :: RecognizeCelebrities)

instance Core.AWSRequest RecognizeCelebrities where
  type
    AWSResponse RecognizeCelebrities =
      RecognizeCelebritiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RecognizeCelebritiesResponse'
            Core.<$> (x Core..?> "UnrecognizedFaces" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "CelebrityFaces" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "OrientationCorrection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RecognizeCelebrities

instance Core.NFData RecognizeCelebrities

instance Core.ToHeaders RecognizeCelebrities where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.RecognizeCelebrities" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RecognizeCelebrities where
  toJSON RecognizeCelebrities' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Image" Core..= image)])

instance Core.ToPath RecognizeCelebrities where
  toPath = Core.const "/"

instance Core.ToQuery RecognizeCelebrities where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRecognizeCelebritiesResponse' smart constructor.
data RecognizeCelebritiesResponse = RecognizeCelebritiesResponse'
  { -- | Details about each unrecognized face in the image.
    unrecognizedFaces :: Core.Maybe [ComparedFace],
    -- | Details about each celebrity found in the image. Amazon Rekognition can
    -- detect a maximum of 64 celebrities in an image.
    celebrityFaces :: Core.Maybe [Celebrity],
    -- | The orientation of the input image (counterclockwise direction). If your
    -- application displays the image, you can use this value to correct the
    -- orientation. The bounding box coordinates returned in @CelebrityFaces@
    -- and @UnrecognizedFaces@ represent face locations before the image
    -- orientation is corrected.
    --
    -- If the input image is in .jpeg format, it might contain exchangeable
    -- image (Exif) metadata that includes the image\'s orientation. If so, and
    -- the Exif metadata for the input image populates the orientation field,
    -- the value of @OrientationCorrection@ is null. The @CelebrityFaces@ and
    -- @UnrecognizedFaces@ bounding box coordinates represent face locations
    -- after Exif metadata is used to correct the image orientation. Images in
    -- .png format don\'t contain Exif metadata.
    orientationCorrection :: Core.Maybe OrientationCorrection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecognizeCelebritiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unrecognizedFaces', 'recognizeCelebritiesResponse_unrecognizedFaces' - Details about each unrecognized face in the image.
--
-- 'celebrityFaces', 'recognizeCelebritiesResponse_celebrityFaces' - Details about each celebrity found in the image. Amazon Rekognition can
-- detect a maximum of 64 celebrities in an image.
--
-- 'orientationCorrection', 'recognizeCelebritiesResponse_orientationCorrection' - The orientation of the input image (counterclockwise direction). If your
-- application displays the image, you can use this value to correct the
-- orientation. The bounding box coordinates returned in @CelebrityFaces@
-- and @UnrecognizedFaces@ represent face locations before the image
-- orientation is corrected.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image (Exif) metadata that includes the image\'s orientation. If so, and
-- the Exif metadata for the input image populates the orientation field,
-- the value of @OrientationCorrection@ is null. The @CelebrityFaces@ and
-- @UnrecognizedFaces@ bounding box coordinates represent face locations
-- after Exif metadata is used to correct the image orientation. Images in
-- .png format don\'t contain Exif metadata.
--
-- 'httpStatus', 'recognizeCelebritiesResponse_httpStatus' - The response's http status code.
newRecognizeCelebritiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RecognizeCelebritiesResponse
newRecognizeCelebritiesResponse pHttpStatus_ =
  RecognizeCelebritiesResponse'
    { unrecognizedFaces =
        Core.Nothing,
      celebrityFaces = Core.Nothing,
      orientationCorrection = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about each unrecognized face in the image.
recognizeCelebritiesResponse_unrecognizedFaces :: Lens.Lens' RecognizeCelebritiesResponse (Core.Maybe [ComparedFace])
recognizeCelebritiesResponse_unrecognizedFaces = Lens.lens (\RecognizeCelebritiesResponse' {unrecognizedFaces} -> unrecognizedFaces) (\s@RecognizeCelebritiesResponse' {} a -> s {unrecognizedFaces = a} :: RecognizeCelebritiesResponse) Core.. Lens.mapping Lens._Coerce

-- | Details about each celebrity found in the image. Amazon Rekognition can
-- detect a maximum of 64 celebrities in an image.
recognizeCelebritiesResponse_celebrityFaces :: Lens.Lens' RecognizeCelebritiesResponse (Core.Maybe [Celebrity])
recognizeCelebritiesResponse_celebrityFaces = Lens.lens (\RecognizeCelebritiesResponse' {celebrityFaces} -> celebrityFaces) (\s@RecognizeCelebritiesResponse' {} a -> s {celebrityFaces = a} :: RecognizeCelebritiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The orientation of the input image (counterclockwise direction). If your
-- application displays the image, you can use this value to correct the
-- orientation. The bounding box coordinates returned in @CelebrityFaces@
-- and @UnrecognizedFaces@ represent face locations before the image
-- orientation is corrected.
--
-- If the input image is in .jpeg format, it might contain exchangeable
-- image (Exif) metadata that includes the image\'s orientation. If so, and
-- the Exif metadata for the input image populates the orientation field,
-- the value of @OrientationCorrection@ is null. The @CelebrityFaces@ and
-- @UnrecognizedFaces@ bounding box coordinates represent face locations
-- after Exif metadata is used to correct the image orientation. Images in
-- .png format don\'t contain Exif metadata.
recognizeCelebritiesResponse_orientationCorrection :: Lens.Lens' RecognizeCelebritiesResponse (Core.Maybe OrientationCorrection)
recognizeCelebritiesResponse_orientationCorrection = Lens.lens (\RecognizeCelebritiesResponse' {orientationCorrection} -> orientationCorrection) (\s@RecognizeCelebritiesResponse' {} a -> s {orientationCorrection = a} :: RecognizeCelebritiesResponse)

-- | The response's http status code.
recognizeCelebritiesResponse_httpStatus :: Lens.Lens' RecognizeCelebritiesResponse Core.Int
recognizeCelebritiesResponse_httpStatus = Lens.lens (\RecognizeCelebritiesResponse' {httpStatus} -> httpStatus) (\s@RecognizeCelebritiesResponse' {} a -> s {httpStatus = a} :: RecognizeCelebritiesResponse)

instance Core.NFData RecognizeCelebritiesResponse
