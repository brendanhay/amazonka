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
-- Module      : Amazonka.Rekognition.DetectFaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces within an image that is provided as input.
--
-- @DetectFaces@ detects the 100 largest faces in the image. For each face
-- detected, the operation returns face details. These details include a
-- bounding box of the face, a confidence value (that the bounding box
-- contains a face), and a fixed set of attributes such as facial landmarks
-- (for example, coordinates of eye and mouth), presence of beard,
-- sunglasses, and so on.
--
-- The face-detection algorithm is most effective on frontal faces. For
-- non-frontal or obscured faces, the algorithm might not detect the faces
-- or might detect faces with lower confidence.
--
-- You pass the input image either as base64-encoded image bytes or as a
-- reference to an image in an Amazon S3 bucket. If you use the AWS CLI to
-- call Amazon Rekognition operations, passing image bytes is not
-- supported. The image must be either a PNG or JPEG formatted file.
--
-- This is a stateless API operation. That is, the operation does not
-- persist any data.
--
-- This operation requires permissions to perform the
-- @rekognition:DetectFaces@ action.
module Amazonka.Rekognition.DetectFaces
  ( -- * Creating a Request
    DetectFaces (..),
    newDetectFaces,

    -- * Request Lenses
    detectFaces_attributes,
    detectFaces_image,

    -- * Destructuring the Response
    DetectFacesResponse (..),
    newDetectFacesResponse,

    -- * Response Lenses
    detectFacesResponse_faceDetails,
    detectFacesResponse_orientationCorrection,
    detectFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectFaces' smart constructor.
data DetectFaces = DetectFaces'
  { -- | An array of facial attributes you want to be returned. This can be the
    -- default list of attributes or all attributes. If you don\'t specify a
    -- value for @Attributes@ or if you specify @[\"DEFAULT\"]@, the API
    -- returns the following subset of facial attributes: @BoundingBox@,
    -- @Confidence@, @Pose@, @Quality@, and @Landmarks@. If you provide
    -- @[\"ALL\"]@, all facial attributes are returned, but the operation takes
    -- longer to complete.
    --
    -- If you provide both, @[\"ALL\", \"DEFAULT\"]@, the service uses a
    -- logical AND operator to determine which attributes to return (in this
    -- case, all attributes).
    attributes :: Prelude.Maybe [Attribute],
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
-- Create a value of 'DetectFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'detectFaces_attributes' - An array of facial attributes you want to be returned. This can be the
-- default list of attributes or all attributes. If you don\'t specify a
-- value for @Attributes@ or if you specify @[\"DEFAULT\"]@, the API
-- returns the following subset of facial attributes: @BoundingBox@,
-- @Confidence@, @Pose@, @Quality@, and @Landmarks@. If you provide
-- @[\"ALL\"]@, all facial attributes are returned, but the operation takes
-- longer to complete.
--
-- If you provide both, @[\"ALL\", \"DEFAULT\"]@, the service uses a
-- logical AND operator to determine which attributes to return (in this
-- case, all attributes).
--
-- 'image', 'detectFaces_image' - The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
newDetectFaces ::
  -- | 'image'
  Image ->
  DetectFaces
newDetectFaces pImage_ =
  DetectFaces'
    { attributes = Prelude.Nothing,
      image = pImage_
    }

-- | An array of facial attributes you want to be returned. This can be the
-- default list of attributes or all attributes. If you don\'t specify a
-- value for @Attributes@ or if you specify @[\"DEFAULT\"]@, the API
-- returns the following subset of facial attributes: @BoundingBox@,
-- @Confidence@, @Pose@, @Quality@, and @Landmarks@. If you provide
-- @[\"ALL\"]@, all facial attributes are returned, but the operation takes
-- longer to complete.
--
-- If you provide both, @[\"ALL\", \"DEFAULT\"]@, the service uses a
-- logical AND operator to determine which attributes to return (in this
-- case, all attributes).
detectFaces_attributes :: Lens.Lens' DetectFaces (Prelude.Maybe [Attribute])
detectFaces_attributes = Lens.lens (\DetectFaces' {attributes} -> attributes) (\s@DetectFaces' {} a -> s {attributes = a} :: DetectFaces) Prelude.. Lens.mapping Lens.coerced

-- | The input image as base64-encoded bytes or an S3 object. If you use the
-- AWS CLI to call Amazon Rekognition operations, passing base64-encoded
-- image bytes is not supported.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not
-- need to base64-encode image bytes passed using the @Bytes@ field. For
-- more information, see Images in the Amazon Rekognition developer guide.
detectFaces_image :: Lens.Lens' DetectFaces Image
detectFaces_image = Lens.lens (\DetectFaces' {image} -> image) (\s@DetectFaces' {} a -> s {image = a} :: DetectFaces)

instance Core.AWSRequest DetectFaces where
  type AWSResponse DetectFaces = DetectFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectFacesResponse'
            Prelude.<$> (x Data..?> "FaceDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "OrientationCorrection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectFaces where
  hashWithSalt _salt DetectFaces' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` image

instance Prelude.NFData DetectFaces where
  rnf DetectFaces' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf image

instance Data.ToHeaders DetectFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DetectFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectFaces where
  toJSON DetectFaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            Prelude.Just ("Image" Data..= image)
          ]
      )

instance Data.ToPath DetectFaces where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectFacesResponse' smart constructor.
data DetectFacesResponse = DetectFacesResponse'
  { -- | Details of each face found in the image.
    faceDetails :: Prelude.Maybe [FaceDetail],
    -- | The value of @OrientationCorrection@ is always null.
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
    orientationCorrection :: Prelude.Maybe OrientationCorrection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceDetails', 'detectFacesResponse_faceDetails' - Details of each face found in the image.
--
-- 'orientationCorrection', 'detectFacesResponse_orientationCorrection' - The value of @OrientationCorrection@ is always null.
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
-- 'httpStatus', 'detectFacesResponse_httpStatus' - The response's http status code.
newDetectFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectFacesResponse
newDetectFacesResponse pHttpStatus_ =
  DetectFacesResponse'
    { faceDetails = Prelude.Nothing,
      orientationCorrection = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of each face found in the image.
detectFacesResponse_faceDetails :: Lens.Lens' DetectFacesResponse (Prelude.Maybe [FaceDetail])
detectFacesResponse_faceDetails = Lens.lens (\DetectFacesResponse' {faceDetails} -> faceDetails) (\s@DetectFacesResponse' {} a -> s {faceDetails = a} :: DetectFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The value of @OrientationCorrection@ is always null.
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
detectFacesResponse_orientationCorrection :: Lens.Lens' DetectFacesResponse (Prelude.Maybe OrientationCorrection)
detectFacesResponse_orientationCorrection = Lens.lens (\DetectFacesResponse' {orientationCorrection} -> orientationCorrection) (\s@DetectFacesResponse' {} a -> s {orientationCorrection = a} :: DetectFacesResponse)

-- | The response's http status code.
detectFacesResponse_httpStatus :: Lens.Lens' DetectFacesResponse Prelude.Int
detectFacesResponse_httpStatus = Lens.lens (\DetectFacesResponse' {httpStatus} -> httpStatus) (\s@DetectFacesResponse' {} a -> s {httpStatus = a} :: DetectFacesResponse)

instance Prelude.NFData DetectFacesResponse where
  rnf DetectFacesResponse' {..} =
    Prelude.rnf faceDetails
      `Prelude.seq` Prelude.rnf orientationCorrection
      `Prelude.seq` Prelude.rnf httpStatus
