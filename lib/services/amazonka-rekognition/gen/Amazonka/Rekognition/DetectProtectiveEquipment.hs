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
-- Module      : Amazonka.Rekognition.DetectProtectiveEquipment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects Personal Protective Equipment (PPE) worn by people detected in
-- an image. Amazon Rekognition can detect the following types of PPE.
--
-- -   Face cover
--
-- -   Hand cover
--
-- -   Head cover
--
-- You pass the input image as base64-encoded image bytes or as a reference
-- to an image in an Amazon S3 bucket. The image must be either a PNG or
-- JPG formatted file.
--
-- @DetectProtectiveEquipment@ detects PPE worn by up to 15 persons
-- detected in an image.
--
-- For each person detected in the image the API returns an array of body
-- parts (face, head, left-hand, right-hand). For each body part, an array
-- of detected items of PPE is returned, including an indicator of whether
-- or not the PPE covers the body part. The API returns the confidence it
-- has in each detection (person, PPE, body part and body part coverage).
-- It also returns a bounding box (BoundingBox) for each detected person
-- and each detected item of PPE.
--
-- You can optionally request a summary of detected PPE items with the
-- @SummarizationAttributes@ input parameter. The summary provides the
-- following information.
--
-- -   The persons detected as wearing all of the types of PPE that you
--     specify.
--
-- -   The persons detected as not wearing all of the types PPE that you
--     specify.
--
-- -   The persons detected where PPE adornment could not be determined.
--
-- This is a stateless API operation. That is, the operation does not
-- persist any data.
--
-- This operation requires permissions to perform the
-- @rekognition:DetectProtectiveEquipment@ action.
module Amazonka.Rekognition.DetectProtectiveEquipment
  ( -- * Creating a Request
    DetectProtectiveEquipment (..),
    newDetectProtectiveEquipment,

    -- * Request Lenses
    detectProtectiveEquipment_summarizationAttributes,
    detectProtectiveEquipment_image,

    -- * Destructuring the Response
    DetectProtectiveEquipmentResponse (..),
    newDetectProtectiveEquipmentResponse,

    -- * Response Lenses
    detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion,
    detectProtectiveEquipmentResponse_summary,
    detectProtectiveEquipmentResponse_persons,
    detectProtectiveEquipmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectProtectiveEquipment' smart constructor.
data DetectProtectiveEquipment = DetectProtectiveEquipment'
  { -- | An array of PPE types that you want to summarize.
    summarizationAttributes :: Prelude.Maybe ProtectiveEquipmentSummarizationAttributes,
    -- | The image in which you want to detect PPE on detected persons. The image
    -- can be passed as image bytes or you can reference an image stored in an
    -- Amazon S3 bucket.
    image :: Image
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectProtectiveEquipment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summarizationAttributes', 'detectProtectiveEquipment_summarizationAttributes' - An array of PPE types that you want to summarize.
--
-- 'image', 'detectProtectiveEquipment_image' - The image in which you want to detect PPE on detected persons. The image
-- can be passed as image bytes or you can reference an image stored in an
-- Amazon S3 bucket.
newDetectProtectiveEquipment ::
  -- | 'image'
  Image ->
  DetectProtectiveEquipment
newDetectProtectiveEquipment pImage_ =
  DetectProtectiveEquipment'
    { summarizationAttributes =
        Prelude.Nothing,
      image = pImage_
    }

-- | An array of PPE types that you want to summarize.
detectProtectiveEquipment_summarizationAttributes :: Lens.Lens' DetectProtectiveEquipment (Prelude.Maybe ProtectiveEquipmentSummarizationAttributes)
detectProtectiveEquipment_summarizationAttributes = Lens.lens (\DetectProtectiveEquipment' {summarizationAttributes} -> summarizationAttributes) (\s@DetectProtectiveEquipment' {} a -> s {summarizationAttributes = a} :: DetectProtectiveEquipment)

-- | The image in which you want to detect PPE on detected persons. The image
-- can be passed as image bytes or you can reference an image stored in an
-- Amazon S3 bucket.
detectProtectiveEquipment_image :: Lens.Lens' DetectProtectiveEquipment Image
detectProtectiveEquipment_image = Lens.lens (\DetectProtectiveEquipment' {image} -> image) (\s@DetectProtectiveEquipment' {} a -> s {image = a} :: DetectProtectiveEquipment)

instance Core.AWSRequest DetectProtectiveEquipment where
  type
    AWSResponse DetectProtectiveEquipment =
      DetectProtectiveEquipmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectProtectiveEquipmentResponse'
            Prelude.<$> (x Core..?> "ProtectiveEquipmentModelVersion")
            Prelude.<*> (x Core..?> "Summary")
            Prelude.<*> (x Core..?> "Persons" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectProtectiveEquipment where
  hashWithSalt _salt DetectProtectiveEquipment' {..} =
    _salt
      `Prelude.hashWithSalt` summarizationAttributes
      `Prelude.hashWithSalt` image

instance Prelude.NFData DetectProtectiveEquipment where
  rnf DetectProtectiveEquipment' {..} =
    Prelude.rnf summarizationAttributes
      `Prelude.seq` Prelude.rnf image

instance Core.ToHeaders DetectProtectiveEquipment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DetectProtectiveEquipment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectProtectiveEquipment where
  toJSON DetectProtectiveEquipment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SummarizationAttributes" Core..=)
              Prelude.<$> summarizationAttributes,
            Prelude.Just ("Image" Core..= image)
          ]
      )

instance Core.ToPath DetectProtectiveEquipment where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectProtectiveEquipment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectProtectiveEquipmentResponse' smart constructor.
data DetectProtectiveEquipmentResponse = DetectProtectiveEquipmentResponse'
  { -- | The version number of the PPE detection model used to detect PPE in the
    -- image.
    protectiveEquipmentModelVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the types of PPE specified in the
    -- @SummarizationAttributes@ input parameter.
    summary :: Prelude.Maybe ProtectiveEquipmentSummary,
    -- | An array of persons detected in the image (including persons not wearing
    -- PPE).
    persons :: Prelude.Maybe [ProtectiveEquipmentPerson],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectProtectiveEquipmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectiveEquipmentModelVersion', 'detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion' - The version number of the PPE detection model used to detect PPE in the
-- image.
--
-- 'summary', 'detectProtectiveEquipmentResponse_summary' - Summary information for the types of PPE specified in the
-- @SummarizationAttributes@ input parameter.
--
-- 'persons', 'detectProtectiveEquipmentResponse_persons' - An array of persons detected in the image (including persons not wearing
-- PPE).
--
-- 'httpStatus', 'detectProtectiveEquipmentResponse_httpStatus' - The response's http status code.
newDetectProtectiveEquipmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectProtectiveEquipmentResponse
newDetectProtectiveEquipmentResponse pHttpStatus_ =
  DetectProtectiveEquipmentResponse'
    { protectiveEquipmentModelVersion =
        Prelude.Nothing,
      summary = Prelude.Nothing,
      persons = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version number of the PPE detection model used to detect PPE in the
-- image.
detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion :: Lens.Lens' DetectProtectiveEquipmentResponse (Prelude.Maybe Prelude.Text)
detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion = Lens.lens (\DetectProtectiveEquipmentResponse' {protectiveEquipmentModelVersion} -> protectiveEquipmentModelVersion) (\s@DetectProtectiveEquipmentResponse' {} a -> s {protectiveEquipmentModelVersion = a} :: DetectProtectiveEquipmentResponse)

-- | Summary information for the types of PPE specified in the
-- @SummarizationAttributes@ input parameter.
detectProtectiveEquipmentResponse_summary :: Lens.Lens' DetectProtectiveEquipmentResponse (Prelude.Maybe ProtectiveEquipmentSummary)
detectProtectiveEquipmentResponse_summary = Lens.lens (\DetectProtectiveEquipmentResponse' {summary} -> summary) (\s@DetectProtectiveEquipmentResponse' {} a -> s {summary = a} :: DetectProtectiveEquipmentResponse)

-- | An array of persons detected in the image (including persons not wearing
-- PPE).
detectProtectiveEquipmentResponse_persons :: Lens.Lens' DetectProtectiveEquipmentResponse (Prelude.Maybe [ProtectiveEquipmentPerson])
detectProtectiveEquipmentResponse_persons = Lens.lens (\DetectProtectiveEquipmentResponse' {persons} -> persons) (\s@DetectProtectiveEquipmentResponse' {} a -> s {persons = a} :: DetectProtectiveEquipmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectProtectiveEquipmentResponse_httpStatus :: Lens.Lens' DetectProtectiveEquipmentResponse Prelude.Int
detectProtectiveEquipmentResponse_httpStatus = Lens.lens (\DetectProtectiveEquipmentResponse' {httpStatus} -> httpStatus) (\s@DetectProtectiveEquipmentResponse' {} a -> s {httpStatus = a} :: DetectProtectiveEquipmentResponse)

instance
  Prelude.NFData
    DetectProtectiveEquipmentResponse
  where
  rnf DetectProtectiveEquipmentResponse' {..} =
    Prelude.rnf protectiveEquipmentModelVersion
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf persons
      `Prelude.seq` Prelude.rnf httpStatus
