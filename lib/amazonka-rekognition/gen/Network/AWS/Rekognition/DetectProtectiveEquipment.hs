{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectProtectiveEquipment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects Personal Protective Equipment (PPE) worn by people detected in an image. Amazon Rekognition can detect the following types of PPE.
--
--
--     * Face cover
--
--
--     * Hand cover
--
--
--     * Head cover
--
--
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. The image must be either a PNG or JPG formatted file.
-- @DetectProtectiveEquipment@ detects PPE worn by up to 15 persons detected in an image.
-- For each person detected in the image the API returns an array of body parts (face, head, left-hand, right-hand). For each body part, an array of detected items of PPE is returned, including an indicator of whether or not the PPE covers the body part. The API returns the confidence it has in each detection (person, PPE, body part and body part coverage). It also returns a bounding box ('BoundingBox' ) for each detected person and each detected item of PPE.
-- You can optionally request a summary of detected PPE items with the @SummarizationAttributes@ input parameter. The summary provides the following information.
--
--     * The persons detected as wearing all of the types of PPE that you specify.
--
--
--     * The persons detected as not wearing all of the types PPE that you specify.
--
--
--     * The persons detected where PPE adornment could not be determined.
--
--
-- This is a stateless API operation. That is, the operation does not persist any data.
-- This operation requires permissions to perform the @rekognition:DetectProtectiveEquipment@ action.
module Network.AWS.Rekognition.DetectProtectiveEquipment
  ( -- * Creating a request
    DetectProtectiveEquipment (..),
    mkDetectProtectiveEquipment,

    -- ** Request lenses
    dpeSummarizationAttributes,
    dpeImage,

    -- * Destructuring the response
    DetectProtectiveEquipmentResponse (..),
    mkDetectProtectiveEquipmentResponse,

    -- ** Response lenses
    dpersSummary,
    dpersProtectiveEquipmentModelVersion,
    dpersPersons,
    dpersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectProtectiveEquipment' smart constructor.
data DetectProtectiveEquipment = DetectProtectiveEquipment'
  { -- | An array of PPE types that you want to summarize.
    summarizationAttributes :: Lude.Maybe ProtectiveEquipmentSummarizationAttributes,
    -- | The image in which you want to detect PPE on detected persons. The image can be passed as image bytes or you can reference an image stored in an Amazon S3 bucket.
    image :: Image
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectProtectiveEquipment' with the minimum fields required to make a request.
--
-- * 'summarizationAttributes' - An array of PPE types that you want to summarize.
-- * 'image' - The image in which you want to detect PPE on detected persons. The image can be passed as image bytes or you can reference an image stored in an Amazon S3 bucket.
mkDetectProtectiveEquipment ::
  -- | 'image'
  Image ->
  DetectProtectiveEquipment
mkDetectProtectiveEquipment pImage_ =
  DetectProtectiveEquipment'
    { summarizationAttributes =
        Lude.Nothing,
      image = pImage_
    }

-- | An array of PPE types that you want to summarize.
--
-- /Note:/ Consider using 'summarizationAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeSummarizationAttributes :: Lens.Lens' DetectProtectiveEquipment (Lude.Maybe ProtectiveEquipmentSummarizationAttributes)
dpeSummarizationAttributes = Lens.lens (summarizationAttributes :: DetectProtectiveEquipment -> Lude.Maybe ProtectiveEquipmentSummarizationAttributes) (\s a -> s {summarizationAttributes = a} :: DetectProtectiveEquipment)
{-# DEPRECATED dpeSummarizationAttributes "Use generic-lens or generic-optics with 'summarizationAttributes' instead." #-}

-- | The image in which you want to detect PPE on detected persons. The image can be passed as image bytes or you can reference an image stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeImage :: Lens.Lens' DetectProtectiveEquipment Image
dpeImage = Lens.lens (image :: DetectProtectiveEquipment -> Image) (\s a -> s {image = a} :: DetectProtectiveEquipment)
{-# DEPRECATED dpeImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.AWSRequest DetectProtectiveEquipment where
  type
    Rs DetectProtectiveEquipment =
      DetectProtectiveEquipmentResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectProtectiveEquipmentResponse'
            Lude.<$> (x Lude..?> "Summary")
            Lude.<*> (x Lude..?> "ProtectiveEquipmentModelVersion")
            Lude.<*> (x Lude..?> "Persons" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectProtectiveEquipment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "RekognitionService.DetectProtectiveEquipment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectProtectiveEquipment where
  toJSON DetectProtectiveEquipment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SummarizationAttributes" Lude..=)
              Lude.<$> summarizationAttributes,
            Lude.Just ("Image" Lude..= image)
          ]
      )

instance Lude.ToPath DetectProtectiveEquipment where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectProtectiveEquipment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectProtectiveEquipmentResponse' smart constructor.
data DetectProtectiveEquipmentResponse = DetectProtectiveEquipmentResponse'
  { -- | Summary information for the types of PPE specified in the @SummarizationAttributes@ input parameter.
    summary :: Lude.Maybe ProtectiveEquipmentSummary,
    -- | The version number of the PPE detection model used to detect PPE in the image.
    protectiveEquipmentModelVersion :: Lude.Maybe Lude.Text,
    -- | An array of persons detected in the image (including persons not wearing PPE).
    persons :: Lude.Maybe [ProtectiveEquipmentPerson],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectProtectiveEquipmentResponse' with the minimum fields required to make a request.
--
-- * 'summary' - Summary information for the types of PPE specified in the @SummarizationAttributes@ input parameter.
-- * 'protectiveEquipmentModelVersion' - The version number of the PPE detection model used to detect PPE in the image.
-- * 'persons' - An array of persons detected in the image (including persons not wearing PPE).
-- * 'responseStatus' - The response status code.
mkDetectProtectiveEquipmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectProtectiveEquipmentResponse
mkDetectProtectiveEquipmentResponse pResponseStatus_ =
  DetectProtectiveEquipmentResponse'
    { summary = Lude.Nothing,
      protectiveEquipmentModelVersion = Lude.Nothing,
      persons = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary information for the types of PPE specified in the @SummarizationAttributes@ input parameter.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersSummary :: Lens.Lens' DetectProtectiveEquipmentResponse (Lude.Maybe ProtectiveEquipmentSummary)
dpersSummary = Lens.lens (summary :: DetectProtectiveEquipmentResponse -> Lude.Maybe ProtectiveEquipmentSummary) (\s a -> s {summary = a} :: DetectProtectiveEquipmentResponse)
{-# DEPRECATED dpersSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The version number of the PPE detection model used to detect PPE in the image.
--
-- /Note:/ Consider using 'protectiveEquipmentModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersProtectiveEquipmentModelVersion :: Lens.Lens' DetectProtectiveEquipmentResponse (Lude.Maybe Lude.Text)
dpersProtectiveEquipmentModelVersion = Lens.lens (protectiveEquipmentModelVersion :: DetectProtectiveEquipmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {protectiveEquipmentModelVersion = a} :: DetectProtectiveEquipmentResponse)
{-# DEPRECATED dpersProtectiveEquipmentModelVersion "Use generic-lens or generic-optics with 'protectiveEquipmentModelVersion' instead." #-}

-- | An array of persons detected in the image (including persons not wearing PPE).
--
-- /Note:/ Consider using 'persons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersPersons :: Lens.Lens' DetectProtectiveEquipmentResponse (Lude.Maybe [ProtectiveEquipmentPerson])
dpersPersons = Lens.lens (persons :: DetectProtectiveEquipmentResponse -> Lude.Maybe [ProtectiveEquipmentPerson]) (\s a -> s {persons = a} :: DetectProtectiveEquipmentResponse)
{-# DEPRECATED dpersPersons "Use generic-lens or generic-optics with 'persons' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersResponseStatus :: Lens.Lens' DetectProtectiveEquipmentResponse Lude.Int
dpersResponseStatus = Lens.lens (responseStatus :: DetectProtectiveEquipmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectProtectiveEquipmentResponse)
{-# DEPRECATED dpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
