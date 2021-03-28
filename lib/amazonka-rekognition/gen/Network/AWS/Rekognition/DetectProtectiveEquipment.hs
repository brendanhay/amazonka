{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DetectProtectiveEquipment (..)
    , mkDetectProtectiveEquipment
    -- ** Request lenses
    , dpeImage
    , dpeSummarizationAttributes

    -- * Destructuring the response
    , DetectProtectiveEquipmentResponse (..)
    , mkDetectProtectiveEquipmentResponse
    -- ** Response lenses
    , dperrsPersons
    , dperrsProtectiveEquipmentModelVersion
    , dperrsSummary
    , dperrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectProtectiveEquipment' smart constructor.
data DetectProtectiveEquipment = DetectProtectiveEquipment'
  { image :: Types.Image
    -- ^ The image in which you want to detect PPE on detected persons. The image can be passed as image bytes or you can reference an image stored in an Amazon S3 bucket. 
  , summarizationAttributes :: Core.Maybe Types.ProtectiveEquipmentSummarizationAttributes
    -- ^ An array of PPE types that you want to summarize.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectProtectiveEquipment' value with any optional fields omitted.
mkDetectProtectiveEquipment
    :: Types.Image -- ^ 'image'
    -> DetectProtectiveEquipment
mkDetectProtectiveEquipment image
  = DetectProtectiveEquipment'{image,
                               summarizationAttributes = Core.Nothing}

-- | The image in which you want to detect PPE on detected persons. The image can be passed as image bytes or you can reference an image stored in an Amazon S3 bucket. 
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeImage :: Lens.Lens' DetectProtectiveEquipment Types.Image
dpeImage = Lens.field @"image"
{-# INLINEABLE dpeImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | An array of PPE types that you want to summarize.
--
-- /Note:/ Consider using 'summarizationAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeSummarizationAttributes :: Lens.Lens' DetectProtectiveEquipment (Core.Maybe Types.ProtectiveEquipmentSummarizationAttributes)
dpeSummarizationAttributes = Lens.field @"summarizationAttributes"
{-# INLINEABLE dpeSummarizationAttributes #-}
{-# DEPRECATED summarizationAttributes "Use generic-lens or generic-optics with 'summarizationAttributes' instead"  #-}

instance Core.ToQuery DetectProtectiveEquipment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetectProtectiveEquipment where
        toHeaders DetectProtectiveEquipment{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.DetectProtectiveEquipment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetectProtectiveEquipment where
        toJSON DetectProtectiveEquipment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Image" Core..= image),
                  ("SummarizationAttributes" Core..=) Core.<$>
                    summarizationAttributes])

instance Core.AWSRequest DetectProtectiveEquipment where
        type Rs DetectProtectiveEquipment =
             DetectProtectiveEquipmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetectProtectiveEquipmentResponse' Core.<$>
                   (x Core..:? "Persons") Core.<*>
                     x Core..:? "ProtectiveEquipmentModelVersion"
                     Core.<*> x Core..:? "Summary"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetectProtectiveEquipmentResponse' smart constructor.
data DetectProtectiveEquipmentResponse = DetectProtectiveEquipmentResponse'
  { persons :: Core.Maybe [Types.ProtectiveEquipmentPerson]
    -- ^ An array of persons detected in the image (including persons not wearing PPE).
  , protectiveEquipmentModelVersion :: Core.Maybe Core.Text
    -- ^ The version number of the PPE detection model used to detect PPE in the image.
  , summary :: Core.Maybe Types.ProtectiveEquipmentSummary
    -- ^ Summary information for the types of PPE specified in the @SummarizationAttributes@ input parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectProtectiveEquipmentResponse' value with any optional fields omitted.
mkDetectProtectiveEquipmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetectProtectiveEquipmentResponse
mkDetectProtectiveEquipmentResponse responseStatus
  = DetectProtectiveEquipmentResponse'{persons = Core.Nothing,
                                       protectiveEquipmentModelVersion = Core.Nothing,
                                       summary = Core.Nothing, responseStatus}

-- | An array of persons detected in the image (including persons not wearing PPE).
--
-- /Note:/ Consider using 'persons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsPersons :: Lens.Lens' DetectProtectiveEquipmentResponse (Core.Maybe [Types.ProtectiveEquipmentPerson])
dperrsPersons = Lens.field @"persons"
{-# INLINEABLE dperrsPersons #-}
{-# DEPRECATED persons "Use generic-lens or generic-optics with 'persons' instead"  #-}

-- | The version number of the PPE detection model used to detect PPE in the image.
--
-- /Note:/ Consider using 'protectiveEquipmentModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsProtectiveEquipmentModelVersion :: Lens.Lens' DetectProtectiveEquipmentResponse (Core.Maybe Core.Text)
dperrsProtectiveEquipmentModelVersion = Lens.field @"protectiveEquipmentModelVersion"
{-# INLINEABLE dperrsProtectiveEquipmentModelVersion #-}
{-# DEPRECATED protectiveEquipmentModelVersion "Use generic-lens or generic-optics with 'protectiveEquipmentModelVersion' instead"  #-}

-- | Summary information for the types of PPE specified in the @SummarizationAttributes@ input parameter.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsSummary :: Lens.Lens' DetectProtectiveEquipmentResponse (Core.Maybe Types.ProtectiveEquipmentSummary)
dperrsSummary = Lens.field @"summary"
{-# INLINEABLE dperrsSummary #-}
{-# DEPRECATED summary "Use generic-lens or generic-optics with 'summary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dperrsResponseStatus :: Lens.Lens' DetectProtectiveEquipmentResponse Core.Int
dperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
