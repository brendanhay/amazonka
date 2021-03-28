{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EquipmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.EquipmentDetection
  ( EquipmentDetection (..)
  -- * Smart constructor
  , mkEquipmentDetection
  -- * Lenses
  , edBoundingBox
  , edConfidence
  , edCoversBodyPart
  , edType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types
import qualified Network.AWS.Rekognition.Types.CoversBodyPart as Types
import qualified Network.AWS.Rekognition.Types.ProtectiveEquipmentType as Types

-- | Information about an item of Personal Protective Equipment (PPE) detected by 'DetectProtectiveEquipment' . For more information, see 'DetectProtectiveEquipment' .
--
-- /See:/ 'mkEquipmentDetection' smart constructor.
data EquipmentDetection = EquipmentDetection'
  { boundingBox :: Core.Maybe Types.BoundingBox
    -- ^ A bounding box surrounding the item of detected PPE.
  , confidence :: Core.Maybe Core.Double
    -- ^ The confidence that Amazon Rekognition has that the bounding box (@BoundingBox@ ) contains an item of PPE.
  , coversBodyPart :: Core.Maybe Types.CoversBodyPart
    -- ^ Information about the body part covered by the detected PPE.
  , type' :: Core.Maybe Types.ProtectiveEquipmentType
    -- ^ The type of detected PPE.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EquipmentDetection' value with any optional fields omitted.
mkEquipmentDetection
    :: EquipmentDetection
mkEquipmentDetection
  = EquipmentDetection'{boundingBox = Core.Nothing,
                        confidence = Core.Nothing, coversBodyPart = Core.Nothing,
                        type' = Core.Nothing}

-- | A bounding box surrounding the item of detected PPE.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edBoundingBox :: Lens.Lens' EquipmentDetection (Core.Maybe Types.BoundingBox)
edBoundingBox = Lens.field @"boundingBox"
{-# INLINEABLE edBoundingBox #-}
{-# DEPRECATED boundingBox "Use generic-lens or generic-optics with 'boundingBox' instead"  #-}

-- | The confidence that Amazon Rekognition has that the bounding box (@BoundingBox@ ) contains an item of PPE.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edConfidence :: Lens.Lens' EquipmentDetection (Core.Maybe Core.Double)
edConfidence = Lens.field @"confidence"
{-# INLINEABLE edConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | Information about the body part covered by the detected PPE.
--
-- /Note:/ Consider using 'coversBodyPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCoversBodyPart :: Lens.Lens' EquipmentDetection (Core.Maybe Types.CoversBodyPart)
edCoversBodyPart = Lens.field @"coversBodyPart"
{-# INLINEABLE edCoversBodyPart #-}
{-# DEPRECATED coversBodyPart "Use generic-lens or generic-optics with 'coversBodyPart' instead"  #-}

-- | The type of detected PPE.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edType :: Lens.Lens' EquipmentDetection (Core.Maybe Types.ProtectiveEquipmentType)
edType = Lens.field @"type'"
{-# INLINEABLE edType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON EquipmentDetection where
        parseJSON
          = Core.withObject "EquipmentDetection" Core.$
              \ x ->
                EquipmentDetection' Core.<$>
                  (x Core..:? "BoundingBox") Core.<*> x Core..:? "Confidence"
                    Core.<*> x Core..:? "CoversBodyPart"
                    Core.<*> x Core..:? "Type"
