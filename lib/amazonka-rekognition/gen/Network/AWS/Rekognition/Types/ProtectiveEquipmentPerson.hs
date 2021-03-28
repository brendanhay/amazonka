{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
  ( ProtectiveEquipmentPerson (..)
  -- * Smart constructor
  , mkProtectiveEquipmentPerson
  -- * Lenses
  , pepBodyParts
  , pepBoundingBox
  , pepConfidence
  , pepId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types
import qualified Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart as Types

-- | A person detected by a call to 'DetectProtectiveEquipment' . The API returns all persons detected in the input image in an array of @ProtectiveEquipmentPerson@ objects.
--
-- /See:/ 'mkProtectiveEquipmentPerson' smart constructor.
data ProtectiveEquipmentPerson = ProtectiveEquipmentPerson'
  { bodyParts :: Core.Maybe [Types.ProtectiveEquipmentBodyPart]
    -- ^ An array of body parts detected on a person's body (including body parts without PPE). 
  , boundingBox :: Core.Maybe Types.BoundingBox
    -- ^ A bounding box around the detected person.
  , confidence :: Core.Maybe Core.Double
    -- ^ The confidence that Amazon Rekognition has that the bounding box contains a person.
  , id :: Core.Maybe Core.Natural
    -- ^ The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectiveEquipmentPerson' value with any optional fields omitted.
mkProtectiveEquipmentPerson
    :: ProtectiveEquipmentPerson
mkProtectiveEquipmentPerson
  = ProtectiveEquipmentPerson'{bodyParts = Core.Nothing,
                               boundingBox = Core.Nothing, confidence = Core.Nothing,
                               id = Core.Nothing}

-- | An array of body parts detected on a person's body (including body parts without PPE). 
--
-- /Note:/ Consider using 'bodyParts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepBodyParts :: Lens.Lens' ProtectiveEquipmentPerson (Core.Maybe [Types.ProtectiveEquipmentBodyPart])
pepBodyParts = Lens.field @"bodyParts"
{-# INLINEABLE pepBodyParts #-}
{-# DEPRECATED bodyParts "Use generic-lens or generic-optics with 'bodyParts' instead"  #-}

-- | A bounding box around the detected person.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepBoundingBox :: Lens.Lens' ProtectiveEquipmentPerson (Core.Maybe Types.BoundingBox)
pepBoundingBox = Lens.field @"boundingBox"
{-# INLINEABLE pepBoundingBox #-}
{-# DEPRECATED boundingBox "Use generic-lens or generic-optics with 'boundingBox' instead"  #-}

-- | The confidence that Amazon Rekognition has that the bounding box contains a person.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepConfidence :: Lens.Lens' ProtectiveEquipmentPerson (Core.Maybe Core.Double)
pepConfidence = Lens.field @"confidence"
{-# INLINEABLE pepConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepId :: Lens.Lens' ProtectiveEquipmentPerson (Core.Maybe Core.Natural)
pepId = Lens.field @"id"
{-# INLINEABLE pepId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON ProtectiveEquipmentPerson where
        parseJSON
          = Core.withObject "ProtectiveEquipmentPerson" Core.$
              \ x ->
                ProtectiveEquipmentPerson' Core.<$>
                  (x Core..:? "BodyParts") Core.<*> x Core..:? "BoundingBox" Core.<*>
                    x Core..:? "Confidence"
                    Core.<*> x Core..:? "Id"
