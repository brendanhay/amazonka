{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart
  ( ProtectiveEquipmentBodyPart (..),

    -- * Smart constructor
    mkProtectiveEquipmentBodyPart,

    -- * Lenses
    pebpConfidence,
    pebpEquipmentDetections,
    pebpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BodyPart as Types
import qualified Network.AWS.Rekognition.Types.EquipmentDetection as Types

-- | Information about a body part detected by 'DetectProtectiveEquipment' that contains PPE. An array of @ProtectiveEquipmentBodyPart@ objects is returned for each person detected by @DetectProtectiveEquipment@ .
--
-- /See:/ 'mkProtectiveEquipmentBodyPart' smart constructor.
data ProtectiveEquipmentBodyPart = ProtectiveEquipmentBodyPart'
  { -- | The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
    confidence :: Core.Maybe Core.Double,
    -- | An array of Personal Protective Equipment items detected around a body part.
    equipmentDetections :: Core.Maybe [Types.EquipmentDetection],
    -- | The detected body part.
    name :: Core.Maybe Types.BodyPart
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectiveEquipmentBodyPart' value with any optional fields omitted.
mkProtectiveEquipmentBodyPart ::
  ProtectiveEquipmentBodyPart
mkProtectiveEquipmentBodyPart =
  ProtectiveEquipmentBodyPart'
    { confidence = Core.Nothing,
      equipmentDetections = Core.Nothing,
      name = Core.Nothing
    }

-- | The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pebpConfidence :: Lens.Lens' ProtectiveEquipmentBodyPart (Core.Maybe Core.Double)
pebpConfidence = Lens.field @"confidence"
{-# DEPRECATED pebpConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | An array of Personal Protective Equipment items detected around a body part.
--
-- /Note:/ Consider using 'equipmentDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pebpEquipmentDetections :: Lens.Lens' ProtectiveEquipmentBodyPart (Core.Maybe [Types.EquipmentDetection])
pebpEquipmentDetections = Lens.field @"equipmentDetections"
{-# DEPRECATED pebpEquipmentDetections "Use generic-lens or generic-optics with 'equipmentDetections' instead." #-}

-- | The detected body part.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pebpName :: Lens.Lens' ProtectiveEquipmentBodyPart (Core.Maybe Types.BodyPart)
pebpName = Lens.field @"name"
{-# DEPRECATED pebpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ProtectiveEquipmentBodyPart where
  parseJSON =
    Core.withObject "ProtectiveEquipmentBodyPart" Core.$
      \x ->
        ProtectiveEquipmentBodyPart'
          Core.<$> (x Core..:? "Confidence")
          Core.<*> (x Core..:? "EquipmentDetections")
          Core.<*> (x Core..:? "Name")
