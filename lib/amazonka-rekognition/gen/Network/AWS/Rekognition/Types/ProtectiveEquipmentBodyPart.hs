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
    pebpEquipmentDetections,
    pebpConfidence,
    pebpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BodyPart
import Network.AWS.Rekognition.Types.EquipmentDetection

-- | Information about a body part detected by 'DetectProtectiveEquipment' that contains PPE. An array of @ProtectiveEquipmentBodyPart@ objects is returned for each person detected by @DetectProtectiveEquipment@ .
--
-- /See:/ 'mkProtectiveEquipmentBodyPart' smart constructor.
data ProtectiveEquipmentBodyPart = ProtectiveEquipmentBodyPart'
  { -- | An array of Personal Protective Equipment items detected around a body part.
    equipmentDetections :: Lude.Maybe [EquipmentDetection],
    -- | The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
    confidence :: Lude.Maybe Lude.Double,
    -- | The detected body part.
    name :: Lude.Maybe BodyPart
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectiveEquipmentBodyPart' with the minimum fields required to make a request.
--
-- * 'equipmentDetections' - An array of Personal Protective Equipment items detected around a body part.
-- * 'confidence' - The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
-- * 'name' - The detected body part.
mkProtectiveEquipmentBodyPart ::
  ProtectiveEquipmentBodyPart
mkProtectiveEquipmentBodyPart =
  ProtectiveEquipmentBodyPart'
    { equipmentDetections = Lude.Nothing,
      confidence = Lude.Nothing,
      name = Lude.Nothing
    }

-- | An array of Personal Protective Equipment items detected around a body part.
--
-- /Note:/ Consider using 'equipmentDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pebpEquipmentDetections :: Lens.Lens' ProtectiveEquipmentBodyPart (Lude.Maybe [EquipmentDetection])
pebpEquipmentDetections = Lens.lens (equipmentDetections :: ProtectiveEquipmentBodyPart -> Lude.Maybe [EquipmentDetection]) (\s a -> s {equipmentDetections = a} :: ProtectiveEquipmentBodyPart)
{-# DEPRECATED pebpEquipmentDetections "Use generic-lens or generic-optics with 'equipmentDetections' instead." #-}

-- | The confidence that Amazon Rekognition has in the detection accuracy of the detected body part.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pebpConfidence :: Lens.Lens' ProtectiveEquipmentBodyPart (Lude.Maybe Lude.Double)
pebpConfidence = Lens.lens (confidence :: ProtectiveEquipmentBodyPart -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: ProtectiveEquipmentBodyPart)
{-# DEPRECATED pebpConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The detected body part.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pebpName :: Lens.Lens' ProtectiveEquipmentBodyPart (Lude.Maybe BodyPart)
pebpName = Lens.lens (name :: ProtectiveEquipmentBodyPart -> Lude.Maybe BodyPart) (\s a -> s {name = a} :: ProtectiveEquipmentBodyPart)
{-# DEPRECATED pebpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ProtectiveEquipmentBodyPart where
  parseJSON =
    Lude.withObject
      "ProtectiveEquipmentBodyPart"
      ( \x ->
          ProtectiveEquipmentBodyPart'
            Lude.<$> (x Lude..:? "EquipmentDetections" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Name")
      )
