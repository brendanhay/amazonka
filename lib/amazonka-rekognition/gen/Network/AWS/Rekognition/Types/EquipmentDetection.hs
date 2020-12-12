{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EquipmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EquipmentDetection
  ( EquipmentDetection (..),

    -- * Smart constructor
    mkEquipmentDetection,

    -- * Lenses
    edBoundingBox,
    edCoversBodyPart,
    edConfidence,
    edType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.CoversBodyPart
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType

-- | Information about an item of Personal Protective Equipment (PPE) detected by 'DetectProtectiveEquipment' . For more information, see 'DetectProtectiveEquipment' .
--
-- /See:/ 'mkEquipmentDetection' smart constructor.
data EquipmentDetection = EquipmentDetection'
  { boundingBox ::
      Lude.Maybe BoundingBox,
    coversBodyPart :: Lude.Maybe CoversBodyPart,
    confidence :: Lude.Maybe Lude.Double,
    type' :: Lude.Maybe ProtectiveEquipmentType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EquipmentDetection' with the minimum fields required to make a request.
--
-- * 'boundingBox' - A bounding box surrounding the item of detected PPE.
-- * 'confidence' - The confidence that Amazon Rekognition has that the bounding box (@BoundingBox@ ) contains an item of PPE.
-- * 'coversBodyPart' - Information about the body part covered by the detected PPE.
-- * 'type'' - The type of detected PPE.
mkEquipmentDetection ::
  EquipmentDetection
mkEquipmentDetection =
  EquipmentDetection'
    { boundingBox = Lude.Nothing,
      coversBodyPart = Lude.Nothing,
      confidence = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A bounding box surrounding the item of detected PPE.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edBoundingBox :: Lens.Lens' EquipmentDetection (Lude.Maybe BoundingBox)
edBoundingBox = Lens.lens (boundingBox :: EquipmentDetection -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: EquipmentDetection)
{-# DEPRECATED edBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Information about the body part covered by the detected PPE.
--
-- /Note:/ Consider using 'coversBodyPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCoversBodyPart :: Lens.Lens' EquipmentDetection (Lude.Maybe CoversBodyPart)
edCoversBodyPart = Lens.lens (coversBodyPart :: EquipmentDetection -> Lude.Maybe CoversBodyPart) (\s a -> s {coversBodyPart = a} :: EquipmentDetection)
{-# DEPRECATED edCoversBodyPart "Use generic-lens or generic-optics with 'coversBodyPart' instead." #-}

-- | The confidence that Amazon Rekognition has that the bounding box (@BoundingBox@ ) contains an item of PPE.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edConfidence :: Lens.Lens' EquipmentDetection (Lude.Maybe Lude.Double)
edConfidence = Lens.lens (confidence :: EquipmentDetection -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: EquipmentDetection)
{-# DEPRECATED edConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The type of detected PPE.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edType :: Lens.Lens' EquipmentDetection (Lude.Maybe ProtectiveEquipmentType)
edType = Lens.lens (type' :: EquipmentDetection -> Lude.Maybe ProtectiveEquipmentType) (\s a -> s {type' = a} :: EquipmentDetection)
{-# DEPRECATED edType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON EquipmentDetection where
  parseJSON =
    Lude.withObject
      "EquipmentDetection"
      ( \x ->
          EquipmentDetection'
            Lude.<$> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "CoversBodyPart")
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Type")
      )
