{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
  ( ProtectiveEquipmentPerson (..),

    -- * Smart constructor
    mkProtectiveEquipmentPerson,

    -- * Lenses
    pepBodyParts,
    pepBoundingBox,
    pepConfidence,
    pepId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart

-- | A person detected by a call to 'DetectProtectiveEquipment' . The API returns all persons detected in the input image in an array of @ProtectiveEquipmentPerson@ objects.
--
-- /See:/ 'mkProtectiveEquipmentPerson' smart constructor.
data ProtectiveEquipmentPerson = ProtectiveEquipmentPerson'
  { -- | An array of body parts detected on a person's body (including body parts without PPE).
    bodyParts :: Lude.Maybe [ProtectiveEquipmentBodyPart],
    -- | A bounding box around the detected person.
    boundingBox :: Lude.Maybe BoundingBox,
    -- | The confidence that Amazon Rekognition has that the bounding box contains a person.
    confidence :: Lude.Maybe Lude.Double,
    -- | The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
    id :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectiveEquipmentPerson' with the minimum fields required to make a request.
--
-- * 'bodyParts' - An array of body parts detected on a person's body (including body parts without PPE).
-- * 'boundingBox' - A bounding box around the detected person.
-- * 'confidence' - The confidence that Amazon Rekognition has that the bounding box contains a person.
-- * 'id' - The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
mkProtectiveEquipmentPerson ::
  ProtectiveEquipmentPerson
mkProtectiveEquipmentPerson =
  ProtectiveEquipmentPerson'
    { bodyParts = Lude.Nothing,
      boundingBox = Lude.Nothing,
      confidence = Lude.Nothing,
      id = Lude.Nothing
    }

-- | An array of body parts detected on a person's body (including body parts without PPE).
--
-- /Note:/ Consider using 'bodyParts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepBodyParts :: Lens.Lens' ProtectiveEquipmentPerson (Lude.Maybe [ProtectiveEquipmentBodyPart])
pepBodyParts = Lens.lens (bodyParts :: ProtectiveEquipmentPerson -> Lude.Maybe [ProtectiveEquipmentBodyPart]) (\s a -> s {bodyParts = a} :: ProtectiveEquipmentPerson)
{-# DEPRECATED pepBodyParts "Use generic-lens or generic-optics with 'bodyParts' instead." #-}

-- | A bounding box around the detected person.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepBoundingBox :: Lens.Lens' ProtectiveEquipmentPerson (Lude.Maybe BoundingBox)
pepBoundingBox = Lens.lens (boundingBox :: ProtectiveEquipmentPerson -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: ProtectiveEquipmentPerson)
{-# DEPRECATED pepBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | The confidence that Amazon Rekognition has that the bounding box contains a person.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepConfidence :: Lens.Lens' ProtectiveEquipmentPerson (Lude.Maybe Lude.Double)
pepConfidence = Lens.lens (confidence :: ProtectiveEquipmentPerson -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: ProtectiveEquipmentPerson)
{-# DEPRECATED pepConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The identifier for the detected person. The identifier is only unique for a single call to @DetectProtectiveEquipment@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pepId :: Lens.Lens' ProtectiveEquipmentPerson (Lude.Maybe Lude.Natural)
pepId = Lens.lens (id :: ProtectiveEquipmentPerson -> Lude.Maybe Lude.Natural) (\s a -> s {id = a} :: ProtectiveEquipmentPerson)
{-# DEPRECATED pepId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ProtectiveEquipmentPerson where
  parseJSON =
    Lude.withObject
      "ProtectiveEquipmentPerson"
      ( \x ->
          ProtectiveEquipmentPerson'
            Lude.<$> (x Lude..:? "BodyParts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Id")
      )
