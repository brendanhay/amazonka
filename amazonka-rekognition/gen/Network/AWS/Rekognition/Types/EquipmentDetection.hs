{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EquipmentDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EquipmentDetection where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.CoversBodyPart
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType

-- | Information about an item of Personal Protective Equipment (PPE)
-- detected by DetectProtectiveEquipment. For more information, see
-- DetectProtectiveEquipment.
--
-- /See:/ 'newEquipmentDetection' smart constructor.
data EquipmentDetection = EquipmentDetection'
  { -- | Information about the body part covered by the detected PPE.
    coversBodyPart :: Prelude.Maybe CoversBodyPart,
    -- | A bounding box surrounding the item of detected PPE.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | The confidence that Amazon Rekognition has that the bounding box
    -- (@BoundingBox@) contains an item of PPE.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The type of detected PPE.
    type' :: Prelude.Maybe ProtectiveEquipmentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EquipmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coversBodyPart', 'equipmentDetection_coversBodyPart' - Information about the body part covered by the detected PPE.
--
-- 'boundingBox', 'equipmentDetection_boundingBox' - A bounding box surrounding the item of detected PPE.
--
-- 'confidence', 'equipmentDetection_confidence' - The confidence that Amazon Rekognition has that the bounding box
-- (@BoundingBox@) contains an item of PPE.
--
-- 'type'', 'equipmentDetection_type' - The type of detected PPE.
newEquipmentDetection ::
  EquipmentDetection
newEquipmentDetection =
  EquipmentDetection'
    { coversBodyPart =
        Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      confidence = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Information about the body part covered by the detected PPE.
equipmentDetection_coversBodyPart :: Lens.Lens' EquipmentDetection (Prelude.Maybe CoversBodyPart)
equipmentDetection_coversBodyPart = Lens.lens (\EquipmentDetection' {coversBodyPart} -> coversBodyPart) (\s@EquipmentDetection' {} a -> s {coversBodyPart = a} :: EquipmentDetection)

-- | A bounding box surrounding the item of detected PPE.
equipmentDetection_boundingBox :: Lens.Lens' EquipmentDetection (Prelude.Maybe BoundingBox)
equipmentDetection_boundingBox = Lens.lens (\EquipmentDetection' {boundingBox} -> boundingBox) (\s@EquipmentDetection' {} a -> s {boundingBox = a} :: EquipmentDetection)

-- | The confidence that Amazon Rekognition has that the bounding box
-- (@BoundingBox@) contains an item of PPE.
equipmentDetection_confidence :: Lens.Lens' EquipmentDetection (Prelude.Maybe Prelude.Double)
equipmentDetection_confidence = Lens.lens (\EquipmentDetection' {confidence} -> confidence) (\s@EquipmentDetection' {} a -> s {confidence = a} :: EquipmentDetection)

-- | The type of detected PPE.
equipmentDetection_type :: Lens.Lens' EquipmentDetection (Prelude.Maybe ProtectiveEquipmentType)
equipmentDetection_type = Lens.lens (\EquipmentDetection' {type'} -> type') (\s@EquipmentDetection' {} a -> s {type' = a} :: EquipmentDetection)

instance Prelude.FromJSON EquipmentDetection where
  parseJSON =
    Prelude.withObject
      "EquipmentDetection"
      ( \x ->
          EquipmentDetection'
            Prelude.<$> (x Prelude..:? "CoversBodyPart")
            Prelude.<*> (x Prelude..:? "BoundingBox")
            Prelude.<*> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable EquipmentDetection

instance Prelude.NFData EquipmentDetection
