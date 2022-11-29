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
-- Module      : Amazonka.Rekognition.Types.EquipmentDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.EquipmentDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.CoversBodyPart
import Amazonka.Rekognition.Types.ProtectiveEquipmentType

-- | Information about an item of Personal Protective Equipment (PPE)
-- detected by DetectProtectiveEquipment. For more information, see
-- DetectProtectiveEquipment.
--
-- /See:/ 'newEquipmentDetection' smart constructor.
data EquipmentDetection = EquipmentDetection'
  { -- | The type of detected PPE.
    type' :: Prelude.Maybe ProtectiveEquipmentType,
    -- | The confidence that Amazon Rekognition has that the bounding box
    -- (@BoundingBox@) contains an item of PPE.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | A bounding box surrounding the item of detected PPE.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Information about the body part covered by the detected PPE.
    coversBodyPart :: Prelude.Maybe CoversBodyPart
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EquipmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'equipmentDetection_type' - The type of detected PPE.
--
-- 'confidence', 'equipmentDetection_confidence' - The confidence that Amazon Rekognition has that the bounding box
-- (@BoundingBox@) contains an item of PPE.
--
-- 'boundingBox', 'equipmentDetection_boundingBox' - A bounding box surrounding the item of detected PPE.
--
-- 'coversBodyPart', 'equipmentDetection_coversBodyPart' - Information about the body part covered by the detected PPE.
newEquipmentDetection ::
  EquipmentDetection
newEquipmentDetection =
  EquipmentDetection'
    { type' = Prelude.Nothing,
      confidence = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      coversBodyPart = Prelude.Nothing
    }

-- | The type of detected PPE.
equipmentDetection_type :: Lens.Lens' EquipmentDetection (Prelude.Maybe ProtectiveEquipmentType)
equipmentDetection_type = Lens.lens (\EquipmentDetection' {type'} -> type') (\s@EquipmentDetection' {} a -> s {type' = a} :: EquipmentDetection)

-- | The confidence that Amazon Rekognition has that the bounding box
-- (@BoundingBox@) contains an item of PPE.
equipmentDetection_confidence :: Lens.Lens' EquipmentDetection (Prelude.Maybe Prelude.Double)
equipmentDetection_confidence = Lens.lens (\EquipmentDetection' {confidence} -> confidence) (\s@EquipmentDetection' {} a -> s {confidence = a} :: EquipmentDetection)

-- | A bounding box surrounding the item of detected PPE.
equipmentDetection_boundingBox :: Lens.Lens' EquipmentDetection (Prelude.Maybe BoundingBox)
equipmentDetection_boundingBox = Lens.lens (\EquipmentDetection' {boundingBox} -> boundingBox) (\s@EquipmentDetection' {} a -> s {boundingBox = a} :: EquipmentDetection)

-- | Information about the body part covered by the detected PPE.
equipmentDetection_coversBodyPart :: Lens.Lens' EquipmentDetection (Prelude.Maybe CoversBodyPart)
equipmentDetection_coversBodyPart = Lens.lens (\EquipmentDetection' {coversBodyPart} -> coversBodyPart) (\s@EquipmentDetection' {} a -> s {coversBodyPart = a} :: EquipmentDetection)

instance Core.FromJSON EquipmentDetection where
  parseJSON =
    Core.withObject
      "EquipmentDetection"
      ( \x ->
          EquipmentDetection'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "BoundingBox")
            Prelude.<*> (x Core..:? "CoversBodyPart")
      )

instance Prelude.Hashable EquipmentDetection where
  hashWithSalt _salt EquipmentDetection' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` coversBodyPart

instance Prelude.NFData EquipmentDetection where
  rnf EquipmentDetection' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf coversBodyPart
