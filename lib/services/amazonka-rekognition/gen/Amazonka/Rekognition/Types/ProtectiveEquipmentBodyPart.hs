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
-- Module      : Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BodyPart
import Amazonka.Rekognition.Types.EquipmentDetection

-- | Information about a body part detected by DetectProtectiveEquipment that
-- contains PPE. An array of @ProtectiveEquipmentBodyPart@ objects is
-- returned for each person detected by @DetectProtectiveEquipment@.
--
-- /See:/ 'newProtectiveEquipmentBodyPart' smart constructor.
data ProtectiveEquipmentBodyPart = ProtectiveEquipmentBodyPart'
  { -- | The detected body part.
    name :: Prelude.Maybe BodyPart,
    -- | The confidence that Amazon Rekognition has in the detection accuracy of
    -- the detected body part.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | An array of Personal Protective Equipment items detected around a body
    -- part.
    equipmentDetections :: Prelude.Maybe [EquipmentDetection]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectiveEquipmentBodyPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'protectiveEquipmentBodyPart_name' - The detected body part.
--
-- 'confidence', 'protectiveEquipmentBodyPart_confidence' - The confidence that Amazon Rekognition has in the detection accuracy of
-- the detected body part.
--
-- 'equipmentDetections', 'protectiveEquipmentBodyPart_equipmentDetections' - An array of Personal Protective Equipment items detected around a body
-- part.
newProtectiveEquipmentBodyPart ::
  ProtectiveEquipmentBodyPart
newProtectiveEquipmentBodyPart =
  ProtectiveEquipmentBodyPart'
    { name =
        Prelude.Nothing,
      confidence = Prelude.Nothing,
      equipmentDetections = Prelude.Nothing
    }

-- | The detected body part.
protectiveEquipmentBodyPart_name :: Lens.Lens' ProtectiveEquipmentBodyPart (Prelude.Maybe BodyPart)
protectiveEquipmentBodyPart_name = Lens.lens (\ProtectiveEquipmentBodyPart' {name} -> name) (\s@ProtectiveEquipmentBodyPart' {} a -> s {name = a} :: ProtectiveEquipmentBodyPart)

-- | The confidence that Amazon Rekognition has in the detection accuracy of
-- the detected body part.
protectiveEquipmentBodyPart_confidence :: Lens.Lens' ProtectiveEquipmentBodyPart (Prelude.Maybe Prelude.Double)
protectiveEquipmentBodyPart_confidence = Lens.lens (\ProtectiveEquipmentBodyPart' {confidence} -> confidence) (\s@ProtectiveEquipmentBodyPart' {} a -> s {confidence = a} :: ProtectiveEquipmentBodyPart)

-- | An array of Personal Protective Equipment items detected around a body
-- part.
protectiveEquipmentBodyPart_equipmentDetections :: Lens.Lens' ProtectiveEquipmentBodyPart (Prelude.Maybe [EquipmentDetection])
protectiveEquipmentBodyPart_equipmentDetections = Lens.lens (\ProtectiveEquipmentBodyPart' {equipmentDetections} -> equipmentDetections) (\s@ProtectiveEquipmentBodyPart' {} a -> s {equipmentDetections = a} :: ProtectiveEquipmentBodyPart) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProtectiveEquipmentBodyPart where
  parseJSON =
    Data.withObject
      "ProtectiveEquipmentBodyPart"
      ( \x ->
          ProtectiveEquipmentBodyPart'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> ( x Data..:? "EquipmentDetections"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProtectiveEquipmentBodyPart where
  hashWithSalt _salt ProtectiveEquipmentBodyPart' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` equipmentDetections

instance Prelude.NFData ProtectiveEquipmentBodyPart where
  rnf ProtectiveEquipmentBodyPart' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf equipmentDetections
