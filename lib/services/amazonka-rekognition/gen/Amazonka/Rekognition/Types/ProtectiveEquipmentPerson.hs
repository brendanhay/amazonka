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
-- Module      : Amazonka.Rekognition.Types.ProtectiveEquipmentPerson
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProtectiveEquipmentPerson where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart

-- | A person detected by a call to DetectProtectiveEquipment. The API
-- returns all persons detected in the input image in an array of
-- @ProtectiveEquipmentPerson@ objects.
--
-- /See:/ 'newProtectiveEquipmentPerson' smart constructor.
data ProtectiveEquipmentPerson = ProtectiveEquipmentPerson'
  { -- | The confidence that Amazon Rekognition has that the bounding box
    -- contains a person.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | An array of body parts detected on a person\'s body (including body
    -- parts without PPE).
    bodyParts :: Prelude.Maybe [ProtectiveEquipmentBodyPart],
    -- | The identifier for the detected person. The identifier is only unique
    -- for a single call to @DetectProtectiveEquipment@.
    id :: Prelude.Maybe Prelude.Natural,
    -- | A bounding box around the detected person.
    boundingBox :: Prelude.Maybe BoundingBox
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectiveEquipmentPerson' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'protectiveEquipmentPerson_confidence' - The confidence that Amazon Rekognition has that the bounding box
-- contains a person.
--
-- 'bodyParts', 'protectiveEquipmentPerson_bodyParts' - An array of body parts detected on a person\'s body (including body
-- parts without PPE).
--
-- 'id', 'protectiveEquipmentPerson_id' - The identifier for the detected person. The identifier is only unique
-- for a single call to @DetectProtectiveEquipment@.
--
-- 'boundingBox', 'protectiveEquipmentPerson_boundingBox' - A bounding box around the detected person.
newProtectiveEquipmentPerson ::
  ProtectiveEquipmentPerson
newProtectiveEquipmentPerson =
  ProtectiveEquipmentPerson'
    { confidence =
        Prelude.Nothing,
      bodyParts = Prelude.Nothing,
      id = Prelude.Nothing,
      boundingBox = Prelude.Nothing
    }

-- | The confidence that Amazon Rekognition has that the bounding box
-- contains a person.
protectiveEquipmentPerson_confidence :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe Prelude.Double)
protectiveEquipmentPerson_confidence = Lens.lens (\ProtectiveEquipmentPerson' {confidence} -> confidence) (\s@ProtectiveEquipmentPerson' {} a -> s {confidence = a} :: ProtectiveEquipmentPerson)

-- | An array of body parts detected on a person\'s body (including body
-- parts without PPE).
protectiveEquipmentPerson_bodyParts :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe [ProtectiveEquipmentBodyPart])
protectiveEquipmentPerson_bodyParts = Lens.lens (\ProtectiveEquipmentPerson' {bodyParts} -> bodyParts) (\s@ProtectiveEquipmentPerson' {} a -> s {bodyParts = a} :: ProtectiveEquipmentPerson) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the detected person. The identifier is only unique
-- for a single call to @DetectProtectiveEquipment@.
protectiveEquipmentPerson_id :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe Prelude.Natural)
protectiveEquipmentPerson_id = Lens.lens (\ProtectiveEquipmentPerson' {id} -> id) (\s@ProtectiveEquipmentPerson' {} a -> s {id = a} :: ProtectiveEquipmentPerson)

-- | A bounding box around the detected person.
protectiveEquipmentPerson_boundingBox :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe BoundingBox)
protectiveEquipmentPerson_boundingBox = Lens.lens (\ProtectiveEquipmentPerson' {boundingBox} -> boundingBox) (\s@ProtectiveEquipmentPerson' {} a -> s {boundingBox = a} :: ProtectiveEquipmentPerson)

instance Core.FromJSON ProtectiveEquipmentPerson where
  parseJSON =
    Core.withObject
      "ProtectiveEquipmentPerson"
      ( \x ->
          ProtectiveEquipmentPerson'
            Prelude.<$> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "BodyParts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "BoundingBox")
      )

instance Prelude.Hashable ProtectiveEquipmentPerson where
  hashWithSalt _salt ProtectiveEquipmentPerson' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` bodyParts
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` boundingBox

instance Prelude.NFData ProtectiveEquipmentPerson where
  rnf ProtectiveEquipmentPerson' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf bodyParts
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf boundingBox
