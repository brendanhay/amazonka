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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProtectiveEquipmentPerson where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart

-- | A person detected by a call to DetectProtectiveEquipment. The API
-- returns all persons detected in the input image in an array of
-- @ProtectiveEquipmentPerson@ objects.
--
-- /See:/ 'newProtectiveEquipmentPerson' smart constructor.
data ProtectiveEquipmentPerson = ProtectiveEquipmentPerson'
  { -- | An array of body parts detected on a person\'s body (including body
    -- parts without PPE).
    bodyParts :: Prelude.Maybe [ProtectiveEquipmentBodyPart],
    -- | A bounding box around the detected person.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | The confidence that Amazon Rekognition has that the bounding box
    -- contains a person.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The identifier for the detected person. The identifier is only unique
    -- for a single call to @DetectProtectiveEquipment@.
    id :: Prelude.Maybe Prelude.Natural
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
-- 'bodyParts', 'protectiveEquipmentPerson_bodyParts' - An array of body parts detected on a person\'s body (including body
-- parts without PPE).
--
-- 'boundingBox', 'protectiveEquipmentPerson_boundingBox' - A bounding box around the detected person.
--
-- 'confidence', 'protectiveEquipmentPerson_confidence' - The confidence that Amazon Rekognition has that the bounding box
-- contains a person.
--
-- 'id', 'protectiveEquipmentPerson_id' - The identifier for the detected person. The identifier is only unique
-- for a single call to @DetectProtectiveEquipment@.
newProtectiveEquipmentPerson ::
  ProtectiveEquipmentPerson
newProtectiveEquipmentPerson =
  ProtectiveEquipmentPerson'
    { bodyParts =
        Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      confidence = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | An array of body parts detected on a person\'s body (including body
-- parts without PPE).
protectiveEquipmentPerson_bodyParts :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe [ProtectiveEquipmentBodyPart])
protectiveEquipmentPerson_bodyParts = Lens.lens (\ProtectiveEquipmentPerson' {bodyParts} -> bodyParts) (\s@ProtectiveEquipmentPerson' {} a -> s {bodyParts = a} :: ProtectiveEquipmentPerson) Prelude.. Lens.mapping Lens.coerced

-- | A bounding box around the detected person.
protectiveEquipmentPerson_boundingBox :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe BoundingBox)
protectiveEquipmentPerson_boundingBox = Lens.lens (\ProtectiveEquipmentPerson' {boundingBox} -> boundingBox) (\s@ProtectiveEquipmentPerson' {} a -> s {boundingBox = a} :: ProtectiveEquipmentPerson)

-- | The confidence that Amazon Rekognition has that the bounding box
-- contains a person.
protectiveEquipmentPerson_confidence :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe Prelude.Double)
protectiveEquipmentPerson_confidence = Lens.lens (\ProtectiveEquipmentPerson' {confidence} -> confidence) (\s@ProtectiveEquipmentPerson' {} a -> s {confidence = a} :: ProtectiveEquipmentPerson)

-- | The identifier for the detected person. The identifier is only unique
-- for a single call to @DetectProtectiveEquipment@.
protectiveEquipmentPerson_id :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe Prelude.Natural)
protectiveEquipmentPerson_id = Lens.lens (\ProtectiveEquipmentPerson' {id} -> id) (\s@ProtectiveEquipmentPerson' {} a -> s {id = a} :: ProtectiveEquipmentPerson)

instance Data.FromJSON ProtectiveEquipmentPerson where
  parseJSON =
    Data.withObject
      "ProtectiveEquipmentPerson"
      ( \x ->
          ProtectiveEquipmentPerson'
            Prelude.<$> (x Data..:? "BodyParts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BoundingBox")
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable ProtectiveEquipmentPerson where
  hashWithSalt _salt ProtectiveEquipmentPerson' {..} =
    _salt
      `Prelude.hashWithSalt` bodyParts
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` id

instance Prelude.NFData ProtectiveEquipmentPerson where
  rnf ProtectiveEquipmentPerson' {..} =
    Prelude.rnf bodyParts `Prelude.seq`
      Prelude.rnf boundingBox `Prelude.seq`
        Prelude.rnf confidence `Prelude.seq`
          Prelude.rnf id
