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
-- Module      : Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart

-- | A person detected by a call to DetectProtectiveEquipment. The API
-- returns all persons detected in the input image in an array of
-- @ProtectiveEquipmentPerson@ objects.
--
-- /See:/ 'newProtectiveEquipmentPerson' smart constructor.
data ProtectiveEquipmentPerson = ProtectiveEquipmentPerson'
  { -- | The identifier for the detected person. The identifier is only unique
    -- for a single call to @DetectProtectiveEquipment@.
    id :: Prelude.Maybe Prelude.Natural,
    -- | A bounding box around the detected person.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | An array of body parts detected on a person\'s body (including body
    -- parts without PPE).
    bodyParts :: Prelude.Maybe [ProtectiveEquipmentBodyPart],
    -- | The confidence that Amazon Rekognition has that the bounding box
    -- contains a person.
    confidence :: Prelude.Maybe Prelude.Double
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
-- 'id', 'protectiveEquipmentPerson_id' - The identifier for the detected person. The identifier is only unique
-- for a single call to @DetectProtectiveEquipment@.
--
-- 'boundingBox', 'protectiveEquipmentPerson_boundingBox' - A bounding box around the detected person.
--
-- 'bodyParts', 'protectiveEquipmentPerson_bodyParts' - An array of body parts detected on a person\'s body (including body
-- parts without PPE).
--
-- 'confidence', 'protectiveEquipmentPerson_confidence' - The confidence that Amazon Rekognition has that the bounding box
-- contains a person.
newProtectiveEquipmentPerson ::
  ProtectiveEquipmentPerson
newProtectiveEquipmentPerson =
  ProtectiveEquipmentPerson'
    { id = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      bodyParts = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | The identifier for the detected person. The identifier is only unique
-- for a single call to @DetectProtectiveEquipment@.
protectiveEquipmentPerson_id :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe Prelude.Natural)
protectiveEquipmentPerson_id = Lens.lens (\ProtectiveEquipmentPerson' {id} -> id) (\s@ProtectiveEquipmentPerson' {} a -> s {id = a} :: ProtectiveEquipmentPerson)

-- | A bounding box around the detected person.
protectiveEquipmentPerson_boundingBox :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe BoundingBox)
protectiveEquipmentPerson_boundingBox = Lens.lens (\ProtectiveEquipmentPerson' {boundingBox} -> boundingBox) (\s@ProtectiveEquipmentPerson' {} a -> s {boundingBox = a} :: ProtectiveEquipmentPerson)

-- | An array of body parts detected on a person\'s body (including body
-- parts without PPE).
protectiveEquipmentPerson_bodyParts :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe [ProtectiveEquipmentBodyPart])
protectiveEquipmentPerson_bodyParts = Lens.lens (\ProtectiveEquipmentPerson' {bodyParts} -> bodyParts) (\s@ProtectiveEquipmentPerson' {} a -> s {bodyParts = a} :: ProtectiveEquipmentPerson) Prelude.. Lens.mapping Lens._Coerce

-- | The confidence that Amazon Rekognition has that the bounding box
-- contains a person.
protectiveEquipmentPerson_confidence :: Lens.Lens' ProtectiveEquipmentPerson (Prelude.Maybe Prelude.Double)
protectiveEquipmentPerson_confidence = Lens.lens (\ProtectiveEquipmentPerson' {confidence} -> confidence) (\s@ProtectiveEquipmentPerson' {} a -> s {confidence = a} :: ProtectiveEquipmentPerson)

instance Core.FromJSON ProtectiveEquipmentPerson where
  parseJSON =
    Core.withObject
      "ProtectiveEquipmentPerson"
      ( \x ->
          ProtectiveEquipmentPerson'
            Prelude.<$> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "BoundingBox")
            Prelude.<*> (x Core..:? "BodyParts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable ProtectiveEquipmentPerson

instance Prelude.NFData ProtectiveEquipmentPerson
