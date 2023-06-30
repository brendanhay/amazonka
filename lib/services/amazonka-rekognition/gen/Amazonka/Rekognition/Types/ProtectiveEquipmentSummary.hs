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
-- Module      : Amazonka.Rekognition.Types.ProtectiveEquipmentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProtectiveEquipmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information for required items of personal protective equipment
-- (PPE) detected on persons by a call to DetectProtectiveEquipment. You
-- specify the required type of PPE in the @SummarizationAttributes@
-- (ProtectiveEquipmentSummarizationAttributes) input parameter. The
-- summary includes which persons were detected wearing the required
-- personal protective equipment (@PersonsWithRequiredEquipment@), which
-- persons were detected as not wearing the required PPE
-- (@PersonsWithoutRequiredEquipment@), and the persons in which a
-- determination could not be made (@PersonsIndeterminate@).
--
-- To get a total for each category, use the size of the field array. For
-- example, to find out how many people were detected as wearing the
-- specified PPE, use the size of the @PersonsWithRequiredEquipment@ array.
-- If you want to find out more about a person, such as the location
-- (BoundingBox) of the person on the image, use the person ID in each
-- array element. Each person ID matches the ID field of a
-- ProtectiveEquipmentPerson object returned in the @Persons@ array by
-- @DetectProtectiveEquipment@.
--
-- /See:/ 'newProtectiveEquipmentSummary' smart constructor.
data ProtectiveEquipmentSummary = ProtectiveEquipmentSummary'
  { -- | An array of IDs for persons where it was not possible to determine if
    -- they are wearing personal protective equipment.
    personsIndeterminate :: Prelude.Maybe [Prelude.Natural],
    -- | An array of IDs for persons who are wearing detected personal protective
    -- equipment.
    personsWithRequiredEquipment :: Prelude.Maybe [Prelude.Natural],
    -- | An array of IDs for persons who are not wearing all of the types of PPE
    -- specified in the @RequiredEquipmentTypes@ field of the detected personal
    -- protective equipment.
    personsWithoutRequiredEquipment :: Prelude.Maybe [Prelude.Natural]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectiveEquipmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'personsIndeterminate', 'protectiveEquipmentSummary_personsIndeterminate' - An array of IDs for persons where it was not possible to determine if
-- they are wearing personal protective equipment.
--
-- 'personsWithRequiredEquipment', 'protectiveEquipmentSummary_personsWithRequiredEquipment' - An array of IDs for persons who are wearing detected personal protective
-- equipment.
--
-- 'personsWithoutRequiredEquipment', 'protectiveEquipmentSummary_personsWithoutRequiredEquipment' - An array of IDs for persons who are not wearing all of the types of PPE
-- specified in the @RequiredEquipmentTypes@ field of the detected personal
-- protective equipment.
newProtectiveEquipmentSummary ::
  ProtectiveEquipmentSummary
newProtectiveEquipmentSummary =
  ProtectiveEquipmentSummary'
    { personsIndeterminate =
        Prelude.Nothing,
      personsWithRequiredEquipment = Prelude.Nothing,
      personsWithoutRequiredEquipment =
        Prelude.Nothing
    }

-- | An array of IDs for persons where it was not possible to determine if
-- they are wearing personal protective equipment.
protectiveEquipmentSummary_personsIndeterminate :: Lens.Lens' ProtectiveEquipmentSummary (Prelude.Maybe [Prelude.Natural])
protectiveEquipmentSummary_personsIndeterminate = Lens.lens (\ProtectiveEquipmentSummary' {personsIndeterminate} -> personsIndeterminate) (\s@ProtectiveEquipmentSummary' {} a -> s {personsIndeterminate = a} :: ProtectiveEquipmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | An array of IDs for persons who are wearing detected personal protective
-- equipment.
protectiveEquipmentSummary_personsWithRequiredEquipment :: Lens.Lens' ProtectiveEquipmentSummary (Prelude.Maybe [Prelude.Natural])
protectiveEquipmentSummary_personsWithRequiredEquipment = Lens.lens (\ProtectiveEquipmentSummary' {personsWithRequiredEquipment} -> personsWithRequiredEquipment) (\s@ProtectiveEquipmentSummary' {} a -> s {personsWithRequiredEquipment = a} :: ProtectiveEquipmentSummary) Prelude.. Lens.mapping Lens.coerced

-- | An array of IDs for persons who are not wearing all of the types of PPE
-- specified in the @RequiredEquipmentTypes@ field of the detected personal
-- protective equipment.
protectiveEquipmentSummary_personsWithoutRequiredEquipment :: Lens.Lens' ProtectiveEquipmentSummary (Prelude.Maybe [Prelude.Natural])
protectiveEquipmentSummary_personsWithoutRequiredEquipment = Lens.lens (\ProtectiveEquipmentSummary' {personsWithoutRequiredEquipment} -> personsWithoutRequiredEquipment) (\s@ProtectiveEquipmentSummary' {} a -> s {personsWithoutRequiredEquipment = a} :: ProtectiveEquipmentSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProtectiveEquipmentSummary where
  parseJSON =
    Data.withObject
      "ProtectiveEquipmentSummary"
      ( \x ->
          ProtectiveEquipmentSummary'
            Prelude.<$> ( x
                            Data..:? "PersonsIndeterminate"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PersonsWithRequiredEquipment"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PersonsWithoutRequiredEquipment"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProtectiveEquipmentSummary where
  hashWithSalt _salt ProtectiveEquipmentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` personsIndeterminate
      `Prelude.hashWithSalt` personsWithRequiredEquipment
      `Prelude.hashWithSalt` personsWithoutRequiredEquipment

instance Prelude.NFData ProtectiveEquipmentSummary where
  rnf ProtectiveEquipmentSummary' {..} =
    Prelude.rnf personsIndeterminate
      `Prelude.seq` Prelude.rnf personsWithRequiredEquipment
      `Prelude.seq` Prelude.rnf personsWithoutRequiredEquipment
