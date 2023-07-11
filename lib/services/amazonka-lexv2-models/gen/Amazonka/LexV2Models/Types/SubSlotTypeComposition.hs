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
-- Module      : Amazonka.LexV2Models.Types.SubSlotTypeComposition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SubSlotTypeComposition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Subslot type composition.
--
-- /See:/ 'newSubSlotTypeComposition' smart constructor.
data SubSlotTypeComposition = SubSlotTypeComposition'
  { -- | Name of a constituent sub slot inside a composite slot.
    name :: Prelude.Text,
    -- | The unique identifier assigned to a slot type. This refers to either a
    -- built-in slot type or the unique slotTypeId of a custom slot type.
    slotTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubSlotTypeComposition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'subSlotTypeComposition_name' - Name of a constituent sub slot inside a composite slot.
--
-- 'slotTypeId', 'subSlotTypeComposition_slotTypeId' - The unique identifier assigned to a slot type. This refers to either a
-- built-in slot type or the unique slotTypeId of a custom slot type.
newSubSlotTypeComposition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'slotTypeId'
  Prelude.Text ->
  SubSlotTypeComposition
newSubSlotTypeComposition pName_ pSlotTypeId_ =
  SubSlotTypeComposition'
    { name = pName_,
      slotTypeId = pSlotTypeId_
    }

-- | Name of a constituent sub slot inside a composite slot.
subSlotTypeComposition_name :: Lens.Lens' SubSlotTypeComposition Prelude.Text
subSlotTypeComposition_name = Lens.lens (\SubSlotTypeComposition' {name} -> name) (\s@SubSlotTypeComposition' {} a -> s {name = a} :: SubSlotTypeComposition)

-- | The unique identifier assigned to a slot type. This refers to either a
-- built-in slot type or the unique slotTypeId of a custom slot type.
subSlotTypeComposition_slotTypeId :: Lens.Lens' SubSlotTypeComposition Prelude.Text
subSlotTypeComposition_slotTypeId = Lens.lens (\SubSlotTypeComposition' {slotTypeId} -> slotTypeId) (\s@SubSlotTypeComposition' {} a -> s {slotTypeId = a} :: SubSlotTypeComposition)

instance Data.FromJSON SubSlotTypeComposition where
  parseJSON =
    Data.withObject
      "SubSlotTypeComposition"
      ( \x ->
          SubSlotTypeComposition'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "slotTypeId")
      )

instance Prelude.Hashable SubSlotTypeComposition where
  hashWithSalt _salt SubSlotTypeComposition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` slotTypeId

instance Prelude.NFData SubSlotTypeComposition where
  rnf SubSlotTypeComposition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf slotTypeId

instance Data.ToJSON SubSlotTypeComposition where
  toJSON SubSlotTypeComposition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("slotTypeId" Data..= slotTypeId)
          ]
      )
