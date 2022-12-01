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
-- Module      : Amazonka.LexV2Models.Types.SubSlotSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SubSlotSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.Specifications
import qualified Amazonka.Prelude as Prelude

-- | Specifications for the constituent sub slots and the expression for the
-- composite slot.
--
-- /See:/ 'newSubSlotSetting' smart constructor.
data SubSlotSetting = SubSlotSetting'
  { -- | The expression text for defining the constituent sub slots in the
    -- composite slot using logical AND and OR operators.
    expression :: Prelude.Maybe Prelude.Text,
    -- | Specifications for the constituent sub slots of a composite slot.
    slotSpecifications :: Prelude.Maybe (Prelude.HashMap Prelude.Text Specifications)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubSlotSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'subSlotSetting_expression' - The expression text for defining the constituent sub slots in the
-- composite slot using logical AND and OR operators.
--
-- 'slotSpecifications', 'subSlotSetting_slotSpecifications' - Specifications for the constituent sub slots of a composite slot.
newSubSlotSetting ::
  SubSlotSetting
newSubSlotSetting =
  SubSlotSetting'
    { expression = Prelude.Nothing,
      slotSpecifications = Prelude.Nothing
    }

-- | The expression text for defining the constituent sub slots in the
-- composite slot using logical AND and OR operators.
subSlotSetting_expression :: Lens.Lens' SubSlotSetting (Prelude.Maybe Prelude.Text)
subSlotSetting_expression = Lens.lens (\SubSlotSetting' {expression} -> expression) (\s@SubSlotSetting' {} a -> s {expression = a} :: SubSlotSetting)

-- | Specifications for the constituent sub slots of a composite slot.
subSlotSetting_slotSpecifications :: Lens.Lens' SubSlotSetting (Prelude.Maybe (Prelude.HashMap Prelude.Text Specifications))
subSlotSetting_slotSpecifications = Lens.lens (\SubSlotSetting' {slotSpecifications} -> slotSpecifications) (\s@SubSlotSetting' {} a -> s {slotSpecifications = a} :: SubSlotSetting) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SubSlotSetting where
  parseJSON =
    Core.withObject
      "SubSlotSetting"
      ( \x ->
          SubSlotSetting'
            Prelude.<$> (x Core..:? "expression")
            Prelude.<*> ( x Core..:? "slotSpecifications"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SubSlotSetting where
  hashWithSalt _salt SubSlotSetting' {..} =
    _salt `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` slotSpecifications

instance Prelude.NFData SubSlotSetting where
  rnf SubSlotSetting' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf slotSpecifications

instance Core.ToJSON SubSlotSetting where
  toJSON SubSlotSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("expression" Core..=) Prelude.<$> expression,
            ("slotSpecifications" Core..=)
              Prelude.<$> slotSpecifications
          ]
      )
