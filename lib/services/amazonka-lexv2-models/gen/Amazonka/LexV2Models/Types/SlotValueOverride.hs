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
-- Module      : Amazonka.LexV2Models.Types.SlotValueOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotValueOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.SlotShape
import Amazonka.LexV2Models.Types.SlotValue
import qualified Amazonka.Prelude as Prelude

-- | The slot values that Amazon Lex uses when it sets slot values in a
-- dialog step.
--
-- /See:/ 'newSlotValueOverride' smart constructor.
data SlotValueOverride = SlotValueOverride'
  { -- | When the shape value is @List@, it indicates that the @values@ field
    -- contains a list of slot values. When the value is @Scalar@, it indicates
    -- that the @value@ field contains a single value.
    shape :: Prelude.Maybe SlotShape,
    -- | A list of one or more values that the user provided for the slot. For
    -- example, for a slot that elicits pizza toppings, the values might be
    -- \"pepperoni\" and \"pineapple.\"
    values :: Prelude.Maybe [SlotValueOverride],
    -- | The current value of the slot.
    value :: Prelude.Maybe SlotValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotValueOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shape', 'slotValueOverride_shape' - When the shape value is @List@, it indicates that the @values@ field
-- contains a list of slot values. When the value is @Scalar@, it indicates
-- that the @value@ field contains a single value.
--
-- 'values', 'slotValueOverride_values' - A list of one or more values that the user provided for the slot. For
-- example, for a slot that elicits pizza toppings, the values might be
-- \"pepperoni\" and \"pineapple.\"
--
-- 'value', 'slotValueOverride_value' - The current value of the slot.
newSlotValueOverride ::
  SlotValueOverride
newSlotValueOverride =
  SlotValueOverride'
    { shape = Prelude.Nothing,
      values = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | When the shape value is @List@, it indicates that the @values@ field
-- contains a list of slot values. When the value is @Scalar@, it indicates
-- that the @value@ field contains a single value.
slotValueOverride_shape :: Lens.Lens' SlotValueOverride (Prelude.Maybe SlotShape)
slotValueOverride_shape = Lens.lens (\SlotValueOverride' {shape} -> shape) (\s@SlotValueOverride' {} a -> s {shape = a} :: SlotValueOverride)

-- | A list of one or more values that the user provided for the slot. For
-- example, for a slot that elicits pizza toppings, the values might be
-- \"pepperoni\" and \"pineapple.\"
slotValueOverride_values :: Lens.Lens' SlotValueOverride (Prelude.Maybe [SlotValueOverride])
slotValueOverride_values = Lens.lens (\SlotValueOverride' {values} -> values) (\s@SlotValueOverride' {} a -> s {values = a} :: SlotValueOverride) Prelude.. Lens.mapping Lens.coerced

-- | The current value of the slot.
slotValueOverride_value :: Lens.Lens' SlotValueOverride (Prelude.Maybe SlotValue)
slotValueOverride_value = Lens.lens (\SlotValueOverride' {value} -> value) (\s@SlotValueOverride' {} a -> s {value = a} :: SlotValueOverride)

instance Core.FromJSON SlotValueOverride where
  parseJSON =
    Core.withObject
      "SlotValueOverride"
      ( \x ->
          SlotValueOverride'
            Prelude.<$> (x Core..:? "shape")
            Prelude.<*> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable SlotValueOverride where
  hashWithSalt _salt SlotValueOverride' {..} =
    _salt `Prelude.hashWithSalt` shape
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` value

instance Prelude.NFData SlotValueOverride where
  rnf SlotValueOverride' {..} =
    Prelude.rnf shape
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON SlotValueOverride where
  toJSON SlotValueOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("shape" Core..=) Prelude.<$> shape,
            ("values" Core..=) Prelude.<$> values,
            ("value" Core..=) Prelude.<$> value
          ]
      )
