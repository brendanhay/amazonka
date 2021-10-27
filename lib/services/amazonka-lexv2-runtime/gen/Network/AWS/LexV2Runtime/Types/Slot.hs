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
-- Module      : Network.AWS.LexV2Runtime.Types.Slot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.Slot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.Shape
import Network.AWS.LexV2Runtime.Types.Value
import qualified Network.AWS.Prelude as Prelude

-- | A value that Amazon Lex V2 uses to fulfill an intent.
--
-- /See:/ 'newSlot' smart constructor.
data Slot = Slot'
  { -- | A list of one or more values that the user provided for the slot. For
    -- example, if a for a slot that elicits pizza toppings, the values might
    -- be \"pepperoni\" and \"pineapple.\"
    values :: Prelude.Maybe [Slot],
    -- | The current value of the slot.
    value :: Prelude.Maybe Value,
    -- | When the @shape@ value is @List@, it indicates that the @values@ field
    -- contains a list of slot values. When the value is @Scalar@, it indicates
    -- that the @value@ field contains a single value.
    shape :: Prelude.Maybe Shape
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Slot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'slot_values' - A list of one or more values that the user provided for the slot. For
-- example, if a for a slot that elicits pizza toppings, the values might
-- be \"pepperoni\" and \"pineapple.\"
--
-- 'value', 'slot_value' - The current value of the slot.
--
-- 'shape', 'slot_shape' - When the @shape@ value is @List@, it indicates that the @values@ field
-- contains a list of slot values. When the value is @Scalar@, it indicates
-- that the @value@ field contains a single value.
newSlot ::
  Slot
newSlot =
  Slot'
    { values = Prelude.Nothing,
      value = Prelude.Nothing,
      shape = Prelude.Nothing
    }

-- | A list of one or more values that the user provided for the slot. For
-- example, if a for a slot that elicits pizza toppings, the values might
-- be \"pepperoni\" and \"pineapple.\"
slot_values :: Lens.Lens' Slot (Prelude.Maybe [Slot])
slot_values = Lens.lens (\Slot' {values} -> values) (\s@Slot' {} a -> s {values = a} :: Slot) Prelude.. Lens.mapping Lens.coerced

-- | The current value of the slot.
slot_value :: Lens.Lens' Slot (Prelude.Maybe Value)
slot_value = Lens.lens (\Slot' {value} -> value) (\s@Slot' {} a -> s {value = a} :: Slot)

-- | When the @shape@ value is @List@, it indicates that the @values@ field
-- contains a list of slot values. When the value is @Scalar@, it indicates
-- that the @value@ field contains a single value.
slot_shape :: Lens.Lens' Slot (Prelude.Maybe Shape)
slot_shape = Lens.lens (\Slot' {shape} -> shape) (\s@Slot' {} a -> s {shape = a} :: Slot)

instance Core.FromJSON Slot where
  parseJSON =
    Core.withObject
      "Slot"
      ( \x ->
          Slot'
            Prelude.<$> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "value")
            Prelude.<*> (x Core..:? "shape")
      )

instance Prelude.Hashable Slot

instance Prelude.NFData Slot

instance Core.ToJSON Slot where
  toJSON Slot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("value" Core..=) Prelude.<$> value,
            ("shape" Core..=) Prelude.<$> shape
          ]
      )
