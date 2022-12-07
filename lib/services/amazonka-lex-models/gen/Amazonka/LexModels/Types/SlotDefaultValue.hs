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
-- Module      : Amazonka.LexModels.Types.SlotDefaultValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.SlotDefaultValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A default value for a slot.
--
-- /See:/ 'newSlotDefaultValue' smart constructor.
data SlotDefaultValue = SlotDefaultValue'
  { -- | The default value for the slot. You can specify one of the following:
    --
    -- -   @#context-name.slot-name@ - The slot value \"slot-name\" in the
    --     context \"context-name.\"
    --
    -- -   @{attribute}@ - The slot value of the session attribute
    --     \"attribute.\"
    --
    -- -   @\'value\'@ - The discrete value \"value.\"
    defaultValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotDefaultValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'slotDefaultValue_defaultValue' - The default value for the slot. You can specify one of the following:
--
-- -   @#context-name.slot-name@ - The slot value \"slot-name\" in the
--     context \"context-name.\"
--
-- -   @{attribute}@ - The slot value of the session attribute
--     \"attribute.\"
--
-- -   @\'value\'@ - The discrete value \"value.\"
newSlotDefaultValue ::
  -- | 'defaultValue'
  Prelude.Text ->
  SlotDefaultValue
newSlotDefaultValue pDefaultValue_ =
  SlotDefaultValue' {defaultValue = pDefaultValue_}

-- | The default value for the slot. You can specify one of the following:
--
-- -   @#context-name.slot-name@ - The slot value \"slot-name\" in the
--     context \"context-name.\"
--
-- -   @{attribute}@ - The slot value of the session attribute
--     \"attribute.\"
--
-- -   @\'value\'@ - The discrete value \"value.\"
slotDefaultValue_defaultValue :: Lens.Lens' SlotDefaultValue Prelude.Text
slotDefaultValue_defaultValue = Lens.lens (\SlotDefaultValue' {defaultValue} -> defaultValue) (\s@SlotDefaultValue' {} a -> s {defaultValue = a} :: SlotDefaultValue)

instance Data.FromJSON SlotDefaultValue where
  parseJSON =
    Data.withObject
      "SlotDefaultValue"
      ( \x ->
          SlotDefaultValue'
            Prelude.<$> (x Data..: "defaultValue")
      )

instance Prelude.Hashable SlotDefaultValue where
  hashWithSalt _salt SlotDefaultValue' {..} =
    _salt `Prelude.hashWithSalt` defaultValue

instance Prelude.NFData SlotDefaultValue where
  rnf SlotDefaultValue' {..} = Prelude.rnf defaultValue

instance Data.ToJSON SlotDefaultValue where
  toJSON SlotDefaultValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("defaultValue" Data..= defaultValue)]
      )
