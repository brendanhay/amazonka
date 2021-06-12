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
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotDefaultValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    defaultValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
slotDefaultValue_defaultValue :: Lens.Lens' SlotDefaultValue Core.Text
slotDefaultValue_defaultValue = Lens.lens (\SlotDefaultValue' {defaultValue} -> defaultValue) (\s@SlotDefaultValue' {} a -> s {defaultValue = a} :: SlotDefaultValue)

instance Core.FromJSON SlotDefaultValue where
  parseJSON =
    Core.withObject
      "SlotDefaultValue"
      ( \x ->
          SlotDefaultValue'
            Core.<$> (x Core..: "defaultValue")
      )

instance Core.Hashable SlotDefaultValue

instance Core.NFData SlotDefaultValue

instance Core.ToJSON SlotDefaultValue where
  toJSON SlotDefaultValue' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("defaultValue" Core..= defaultValue)]
      )
