{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON SlotDefaultValue where
  parseJSON =
    Prelude.withObject
      "SlotDefaultValue"
      ( \x ->
          SlotDefaultValue'
            Prelude.<$> (x Prelude..: "defaultValue")
      )

instance Prelude.Hashable SlotDefaultValue

instance Prelude.NFData SlotDefaultValue

instance Prelude.ToJSON SlotDefaultValue where
  toJSON SlotDefaultValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("defaultValue" Prelude..= defaultValue)
          ]
      )
