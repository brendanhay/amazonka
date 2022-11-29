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
-- Module      : Amazonka.LexV2Models.Types.SlotValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The value to set in a slot.
--
-- /See:/ 'newSlotValue' smart constructor.
data SlotValue = SlotValue'
  { -- | The value that Amazon Lex determines for the slot. The actual value
    -- depends on the setting of the value selection strategy for the bot. You
    -- can choose to use the value entered by the user, or you can have Amazon
    -- Lex choose the first value in the @resolvedValues@ list.
    interpretedValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interpretedValue', 'slotValue_interpretedValue' - The value that Amazon Lex determines for the slot. The actual value
-- depends on the setting of the value selection strategy for the bot. You
-- can choose to use the value entered by the user, or you can have Amazon
-- Lex choose the first value in the @resolvedValues@ list.
newSlotValue ::
  SlotValue
newSlotValue =
  SlotValue' {interpretedValue = Prelude.Nothing}

-- | The value that Amazon Lex determines for the slot. The actual value
-- depends on the setting of the value selection strategy for the bot. You
-- can choose to use the value entered by the user, or you can have Amazon
-- Lex choose the first value in the @resolvedValues@ list.
slotValue_interpretedValue :: Lens.Lens' SlotValue (Prelude.Maybe Prelude.Text)
slotValue_interpretedValue = Lens.lens (\SlotValue' {interpretedValue} -> interpretedValue) (\s@SlotValue' {} a -> s {interpretedValue = a} :: SlotValue)

instance Core.FromJSON SlotValue where
  parseJSON =
    Core.withObject
      "SlotValue"
      ( \x ->
          SlotValue'
            Prelude.<$> (x Core..:? "interpretedValue")
      )

instance Prelude.Hashable SlotValue where
  hashWithSalt _salt SlotValue' {..} =
    _salt `Prelude.hashWithSalt` interpretedValue

instance Prelude.NFData SlotValue where
  rnf SlotValue' {..} = Prelude.rnf interpretedValue

instance Core.ToJSON SlotValue where
  toJSON SlotValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("interpretedValue" Core..=)
              Prelude.<$> interpretedValue
          ]
      )
