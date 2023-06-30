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
-- Module      : Amazonka.LexV2Models.Types.IntentOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SlotValueOverride
import qualified Amazonka.Prelude as Prelude

-- | Override settings to configure the intent state.
--
-- /See:/ 'newIntentOverride' smart constructor.
data IntentOverride = IntentOverride'
  { -- | The name of the intent. Only required when you\'re switching intents.
    name :: Prelude.Maybe Prelude.Text,
    -- | A map of all of the slot value overrides for the intent. The name of the
    -- slot maps to the value of the slot. Slots that are not included in the
    -- map aren\'t overridden.,
    slots :: Prelude.Maybe (Prelude.HashMap Prelude.Text SlotValueOverride)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'intentOverride_name' - The name of the intent. Only required when you\'re switching intents.
--
-- 'slots', 'intentOverride_slots' - A map of all of the slot value overrides for the intent. The name of the
-- slot maps to the value of the slot. Slots that are not included in the
-- map aren\'t overridden.,
newIntentOverride ::
  IntentOverride
newIntentOverride =
  IntentOverride'
    { name = Prelude.Nothing,
      slots = Prelude.Nothing
    }

-- | The name of the intent. Only required when you\'re switching intents.
intentOverride_name :: Lens.Lens' IntentOverride (Prelude.Maybe Prelude.Text)
intentOverride_name = Lens.lens (\IntentOverride' {name} -> name) (\s@IntentOverride' {} a -> s {name = a} :: IntentOverride)

-- | A map of all of the slot value overrides for the intent. The name of the
-- slot maps to the value of the slot. Slots that are not included in the
-- map aren\'t overridden.,
intentOverride_slots :: Lens.Lens' IntentOverride (Prelude.Maybe (Prelude.HashMap Prelude.Text SlotValueOverride))
intentOverride_slots = Lens.lens (\IntentOverride' {slots} -> slots) (\s@IntentOverride' {} a -> s {slots = a} :: IntentOverride) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON IntentOverride where
  parseJSON =
    Data.withObject
      "IntentOverride"
      ( \x ->
          IntentOverride'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "slots" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable IntentOverride where
  hashWithSalt _salt IntentOverride' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` slots

instance Prelude.NFData IntentOverride where
  rnf IntentOverride' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf slots

instance Data.ToJSON IntentOverride where
  toJSON IntentOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("slots" Data..=) Prelude.<$> slots
          ]
      )
