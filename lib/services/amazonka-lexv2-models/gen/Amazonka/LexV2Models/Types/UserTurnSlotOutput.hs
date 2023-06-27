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
-- Module      : Amazonka.LexV2Models.Types.UserTurnSlotOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UserTurnSlotOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a slot output by the test set execution.
--
-- /See:/ 'newUserTurnSlotOutput' smart constructor.
data UserTurnSlotOutput = UserTurnSlotOutput'
  { -- | A list of items mapping the name of the subslots to information about
    -- those subslots.
    subSlots :: Prelude.Maybe (Prelude.HashMap Name UserTurnSlotOutput),
    -- | The value output by the slot recognition.
    value :: Prelude.Maybe Prelude.Text,
    -- | Values that are output by the slot recognition.
    values :: Prelude.Maybe [UserTurnSlotOutput]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTurnSlotOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subSlots', 'userTurnSlotOutput_subSlots' - A list of items mapping the name of the subslots to information about
-- those subslots.
--
-- 'value', 'userTurnSlotOutput_value' - The value output by the slot recognition.
--
-- 'values', 'userTurnSlotOutput_values' - Values that are output by the slot recognition.
newUserTurnSlotOutput ::
  UserTurnSlotOutput
newUserTurnSlotOutput =
  UserTurnSlotOutput'
    { subSlots = Prelude.Nothing,
      value = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | A list of items mapping the name of the subslots to information about
-- those subslots.
userTurnSlotOutput_subSlots :: Lens.Lens' UserTurnSlotOutput (Prelude.Maybe (Prelude.HashMap Name UserTurnSlotOutput))
userTurnSlotOutput_subSlots = Lens.lens (\UserTurnSlotOutput' {subSlots} -> subSlots) (\s@UserTurnSlotOutput' {} a -> s {subSlots = a} :: UserTurnSlotOutput) Prelude.. Lens.mapping Lens.coerced

-- | The value output by the slot recognition.
userTurnSlotOutput_value :: Lens.Lens' UserTurnSlotOutput (Prelude.Maybe Prelude.Text)
userTurnSlotOutput_value = Lens.lens (\UserTurnSlotOutput' {value} -> value) (\s@UserTurnSlotOutput' {} a -> s {value = a} :: UserTurnSlotOutput)

-- | Values that are output by the slot recognition.
userTurnSlotOutput_values :: Lens.Lens' UserTurnSlotOutput (Prelude.Maybe [UserTurnSlotOutput])
userTurnSlotOutput_values = Lens.lens (\UserTurnSlotOutput' {values} -> values) (\s@UserTurnSlotOutput' {} a -> s {values = a} :: UserTurnSlotOutput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UserTurnSlotOutput where
  parseJSON =
    Data.withObject
      "UserTurnSlotOutput"
      ( \x ->
          UserTurnSlotOutput'
            Prelude.<$> (x Data..:? "subSlots" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "value")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UserTurnSlotOutput where
  hashWithSalt _salt UserTurnSlotOutput' {..} =
    _salt
      `Prelude.hashWithSalt` subSlots
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` values

instance Prelude.NFData UserTurnSlotOutput where
  rnf UserTurnSlotOutput' {..} =
    Prelude.rnf subSlots
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf values
