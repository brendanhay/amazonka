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
-- Module      : Amazonka.LexV2Models.Types.UserTurnIntentOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UserTurnIntentOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.UserTurnSlotOutput
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the intent that is output for the turn by the
-- test execution.
--
-- /See:/ 'newUserTurnIntentOutput' smart constructor.
data UserTurnIntentOutput = UserTurnIntentOutput'
  { -- | The slots associated with the intent.
    slots :: Prelude.Maybe (Prelude.HashMap Prelude.Text UserTurnSlotOutput),
    -- | The name of the intent.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTurnIntentOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slots', 'userTurnIntentOutput_slots' - The slots associated with the intent.
--
-- 'name', 'userTurnIntentOutput_name' - The name of the intent.
newUserTurnIntentOutput ::
  -- | 'name'
  Prelude.Text ->
  UserTurnIntentOutput
newUserTurnIntentOutput pName_ =
  UserTurnIntentOutput'
    { slots = Prelude.Nothing,
      name = pName_
    }

-- | The slots associated with the intent.
userTurnIntentOutput_slots :: Lens.Lens' UserTurnIntentOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text UserTurnSlotOutput))
userTurnIntentOutput_slots = Lens.lens (\UserTurnIntentOutput' {slots} -> slots) (\s@UserTurnIntentOutput' {} a -> s {slots = a} :: UserTurnIntentOutput) Prelude.. Lens.mapping Lens.coerced

-- | The name of the intent.
userTurnIntentOutput_name :: Lens.Lens' UserTurnIntentOutput Prelude.Text
userTurnIntentOutput_name = Lens.lens (\UserTurnIntentOutput' {name} -> name) (\s@UserTurnIntentOutput' {} a -> s {name = a} :: UserTurnIntentOutput)

instance Data.FromJSON UserTurnIntentOutput where
  parseJSON =
    Data.withObject
      "UserTurnIntentOutput"
      ( \x ->
          UserTurnIntentOutput'
            Prelude.<$> (x Data..:? "slots" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable UserTurnIntentOutput where
  hashWithSalt _salt UserTurnIntentOutput' {..} =
    _salt
      `Prelude.hashWithSalt` slots
      `Prelude.hashWithSalt` name

instance Prelude.NFData UserTurnIntentOutput where
  rnf UserTurnIntentOutput' {..} =
    Prelude.rnf slots `Prelude.seq` Prelude.rnf name
