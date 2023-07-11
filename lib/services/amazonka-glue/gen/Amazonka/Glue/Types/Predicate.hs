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
-- Module      : Amazonka.Glue.Types.Predicate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Predicate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Condition
import Amazonka.Glue.Types.Logical
import qualified Amazonka.Prelude as Prelude

-- | Defines the predicate of the trigger, which determines when it fires.
--
-- /See:/ 'newPredicate' smart constructor.
data Predicate = Predicate'
  { -- | A list of the conditions that determine when the trigger will fire.
    conditions :: Prelude.Maybe [Condition],
    -- | An optional field if only one condition is listed. If multiple
    -- conditions are listed, then this field is required.
    logical :: Prelude.Maybe Logical
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Predicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditions', 'predicate_conditions' - A list of the conditions that determine when the trigger will fire.
--
-- 'logical', 'predicate_logical' - An optional field if only one condition is listed. If multiple
-- conditions are listed, then this field is required.
newPredicate ::
  Predicate
newPredicate =
  Predicate'
    { conditions = Prelude.Nothing,
      logical = Prelude.Nothing
    }

-- | A list of the conditions that determine when the trigger will fire.
predicate_conditions :: Lens.Lens' Predicate (Prelude.Maybe [Condition])
predicate_conditions = Lens.lens (\Predicate' {conditions} -> conditions) (\s@Predicate' {} a -> s {conditions = a} :: Predicate) Prelude.. Lens.mapping Lens.coerced

-- | An optional field if only one condition is listed. If multiple
-- conditions are listed, then this field is required.
predicate_logical :: Lens.Lens' Predicate (Prelude.Maybe Logical)
predicate_logical = Lens.lens (\Predicate' {logical} -> logical) (\s@Predicate' {} a -> s {logical = a} :: Predicate)

instance Data.FromJSON Predicate where
  parseJSON =
    Data.withObject
      "Predicate"
      ( \x ->
          Predicate'
            Prelude.<$> (x Data..:? "Conditions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Logical")
      )

instance Prelude.Hashable Predicate where
  hashWithSalt _salt Predicate' {..} =
    _salt
      `Prelude.hashWithSalt` conditions
      `Prelude.hashWithSalt` logical

instance Prelude.NFData Predicate where
  rnf Predicate' {..} =
    Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf logical

instance Data.ToJSON Predicate where
  toJSON Predicate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Conditions" Data..=) Prelude.<$> conditions,
            ("Logical" Data..=) Prelude.<$> logical
          ]
      )
