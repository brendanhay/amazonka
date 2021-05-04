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
-- Module      : Network.AWS.Glue.Types.Predicate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Predicate where

import Network.AWS.Glue.Types.Condition
import Network.AWS.Glue.Types.Logical
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the predicate of the trigger, which determines when it fires.
--
-- /See:/ 'newPredicate' smart constructor.
data Predicate = Predicate'
  { -- | An optional field if only one condition is listed. If multiple
    -- conditions are listed, then this field is required.
    logical :: Prelude.Maybe Logical,
    -- | A list of the conditions that determine when the trigger will fire.
    conditions :: Prelude.Maybe [Condition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Predicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logical', 'predicate_logical' - An optional field if only one condition is listed. If multiple
-- conditions are listed, then this field is required.
--
-- 'conditions', 'predicate_conditions' - A list of the conditions that determine when the trigger will fire.
newPredicate ::
  Predicate
newPredicate =
  Predicate'
    { logical = Prelude.Nothing,
      conditions = Prelude.Nothing
    }

-- | An optional field if only one condition is listed. If multiple
-- conditions are listed, then this field is required.
predicate_logical :: Lens.Lens' Predicate (Prelude.Maybe Logical)
predicate_logical = Lens.lens (\Predicate' {logical} -> logical) (\s@Predicate' {} a -> s {logical = a} :: Predicate)

-- | A list of the conditions that determine when the trigger will fire.
predicate_conditions :: Lens.Lens' Predicate (Prelude.Maybe [Condition])
predicate_conditions = Lens.lens (\Predicate' {conditions} -> conditions) (\s@Predicate' {} a -> s {conditions = a} :: Predicate) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Predicate where
  parseJSON =
    Prelude.withObject
      "Predicate"
      ( \x ->
          Predicate'
            Prelude.<$> (x Prelude..:? "Logical")
            Prelude.<*> ( x Prelude..:? "Conditions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Predicate

instance Prelude.NFData Predicate

instance Prelude.ToJSON Predicate where
  toJSON Predicate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Logical" Prelude..=) Prelude.<$> logical,
            ("Conditions" Prelude..=) Prelude.<$> conditions
          ]
      )
