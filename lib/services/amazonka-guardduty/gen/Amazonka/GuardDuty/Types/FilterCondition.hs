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
-- Module      : Amazonka.GuardDuty.Types.FilterCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FilterCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the condition.
--
-- /See:/ 'newFilterCondition' smart constructor.
data FilterCondition = FilterCondition'
  { -- | Represents an /equal/ ____ condition to be applied to a single field
    -- when querying for scan entries.
    equalsValue :: Prelude.Maybe Prelude.Text,
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for scan entries.
    greaterThan :: Prelude.Maybe Prelude.Integer,
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for scan entries.
    lessThan :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'equalsValue', 'filterCondition_equalsValue' - Represents an /equal/ ____ condition to be applied to a single field
-- when querying for scan entries.
--
-- 'greaterThan', 'filterCondition_greaterThan' - Represents a /greater than/ condition to be applied to a single field
-- when querying for scan entries.
--
-- 'lessThan', 'filterCondition_lessThan' - Represents a /less than/ condition to be applied to a single field when
-- querying for scan entries.
newFilterCondition ::
  FilterCondition
newFilterCondition =
  FilterCondition'
    { equalsValue = Prelude.Nothing,
      greaterThan = Prelude.Nothing,
      lessThan = Prelude.Nothing
    }

-- | Represents an /equal/ ____ condition to be applied to a single field
-- when querying for scan entries.
filterCondition_equalsValue :: Lens.Lens' FilterCondition (Prelude.Maybe Prelude.Text)
filterCondition_equalsValue = Lens.lens (\FilterCondition' {equalsValue} -> equalsValue) (\s@FilterCondition' {} a -> s {equalsValue = a} :: FilterCondition)

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for scan entries.
filterCondition_greaterThan :: Lens.Lens' FilterCondition (Prelude.Maybe Prelude.Integer)
filterCondition_greaterThan = Lens.lens (\FilterCondition' {greaterThan} -> greaterThan) (\s@FilterCondition' {} a -> s {greaterThan = a} :: FilterCondition)

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for scan entries.
filterCondition_lessThan :: Lens.Lens' FilterCondition (Prelude.Maybe Prelude.Integer)
filterCondition_lessThan = Lens.lens (\FilterCondition' {lessThan} -> lessThan) (\s@FilterCondition' {} a -> s {lessThan = a} :: FilterCondition)

instance Prelude.Hashable FilterCondition where
  hashWithSalt _salt FilterCondition' {..} =
    _salt `Prelude.hashWithSalt` equalsValue
      `Prelude.hashWithSalt` greaterThan
      `Prelude.hashWithSalt` lessThan

instance Prelude.NFData FilterCondition where
  rnf FilterCondition' {..} =
    Prelude.rnf equalsValue
      `Prelude.seq` Prelude.rnf greaterThan
      `Prelude.seq` Prelude.rnf lessThan

instance Data.ToJSON FilterCondition where
  toJSON FilterCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("equalsValue" Data..=) Prelude.<$> equalsValue,
            ("greaterThan" Data..=) Prelude.<$> greaterThan,
            ("lessThan" Data..=) Prelude.<$> lessThan
          ]
      )
