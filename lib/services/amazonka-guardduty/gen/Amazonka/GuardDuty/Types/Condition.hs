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
-- Module      : Amazonka.GuardDuty.Types.Condition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the condition.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | Represents a /not equal/ ____ condition to be applied to a single field
    -- when querying for findings.
    notEquals :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /less than or equal/ condition to be applied to a single
    -- field when querying for findings.
    lessThanOrEqual :: Prelude.Maybe Prelude.Integer,
    -- | Represents the /not equal/ condition to be applied to a single field
    -- when querying for findings.
    neq :: Prelude.Maybe [Prelude.Text],
    -- | Represents an /equal/ ____ condition to be applied to a single field
    -- when querying for findings.
    equals :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for findings.
    lessThan :: Prelude.Maybe Prelude.Integer,
    -- | Represents a /less than or equal/ condition to be applied to a single
    -- field when querying for findings.
    lte :: Prelude.Maybe Prelude.Int,
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for findings.
    lt :: Prelude.Maybe Prelude.Int,
    -- | Represents a /greater than or equal/ condition to be applied to a single
    -- field when querying for findings.
    gte :: Prelude.Maybe Prelude.Int,
    -- | Represents the /equal/ condition to be applied to a single field when
    -- querying for findings.
    eq :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for findings.
    gt :: Prelude.Maybe Prelude.Int,
    -- | Represents a /greater than or equal/ condition to be applied to a single
    -- field when querying for findings.
    greaterThanOrEqual :: Prelude.Maybe Prelude.Integer,
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for findings.
    greaterThan :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notEquals', 'condition_notEquals' - Represents a /not equal/ ____ condition to be applied to a single field
-- when querying for findings.
--
-- 'lessThanOrEqual', 'condition_lessThanOrEqual' - Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'neq', 'condition_neq' - Represents the /not equal/ condition to be applied to a single field
-- when querying for findings.
--
-- 'equals', 'condition_equals' - Represents an /equal/ ____ condition to be applied to a single field
-- when querying for findings.
--
-- 'lessThan', 'condition_lessThan' - Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
--
-- 'lte', 'condition_lte' - Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'lt', 'condition_lt' - Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
--
-- 'gte', 'condition_gte' - Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'eq', 'condition_eq' - Represents the /equal/ condition to be applied to a single field when
-- querying for findings.
--
-- 'gt', 'condition_gt' - Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
--
-- 'greaterThanOrEqual', 'condition_greaterThanOrEqual' - Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'greaterThan', 'condition_greaterThan' - Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
newCondition ::
  Condition
newCondition =
  Condition'
    { notEquals = Prelude.Nothing,
      lessThanOrEqual = Prelude.Nothing,
      neq = Prelude.Nothing,
      equals = Prelude.Nothing,
      lessThan = Prelude.Nothing,
      lte = Prelude.Nothing,
      lt = Prelude.Nothing,
      gte = Prelude.Nothing,
      eq = Prelude.Nothing,
      gt = Prelude.Nothing,
      greaterThanOrEqual = Prelude.Nothing,
      greaterThan = Prelude.Nothing
    }

-- | Represents a /not equal/ ____ condition to be applied to a single field
-- when querying for findings.
condition_notEquals :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_notEquals = Lens.lens (\Condition' {notEquals} -> notEquals) (\s@Condition' {} a -> s {notEquals = a} :: Condition) Prelude.. Lens.mapping Lens.coerced

-- | Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_lessThanOrEqual :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_lessThanOrEqual = Lens.lens (\Condition' {lessThanOrEqual} -> lessThanOrEqual) (\s@Condition' {} a -> s {lessThanOrEqual = a} :: Condition)

-- | Represents the /not equal/ condition to be applied to a single field
-- when querying for findings.
condition_neq :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_neq = Lens.lens (\Condition' {neq} -> neq) (\s@Condition' {} a -> s {neq = a} :: Condition) Prelude.. Lens.mapping Lens.coerced

-- | Represents an /equal/ ____ condition to be applied to a single field
-- when querying for findings.
condition_equals :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_equals = Lens.lens (\Condition' {equals} -> equals) (\s@Condition' {} a -> s {equals = a} :: Condition) Prelude.. Lens.mapping Lens.coerced

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
condition_lessThan :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_lessThan = Lens.lens (\Condition' {lessThan} -> lessThan) (\s@Condition' {} a -> s {lessThan = a} :: Condition)

-- | Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_lte :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_lte = Lens.lens (\Condition' {lte} -> lte) (\s@Condition' {} a -> s {lte = a} :: Condition)

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
condition_lt :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_lt = Lens.lens (\Condition' {lt} -> lt) (\s@Condition' {} a -> s {lt = a} :: Condition)

-- | Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_gte :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_gte = Lens.lens (\Condition' {gte} -> gte) (\s@Condition' {} a -> s {gte = a} :: Condition)

-- | Represents the /equal/ condition to be applied to a single field when
-- querying for findings.
condition_eq :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_eq = Lens.lens (\Condition' {eq} -> eq) (\s@Condition' {} a -> s {eq = a} :: Condition) Prelude.. Lens.mapping Lens.coerced

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
condition_gt :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_gt = Lens.lens (\Condition' {gt} -> gt) (\s@Condition' {} a -> s {gt = a} :: Condition)

-- | Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_greaterThanOrEqual :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_greaterThanOrEqual = Lens.lens (\Condition' {greaterThanOrEqual} -> greaterThanOrEqual) (\s@Condition' {} a -> s {greaterThanOrEqual = a} :: Condition)

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
condition_greaterThan :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_greaterThan = Lens.lens (\Condition' {greaterThan} -> greaterThan) (\s@Condition' {} a -> s {greaterThan = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Core..:? "notEquals" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "lessThanOrEqual")
            Prelude.<*> (x Core..:? "neq" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "equals" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "lessThan")
            Prelude.<*> (x Core..:? "lte")
            Prelude.<*> (x Core..:? "lt")
            Prelude.<*> (x Core..:? "gte")
            Prelude.<*> (x Core..:? "eq" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "gt")
            Prelude.<*> (x Core..:? "greaterThanOrEqual")
            Prelude.<*> (x Core..:? "greaterThan")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt `Prelude.hashWithSalt` notEquals
      `Prelude.hashWithSalt` lessThanOrEqual
      `Prelude.hashWithSalt` neq
      `Prelude.hashWithSalt` equals
      `Prelude.hashWithSalt` lessThan
      `Prelude.hashWithSalt` lte
      `Prelude.hashWithSalt` lt
      `Prelude.hashWithSalt` gte
      `Prelude.hashWithSalt` eq
      `Prelude.hashWithSalt` gt
      `Prelude.hashWithSalt` greaterThanOrEqual
      `Prelude.hashWithSalt` greaterThan

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf notEquals
      `Prelude.seq` Prelude.rnf lessThanOrEqual
      `Prelude.seq` Prelude.rnf neq
      `Prelude.seq` Prelude.rnf equals
      `Prelude.seq` Prelude.rnf lessThan
      `Prelude.seq` Prelude.rnf lte
      `Prelude.seq` Prelude.rnf lt
      `Prelude.seq` Prelude.rnf gte
      `Prelude.seq` Prelude.rnf eq
      `Prelude.seq` Prelude.rnf gt
      `Prelude.seq` Prelude.rnf greaterThanOrEqual
      `Prelude.seq` Prelude.rnf greaterThan

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("notEquals" Core..=) Prelude.<$> notEquals,
            ("lessThanOrEqual" Core..=)
              Prelude.<$> lessThanOrEqual,
            ("neq" Core..=) Prelude.<$> neq,
            ("equals" Core..=) Prelude.<$> equals,
            ("lessThan" Core..=) Prelude.<$> lessThan,
            ("lte" Core..=) Prelude.<$> lte,
            ("lt" Core..=) Prelude.<$> lt,
            ("gte" Core..=) Prelude.<$> gte,
            ("eq" Core..=) Prelude.<$> eq,
            ("gt" Core..=) Prelude.<$> gt,
            ("greaterThanOrEqual" Core..=)
              Prelude.<$> greaterThanOrEqual,
            ("greaterThan" Core..=) Prelude.<$> greaterThan
          ]
      )
