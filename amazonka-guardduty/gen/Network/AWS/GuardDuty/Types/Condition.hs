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
-- Module      : Network.AWS.GuardDuty.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Condition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the condition.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | Represents the /equal/ condition to be applied to a single field when
    -- querying for findings.
    eq :: Core.Maybe [Core.Text],
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for findings.
    greaterThan :: Core.Maybe Core.Integer,
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for findings.
    gt :: Core.Maybe Core.Int,
    -- | Represents a /greater than or equal/ condition to be applied to a single
    -- field when querying for findings.
    greaterThanOrEqual :: Core.Maybe Core.Integer,
    -- | Represents a /less than or equal/ condition to be applied to a single
    -- field when querying for findings.
    lte :: Core.Maybe Core.Int,
    -- | Represents the /not equal/ condition to be applied to a single field
    -- when querying for findings.
    neq :: Core.Maybe [Core.Text],
    -- | Represents a /not equal/ ____ condition to be applied to a single field
    -- when querying for findings.
    notEquals :: Core.Maybe [Core.Text],
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for findings.
    lessThan :: Core.Maybe Core.Integer,
    -- | Represents an /equal/ ____ condition to be applied to a single field
    -- when querying for findings.
    equals :: Core.Maybe [Core.Text],
    -- | Represents a /greater than or equal/ condition to be applied to a single
    -- field when querying for findings.
    gte :: Core.Maybe Core.Int,
    -- | Represents a /less than or equal/ condition to be applied to a single
    -- field when querying for findings.
    lessThanOrEqual :: Core.Maybe Core.Integer,
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for findings.
    lt :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eq', 'condition_eq' - Represents the /equal/ condition to be applied to a single field when
-- querying for findings.
--
-- 'greaterThan', 'condition_greaterThan' - Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
--
-- 'gt', 'condition_gt' - Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
--
-- 'greaterThanOrEqual', 'condition_greaterThanOrEqual' - Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'lte', 'condition_lte' - Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'neq', 'condition_neq' - Represents the /not equal/ condition to be applied to a single field
-- when querying for findings.
--
-- 'notEquals', 'condition_notEquals' - Represents a /not equal/ ____ condition to be applied to a single field
-- when querying for findings.
--
-- 'lessThan', 'condition_lessThan' - Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
--
-- 'equals', 'condition_equals' - Represents an /equal/ ____ condition to be applied to a single field
-- when querying for findings.
--
-- 'gte', 'condition_gte' - Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'lessThanOrEqual', 'condition_lessThanOrEqual' - Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
--
-- 'lt', 'condition_lt' - Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
newCondition ::
  Condition
newCondition =
  Condition'
    { eq = Core.Nothing,
      greaterThan = Core.Nothing,
      gt = Core.Nothing,
      greaterThanOrEqual = Core.Nothing,
      lte = Core.Nothing,
      neq = Core.Nothing,
      notEquals = Core.Nothing,
      lessThan = Core.Nothing,
      equals = Core.Nothing,
      gte = Core.Nothing,
      lessThanOrEqual = Core.Nothing,
      lt = Core.Nothing
    }

-- | Represents the /equal/ condition to be applied to a single field when
-- querying for findings.
condition_eq :: Lens.Lens' Condition (Core.Maybe [Core.Text])
condition_eq = Lens.lens (\Condition' {eq} -> eq) (\s@Condition' {} a -> s {eq = a} :: Condition) Core.. Lens.mapping Lens._Coerce

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
condition_greaterThan :: Lens.Lens' Condition (Core.Maybe Core.Integer)
condition_greaterThan = Lens.lens (\Condition' {greaterThan} -> greaterThan) (\s@Condition' {} a -> s {greaterThan = a} :: Condition)

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
condition_gt :: Lens.Lens' Condition (Core.Maybe Core.Int)
condition_gt = Lens.lens (\Condition' {gt} -> gt) (\s@Condition' {} a -> s {gt = a} :: Condition)

-- | Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_greaterThanOrEqual :: Lens.Lens' Condition (Core.Maybe Core.Integer)
condition_greaterThanOrEqual = Lens.lens (\Condition' {greaterThanOrEqual} -> greaterThanOrEqual) (\s@Condition' {} a -> s {greaterThanOrEqual = a} :: Condition)

-- | Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_lte :: Lens.Lens' Condition (Core.Maybe Core.Int)
condition_lte = Lens.lens (\Condition' {lte} -> lte) (\s@Condition' {} a -> s {lte = a} :: Condition)

-- | Represents the /not equal/ condition to be applied to a single field
-- when querying for findings.
condition_neq :: Lens.Lens' Condition (Core.Maybe [Core.Text])
condition_neq = Lens.lens (\Condition' {neq} -> neq) (\s@Condition' {} a -> s {neq = a} :: Condition) Core.. Lens.mapping Lens._Coerce

-- | Represents a /not equal/ ____ condition to be applied to a single field
-- when querying for findings.
condition_notEquals :: Lens.Lens' Condition (Core.Maybe [Core.Text])
condition_notEquals = Lens.lens (\Condition' {notEquals} -> notEquals) (\s@Condition' {} a -> s {notEquals = a} :: Condition) Core.. Lens.mapping Lens._Coerce

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
condition_lessThan :: Lens.Lens' Condition (Core.Maybe Core.Integer)
condition_lessThan = Lens.lens (\Condition' {lessThan} -> lessThan) (\s@Condition' {} a -> s {lessThan = a} :: Condition)

-- | Represents an /equal/ ____ condition to be applied to a single field
-- when querying for findings.
condition_equals :: Lens.Lens' Condition (Core.Maybe [Core.Text])
condition_equals = Lens.lens (\Condition' {equals} -> equals) (\s@Condition' {} a -> s {equals = a} :: Condition) Core.. Lens.mapping Lens._Coerce

-- | Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_gte :: Lens.Lens' Condition (Core.Maybe Core.Int)
condition_gte = Lens.lens (\Condition' {gte} -> gte) (\s@Condition' {} a -> s {gte = a} :: Condition)

-- | Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_lessThanOrEqual :: Lens.Lens' Condition (Core.Maybe Core.Integer)
condition_lessThanOrEqual = Lens.lens (\Condition' {lessThanOrEqual} -> lessThanOrEqual) (\s@Condition' {} a -> s {lessThanOrEqual = a} :: Condition)

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
condition_lt :: Lens.Lens' Condition (Core.Maybe Core.Int)
condition_lt = Lens.lens (\Condition' {lt} -> lt) (\s@Condition' {} a -> s {lt = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Core.<$> (x Core..:? "eq" Core..!= Core.mempty)
            Core.<*> (x Core..:? "greaterThan")
            Core.<*> (x Core..:? "gt")
            Core.<*> (x Core..:? "greaterThanOrEqual")
            Core.<*> (x Core..:? "lte")
            Core.<*> (x Core..:? "neq" Core..!= Core.mempty)
            Core.<*> (x Core..:? "notEquals" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lessThan")
            Core.<*> (x Core..:? "equals" Core..!= Core.mempty)
            Core.<*> (x Core..:? "gte")
            Core.<*> (x Core..:? "lessThanOrEqual")
            Core.<*> (x Core..:? "lt")
      )

instance Core.Hashable Condition

instance Core.NFData Condition

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("eq" Core..=) Core.<$> eq,
            ("greaterThan" Core..=) Core.<$> greaterThan,
            ("gt" Core..=) Core.<$> gt,
            ("greaterThanOrEqual" Core..=)
              Core.<$> greaterThanOrEqual,
            ("lte" Core..=) Core.<$> lte,
            ("neq" Core..=) Core.<$> neq,
            ("notEquals" Core..=) Core.<$> notEquals,
            ("lessThan" Core..=) Core.<$> lessThan,
            ("equals" Core..=) Core.<$> equals,
            ("gte" Core..=) Core.<$> gte,
            ("lessThanOrEqual" Core..=) Core.<$> lessThanOrEqual,
            ("lt" Core..=) Core.<$> lt
          ]
      )
