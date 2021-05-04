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
-- Module      : Network.AWS.GuardDuty.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Condition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the condition.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | Represents the /equal/ condition to be applied to a single field when
    -- querying for findings.
    eq :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for findings.
    greaterThan :: Prelude.Maybe Prelude.Integer,
    -- | Represents a /greater than/ condition to be applied to a single field
    -- when querying for findings.
    gt :: Prelude.Maybe Prelude.Int,
    -- | Represents a /greater than or equal/ condition to be applied to a single
    -- field when querying for findings.
    greaterThanOrEqual :: Prelude.Maybe Prelude.Integer,
    -- | Represents a /less than or equal/ condition to be applied to a single
    -- field when querying for findings.
    lte :: Prelude.Maybe Prelude.Int,
    -- | Represents the /not equal/ condition to be applied to a single field
    -- when querying for findings.
    neq :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /not equal/ ____ condition to be applied to a single field
    -- when querying for findings.
    notEquals :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for findings.
    lessThan :: Prelude.Maybe Prelude.Integer,
    -- | Represents an /equal/ ____ condition to be applied to a single field
    -- when querying for findings.
    equals :: Prelude.Maybe [Prelude.Text],
    -- | Represents a /greater than or equal/ condition to be applied to a single
    -- field when querying for findings.
    gte :: Prelude.Maybe Prelude.Int,
    -- | Represents a /less than or equal/ condition to be applied to a single
    -- field when querying for findings.
    lessThanOrEqual :: Prelude.Maybe Prelude.Integer,
    -- | Represents a /less than/ condition to be applied to a single field when
    -- querying for findings.
    lt :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { eq = Prelude.Nothing,
      greaterThan = Prelude.Nothing,
      gt = Prelude.Nothing,
      greaterThanOrEqual = Prelude.Nothing,
      lte = Prelude.Nothing,
      neq = Prelude.Nothing,
      notEquals = Prelude.Nothing,
      lessThan = Prelude.Nothing,
      equals = Prelude.Nothing,
      gte = Prelude.Nothing,
      lessThanOrEqual = Prelude.Nothing,
      lt = Prelude.Nothing
    }

-- | Represents the /equal/ condition to be applied to a single field when
-- querying for findings.
condition_eq :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_eq = Lens.lens (\Condition' {eq} -> eq) (\s@Condition' {} a -> s {eq = a} :: Condition) Prelude.. Lens.mapping Prelude._Coerce

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
condition_greaterThan :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_greaterThan = Lens.lens (\Condition' {greaterThan} -> greaterThan) (\s@Condition' {} a -> s {greaterThan = a} :: Condition)

-- | Represents a /greater than/ condition to be applied to a single field
-- when querying for findings.
condition_gt :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_gt = Lens.lens (\Condition' {gt} -> gt) (\s@Condition' {} a -> s {gt = a} :: Condition)

-- | Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_greaterThanOrEqual :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_greaterThanOrEqual = Lens.lens (\Condition' {greaterThanOrEqual} -> greaterThanOrEqual) (\s@Condition' {} a -> s {greaterThanOrEqual = a} :: Condition)

-- | Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_lte :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_lte = Lens.lens (\Condition' {lte} -> lte) (\s@Condition' {} a -> s {lte = a} :: Condition)

-- | Represents the /not equal/ condition to be applied to a single field
-- when querying for findings.
condition_neq :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_neq = Lens.lens (\Condition' {neq} -> neq) (\s@Condition' {} a -> s {neq = a} :: Condition) Prelude.. Lens.mapping Prelude._Coerce

-- | Represents a /not equal/ ____ condition to be applied to a single field
-- when querying for findings.
condition_notEquals :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_notEquals = Lens.lens (\Condition' {notEquals} -> notEquals) (\s@Condition' {} a -> s {notEquals = a} :: Condition) Prelude.. Lens.mapping Prelude._Coerce

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
condition_lessThan :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_lessThan = Lens.lens (\Condition' {lessThan} -> lessThan) (\s@Condition' {} a -> s {lessThan = a} :: Condition)

-- | Represents an /equal/ ____ condition to be applied to a single field
-- when querying for findings.
condition_equals :: Lens.Lens' Condition (Prelude.Maybe [Prelude.Text])
condition_equals = Lens.lens (\Condition' {equals} -> equals) (\s@Condition' {} a -> s {equals = a} :: Condition) Prelude.. Lens.mapping Prelude._Coerce

-- | Represents a /greater than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_gte :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_gte = Lens.lens (\Condition' {gte} -> gte) (\s@Condition' {} a -> s {gte = a} :: Condition)

-- | Represents a /less than or equal/ condition to be applied to a single
-- field when querying for findings.
condition_lessThanOrEqual :: Lens.Lens' Condition (Prelude.Maybe Prelude.Integer)
condition_lessThanOrEqual = Lens.lens (\Condition' {lessThanOrEqual} -> lessThanOrEqual) (\s@Condition' {} a -> s {lessThanOrEqual = a} :: Condition)

-- | Represents a /less than/ condition to be applied to a single field when
-- querying for findings.
condition_lt :: Lens.Lens' Condition (Prelude.Maybe Prelude.Int)
condition_lt = Lens.lens (\Condition' {lt} -> lt) (\s@Condition' {} a -> s {lt = a} :: Condition)

instance Prelude.FromJSON Condition where
  parseJSON =
    Prelude.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Prelude..:? "eq" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "greaterThan")
            Prelude.<*> (x Prelude..:? "gt")
            Prelude.<*> (x Prelude..:? "greaterThanOrEqual")
            Prelude.<*> (x Prelude..:? "lte")
            Prelude.<*> (x Prelude..:? "neq" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "notEquals"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "lessThan")
            Prelude.<*> (x Prelude..:? "equals" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "gte")
            Prelude.<*> (x Prelude..:? "lessThanOrEqual")
            Prelude.<*> (x Prelude..:? "lt")
      )

instance Prelude.Hashable Condition

instance Prelude.NFData Condition

instance Prelude.ToJSON Condition where
  toJSON Condition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("eq" Prelude..=) Prelude.<$> eq,
            ("greaterThan" Prelude..=) Prelude.<$> greaterThan,
            ("gt" Prelude..=) Prelude.<$> gt,
            ("greaterThanOrEqual" Prelude..=)
              Prelude.<$> greaterThanOrEqual,
            ("lte" Prelude..=) Prelude.<$> lte,
            ("neq" Prelude..=) Prelude.<$> neq,
            ("notEquals" Prelude..=) Prelude.<$> notEquals,
            ("lessThan" Prelude..=) Prelude.<$> lessThan,
            ("equals" Prelude..=) Prelude.<$> equals,
            ("gte" Prelude..=) Prelude.<$> gte,
            ("lessThanOrEqual" Prelude..=)
              Prelude.<$> lessThanOrEqual,
            ("lt" Prelude..=) Prelude.<$> lt
          ]
      )
