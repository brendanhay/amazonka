-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Condition
  ( Condition (..),

    -- * Smart constructor
    mkCondition,

    -- * Lenses
    cEQ,
    cLessThan,
    cLte,
    cGreaterThanOrEqual,
    cLessThanOrEqual,
    cGT,
    cEquals,
    cNeq,
    cNotEquals,
    cLT,
    cGte,
    cGreaterThan,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the condition.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { eQ :: Lude.Maybe [Lude.Text],
    lessThan :: Lude.Maybe Lude.Integer,
    lte :: Lude.Maybe Lude.Int,
    greaterThanOrEqual :: Lude.Maybe Lude.Integer,
    lessThanOrEqual :: Lude.Maybe Lude.Integer,
    gT :: Lude.Maybe Lude.Int,
    equals :: Lude.Maybe [Lude.Text],
    neq :: Lude.Maybe [Lude.Text],
    notEquals :: Lude.Maybe [Lude.Text],
    lT :: Lude.Maybe Lude.Int,
    gte :: Lude.Maybe Lude.Int,
    greaterThan :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- * 'eQ' - Represents the /equal/ condition to be applied to a single field when querying for findings.
-- * 'equals' - Represents an /equal/ ____ condition to be applied to a single field when querying for findings.
-- * 'gT' - Represents a /greater than/ condition to be applied to a single field when querying for findings.
-- * 'greaterThan' - Represents a /greater than/ condition to be applied to a single field when querying for findings.
-- * 'greaterThanOrEqual' - Represents a /greater than or equal/ condition to be applied to a single field when querying for findings.
-- * 'gte' - Represents a /greater than or equal/ condition to be applied to a single field when querying for findings.
-- * 'lT' - Represents a /less than/ condition to be applied to a single field when querying for findings.
-- * 'lessThan' - Represents a /less than/ condition to be applied to a single field when querying for findings.
-- * 'lessThanOrEqual' - Represents a /less than or equal/ condition to be applied to a single field when querying for findings.
-- * 'lte' - Represents a /less than or equal/ condition to be applied to a single field when querying for findings.
-- * 'neq' - Represents the /not equal/ condition to be applied to a single field when querying for findings.
-- * 'notEquals' - Represents a /not equal/ ____ condition to be applied to a single field when querying for findings.
mkCondition ::
  Condition
mkCondition =
  Condition'
    { eQ = Lude.Nothing,
      lessThan = Lude.Nothing,
      lte = Lude.Nothing,
      greaterThanOrEqual = Lude.Nothing,
      lessThanOrEqual = Lude.Nothing,
      gT = Lude.Nothing,
      equals = Lude.Nothing,
      neq = Lude.Nothing,
      notEquals = Lude.Nothing,
      lT = Lude.Nothing,
      gte = Lude.Nothing,
      greaterThan = Lude.Nothing
    }

-- | Represents the /equal/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'eQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEQ :: Lens.Lens' Condition (Lude.Maybe [Lude.Text])
cEQ = Lens.lens (eQ :: Condition -> Lude.Maybe [Lude.Text]) (\s a -> s {eQ = a} :: Condition)
{-# DEPRECATED cEQ "Use generic-lens or generic-optics with 'eQ' instead." #-}

-- | Represents a /less than/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'lessThan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLessThan :: Lens.Lens' Condition (Lude.Maybe Lude.Integer)
cLessThan = Lens.lens (lessThan :: Condition -> Lude.Maybe Lude.Integer) (\s a -> s {lessThan = a} :: Condition)
{-# DEPRECATED cLessThan "Use generic-lens or generic-optics with 'lessThan' instead." #-}

-- | Represents a /less than or equal/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'lte' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLte :: Lens.Lens' Condition (Lude.Maybe Lude.Int)
cLte = Lens.lens (lte :: Condition -> Lude.Maybe Lude.Int) (\s a -> s {lte = a} :: Condition)
{-# DEPRECATED cLte "Use generic-lens or generic-optics with 'lte' instead." #-}

-- | Represents a /greater than or equal/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'greaterThanOrEqual' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGreaterThanOrEqual :: Lens.Lens' Condition (Lude.Maybe Lude.Integer)
cGreaterThanOrEqual = Lens.lens (greaterThanOrEqual :: Condition -> Lude.Maybe Lude.Integer) (\s a -> s {greaterThanOrEqual = a} :: Condition)
{-# DEPRECATED cGreaterThanOrEqual "Use generic-lens or generic-optics with 'greaterThanOrEqual' instead." #-}

-- | Represents a /less than or equal/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'lessThanOrEqual' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLessThanOrEqual :: Lens.Lens' Condition (Lude.Maybe Lude.Integer)
cLessThanOrEqual = Lens.lens (lessThanOrEqual :: Condition -> Lude.Maybe Lude.Integer) (\s a -> s {lessThanOrEqual = a} :: Condition)
{-# DEPRECATED cLessThanOrEqual "Use generic-lens or generic-optics with 'lessThanOrEqual' instead." #-}

-- | Represents a /greater than/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'gT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGT :: Lens.Lens' Condition (Lude.Maybe Lude.Int)
cGT = Lens.lens (gT :: Condition -> Lude.Maybe Lude.Int) (\s a -> s {gT = a} :: Condition)
{-# DEPRECATED cGT "Use generic-lens or generic-optics with 'gT' instead." #-}

-- | Represents an /equal/ ____ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'equals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEquals :: Lens.Lens' Condition (Lude.Maybe [Lude.Text])
cEquals = Lens.lens (equals :: Condition -> Lude.Maybe [Lude.Text]) (\s a -> s {equals = a} :: Condition)
{-# DEPRECATED cEquals "Use generic-lens or generic-optics with 'equals' instead." #-}

-- | Represents the /not equal/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'neq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNeq :: Lens.Lens' Condition (Lude.Maybe [Lude.Text])
cNeq = Lens.lens (neq :: Condition -> Lude.Maybe [Lude.Text]) (\s a -> s {neq = a} :: Condition)
{-# DEPRECATED cNeq "Use generic-lens or generic-optics with 'neq' instead." #-}

-- | Represents a /not equal/ ____ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'notEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotEquals :: Lens.Lens' Condition (Lude.Maybe [Lude.Text])
cNotEquals = Lens.lens (notEquals :: Condition -> Lude.Maybe [Lude.Text]) (\s a -> s {notEquals = a} :: Condition)
{-# DEPRECATED cNotEquals "Use generic-lens or generic-optics with 'notEquals' instead." #-}

-- | Represents a /less than/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'lT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLT :: Lens.Lens' Condition (Lude.Maybe Lude.Int)
cLT = Lens.lens (lT :: Condition -> Lude.Maybe Lude.Int) (\s a -> s {lT = a} :: Condition)
{-# DEPRECATED cLT "Use generic-lens or generic-optics with 'lT' instead." #-}

-- | Represents a /greater than or equal/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'gte' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGte :: Lens.Lens' Condition (Lude.Maybe Lude.Int)
cGte = Lens.lens (gte :: Condition -> Lude.Maybe Lude.Int) (\s a -> s {gte = a} :: Condition)
{-# DEPRECATED cGte "Use generic-lens or generic-optics with 'gte' instead." #-}

-- | Represents a /greater than/ condition to be applied to a single field when querying for findings.
--
-- /Note:/ Consider using 'greaterThan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGreaterThan :: Lens.Lens' Condition (Lude.Maybe Lude.Integer)
cGreaterThan = Lens.lens (greaterThan :: Condition -> Lude.Maybe Lude.Integer) (\s a -> s {greaterThan = a} :: Condition)
{-# DEPRECATED cGreaterThan "Use generic-lens or generic-optics with 'greaterThan' instead." #-}

instance Lude.FromJSON Condition where
  parseJSON =
    Lude.withObject
      "Condition"
      ( \x ->
          Condition'
            Lude.<$> (x Lude..:? "eq" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lessThan")
            Lude.<*> (x Lude..:? "lte")
            Lude.<*> (x Lude..:? "greaterThanOrEqual")
            Lude.<*> (x Lude..:? "lessThanOrEqual")
            Lude.<*> (x Lude..:? "gt")
            Lude.<*> (x Lude..:? "equals" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "neq" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "notEquals" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lt")
            Lude.<*> (x Lude..:? "gte")
            Lude.<*> (x Lude..:? "greaterThan")
      )

instance Lude.ToJSON Condition where
  toJSON Condition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("eq" Lude..=) Lude.<$> eQ,
            ("lessThan" Lude..=) Lude.<$> lessThan,
            ("lte" Lude..=) Lude.<$> lte,
            ("greaterThanOrEqual" Lude..=) Lude.<$> greaterThanOrEqual,
            ("lessThanOrEqual" Lude..=) Lude.<$> lessThanOrEqual,
            ("gt" Lude..=) Lude.<$> gT,
            ("equals" Lude..=) Lude.<$> equals,
            ("neq" Lude..=) Lude.<$> neq,
            ("notEquals" Lude..=) Lude.<$> notEquals,
            ("lt" Lude..=) Lude.<$> lT,
            ("gte" Lude..=) Lude.<$> gte,
            ("greaterThan" Lude..=) Lude.<$> greaterThan
          ]
      )
