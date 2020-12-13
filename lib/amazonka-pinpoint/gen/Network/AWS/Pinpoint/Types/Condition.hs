{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Condition
  ( Condition (..),

    -- * Smart constructor
    mkCondition,

    -- * Lenses
    cOperator,
    cConditions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Operator
import Network.AWS.Pinpoint.Types.SimpleCondition
import qualified Network.AWS.Prelude as Lude

-- | Specifies the conditions to evaluate for an activity in a journey, and how to evaluate those conditions.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { -- | Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
    operator :: Lude.Maybe Operator,
    -- | The conditions to evaluate for the activity.
    conditions :: Lude.Maybe [SimpleCondition]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- * 'operator' - Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
-- * 'conditions' - The conditions to evaluate for the activity.
mkCondition ::
  Condition
mkCondition =
  Condition' {operator = Lude.Nothing, conditions = Lude.Nothing}

-- | Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOperator :: Lens.Lens' Condition (Lude.Maybe Operator)
cOperator = Lens.lens (operator :: Condition -> Lude.Maybe Operator) (\s a -> s {operator = a} :: Condition)
{-# DEPRECATED cOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | The conditions to evaluate for the activity.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConditions :: Lens.Lens' Condition (Lude.Maybe [SimpleCondition])
cConditions = Lens.lens (conditions :: Condition -> Lude.Maybe [SimpleCondition]) (\s a -> s {conditions = a} :: Condition)
{-# DEPRECATED cConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

instance Lude.FromJSON Condition where
  parseJSON =
    Lude.withObject
      "Condition"
      ( \x ->
          Condition'
            Lude.<$> (x Lude..:? "Operator")
            Lude.<*> (x Lude..:? "Conditions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Condition where
  toJSON Condition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Operator" Lude..=) Lude.<$> operator,
            ("Conditions" Lude..=) Lude.<$> conditions
          ]
      )
