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
    cConditions,
    cOperator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Operator as Types
import qualified Network.AWS.Pinpoint.Types.SimpleCondition as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the conditions to evaluate for an activity in a journey, and how to evaluate those conditions.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { -- | The conditions to evaluate for the activity.
    conditions :: Core.Maybe [Types.SimpleCondition],
    -- | Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
    operator :: Core.Maybe Types.Operator
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Condition' value with any optional fields omitted.
mkCondition ::
  Condition
mkCondition =
  Condition' {conditions = Core.Nothing, operator = Core.Nothing}

-- | The conditions to evaluate for the activity.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConditions :: Lens.Lens' Condition (Core.Maybe [Types.SimpleCondition])
cConditions = Lens.field @"conditions"
{-# DEPRECATED cConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

-- | Specifies how to handle multiple conditions for the activity. For example, if you specify two conditions for an activity, whether both or only one of the conditions must be met for the activity to be performed.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOperator :: Lens.Lens' Condition (Core.Maybe Types.Operator)
cOperator = Lens.field @"operator"
{-# DEPRECATED cOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

instance Core.FromJSON Condition where
  toJSON Condition {..} =
    Core.object
      ( Core.catMaybes
          [ ("Conditions" Core..=) Core.<$> conditions,
            ("Operator" Core..=) Core.<$> operator
          ]
      )

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject "Condition" Core.$
      \x ->
        Condition'
          Core.<$> (x Core..:? "Conditions") Core.<*> (x Core..:? "Operator")
