{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ConditionalSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ConditionalSplitActivity
  ( ConditionalSplitActivity (..),

    -- * Smart constructor
    mkConditionalSplitActivity,

    -- * Lenses
    csaCondition,
    csaEvaluationWaitTime,
    csaFalseActivity,
    csaTrueActivity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Condition as Types
import qualified Network.AWS.Pinpoint.Types.WaitTime as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a yes/no split activity in a journey. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
--
-- /See:/ 'mkConditionalSplitActivity' smart constructor.
data ConditionalSplitActivity = ConditionalSplitActivity'
  { -- | The conditions that define the paths for the activity, and the relationship between the conditions.
    condition :: Core.Maybe Types.Condition,
    -- | The amount of time to wait before determining whether the conditions are met, or the date and time when Amazon Pinpoint determines whether the conditions are met.
    evaluationWaitTime :: Core.Maybe Types.WaitTime,
    -- | The unique identifier for the activity to perform if the conditions aren't met.
    falseActivity :: Core.Maybe Core.Text,
    -- | The unique identifier for the activity to perform if the conditions are met.
    trueActivity :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConditionalSplitActivity' value with any optional fields omitted.
mkConditionalSplitActivity ::
  ConditionalSplitActivity
mkConditionalSplitActivity =
  ConditionalSplitActivity'
    { condition = Core.Nothing,
      evaluationWaitTime = Core.Nothing,
      falseActivity = Core.Nothing,
      trueActivity = Core.Nothing
    }

-- | The conditions that define the paths for the activity, and the relationship between the conditions.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaCondition :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Types.Condition)
csaCondition = Lens.field @"condition"
{-# DEPRECATED csaCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

-- | The amount of time to wait before determining whether the conditions are met, or the date and time when Amazon Pinpoint determines whether the conditions are met.
--
-- /Note:/ Consider using 'evaluationWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaEvaluationWaitTime :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Types.WaitTime)
csaEvaluationWaitTime = Lens.field @"evaluationWaitTime"
{-# DEPRECATED csaEvaluationWaitTime "Use generic-lens or generic-optics with 'evaluationWaitTime' instead." #-}

-- | The unique identifier for the activity to perform if the conditions aren't met.
--
-- /Note:/ Consider using 'falseActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaFalseActivity :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Core.Text)
csaFalseActivity = Lens.field @"falseActivity"
{-# DEPRECATED csaFalseActivity "Use generic-lens or generic-optics with 'falseActivity' instead." #-}

-- | The unique identifier for the activity to perform if the conditions are met.
--
-- /Note:/ Consider using 'trueActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTrueActivity :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Core.Text)
csaTrueActivity = Lens.field @"trueActivity"
{-# DEPRECATED csaTrueActivity "Use generic-lens or generic-optics with 'trueActivity' instead." #-}

instance Core.FromJSON ConditionalSplitActivity where
  toJSON ConditionalSplitActivity {..} =
    Core.object
      ( Core.catMaybes
          [ ("Condition" Core..=) Core.<$> condition,
            ("EvaluationWaitTime" Core..=) Core.<$> evaluationWaitTime,
            ("FalseActivity" Core..=) Core.<$> falseActivity,
            ("TrueActivity" Core..=) Core.<$> trueActivity
          ]
      )

instance Core.FromJSON ConditionalSplitActivity where
  parseJSON =
    Core.withObject "ConditionalSplitActivity" Core.$
      \x ->
        ConditionalSplitActivity'
          Core.<$> (x Core..:? "Condition")
          Core.<*> (x Core..:? "EvaluationWaitTime")
          Core.<*> (x Core..:? "FalseActivity")
          Core.<*> (x Core..:? "TrueActivity")
