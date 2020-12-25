{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MultiConditionalBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MultiConditionalBranch
  ( MultiConditionalBranch (..),

    -- * Smart constructor
    mkMultiConditionalBranch,

    -- * Lenses
    mcbCondition,
    mcbNextActivity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SimpleCondition as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a condition to evaluate for an activity path in a journey.
--
-- /See:/ 'mkMultiConditionalBranch' smart constructor.
data MultiConditionalBranch = MultiConditionalBranch'
  { -- | The condition to evaluate for the activity path.
    condition :: Core.Maybe Types.SimpleCondition,
    -- | The unique identifier for the next activity to perform, after completing the activity for the path.
    nextActivity :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiConditionalBranch' value with any optional fields omitted.
mkMultiConditionalBranch ::
  MultiConditionalBranch
mkMultiConditionalBranch =
  MultiConditionalBranch'
    { condition = Core.Nothing,
      nextActivity = Core.Nothing
    }

-- | The condition to evaluate for the activity path.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcbCondition :: Lens.Lens' MultiConditionalBranch (Core.Maybe Types.SimpleCondition)
mcbCondition = Lens.field @"condition"
{-# DEPRECATED mcbCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

-- | The unique identifier for the next activity to perform, after completing the activity for the path.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcbNextActivity :: Lens.Lens' MultiConditionalBranch (Core.Maybe Core.Text)
mcbNextActivity = Lens.field @"nextActivity"
{-# DEPRECATED mcbNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

instance Core.FromJSON MultiConditionalBranch where
  toJSON MultiConditionalBranch {..} =
    Core.object
      ( Core.catMaybes
          [ ("Condition" Core..=) Core.<$> condition,
            ("NextActivity" Core..=) Core.<$> nextActivity
          ]
      )

instance Core.FromJSON MultiConditionalBranch where
  parseJSON =
    Core.withObject "MultiConditionalBranch" Core.$
      \x ->
        MultiConditionalBranch'
          Core.<$> (x Core..:? "Condition") Core.<*> (x Core..:? "NextActivity")
