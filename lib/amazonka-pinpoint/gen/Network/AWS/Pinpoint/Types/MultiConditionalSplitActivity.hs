{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity
  ( MultiConditionalSplitActivity (..),

    -- * Smart constructor
    mkMultiConditionalSplitActivity,

    -- * Lenses
    mcsaBranches,
    mcsaDefaultActivity,
    mcsaEvaluationWaitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.MultiConditionalBranch as Types
import qualified Network.AWS.Pinpoint.Types.WaitTime as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a multivariate split activity in a journey. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
--
-- /See:/ 'mkMultiConditionalSplitActivity' smart constructor.
data MultiConditionalSplitActivity = MultiConditionalSplitActivity'
  { -- | The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
    branches :: Core.Maybe [Types.MultiConditionalBranch],
    -- | The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
    defaultActivity :: Core.Maybe Core.Text,
    -- | The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
    evaluationWaitTime :: Core.Maybe Types.WaitTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiConditionalSplitActivity' value with any optional fields omitted.
mkMultiConditionalSplitActivity ::
  MultiConditionalSplitActivity
mkMultiConditionalSplitActivity =
  MultiConditionalSplitActivity'
    { branches = Core.Nothing,
      defaultActivity = Core.Nothing,
      evaluationWaitTime = Core.Nothing
    }

-- | The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsaBranches :: Lens.Lens' MultiConditionalSplitActivity (Core.Maybe [Types.MultiConditionalBranch])
mcsaBranches = Lens.field @"branches"
{-# DEPRECATED mcsaBranches "Use generic-lens or generic-optics with 'branches' instead." #-}

-- | The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
--
-- /Note:/ Consider using 'defaultActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsaDefaultActivity :: Lens.Lens' MultiConditionalSplitActivity (Core.Maybe Core.Text)
mcsaDefaultActivity = Lens.field @"defaultActivity"
{-# DEPRECATED mcsaDefaultActivity "Use generic-lens or generic-optics with 'defaultActivity' instead." #-}

-- | The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
--
-- /Note:/ Consider using 'evaluationWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsaEvaluationWaitTime :: Lens.Lens' MultiConditionalSplitActivity (Core.Maybe Types.WaitTime)
mcsaEvaluationWaitTime = Lens.field @"evaluationWaitTime"
{-# DEPRECATED mcsaEvaluationWaitTime "Use generic-lens or generic-optics with 'evaluationWaitTime' instead." #-}

instance Core.FromJSON MultiConditionalSplitActivity where
  toJSON MultiConditionalSplitActivity {..} =
    Core.object
      ( Core.catMaybes
          [ ("Branches" Core..=) Core.<$> branches,
            ("DefaultActivity" Core..=) Core.<$> defaultActivity,
            ("EvaluationWaitTime" Core..=) Core.<$> evaluationWaitTime
          ]
      )

instance Core.FromJSON MultiConditionalSplitActivity where
  parseJSON =
    Core.withObject "MultiConditionalSplitActivity" Core.$
      \x ->
        MultiConditionalSplitActivity'
          Core.<$> (x Core..:? "Branches")
          Core.<*> (x Core..:? "DefaultActivity")
          Core.<*> (x Core..:? "EvaluationWaitTime")
