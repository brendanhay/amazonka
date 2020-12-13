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
    mcsaEvaluationWaitTime,
    mcsaDefaultActivity,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MultiConditionalBranch
import Network.AWS.Pinpoint.Types.WaitTime
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a multivariate split activity in a journey. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
--
-- /See:/ 'mkMultiConditionalSplitActivity' smart constructor.
data MultiConditionalSplitActivity = MultiConditionalSplitActivity'
  { -- | The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
    branches :: Lude.Maybe [MultiConditionalBranch],
    -- | The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
    evaluationWaitTime :: Lude.Maybe WaitTime,
    -- | The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
    defaultActivity :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiConditionalSplitActivity' with the minimum fields required to make a request.
--
-- * 'branches' - The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
-- * 'evaluationWaitTime' - The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
-- * 'defaultActivity' - The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
mkMultiConditionalSplitActivity ::
  MultiConditionalSplitActivity
mkMultiConditionalSplitActivity =
  MultiConditionalSplitActivity'
    { branches = Lude.Nothing,
      evaluationWaitTime = Lude.Nothing,
      defaultActivity = Lude.Nothing
    }

-- | The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsaBranches :: Lens.Lens' MultiConditionalSplitActivity (Lude.Maybe [MultiConditionalBranch])
mcsaBranches = Lens.lens (branches :: MultiConditionalSplitActivity -> Lude.Maybe [MultiConditionalBranch]) (\s a -> s {branches = a} :: MultiConditionalSplitActivity)
{-# DEPRECATED mcsaBranches "Use generic-lens or generic-optics with 'branches' instead." #-}

-- | The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
--
-- /Note:/ Consider using 'evaluationWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsaEvaluationWaitTime :: Lens.Lens' MultiConditionalSplitActivity (Lude.Maybe WaitTime)
mcsaEvaluationWaitTime = Lens.lens (evaluationWaitTime :: MultiConditionalSplitActivity -> Lude.Maybe WaitTime) (\s a -> s {evaluationWaitTime = a} :: MultiConditionalSplitActivity)
{-# DEPRECATED mcsaEvaluationWaitTime "Use generic-lens or generic-optics with 'evaluationWaitTime' instead." #-}

-- | The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
--
-- /Note:/ Consider using 'defaultActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsaDefaultActivity :: Lens.Lens' MultiConditionalSplitActivity (Lude.Maybe Lude.Text)
mcsaDefaultActivity = Lens.lens (defaultActivity :: MultiConditionalSplitActivity -> Lude.Maybe Lude.Text) (\s a -> s {defaultActivity = a} :: MultiConditionalSplitActivity)
{-# DEPRECATED mcsaDefaultActivity "Use generic-lens or generic-optics with 'defaultActivity' instead." #-}

instance Lude.FromJSON MultiConditionalSplitActivity where
  parseJSON =
    Lude.withObject
      "MultiConditionalSplitActivity"
      ( \x ->
          MultiConditionalSplitActivity'
            Lude.<$> (x Lude..:? "Branches" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EvaluationWaitTime")
            Lude.<*> (x Lude..:? "DefaultActivity")
      )

instance Lude.ToJSON MultiConditionalSplitActivity where
  toJSON MultiConditionalSplitActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Branches" Lude..=) Lude.<$> branches,
            ("EvaluationWaitTime" Lude..=) Lude.<$> evaluationWaitTime,
            ("DefaultActivity" Lude..=) Lude.<$> defaultActivity
          ]
      )
