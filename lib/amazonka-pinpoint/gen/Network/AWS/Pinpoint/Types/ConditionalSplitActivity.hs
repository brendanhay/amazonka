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
    csaEvaluationWaitTime,
    csaTrueActivity,
    csaFalseActivity,
    csaCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Condition
import Network.AWS.Pinpoint.Types.WaitTime
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a yes/no split activity in a journey. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
--
-- /See:/ 'mkConditionalSplitActivity' smart constructor.
data ConditionalSplitActivity = ConditionalSplitActivity'
  { evaluationWaitTime ::
      Lude.Maybe WaitTime,
    trueActivity :: Lude.Maybe Lude.Text,
    falseActivity :: Lude.Maybe Lude.Text,
    condition :: Lude.Maybe Condition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConditionalSplitActivity' with the minimum fields required to make a request.
--
-- * 'condition' - The conditions that define the paths for the activity, and the relationship between the conditions.
-- * 'evaluationWaitTime' - The amount of time to wait before determining whether the conditions are met, or the date and time when Amazon Pinpoint determines whether the conditions are met.
-- * 'falseActivity' - The unique identifier for the activity to perform if the conditions aren't met.
-- * 'trueActivity' - The unique identifier for the activity to perform if the conditions are met.
mkConditionalSplitActivity ::
  ConditionalSplitActivity
mkConditionalSplitActivity =
  ConditionalSplitActivity'
    { evaluationWaitTime = Lude.Nothing,
      trueActivity = Lude.Nothing,
      falseActivity = Lude.Nothing,
      condition = Lude.Nothing
    }

-- | The amount of time to wait before determining whether the conditions are met, or the date and time when Amazon Pinpoint determines whether the conditions are met.
--
-- /Note:/ Consider using 'evaluationWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaEvaluationWaitTime :: Lens.Lens' ConditionalSplitActivity (Lude.Maybe WaitTime)
csaEvaluationWaitTime = Lens.lens (evaluationWaitTime :: ConditionalSplitActivity -> Lude.Maybe WaitTime) (\s a -> s {evaluationWaitTime = a} :: ConditionalSplitActivity)
{-# DEPRECATED csaEvaluationWaitTime "Use generic-lens or generic-optics with 'evaluationWaitTime' instead." #-}

-- | The unique identifier for the activity to perform if the conditions are met.
--
-- /Note:/ Consider using 'trueActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTrueActivity :: Lens.Lens' ConditionalSplitActivity (Lude.Maybe Lude.Text)
csaTrueActivity = Lens.lens (trueActivity :: ConditionalSplitActivity -> Lude.Maybe Lude.Text) (\s a -> s {trueActivity = a} :: ConditionalSplitActivity)
{-# DEPRECATED csaTrueActivity "Use generic-lens or generic-optics with 'trueActivity' instead." #-}

-- | The unique identifier for the activity to perform if the conditions aren't met.
--
-- /Note:/ Consider using 'falseActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaFalseActivity :: Lens.Lens' ConditionalSplitActivity (Lude.Maybe Lude.Text)
csaFalseActivity = Lens.lens (falseActivity :: ConditionalSplitActivity -> Lude.Maybe Lude.Text) (\s a -> s {falseActivity = a} :: ConditionalSplitActivity)
{-# DEPRECATED csaFalseActivity "Use generic-lens or generic-optics with 'falseActivity' instead." #-}

-- | The conditions that define the paths for the activity, and the relationship between the conditions.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaCondition :: Lens.Lens' ConditionalSplitActivity (Lude.Maybe Condition)
csaCondition = Lens.lens (condition :: ConditionalSplitActivity -> Lude.Maybe Condition) (\s a -> s {condition = a} :: ConditionalSplitActivity)
{-# DEPRECATED csaCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.FromJSON ConditionalSplitActivity where
  parseJSON =
    Lude.withObject
      "ConditionalSplitActivity"
      ( \x ->
          ConditionalSplitActivity'
            Lude.<$> (x Lude..:? "EvaluationWaitTime")
            Lude.<*> (x Lude..:? "TrueActivity")
            Lude.<*> (x Lude..:? "FalseActivity")
            Lude.<*> (x Lude..:? "Condition")
      )

instance Lude.ToJSON ConditionalSplitActivity where
  toJSON ConditionalSplitActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EvaluationWaitTime" Lude..=) Lude.<$> evaluationWaitTime,
            ("TrueActivity" Lude..=) Lude.<$> trueActivity,
            ("FalseActivity" Lude..=) Lude.<$> falseActivity,
            ("Condition" Lude..=) Lude.<$> condition
          ]
      )
