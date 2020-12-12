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
    mcbNextActivity,
    mcbCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SimpleCondition
import qualified Network.AWS.Prelude as Lude

-- | Specifies a condition to evaluate for an activity path in a journey.
--
-- /See:/ 'mkMultiConditionalBranch' smart constructor.
data MultiConditionalBranch = MultiConditionalBranch'
  { nextActivity ::
      Lude.Maybe Lude.Text,
    condition :: Lude.Maybe SimpleCondition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiConditionalBranch' with the minimum fields required to make a request.
--
-- * 'condition' - The condition to evaluate for the activity path.
-- * 'nextActivity' - The unique identifier for the next activity to perform, after completing the activity for the path.
mkMultiConditionalBranch ::
  MultiConditionalBranch
mkMultiConditionalBranch =
  MultiConditionalBranch'
    { nextActivity = Lude.Nothing,
      condition = Lude.Nothing
    }

-- | The unique identifier for the next activity to perform, after completing the activity for the path.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcbNextActivity :: Lens.Lens' MultiConditionalBranch (Lude.Maybe Lude.Text)
mcbNextActivity = Lens.lens (nextActivity :: MultiConditionalBranch -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: MultiConditionalBranch)
{-# DEPRECATED mcbNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The condition to evaluate for the activity path.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcbCondition :: Lens.Lens' MultiConditionalBranch (Lude.Maybe SimpleCondition)
mcbCondition = Lens.lens (condition :: MultiConditionalBranch -> Lude.Maybe SimpleCondition) (\s a -> s {condition = a} :: MultiConditionalBranch)
{-# DEPRECATED mcbCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.FromJSON MultiConditionalBranch where
  parseJSON =
    Lude.withObject
      "MultiConditionalBranch"
      ( \x ->
          MultiConditionalBranch'
            Lude.<$> (x Lude..:? "NextActivity") Lude.<*> (x Lude..:? "Condition")
      )

instance Lude.ToJSON MultiConditionalBranch where
  toJSON MultiConditionalBranch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("Condition" Lude..=) Lude.<$> condition
          ]
      )
