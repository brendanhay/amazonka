{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewReport
  ( ReviewReport (..),

    -- * Smart constructor
    mkReviewReport,

    -- * Lenses
    rrReviewActions,
    rrReviewResults,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ReviewActionDetail
import Network.AWS.MechanicalTurk.Types.ReviewResultDetail
import qualified Network.AWS.Prelude as Lude

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
-- /See:/ 'mkReviewReport' smart constructor.
data ReviewReport = ReviewReport'
  { reviewActions ::
      Lude.Maybe [ReviewActionDetail],
    reviewResults :: Lude.Maybe [ReviewResultDetail]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReviewReport' with the minimum fields required to make a request.
--
-- * 'reviewActions' - A list of ReviewAction objects for each action specified in the Review Policy.
-- * 'reviewResults' - A list of ReviewResults objects for each action specified in the Review Policy.
mkReviewReport ::
  ReviewReport
mkReviewReport =
  ReviewReport'
    { reviewActions = Lude.Nothing,
      reviewResults = Lude.Nothing
    }

-- | A list of ReviewAction objects for each action specified in the Review Policy.
--
-- /Note:/ Consider using 'reviewActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrReviewActions :: Lens.Lens' ReviewReport (Lude.Maybe [ReviewActionDetail])
rrReviewActions = Lens.lens (reviewActions :: ReviewReport -> Lude.Maybe [ReviewActionDetail]) (\s a -> s {reviewActions = a} :: ReviewReport)
{-# DEPRECATED rrReviewActions "Use generic-lens or generic-optics with 'reviewActions' instead." #-}

-- | A list of ReviewResults objects for each action specified in the Review Policy.
--
-- /Note:/ Consider using 'reviewResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrReviewResults :: Lens.Lens' ReviewReport (Lude.Maybe [ReviewResultDetail])
rrReviewResults = Lens.lens (reviewResults :: ReviewReport -> Lude.Maybe [ReviewResultDetail]) (\s a -> s {reviewResults = a} :: ReviewReport)
{-# DEPRECATED rrReviewResults "Use generic-lens or generic-optics with 'reviewResults' instead." #-}

instance Lude.FromJSON ReviewReport where
  parseJSON =
    Lude.withObject
      "ReviewReport"
      ( \x ->
          ReviewReport'
            Lude.<$> (x Lude..:? "ReviewActions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ReviewResults" Lude..!= Lude.mempty)
      )
