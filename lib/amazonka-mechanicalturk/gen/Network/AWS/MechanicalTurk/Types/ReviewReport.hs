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
import qualified Network.AWS.MechanicalTurk.Types.ReviewActionDetail as Types
import qualified Network.AWS.MechanicalTurk.Types.ReviewResultDetail as Types
import qualified Network.AWS.Prelude as Core

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
-- /See:/ 'mkReviewReport' smart constructor.
data ReviewReport = ReviewReport'
  { -- | A list of ReviewAction objects for each action specified in the Review Policy.
    reviewActions :: Core.Maybe [Types.ReviewActionDetail],
    -- | A list of ReviewResults objects for each action specified in the Review Policy.
    reviewResults :: Core.Maybe [Types.ReviewResultDetail]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReviewReport' value with any optional fields omitted.
mkReviewReport ::
  ReviewReport
mkReviewReport =
  ReviewReport'
    { reviewActions = Core.Nothing,
      reviewResults = Core.Nothing
    }

-- | A list of ReviewAction objects for each action specified in the Review Policy.
--
-- /Note:/ Consider using 'reviewActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrReviewActions :: Lens.Lens' ReviewReport (Core.Maybe [Types.ReviewActionDetail])
rrReviewActions = Lens.field @"reviewActions"
{-# DEPRECATED rrReviewActions "Use generic-lens or generic-optics with 'reviewActions' instead." #-}

-- | A list of ReviewResults objects for each action specified in the Review Policy.
--
-- /Note:/ Consider using 'reviewResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrReviewResults :: Lens.Lens' ReviewReport (Core.Maybe [Types.ReviewResultDetail])
rrReviewResults = Lens.field @"reviewResults"
{-# DEPRECATED rrReviewResults "Use generic-lens or generic-optics with 'reviewResults' instead." #-}

instance Core.FromJSON ReviewReport where
  parseJSON =
    Core.withObject "ReviewReport" Core.$
      \x ->
        ReviewReport'
          Core.<$> (x Core..:? "ReviewActions") Core.<*> (x Core..:? "ReviewResults")
