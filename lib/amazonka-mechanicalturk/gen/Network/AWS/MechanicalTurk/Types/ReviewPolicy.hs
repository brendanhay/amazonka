{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewPolicy
  ( ReviewPolicy (..),

    -- * Smart constructor
    mkReviewPolicy,

    -- * Lenses
    rpPolicyName,
    rpParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.PolicyName as Types
import qualified Network.AWS.MechanicalTurk.Types.PolicyParameter as Types
import qualified Network.AWS.Prelude as Core

-- | HIT Review Policy data structures represent HIT review policies, which you specify when you create a HIT.
--
-- /See:/ 'mkReviewPolicy' smart constructor.
data ReviewPolicy = ReviewPolicy'
  { -- | Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
    policyName :: Types.PolicyName,
    -- | Name of the parameter from the Review policy.
    parameters :: Core.Maybe [Types.PolicyParameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReviewPolicy' value with any optional fields omitted.
mkReviewPolicy ::
  -- | 'policyName'
  Types.PolicyName ->
  ReviewPolicy
mkReviewPolicy policyName =
  ReviewPolicy' {policyName, parameters = Core.Nothing}

-- | Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPolicyName :: Lens.Lens' ReviewPolicy Types.PolicyName
rpPolicyName = Lens.field @"policyName"
{-# DEPRECATED rpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Name of the parameter from the Review policy.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpParameters :: Lens.Lens' ReviewPolicy (Core.Maybe [Types.PolicyParameter])
rpParameters = Lens.field @"parameters"
{-# DEPRECATED rpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON ReviewPolicy where
  toJSON ReviewPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyName" Core..= policyName),
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.FromJSON ReviewPolicy where
  parseJSON =
    Core.withObject "ReviewPolicy" Core.$
      \x ->
        ReviewPolicy'
          Core.<$> (x Core..: "PolicyName") Core.<*> (x Core..:? "Parameters")
