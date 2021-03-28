{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourcesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.TrustedAdvisorResourcesSummary
  ( TrustedAdvisorResourcesSummary (..)
  -- * Smart constructor
  , mkTrustedAdvisorResourcesSummary
  -- * Lenses
  , tarsResourcesProcessed
  , tarsResourcesFlagged
  , tarsResourcesIgnored
  , tarsResourcesSuppressed
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor 'DescribeTrustedAdvisorCheckSummaries' .
--
-- /See:/ 'mkTrustedAdvisorResourcesSummary' smart constructor.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
  { resourcesProcessed :: Core.Integer
    -- ^ The number of AWS resources that were analyzed by the Trusted Advisor check.
  , resourcesFlagged :: Core.Integer
    -- ^ The number of AWS resources that were flagged (listed) by the Trusted Advisor check.
  , resourcesIgnored :: Core.Integer
    -- ^ The number of AWS resources ignored by Trusted Advisor because information was unavailable.
  , resourcesSuppressed :: Core.Integer
    -- ^ The number of AWS resources ignored by Trusted Advisor because they were marked as suppressed by the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorResourcesSummary' value with any optional fields omitted.
mkTrustedAdvisorResourcesSummary
    :: Core.Integer -- ^ 'resourcesProcessed'
    -> Core.Integer -- ^ 'resourcesFlagged'
    -> Core.Integer -- ^ 'resourcesIgnored'
    -> Core.Integer -- ^ 'resourcesSuppressed'
    -> TrustedAdvisorResourcesSummary
mkTrustedAdvisorResourcesSummary resourcesProcessed
  resourcesFlagged resourcesIgnored resourcesSuppressed
  = TrustedAdvisorResourcesSummary'{resourcesProcessed,
                                    resourcesFlagged, resourcesIgnored, resourcesSuppressed}

-- | The number of AWS resources that were analyzed by the Trusted Advisor check.
--
-- /Note:/ Consider using 'resourcesProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesProcessed :: Lens.Lens' TrustedAdvisorResourcesSummary Core.Integer
tarsResourcesProcessed = Lens.field @"resourcesProcessed"
{-# INLINEABLE tarsResourcesProcessed #-}
{-# DEPRECATED resourcesProcessed "Use generic-lens or generic-optics with 'resourcesProcessed' instead"  #-}

-- | The number of AWS resources that were flagged (listed) by the Trusted Advisor check.
--
-- /Note:/ Consider using 'resourcesFlagged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesFlagged :: Lens.Lens' TrustedAdvisorResourcesSummary Core.Integer
tarsResourcesFlagged = Lens.field @"resourcesFlagged"
{-# INLINEABLE tarsResourcesFlagged #-}
{-# DEPRECATED resourcesFlagged "Use generic-lens or generic-optics with 'resourcesFlagged' instead"  #-}

-- | The number of AWS resources ignored by Trusted Advisor because information was unavailable.
--
-- /Note:/ Consider using 'resourcesIgnored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesIgnored :: Lens.Lens' TrustedAdvisorResourcesSummary Core.Integer
tarsResourcesIgnored = Lens.field @"resourcesIgnored"
{-# INLINEABLE tarsResourcesIgnored #-}
{-# DEPRECATED resourcesIgnored "Use generic-lens or generic-optics with 'resourcesIgnored' instead"  #-}

-- | The number of AWS resources ignored by Trusted Advisor because they were marked as suppressed by the user.
--
-- /Note:/ Consider using 'resourcesSuppressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesSuppressed :: Lens.Lens' TrustedAdvisorResourcesSummary Core.Integer
tarsResourcesSuppressed = Lens.field @"resourcesSuppressed"
{-# INLINEABLE tarsResourcesSuppressed #-}
{-# DEPRECATED resourcesSuppressed "Use generic-lens or generic-optics with 'resourcesSuppressed' instead"  #-}

instance Core.FromJSON TrustedAdvisorResourcesSummary where
        parseJSON
          = Core.withObject "TrustedAdvisorResourcesSummary" Core.$
              \ x ->
                TrustedAdvisorResourcesSummary' Core.<$>
                  (x Core..: "resourcesProcessed") Core.<*>
                    x Core..: "resourcesFlagged"
                    Core.<*> x Core..: "resourcesIgnored"
                    Core.<*> x Core..: "resourcesSuppressed"
