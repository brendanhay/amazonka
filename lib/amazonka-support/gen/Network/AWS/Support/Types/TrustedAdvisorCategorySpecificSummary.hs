{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
  ( TrustedAdvisorCategorySpecificSummary (..)
  -- * Smart constructor
  , mkTrustedAdvisorCategorySpecificSummary
  -- * Lenses
  , tacssCostOptimizing
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary as Types

-- | The container for summary information that relates to the category of the Trusted Advisor check.
--
-- /See:/ 'mkTrustedAdvisorCategorySpecificSummary' smart constructor.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
  { costOptimizing :: Core.Maybe Types.TrustedAdvisorCostOptimizingSummary
    -- ^ The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCategorySpecificSummary' value with any optional fields omitted.
mkTrustedAdvisorCategorySpecificSummary
    :: TrustedAdvisorCategorySpecificSummary
mkTrustedAdvisorCategorySpecificSummary
  = TrustedAdvisorCategorySpecificSummary'{costOptimizing =
                                             Core.Nothing}

-- | The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
--
-- /Note:/ Consider using 'costOptimizing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacssCostOptimizing :: Lens.Lens' TrustedAdvisorCategorySpecificSummary (Core.Maybe Types.TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing = Lens.field @"costOptimizing"
{-# INLINEABLE tacssCostOptimizing #-}
{-# DEPRECATED costOptimizing "Use generic-lens or generic-optics with 'costOptimizing' instead"  #-}

instance Core.FromJSON TrustedAdvisorCategorySpecificSummary where
        parseJSON
          = Core.withObject "TrustedAdvisorCategorySpecificSummary" Core.$
              \ x ->
                TrustedAdvisorCategorySpecificSummary' Core.<$>
                  (x Core..:? "costOptimizing")
