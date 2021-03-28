{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.TrustedAdvisorCheckResult
  ( TrustedAdvisorCheckResult (..)
  -- * Smart constructor
  , mkTrustedAdvisorCheckResult
  -- * Lenses
  , tacrCheckId
  , tacrTimestamp
  , tacrStatus
  , tacrResourcesSummary
  , tacrCategorySpecificSummary
  , tacrFlaggedResources
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorResourceDetail as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorResourcesSummary as Types

-- | The results of a Trusted Advisor check returned by 'DescribeTrustedAdvisorCheckResult' .
--
-- /See:/ 'mkTrustedAdvisorCheckResult' smart constructor.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult'
  { checkId :: Core.Text
    -- ^ The unique identifier for the Trusted Advisor check.
  , timestamp :: Core.Text
    -- ^ The time of the last refresh of the check.
  , status :: Core.Text
    -- ^ The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
  , resourcesSummary :: Types.TrustedAdvisorResourcesSummary
  , categorySpecificSummary :: Types.TrustedAdvisorCategorySpecificSummary
    -- ^ Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
  , flaggedResources :: [Types.TrustedAdvisorResourceDetail]
    -- ^ The details about each resource listed in the check result.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCheckResult' value with any optional fields omitted.
mkTrustedAdvisorCheckResult
    :: Core.Text -- ^ 'checkId'
    -> Core.Text -- ^ 'timestamp'
    -> Core.Text -- ^ 'status'
    -> Types.TrustedAdvisorResourcesSummary -- ^ 'resourcesSummary'
    -> Types.TrustedAdvisorCategorySpecificSummary -- ^ 'categorySpecificSummary'
    -> TrustedAdvisorCheckResult
mkTrustedAdvisorCheckResult checkId timestamp status
  resourcesSummary categorySpecificSummary
  = TrustedAdvisorCheckResult'{checkId, timestamp, status,
                               resourcesSummary, categorySpecificSummary,
                               flaggedResources = Core.mempty}

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrCheckId :: Lens.Lens' TrustedAdvisorCheckResult Core.Text
tacrCheckId = Lens.field @"checkId"
{-# INLINEABLE tacrCheckId #-}
{-# DEPRECATED checkId "Use generic-lens or generic-optics with 'checkId' instead"  #-}

-- | The time of the last refresh of the check.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrTimestamp :: Lens.Lens' TrustedAdvisorCheckResult Core.Text
tacrTimestamp = Lens.field @"timestamp"
{-# INLINEABLE tacrTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrStatus :: Lens.Lens' TrustedAdvisorCheckResult Core.Text
tacrStatus = Lens.field @"status"
{-# INLINEABLE tacrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourcesSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrResourcesSummary :: Lens.Lens' TrustedAdvisorCheckResult Types.TrustedAdvisorResourcesSummary
tacrResourcesSummary = Lens.field @"resourcesSummary"
{-# INLINEABLE tacrResourcesSummary #-}
{-# DEPRECATED resourcesSummary "Use generic-lens or generic-optics with 'resourcesSummary' instead"  #-}

-- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
--
-- /Note:/ Consider using 'categorySpecificSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrCategorySpecificSummary :: Lens.Lens' TrustedAdvisorCheckResult Types.TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary = Lens.field @"categorySpecificSummary"
{-# INLINEABLE tacrCategorySpecificSummary #-}
{-# DEPRECATED categorySpecificSummary "Use generic-lens or generic-optics with 'categorySpecificSummary' instead"  #-}

-- | The details about each resource listed in the check result.
--
-- /Note:/ Consider using 'flaggedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrFlaggedResources :: Lens.Lens' TrustedAdvisorCheckResult [Types.TrustedAdvisorResourceDetail]
tacrFlaggedResources = Lens.field @"flaggedResources"
{-# INLINEABLE tacrFlaggedResources #-}
{-# DEPRECATED flaggedResources "Use generic-lens or generic-optics with 'flaggedResources' instead"  #-}

instance Core.FromJSON TrustedAdvisorCheckResult where
        parseJSON
          = Core.withObject "TrustedAdvisorCheckResult" Core.$
              \ x ->
                TrustedAdvisorCheckResult' Core.<$>
                  (x Core..: "checkId") Core.<*> x Core..: "timestamp" Core.<*>
                    x Core..: "status"
                    Core.<*> x Core..: "resourcesSummary"
                    Core.<*> x Core..: "categorySpecificSummary"
                    Core.<*> x Core..:? "flaggedResources" Core..!= Core.mempty
