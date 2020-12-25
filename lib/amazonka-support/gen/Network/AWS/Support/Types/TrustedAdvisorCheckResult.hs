{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckResult
  ( TrustedAdvisorCheckResult (..),

    -- * Smart constructor
    mkTrustedAdvisorCheckResult,

    -- * Lenses
    tacrCheckId,
    tacrTimestamp,
    tacrStatus,
    tacrResourcesSummary,
    tacrCategorySpecificSummary,
    tacrFlaggedResources,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.String as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorResourceDetail as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorResourcesSummary as Types

-- | The results of a Trusted Advisor check returned by 'DescribeTrustedAdvisorCheckResult' .
--
-- /See:/ 'mkTrustedAdvisorCheckResult' smart constructor.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Types.String,
    -- | The time of the last refresh of the check.
    timestamp :: Types.String,
    -- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
    status :: Types.String,
    resourcesSummary :: Types.TrustedAdvisorResourcesSummary,
    -- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
    categorySpecificSummary :: Types.TrustedAdvisorCategorySpecificSummary,
    -- | The details about each resource listed in the check result.
    flaggedResources :: [Types.TrustedAdvisorResourceDetail]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCheckResult' value with any optional fields omitted.
mkTrustedAdvisorCheckResult ::
  -- | 'checkId'
  Types.String ->
  -- | 'timestamp'
  Types.String ->
  -- | 'status'
  Types.String ->
  -- | 'resourcesSummary'
  Types.TrustedAdvisorResourcesSummary ->
  -- | 'categorySpecificSummary'
  Types.TrustedAdvisorCategorySpecificSummary ->
  TrustedAdvisorCheckResult
mkTrustedAdvisorCheckResult
  checkId
  timestamp
  status
  resourcesSummary
  categorySpecificSummary =
    TrustedAdvisorCheckResult'
      { checkId,
        timestamp,
        status,
        resourcesSummary,
        categorySpecificSummary,
        flaggedResources = Core.mempty
      }

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrCheckId :: Lens.Lens' TrustedAdvisorCheckResult Types.String
tacrCheckId = Lens.field @"checkId"
{-# DEPRECATED tacrCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

-- | The time of the last refresh of the check.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrTimestamp :: Lens.Lens' TrustedAdvisorCheckResult Types.String
tacrTimestamp = Lens.field @"timestamp"
{-# DEPRECATED tacrTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrStatus :: Lens.Lens' TrustedAdvisorCheckResult Types.String
tacrStatus = Lens.field @"status"
{-# DEPRECATED tacrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourcesSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrResourcesSummary :: Lens.Lens' TrustedAdvisorCheckResult Types.TrustedAdvisorResourcesSummary
tacrResourcesSummary = Lens.field @"resourcesSummary"
{-# DEPRECATED tacrResourcesSummary "Use generic-lens or generic-optics with 'resourcesSummary' instead." #-}

-- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
--
-- /Note:/ Consider using 'categorySpecificSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrCategorySpecificSummary :: Lens.Lens' TrustedAdvisorCheckResult Types.TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary = Lens.field @"categorySpecificSummary"
{-# DEPRECATED tacrCategorySpecificSummary "Use generic-lens or generic-optics with 'categorySpecificSummary' instead." #-}

-- | The details about each resource listed in the check result.
--
-- /Note:/ Consider using 'flaggedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrFlaggedResources :: Lens.Lens' TrustedAdvisorCheckResult [Types.TrustedAdvisorResourceDetail]
tacrFlaggedResources = Lens.field @"flaggedResources"
{-# DEPRECATED tacrFlaggedResources "Use generic-lens or generic-optics with 'flaggedResources' instead." #-}

instance Core.FromJSON TrustedAdvisorCheckResult where
  parseJSON =
    Core.withObject "TrustedAdvisorCheckResult" Core.$
      \x ->
        TrustedAdvisorCheckResult'
          Core.<$> (x Core..: "checkId")
          Core.<*> (x Core..: "timestamp")
          Core.<*> (x Core..: "status")
          Core.<*> (x Core..: "resourcesSummary")
          Core.<*> (x Core..: "categorySpecificSummary")
          Core.<*> (x Core..:? "flaggedResources" Core..!= Core.mempty)
