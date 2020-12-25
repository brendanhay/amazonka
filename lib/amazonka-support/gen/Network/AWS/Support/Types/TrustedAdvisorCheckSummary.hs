{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckSummary
  ( TrustedAdvisorCheckSummary (..),

    -- * Smart constructor
    mkTrustedAdvisorCheckSummary,

    -- * Lenses
    tacsCheckId,
    tacsTimestamp,
    tacsStatus,
    tacsResourcesSummary,
    tacsCategorySpecificSummary,
    tacsHasFlaggedResources,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.String as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary as Types
import qualified Network.AWS.Support.Types.TrustedAdvisorResourcesSummary as Types

-- | A summary of a Trusted Advisor check result, including the alert status, last refresh, and number of resources examined.
--
-- /See:/ 'mkTrustedAdvisorCheckSummary' smart constructor.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Types.String,
    -- | The time of the last refresh of the check.
    timestamp :: Types.String,
    -- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
    status :: Types.String,
    resourcesSummary :: Types.TrustedAdvisorResourcesSummary,
    -- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
    categorySpecificSummary :: Types.TrustedAdvisorCategorySpecificSummary,
    -- | Specifies whether the Trusted Advisor check has flagged resources.
    hasFlaggedResources :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCheckSummary' value with any optional fields omitted.
mkTrustedAdvisorCheckSummary ::
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
  TrustedAdvisorCheckSummary
mkTrustedAdvisorCheckSummary
  checkId
  timestamp
  status
  resourcesSummary
  categorySpecificSummary =
    TrustedAdvisorCheckSummary'
      { checkId,
        timestamp,
        status,
        resourcesSummary,
        categorySpecificSummary,
        hasFlaggedResources = Core.Nothing
      }

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsCheckId :: Lens.Lens' TrustedAdvisorCheckSummary Types.String
tacsCheckId = Lens.field @"checkId"
{-# DEPRECATED tacsCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

-- | The time of the last refresh of the check.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsTimestamp :: Lens.Lens' TrustedAdvisorCheckSummary Types.String
tacsTimestamp = Lens.field @"timestamp"
{-# DEPRECATED tacsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsStatus :: Lens.Lens' TrustedAdvisorCheckSummary Types.String
tacsStatus = Lens.field @"status"
{-# DEPRECATED tacsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourcesSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsResourcesSummary :: Lens.Lens' TrustedAdvisorCheckSummary Types.TrustedAdvisorResourcesSummary
tacsResourcesSummary = Lens.field @"resourcesSummary"
{-# DEPRECATED tacsResourcesSummary "Use generic-lens or generic-optics with 'resourcesSummary' instead." #-}

-- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
--
-- /Note:/ Consider using 'categorySpecificSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsCategorySpecificSummary :: Lens.Lens' TrustedAdvisorCheckSummary Types.TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary = Lens.field @"categorySpecificSummary"
{-# DEPRECATED tacsCategorySpecificSummary "Use generic-lens or generic-optics with 'categorySpecificSummary' instead." #-}

-- | Specifies whether the Trusted Advisor check has flagged resources.
--
-- /Note:/ Consider using 'hasFlaggedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsHasFlaggedResources :: Lens.Lens' TrustedAdvisorCheckSummary (Core.Maybe Core.Bool)
tacsHasFlaggedResources = Lens.field @"hasFlaggedResources"
{-# DEPRECATED tacsHasFlaggedResources "Use generic-lens or generic-optics with 'hasFlaggedResources' instead." #-}

instance Core.FromJSON TrustedAdvisorCheckSummary where
  parseJSON =
    Core.withObject "TrustedAdvisorCheckSummary" Core.$
      \x ->
        TrustedAdvisorCheckSummary'
          Core.<$> (x Core..: "checkId")
          Core.<*> (x Core..: "timestamp")
          Core.<*> (x Core..: "status")
          Core.<*> (x Core..: "resourcesSummary")
          Core.<*> (x Core..: "categorySpecificSummary")
          Core.<*> (x Core..:? "hasFlaggedResources")
