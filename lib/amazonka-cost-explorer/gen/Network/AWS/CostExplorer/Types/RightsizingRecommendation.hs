{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendation
  ( RightsizingRecommendation (..),

    -- * Smart constructor
    mkRightsizingRecommendation,

    -- * Lenses
    rrAccountId,
    rrCurrentInstance,
    rrModifyRecommendationDetail,
    rrRightsizingType,
    rrTerminateRecommendationDetail,
  )
where

import qualified Network.AWS.CostExplorer.Types.CurrentInstance as Types
import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.CostExplorer.Types.ModifyRecommendationDetail as Types
import qualified Network.AWS.CostExplorer.Types.RightsizingType as Types
import qualified Network.AWS.CostExplorer.Types.TerminateRecommendationDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Recommendations to rightsize resources.
--
-- /See:/ 'mkRightsizingRecommendation' smart constructor.
data RightsizingRecommendation = RightsizingRecommendation'
  { -- | The account that this recommendation is for.
    accountId :: Core.Maybe Types.GenericString,
    -- | Context regarding the current instance.
    currentInstance :: Core.Maybe Types.CurrentInstance,
    -- | Details for modification recommendations.
    modifyRecommendationDetail :: Core.Maybe Types.ModifyRecommendationDetail,
    -- | Recommendation to either terminate or modify the resource.
    rightsizingType :: Core.Maybe Types.RightsizingType,
    -- | Details for termination recommendations.
    terminateRecommendationDetail :: Core.Maybe Types.TerminateRecommendationDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RightsizingRecommendation' value with any optional fields omitted.
mkRightsizingRecommendation ::
  RightsizingRecommendation
mkRightsizingRecommendation =
  RightsizingRecommendation'
    { accountId = Core.Nothing,
      currentInstance = Core.Nothing,
      modifyRecommendationDetail = Core.Nothing,
      rightsizingType = Core.Nothing,
      terminateRecommendationDetail = Core.Nothing
    }

-- | The account that this recommendation is for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrAccountId :: Lens.Lens' RightsizingRecommendation (Core.Maybe Types.GenericString)
rrAccountId = Lens.field @"accountId"
{-# DEPRECATED rrAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Context regarding the current instance.
--
-- /Note:/ Consider using 'currentInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCurrentInstance :: Lens.Lens' RightsizingRecommendation (Core.Maybe Types.CurrentInstance)
rrCurrentInstance = Lens.field @"currentInstance"
{-# DEPRECATED rrCurrentInstance "Use generic-lens or generic-optics with 'currentInstance' instead." #-}

-- | Details for modification recommendations.
--
-- /Note:/ Consider using 'modifyRecommendationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrModifyRecommendationDetail :: Lens.Lens' RightsizingRecommendation (Core.Maybe Types.ModifyRecommendationDetail)
rrModifyRecommendationDetail = Lens.field @"modifyRecommendationDetail"
{-# DEPRECATED rrModifyRecommendationDetail "Use generic-lens or generic-optics with 'modifyRecommendationDetail' instead." #-}

-- | Recommendation to either terminate or modify the resource.
--
-- /Note:/ Consider using 'rightsizingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRightsizingType :: Lens.Lens' RightsizingRecommendation (Core.Maybe Types.RightsizingType)
rrRightsizingType = Lens.field @"rightsizingType"
{-# DEPRECATED rrRightsizingType "Use generic-lens or generic-optics with 'rightsizingType' instead." #-}

-- | Details for termination recommendations.
--
-- /Note:/ Consider using 'terminateRecommendationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTerminateRecommendationDetail :: Lens.Lens' RightsizingRecommendation (Core.Maybe Types.TerminateRecommendationDetail)
rrTerminateRecommendationDetail = Lens.field @"terminateRecommendationDetail"
{-# DEPRECATED rrTerminateRecommendationDetail "Use generic-lens or generic-optics with 'terminateRecommendationDetail' instead." #-}

instance Core.FromJSON RightsizingRecommendation where
  parseJSON =
    Core.withObject "RightsizingRecommendation" Core.$
      \x ->
        RightsizingRecommendation'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "CurrentInstance")
          Core.<*> (x Core..:? "ModifyRecommendationDetail")
          Core.<*> (x Core..:? "RightsizingType")
          Core.<*> (x Core..:? "TerminateRecommendationDetail")
