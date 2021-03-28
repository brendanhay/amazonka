{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
  ( RightsizingRecommendationMetadata (..)
  -- * Smart constructor
  , mkRightsizingRecommendationMetadata
  -- * Lenses
  , rrmGenerationTimestamp
  , rrmLookbackPeriodInDays
  , rrmRecommendationId
  ) where

import qualified Network.AWS.CostExplorer.Types.GenerationTimestamp as Types
import qualified Network.AWS.CostExplorer.Types.LookbackPeriodInDays as Types
import qualified Network.AWS.CostExplorer.Types.RecommendationId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metadata for this recommendation set.
--
-- /See:/ 'mkRightsizingRecommendationMetadata' smart constructor.
data RightsizingRecommendationMetadata = RightsizingRecommendationMetadata'
  { generationTimestamp :: Core.Maybe Types.GenerationTimestamp
    -- ^ The timestamp for when AWS made this recommendation.
  , lookbackPeriodInDays :: Core.Maybe Types.LookbackPeriodInDays
    -- ^ How many days of previous usage that AWS considers when making this recommendation.
  , recommendationId :: Core.Maybe Types.RecommendationId
    -- ^ The ID for this specific recommendation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RightsizingRecommendationMetadata' value with any optional fields omitted.
mkRightsizingRecommendationMetadata
    :: RightsizingRecommendationMetadata
mkRightsizingRecommendationMetadata
  = RightsizingRecommendationMetadata'{generationTimestamp =
                                         Core.Nothing,
                                       lookbackPeriodInDays = Core.Nothing,
                                       recommendationId = Core.Nothing}

-- | The timestamp for when AWS made this recommendation.
--
-- /Note:/ Consider using 'generationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrmGenerationTimestamp :: Lens.Lens' RightsizingRecommendationMetadata (Core.Maybe Types.GenerationTimestamp)
rrmGenerationTimestamp = Lens.field @"generationTimestamp"
{-# INLINEABLE rrmGenerationTimestamp #-}
{-# DEPRECATED generationTimestamp "Use generic-lens or generic-optics with 'generationTimestamp' instead"  #-}

-- | How many days of previous usage that AWS considers when making this recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrmLookbackPeriodInDays :: Lens.Lens' RightsizingRecommendationMetadata (Core.Maybe Types.LookbackPeriodInDays)
rrmLookbackPeriodInDays = Lens.field @"lookbackPeriodInDays"
{-# INLINEABLE rrmLookbackPeriodInDays #-}
{-# DEPRECATED lookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead"  #-}

-- | The ID for this specific recommendation.
--
-- /Note:/ Consider using 'recommendationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrmRecommendationId :: Lens.Lens' RightsizingRecommendationMetadata (Core.Maybe Types.RecommendationId)
rrmRecommendationId = Lens.field @"recommendationId"
{-# INLINEABLE rrmRecommendationId #-}
{-# DEPRECATED recommendationId "Use generic-lens or generic-optics with 'recommendationId' instead"  #-}

instance Core.FromJSON RightsizingRecommendationMetadata where
        parseJSON
          = Core.withObject "RightsizingRecommendationMetadata" Core.$
              \ x ->
                RightsizingRecommendationMetadata' Core.<$>
                  (x Core..:? "GenerationTimestamp") Core.<*>
                    x Core..:? "LookbackPeriodInDays"
                    Core.<*> x Core..:? "RecommendationId"
