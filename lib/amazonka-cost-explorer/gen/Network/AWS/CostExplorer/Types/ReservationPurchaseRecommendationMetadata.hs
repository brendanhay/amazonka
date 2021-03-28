{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
  ( ReservationPurchaseRecommendationMetadata (..)
  -- * Smart constructor
  , mkReservationPurchaseRecommendationMetadata
  -- * Lenses
  , rprmGenerationTimestamp
  , rprmRecommendationId
  ) where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about this specific recommendation, such as the timestamp for when AWS made a specific recommendation.
--
-- /See:/ 'mkReservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { generationTimestamp :: Core.Maybe Types.GenericString
    -- ^ The timestamp for when AWS made this recommendation.
  , recommendationId :: Core.Maybe Types.GenericString
    -- ^ The ID for this specific recommendation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationPurchaseRecommendationMetadata' value with any optional fields omitted.
mkReservationPurchaseRecommendationMetadata
    :: ReservationPurchaseRecommendationMetadata
mkReservationPurchaseRecommendationMetadata
  = ReservationPurchaseRecommendationMetadata'{generationTimestamp =
                                                 Core.Nothing,
                                               recommendationId = Core.Nothing}

-- | The timestamp for when AWS made this recommendation.
--
-- /Note:/ Consider using 'generationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprmGenerationTimestamp :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Core.Maybe Types.GenericString)
rprmGenerationTimestamp = Lens.field @"generationTimestamp"
{-# INLINEABLE rprmGenerationTimestamp #-}
{-# DEPRECATED generationTimestamp "Use generic-lens or generic-optics with 'generationTimestamp' instead"  #-}

-- | The ID for this specific recommendation.
--
-- /Note:/ Consider using 'recommendationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprmRecommendationId :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Core.Maybe Types.GenericString)
rprmRecommendationId = Lens.field @"recommendationId"
{-# INLINEABLE rprmRecommendationId #-}
{-# DEPRECATED recommendationId "Use generic-lens or generic-optics with 'recommendationId' instead"  #-}

instance Core.FromJSON ReservationPurchaseRecommendationMetadata
         where
        parseJSON
          = Core.withObject "ReservationPurchaseRecommendationMetadata"
              Core.$
              \ x ->
                ReservationPurchaseRecommendationMetadata' Core.<$>
                  (x Core..:? "GenerationTimestamp") Core.<*>
                    x Core..:? "RecommendationId"
