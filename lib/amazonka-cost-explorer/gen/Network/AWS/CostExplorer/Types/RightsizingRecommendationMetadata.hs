{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
  ( RightsizingRecommendationMetadata (..),

    -- * Smart constructor
    mkRightsizingRecommendationMetadata,

    -- * Lenses
    rrmRecommendationId,
    rrmGenerationTimestamp,
    rrmLookbackPeriodInDays,
  )
where

import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metadata for this recommendation set.
--
-- /See:/ 'mkRightsizingRecommendationMetadata' smart constructor.
data RightsizingRecommendationMetadata = RightsizingRecommendationMetadata'
  { -- | The ID for this specific recommendation.
    recommendationId :: Lude.Maybe Lude.Text,
    -- | The timestamp for when AWS made this recommendation.
    generationTimestamp :: Lude.Maybe Lude.Text,
    -- | How many days of previous usage that AWS considers when making this recommendation.
    lookbackPeriodInDays :: Lude.Maybe LookbackPeriodInDays
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RightsizingRecommendationMetadata' with the minimum fields required to make a request.
--
-- * 'recommendationId' - The ID for this specific recommendation.
-- * 'generationTimestamp' - The timestamp for when AWS made this recommendation.
-- * 'lookbackPeriodInDays' - How many days of previous usage that AWS considers when making this recommendation.
mkRightsizingRecommendationMetadata ::
  RightsizingRecommendationMetadata
mkRightsizingRecommendationMetadata =
  RightsizingRecommendationMetadata'
    { recommendationId =
        Lude.Nothing,
      generationTimestamp = Lude.Nothing,
      lookbackPeriodInDays = Lude.Nothing
    }

-- | The ID for this specific recommendation.
--
-- /Note:/ Consider using 'recommendationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrmRecommendationId :: Lens.Lens' RightsizingRecommendationMetadata (Lude.Maybe Lude.Text)
rrmRecommendationId = Lens.lens (recommendationId :: RightsizingRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {recommendationId = a} :: RightsizingRecommendationMetadata)
{-# DEPRECATED rrmRecommendationId "Use generic-lens or generic-optics with 'recommendationId' instead." #-}

-- | The timestamp for when AWS made this recommendation.
--
-- /Note:/ Consider using 'generationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrmGenerationTimestamp :: Lens.Lens' RightsizingRecommendationMetadata (Lude.Maybe Lude.Text)
rrmGenerationTimestamp = Lens.lens (generationTimestamp :: RightsizingRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {generationTimestamp = a} :: RightsizingRecommendationMetadata)
{-# DEPRECATED rrmGenerationTimestamp "Use generic-lens or generic-optics with 'generationTimestamp' instead." #-}

-- | How many days of previous usage that AWS considers when making this recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrmLookbackPeriodInDays :: Lens.Lens' RightsizingRecommendationMetadata (Lude.Maybe LookbackPeriodInDays)
rrmLookbackPeriodInDays = Lens.lens (lookbackPeriodInDays :: RightsizingRecommendationMetadata -> Lude.Maybe LookbackPeriodInDays) (\s a -> s {lookbackPeriodInDays = a} :: RightsizingRecommendationMetadata)
{-# DEPRECATED rrmLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

instance Lude.FromJSON RightsizingRecommendationMetadata where
  parseJSON =
    Lude.withObject
      "RightsizingRecommendationMetadata"
      ( \x ->
          RightsizingRecommendationMetadata'
            Lude.<$> (x Lude..:? "RecommendationId")
            Lude.<*> (x Lude..:? "GenerationTimestamp")
            Lude.<*> (x Lude..:? "LookbackPeriodInDays")
      )
