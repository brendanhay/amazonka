{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
  ( ReservationPurchaseRecommendationMetadata (..),

    -- * Smart constructor
    mkReservationPurchaseRecommendationMetadata,

    -- * Lenses
    rprmRecommendationId,
    rprmGenerationTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about this specific recommendation, such as the timestamp for when AWS made a specific recommendation.
--
-- /See:/ 'mkReservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { recommendationId ::
      Lude.Maybe
        Lude.Text,
    generationTimestamp ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationPurchaseRecommendationMetadata' with the minimum fields required to make a request.
--
-- * 'generationTimestamp' - The timestamp for when AWS made this recommendation.
-- * 'recommendationId' - The ID for this specific recommendation.
mkReservationPurchaseRecommendationMetadata ::
  ReservationPurchaseRecommendationMetadata
mkReservationPurchaseRecommendationMetadata =
  ReservationPurchaseRecommendationMetadata'
    { recommendationId =
        Lude.Nothing,
      generationTimestamp = Lude.Nothing
    }

-- | The ID for this specific recommendation.
--
-- /Note:/ Consider using 'recommendationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprmRecommendationId :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Lude.Maybe Lude.Text)
rprmRecommendationId = Lens.lens (recommendationId :: ReservationPurchaseRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {recommendationId = a} :: ReservationPurchaseRecommendationMetadata)
{-# DEPRECATED rprmRecommendationId "Use generic-lens or generic-optics with 'recommendationId' instead." #-}

-- | The timestamp for when AWS made this recommendation.
--
-- /Note:/ Consider using 'generationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprmGenerationTimestamp :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Lude.Maybe Lude.Text)
rprmGenerationTimestamp = Lens.lens (generationTimestamp :: ReservationPurchaseRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {generationTimestamp = a} :: ReservationPurchaseRecommendationMetadata)
{-# DEPRECATED rprmGenerationTimestamp "Use generic-lens or generic-optics with 'generationTimestamp' instead." #-}

instance Lude.FromJSON ReservationPurchaseRecommendationMetadata where
  parseJSON =
    Lude.withObject
      "ReservationPurchaseRecommendationMetadata"
      ( \x ->
          ReservationPurchaseRecommendationMetadata'
            Lude.<$> (x Lude..:? "RecommendationId")
            Lude.<*> (x Lude..:? "GenerationTimestamp")
      )
