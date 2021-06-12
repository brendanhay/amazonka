{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about this specific recommendation, such as the timestamp
-- for when AWS made a specific recommendation.
--
-- /See:/ 'newReservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { -- | The ID for this specific recommendation.
    recommendationId :: Core.Maybe Core.Text,
    -- | The timestamp for when AWS made this recommendation.
    generationTimestamp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationId', 'reservationPurchaseRecommendationMetadata_recommendationId' - The ID for this specific recommendation.
--
-- 'generationTimestamp', 'reservationPurchaseRecommendationMetadata_generationTimestamp' - The timestamp for when AWS made this recommendation.
newReservationPurchaseRecommendationMetadata ::
  ReservationPurchaseRecommendationMetadata
newReservationPurchaseRecommendationMetadata =
  ReservationPurchaseRecommendationMetadata'
    { recommendationId =
        Core.Nothing,
      generationTimestamp =
        Core.Nothing
    }

-- | The ID for this specific recommendation.
reservationPurchaseRecommendationMetadata_recommendationId :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Core.Maybe Core.Text)
reservationPurchaseRecommendationMetadata_recommendationId = Lens.lens (\ReservationPurchaseRecommendationMetadata' {recommendationId} -> recommendationId) (\s@ReservationPurchaseRecommendationMetadata' {} a -> s {recommendationId = a} :: ReservationPurchaseRecommendationMetadata)

-- | The timestamp for when AWS made this recommendation.
reservationPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Core.Maybe Core.Text)
reservationPurchaseRecommendationMetadata_generationTimestamp = Lens.lens (\ReservationPurchaseRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@ReservationPurchaseRecommendationMetadata' {} a -> s {generationTimestamp = a} :: ReservationPurchaseRecommendationMetadata)

instance
  Core.FromJSON
    ReservationPurchaseRecommendationMetadata
  where
  parseJSON =
    Core.withObject
      "ReservationPurchaseRecommendationMetadata"
      ( \x ->
          ReservationPurchaseRecommendationMetadata'
            Core.<$> (x Core..:? "RecommendationId")
            Core.<*> (x Core..:? "GenerationTimestamp")
      )

instance
  Core.Hashable
    ReservationPurchaseRecommendationMetadata

instance
  Core.NFData
    ReservationPurchaseRecommendationMetadata
