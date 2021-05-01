{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about this specific recommendation, such as the timestamp
-- for when AWS made a specific recommendation.
--
-- /See:/ 'newReservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { -- | The ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when AWS made this recommendation.
    generationTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      generationTimestamp =
        Prelude.Nothing
    }

-- | The ID for this specific recommendation.
reservationPurchaseRecommendationMetadata_recommendationId :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationMetadata_recommendationId = Lens.lens (\ReservationPurchaseRecommendationMetadata' {recommendationId} -> recommendationId) (\s@ReservationPurchaseRecommendationMetadata' {} a -> s {recommendationId = a} :: ReservationPurchaseRecommendationMetadata)

-- | The timestamp for when AWS made this recommendation.
reservationPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationMetadata_generationTimestamp = Lens.lens (\ReservationPurchaseRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@ReservationPurchaseRecommendationMetadata' {} a -> s {generationTimestamp = a} :: ReservationPurchaseRecommendationMetadata)

instance
  Prelude.FromJSON
    ReservationPurchaseRecommendationMetadata
  where
  parseJSON =
    Prelude.withObject
      "ReservationPurchaseRecommendationMetadata"
      ( \x ->
          ReservationPurchaseRecommendationMetadata'
            Prelude.<$> (x Prelude..:? "RecommendationId")
              Prelude.<*> (x Prelude..:? "GenerationTimestamp")
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendationMetadata

instance
  Prelude.NFData
    ReservationPurchaseRecommendationMetadata
