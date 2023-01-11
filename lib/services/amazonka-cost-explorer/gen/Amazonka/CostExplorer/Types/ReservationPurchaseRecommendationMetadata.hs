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
-- Module      : Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about this specific recommendation, such as the timestamp
-- for when Amazon Web Services made a specific recommendation.
--
-- /See:/ 'newReservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { -- | The timestamp for when Amazon Web Services made this recommendation.
    generationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generationTimestamp', 'reservationPurchaseRecommendationMetadata_generationTimestamp' - The timestamp for when Amazon Web Services made this recommendation.
--
-- 'recommendationId', 'reservationPurchaseRecommendationMetadata_recommendationId' - The ID for this specific recommendation.
newReservationPurchaseRecommendationMetadata ::
  ReservationPurchaseRecommendationMetadata
newReservationPurchaseRecommendationMetadata =
  ReservationPurchaseRecommendationMetadata'
    { generationTimestamp =
        Prelude.Nothing,
      recommendationId =
        Prelude.Nothing
    }

-- | The timestamp for when Amazon Web Services made this recommendation.
reservationPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationMetadata_generationTimestamp = Lens.lens (\ReservationPurchaseRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@ReservationPurchaseRecommendationMetadata' {} a -> s {generationTimestamp = a} :: ReservationPurchaseRecommendationMetadata)

-- | The ID for this specific recommendation.
reservationPurchaseRecommendationMetadata_recommendationId :: Lens.Lens' ReservationPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationMetadata_recommendationId = Lens.lens (\ReservationPurchaseRecommendationMetadata' {recommendationId} -> recommendationId) (\s@ReservationPurchaseRecommendationMetadata' {} a -> s {recommendationId = a} :: ReservationPurchaseRecommendationMetadata)

instance
  Data.FromJSON
    ReservationPurchaseRecommendationMetadata
  where
  parseJSON =
    Data.withObject
      "ReservationPurchaseRecommendationMetadata"
      ( \x ->
          ReservationPurchaseRecommendationMetadata'
            Prelude.<$> (x Data..:? "GenerationTimestamp")
              Prelude.<*> (x Data..:? "RecommendationId")
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendationMetadata
  where
  hashWithSalt
    _salt
    ReservationPurchaseRecommendationMetadata' {..} =
      _salt `Prelude.hashWithSalt` generationTimestamp
        `Prelude.hashWithSalt` recommendationId

instance
  Prelude.NFData
    ReservationPurchaseRecommendationMetadata
  where
  rnf ReservationPurchaseRecommendationMetadata' {..} =
    Prelude.rnf generationTimestamp
      `Prelude.seq` Prelude.rnf recommendationId
