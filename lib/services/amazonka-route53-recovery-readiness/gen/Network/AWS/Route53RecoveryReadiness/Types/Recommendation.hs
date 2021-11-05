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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.Recommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Guidance for improving Recovery Group resilliancy
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | Guidance text for recommendation
    recommendationText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationText', 'recommendation_recommendationText' - Guidance text for recommendation
newRecommendation ::
  -- | 'recommendationText'
  Prelude.Text ->
  Recommendation
newRecommendation pRecommendationText_ =
  Recommendation'
    { recommendationText =
        pRecommendationText_
    }

-- | Guidance text for recommendation
recommendation_recommendationText :: Lens.Lens' Recommendation Prelude.Text
recommendation_recommendationText = Lens.lens (\Recommendation' {recommendationText} -> recommendationText) (\s@Recommendation' {} a -> s {recommendationText = a} :: Recommendation)

instance Core.FromJSON Recommendation where
  parseJSON =
    Core.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Core..: "recommendationText")
      )

instance Prelude.Hashable Recommendation

instance Prelude.NFData Recommendation
