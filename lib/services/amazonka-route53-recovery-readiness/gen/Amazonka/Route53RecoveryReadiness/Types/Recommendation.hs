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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Recommendations that are provided to make an application more recovery
-- resilient.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | Text of the recommendations that are provided to make an application
    -- more recovery resilient.
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
-- 'recommendationText', 'recommendation_recommendationText' - Text of the recommendations that are provided to make an application
-- more recovery resilient.
newRecommendation ::
  -- | 'recommendationText'
  Prelude.Text ->
  Recommendation
newRecommendation pRecommendationText_ =
  Recommendation'
    { recommendationText =
        pRecommendationText_
    }

-- | Text of the recommendations that are provided to make an application
-- more recovery resilient.
recommendation_recommendationText :: Lens.Lens' Recommendation Prelude.Text
recommendation_recommendationText = Lens.lens (\Recommendation' {recommendationText} -> recommendationText) (\s@Recommendation' {} a -> s {recommendationText = a} :: Recommendation)

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..: "recommendationText")
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt `Prelude.hashWithSalt` recommendationText

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf recommendationText
