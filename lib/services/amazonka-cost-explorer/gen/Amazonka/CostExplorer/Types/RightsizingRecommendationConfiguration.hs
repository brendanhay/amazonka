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
-- Module      : Amazonka.CostExplorer.Types.RightsizingRecommendationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.RightsizingRecommendationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.RecommendationTarget
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | You can use @RightsizingRecommendationConfiguration@ to customize
-- recommendations across two attributes. You can choose to view
-- recommendations for instances within the same instance families or
-- across different instance families. You can also choose to view your
-- estimated savings that are associated with recommendations with
-- consideration of existing Savings Plans or Reserved Instance (RI)
-- benefits, or neither.
--
-- /See:/ 'newRightsizingRecommendationConfiguration' smart constructor.
data RightsizingRecommendationConfiguration = RightsizingRecommendationConfiguration'
  { -- | The option to see recommendations within the same instance family or
    -- recommendations for instances across other families. The default value
    -- is @SAME_INSTANCE_FAMILY@.
    recommendationTarget :: RecommendationTarget,
    -- | The option to consider RI or Savings Plans discount benefits in your
    -- savings calculation. The default value is @TRUE@.
    benefitsConsidered :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RightsizingRecommendationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationTarget', 'rightsizingRecommendationConfiguration_recommendationTarget' - The option to see recommendations within the same instance family or
-- recommendations for instances across other families. The default value
-- is @SAME_INSTANCE_FAMILY@.
--
-- 'benefitsConsidered', 'rightsizingRecommendationConfiguration_benefitsConsidered' - The option to consider RI or Savings Plans discount benefits in your
-- savings calculation. The default value is @TRUE@.
newRightsizingRecommendationConfiguration ::
  -- | 'recommendationTarget'
  RecommendationTarget ->
  -- | 'benefitsConsidered'
  Prelude.Bool ->
  RightsizingRecommendationConfiguration
newRightsizingRecommendationConfiguration
  pRecommendationTarget_
  pBenefitsConsidered_ =
    RightsizingRecommendationConfiguration'
      { recommendationTarget =
          pRecommendationTarget_,
        benefitsConsidered =
          pBenefitsConsidered_
      }

-- | The option to see recommendations within the same instance family or
-- recommendations for instances across other families. The default value
-- is @SAME_INSTANCE_FAMILY@.
rightsizingRecommendationConfiguration_recommendationTarget :: Lens.Lens' RightsizingRecommendationConfiguration RecommendationTarget
rightsizingRecommendationConfiguration_recommendationTarget = Lens.lens (\RightsizingRecommendationConfiguration' {recommendationTarget} -> recommendationTarget) (\s@RightsizingRecommendationConfiguration' {} a -> s {recommendationTarget = a} :: RightsizingRecommendationConfiguration)

-- | The option to consider RI or Savings Plans discount benefits in your
-- savings calculation. The default value is @TRUE@.
rightsizingRecommendationConfiguration_benefitsConsidered :: Lens.Lens' RightsizingRecommendationConfiguration Prelude.Bool
rightsizingRecommendationConfiguration_benefitsConsidered = Lens.lens (\RightsizingRecommendationConfiguration' {benefitsConsidered} -> benefitsConsidered) (\s@RightsizingRecommendationConfiguration' {} a -> s {benefitsConsidered = a} :: RightsizingRecommendationConfiguration)

instance
  Data.FromJSON
    RightsizingRecommendationConfiguration
  where
  parseJSON =
    Data.withObject
      "RightsizingRecommendationConfiguration"
      ( \x ->
          RightsizingRecommendationConfiguration'
            Prelude.<$> (x Data..: "RecommendationTarget")
            Prelude.<*> (x Data..: "BenefitsConsidered")
      )

instance
  Prelude.Hashable
    RightsizingRecommendationConfiguration
  where
  hashWithSalt
    _salt
    RightsizingRecommendationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` recommendationTarget
        `Prelude.hashWithSalt` benefitsConsidered

instance
  Prelude.NFData
    RightsizingRecommendationConfiguration
  where
  rnf RightsizingRecommendationConfiguration' {..} =
    Prelude.rnf recommendationTarget `Prelude.seq`
      Prelude.rnf benefitsConsidered

instance
  Data.ToJSON
    RightsizingRecommendationConfiguration
  where
  toJSON RightsizingRecommendationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RecommendationTarget"
                  Data..= recommendationTarget
              ),
            Prelude.Just
              ("BenefitsConsidered" Data..= benefitsConsidered)
          ]
      )
