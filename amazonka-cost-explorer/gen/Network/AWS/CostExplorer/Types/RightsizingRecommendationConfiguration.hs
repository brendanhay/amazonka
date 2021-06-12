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
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.RecommendationTarget
import qualified Network.AWS.Lens as Lens

-- | Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
--
-- /See:/ 'newRightsizingRecommendationConfiguration' smart constructor.
data RightsizingRecommendationConfiguration = RightsizingRecommendationConfiguration'
  { -- | The option to see recommendations within the same instance family, or
    -- recommendations for instances across other families. The default value
    -- is @SAME_INSTANCE_FAMILY@.
    recommendationTarget :: RecommendationTarget,
    -- | The option to consider RI or Savings Plans discount benefits in your
    -- savings calculation. The default value is @TRUE@.
    benefitsConsidered :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RightsizingRecommendationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationTarget', 'rightsizingRecommendationConfiguration_recommendationTarget' - The option to see recommendations within the same instance family, or
-- recommendations for instances across other families. The default value
-- is @SAME_INSTANCE_FAMILY@.
--
-- 'benefitsConsidered', 'rightsizingRecommendationConfiguration_benefitsConsidered' - The option to consider RI or Savings Plans discount benefits in your
-- savings calculation. The default value is @TRUE@.
newRightsizingRecommendationConfiguration ::
  -- | 'recommendationTarget'
  RecommendationTarget ->
  -- | 'benefitsConsidered'
  Core.Bool ->
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

-- | The option to see recommendations within the same instance family, or
-- recommendations for instances across other families. The default value
-- is @SAME_INSTANCE_FAMILY@.
rightsizingRecommendationConfiguration_recommendationTarget :: Lens.Lens' RightsizingRecommendationConfiguration RecommendationTarget
rightsizingRecommendationConfiguration_recommendationTarget = Lens.lens (\RightsizingRecommendationConfiguration' {recommendationTarget} -> recommendationTarget) (\s@RightsizingRecommendationConfiguration' {} a -> s {recommendationTarget = a} :: RightsizingRecommendationConfiguration)

-- | The option to consider RI or Savings Plans discount benefits in your
-- savings calculation. The default value is @TRUE@.
rightsizingRecommendationConfiguration_benefitsConsidered :: Lens.Lens' RightsizingRecommendationConfiguration Core.Bool
rightsizingRecommendationConfiguration_benefitsConsidered = Lens.lens (\RightsizingRecommendationConfiguration' {benefitsConsidered} -> benefitsConsidered) (\s@RightsizingRecommendationConfiguration' {} a -> s {benefitsConsidered = a} :: RightsizingRecommendationConfiguration)

instance
  Core.FromJSON
    RightsizingRecommendationConfiguration
  where
  parseJSON =
    Core.withObject
      "RightsizingRecommendationConfiguration"
      ( \x ->
          RightsizingRecommendationConfiguration'
            Core.<$> (x Core..: "RecommendationTarget")
            Core.<*> (x Core..: "BenefitsConsidered")
      )

instance
  Core.Hashable
    RightsizingRecommendationConfiguration

instance
  Core.NFData
    RightsizingRecommendationConfiguration

instance
  Core.ToJSON
    RightsizingRecommendationConfiguration
  where
  toJSON RightsizingRecommendationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "RecommendationTarget"
                  Core..= recommendationTarget
              ),
            Core.Just
              ("BenefitsConsidered" Core..= benefitsConsidered)
          ]
      )
