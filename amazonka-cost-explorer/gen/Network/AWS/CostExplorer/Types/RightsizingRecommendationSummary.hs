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
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary of rightsizing recommendations
--
-- /See:/ 'newRightsizingRecommendationSummary' smart constructor.
data RightsizingRecommendationSummary = RightsizingRecommendationSummary'
  { -- | Estimated total savings resulting from modifications, on a monthly
    -- basis.
    estimatedTotalMonthlySavingsAmount :: Core.Maybe Core.Text,
    -- | Savings percentage based on the recommended modifications, relative to
    -- the total On-Demand costs associated with these instances.
    savingsPercentage :: Core.Maybe Core.Text,
    -- | Total number of instance recommendations.
    totalRecommendationCount :: Core.Maybe Core.Text,
    -- | The currency code that AWS used to calculate the savings.
    savingsCurrencyCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RightsizingRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedTotalMonthlySavingsAmount', 'rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount' - Estimated total savings resulting from modifications, on a monthly
-- basis.
--
-- 'savingsPercentage', 'rightsizingRecommendationSummary_savingsPercentage' - Savings percentage based on the recommended modifications, relative to
-- the total On-Demand costs associated with these instances.
--
-- 'totalRecommendationCount', 'rightsizingRecommendationSummary_totalRecommendationCount' - Total number of instance recommendations.
--
-- 'savingsCurrencyCode', 'rightsizingRecommendationSummary_savingsCurrencyCode' - The currency code that AWS used to calculate the savings.
newRightsizingRecommendationSummary ::
  RightsizingRecommendationSummary
newRightsizingRecommendationSummary =
  RightsizingRecommendationSummary'
    { estimatedTotalMonthlySavingsAmount =
        Core.Nothing,
      savingsPercentage = Core.Nothing,
      totalRecommendationCount = Core.Nothing,
      savingsCurrencyCode = Core.Nothing
    }

-- | Estimated total savings resulting from modifications, on a monthly
-- basis.
rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Core.Text)
rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount = Lens.lens (\RightsizingRecommendationSummary' {estimatedTotalMonthlySavingsAmount} -> estimatedTotalMonthlySavingsAmount) (\s@RightsizingRecommendationSummary' {} a -> s {estimatedTotalMonthlySavingsAmount = a} :: RightsizingRecommendationSummary)

-- | Savings percentage based on the recommended modifications, relative to
-- the total On-Demand costs associated with these instances.
rightsizingRecommendationSummary_savingsPercentage :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Core.Text)
rightsizingRecommendationSummary_savingsPercentage = Lens.lens (\RightsizingRecommendationSummary' {savingsPercentage} -> savingsPercentage) (\s@RightsizingRecommendationSummary' {} a -> s {savingsPercentage = a} :: RightsizingRecommendationSummary)

-- | Total number of instance recommendations.
rightsizingRecommendationSummary_totalRecommendationCount :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Core.Text)
rightsizingRecommendationSummary_totalRecommendationCount = Lens.lens (\RightsizingRecommendationSummary' {totalRecommendationCount} -> totalRecommendationCount) (\s@RightsizingRecommendationSummary' {} a -> s {totalRecommendationCount = a} :: RightsizingRecommendationSummary)

-- | The currency code that AWS used to calculate the savings.
rightsizingRecommendationSummary_savingsCurrencyCode :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Core.Text)
rightsizingRecommendationSummary_savingsCurrencyCode = Lens.lens (\RightsizingRecommendationSummary' {savingsCurrencyCode} -> savingsCurrencyCode) (\s@RightsizingRecommendationSummary' {} a -> s {savingsCurrencyCode = a} :: RightsizingRecommendationSummary)

instance
  Core.FromJSON
    RightsizingRecommendationSummary
  where
  parseJSON =
    Core.withObject
      "RightsizingRecommendationSummary"
      ( \x ->
          RightsizingRecommendationSummary'
            Core.<$> (x Core..:? "EstimatedTotalMonthlySavingsAmount")
            Core.<*> (x Core..:? "SavingsPercentage")
            Core.<*> (x Core..:? "TotalRecommendationCount")
            Core.<*> (x Core..:? "SavingsCurrencyCode")
      )

instance
  Core.Hashable
    RightsizingRecommendationSummary

instance Core.NFData RightsizingRecommendationSummary
