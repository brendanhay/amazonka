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
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary of rightsizing recommendations
--
-- /See:/ 'newRightsizingRecommendationSummary' smart constructor.
data RightsizingRecommendationSummary = RightsizingRecommendationSummary'
  { -- | Estimated total savings resulting from modifications, on a monthly
    -- basis.
    estimatedTotalMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | Savings percentage based on the recommended modifications, relative to
    -- the total On-Demand costs associated with these instances.
    savingsPercentage :: Prelude.Maybe Prelude.Text,
    -- | Total number of instance recommendations.
    totalRecommendationCount :: Prelude.Maybe Prelude.Text,
    -- | The currency code that AWS used to calculate the savings.
    savingsCurrencyCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      savingsPercentage = Prelude.Nothing,
      totalRecommendationCount =
        Prelude.Nothing,
      savingsCurrencyCode = Prelude.Nothing
    }

-- | Estimated total savings resulting from modifications, on a monthly
-- basis.
rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount = Lens.lens (\RightsizingRecommendationSummary' {estimatedTotalMonthlySavingsAmount} -> estimatedTotalMonthlySavingsAmount) (\s@RightsizingRecommendationSummary' {} a -> s {estimatedTotalMonthlySavingsAmount = a} :: RightsizingRecommendationSummary)

-- | Savings percentage based on the recommended modifications, relative to
-- the total On-Demand costs associated with these instances.
rightsizingRecommendationSummary_savingsPercentage :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_savingsPercentage = Lens.lens (\RightsizingRecommendationSummary' {savingsPercentage} -> savingsPercentage) (\s@RightsizingRecommendationSummary' {} a -> s {savingsPercentage = a} :: RightsizingRecommendationSummary)

-- | Total number of instance recommendations.
rightsizingRecommendationSummary_totalRecommendationCount :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_totalRecommendationCount = Lens.lens (\RightsizingRecommendationSummary' {totalRecommendationCount} -> totalRecommendationCount) (\s@RightsizingRecommendationSummary' {} a -> s {totalRecommendationCount = a} :: RightsizingRecommendationSummary)

-- | The currency code that AWS used to calculate the savings.
rightsizingRecommendationSummary_savingsCurrencyCode :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_savingsCurrencyCode = Lens.lens (\RightsizingRecommendationSummary' {savingsCurrencyCode} -> savingsCurrencyCode) (\s@RightsizingRecommendationSummary' {} a -> s {savingsCurrencyCode = a} :: RightsizingRecommendationSummary)

instance
  Prelude.FromJSON
    RightsizingRecommendationSummary
  where
  parseJSON =
    Prelude.withObject
      "RightsizingRecommendationSummary"
      ( \x ->
          RightsizingRecommendationSummary'
            Prelude.<$> (x Prelude..:? "EstimatedTotalMonthlySavingsAmount")
            Prelude.<*> (x Prelude..:? "SavingsPercentage")
            Prelude.<*> (x Prelude..:? "TotalRecommendationCount")
            Prelude.<*> (x Prelude..:? "SavingsCurrencyCode")
      )

instance
  Prelude.Hashable
    RightsizingRecommendationSummary

instance
  Prelude.NFData
    RightsizingRecommendationSummary
