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
-- Module      : Amazonka.CostExplorer.Types.RightsizingRecommendationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.RightsizingRecommendationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of rightsizing recommendations
--
-- /See:/ 'newRightsizingRecommendationSummary' smart constructor.
data RightsizingRecommendationSummary = RightsizingRecommendationSummary'
  { -- | The savings percentage based on the recommended modifications. It\'s
    -- relative to the total On-Demand costs that are associated with these
    -- instances.
    savingsPercentage :: Prelude.Maybe Prelude.Text,
    -- | The currency code that Amazon Web Services used to calculate the
    -- savings.
    savingsCurrencyCode :: Prelude.Maybe Prelude.Text,
    -- | The estimated total savings resulting from modifications, on a monthly
    -- basis.
    estimatedTotalMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The total number of instance recommendations.
    totalRecommendationCount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RightsizingRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savingsPercentage', 'rightsizingRecommendationSummary_savingsPercentage' - The savings percentage based on the recommended modifications. It\'s
-- relative to the total On-Demand costs that are associated with these
-- instances.
--
-- 'savingsCurrencyCode', 'rightsizingRecommendationSummary_savingsCurrencyCode' - The currency code that Amazon Web Services used to calculate the
-- savings.
--
-- 'estimatedTotalMonthlySavingsAmount', 'rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount' - The estimated total savings resulting from modifications, on a monthly
-- basis.
--
-- 'totalRecommendationCount', 'rightsizingRecommendationSummary_totalRecommendationCount' - The total number of instance recommendations.
newRightsizingRecommendationSummary ::
  RightsizingRecommendationSummary
newRightsizingRecommendationSummary =
  RightsizingRecommendationSummary'
    { savingsPercentage =
        Prelude.Nothing,
      savingsCurrencyCode = Prelude.Nothing,
      estimatedTotalMonthlySavingsAmount =
        Prelude.Nothing,
      totalRecommendationCount =
        Prelude.Nothing
    }

-- | The savings percentage based on the recommended modifications. It\'s
-- relative to the total On-Demand costs that are associated with these
-- instances.
rightsizingRecommendationSummary_savingsPercentage :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_savingsPercentage = Lens.lens (\RightsizingRecommendationSummary' {savingsPercentage} -> savingsPercentage) (\s@RightsizingRecommendationSummary' {} a -> s {savingsPercentage = a} :: RightsizingRecommendationSummary)

-- | The currency code that Amazon Web Services used to calculate the
-- savings.
rightsizingRecommendationSummary_savingsCurrencyCode :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_savingsCurrencyCode = Lens.lens (\RightsizingRecommendationSummary' {savingsCurrencyCode} -> savingsCurrencyCode) (\s@RightsizingRecommendationSummary' {} a -> s {savingsCurrencyCode = a} :: RightsizingRecommendationSummary)

-- | The estimated total savings resulting from modifications, on a monthly
-- basis.
rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount = Lens.lens (\RightsizingRecommendationSummary' {estimatedTotalMonthlySavingsAmount} -> estimatedTotalMonthlySavingsAmount) (\s@RightsizingRecommendationSummary' {} a -> s {estimatedTotalMonthlySavingsAmount = a} :: RightsizingRecommendationSummary)

-- | The total number of instance recommendations.
rightsizingRecommendationSummary_totalRecommendationCount :: Lens.Lens' RightsizingRecommendationSummary (Prelude.Maybe Prelude.Text)
rightsizingRecommendationSummary_totalRecommendationCount = Lens.lens (\RightsizingRecommendationSummary' {totalRecommendationCount} -> totalRecommendationCount) (\s@RightsizingRecommendationSummary' {} a -> s {totalRecommendationCount = a} :: RightsizingRecommendationSummary)

instance
  Core.FromJSON
    RightsizingRecommendationSummary
  where
  parseJSON =
    Core.withObject
      "RightsizingRecommendationSummary"
      ( \x ->
          RightsizingRecommendationSummary'
            Prelude.<$> (x Core..:? "SavingsPercentage")
            Prelude.<*> (x Core..:? "SavingsCurrencyCode")
            Prelude.<*> (x Core..:? "EstimatedTotalMonthlySavingsAmount")
            Prelude.<*> (x Core..:? "TotalRecommendationCount")
      )

instance
  Prelude.Hashable
    RightsizingRecommendationSummary
  where
  hashWithSalt
    _salt
    RightsizingRecommendationSummary' {..} =
      _salt `Prelude.hashWithSalt` savingsPercentage
        `Prelude.hashWithSalt` savingsCurrencyCode
        `Prelude.hashWithSalt` estimatedTotalMonthlySavingsAmount
        `Prelude.hashWithSalt` totalRecommendationCount

instance
  Prelude.NFData
    RightsizingRecommendationSummary
  where
  rnf RightsizingRecommendationSummary' {..} =
    Prelude.rnf savingsPercentage
      `Prelude.seq` Prelude.rnf savingsCurrencyCode
      `Prelude.seq` Prelude.rnf estimatedTotalMonthlySavingsAmount
      `Prelude.seq` Prelude.rnf totalRecommendationCount
