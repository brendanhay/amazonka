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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansCoverageData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansCoverageData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specific coverage percentage, On-Demand costs, and spend covered by
-- Savings Plans, and total Savings Plans costs for an account.
--
-- /See:/ 'newSavingsPlansCoverageData' smart constructor.
data SavingsPlansCoverageData = SavingsPlansCoverageData'
  { -- | The amount of your Amazon Web Services usage that\'s covered by a
    -- Savings Plans.
    spendCoveredBySavingsPlans :: Prelude.Maybe Prelude.Text,
    -- | The percentage of your existing Savings Plans covered usage, divided by
    -- all of your eligible Savings Plans usage in an account (or set of
    -- accounts).
    coveragePercentage :: Prelude.Maybe Prelude.Text,
    -- | The total cost of your Amazon Web Services usage, regardless of your
    -- purchase option.
    totalCost :: Prelude.Maybe Prelude.Text,
    -- | The cost of your Amazon Web Services usage at the public On-Demand rate.
    onDemandCost :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansCoverageData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spendCoveredBySavingsPlans', 'savingsPlansCoverageData_spendCoveredBySavingsPlans' - The amount of your Amazon Web Services usage that\'s covered by a
-- Savings Plans.
--
-- 'coveragePercentage', 'savingsPlansCoverageData_coveragePercentage' - The percentage of your existing Savings Plans covered usage, divided by
-- all of your eligible Savings Plans usage in an account (or set of
-- accounts).
--
-- 'totalCost', 'savingsPlansCoverageData_totalCost' - The total cost of your Amazon Web Services usage, regardless of your
-- purchase option.
--
-- 'onDemandCost', 'savingsPlansCoverageData_onDemandCost' - The cost of your Amazon Web Services usage at the public On-Demand rate.
newSavingsPlansCoverageData ::
  SavingsPlansCoverageData
newSavingsPlansCoverageData =
  SavingsPlansCoverageData'
    { spendCoveredBySavingsPlans =
        Prelude.Nothing,
      coveragePercentage = Prelude.Nothing,
      totalCost = Prelude.Nothing,
      onDemandCost = Prelude.Nothing
    }

-- | The amount of your Amazon Web Services usage that\'s covered by a
-- Savings Plans.
savingsPlansCoverageData_spendCoveredBySavingsPlans :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_spendCoveredBySavingsPlans = Lens.lens (\SavingsPlansCoverageData' {spendCoveredBySavingsPlans} -> spendCoveredBySavingsPlans) (\s@SavingsPlansCoverageData' {} a -> s {spendCoveredBySavingsPlans = a} :: SavingsPlansCoverageData)

-- | The percentage of your existing Savings Plans covered usage, divided by
-- all of your eligible Savings Plans usage in an account (or set of
-- accounts).
savingsPlansCoverageData_coveragePercentage :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_coveragePercentage = Lens.lens (\SavingsPlansCoverageData' {coveragePercentage} -> coveragePercentage) (\s@SavingsPlansCoverageData' {} a -> s {coveragePercentage = a} :: SavingsPlansCoverageData)

-- | The total cost of your Amazon Web Services usage, regardless of your
-- purchase option.
savingsPlansCoverageData_totalCost :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_totalCost = Lens.lens (\SavingsPlansCoverageData' {totalCost} -> totalCost) (\s@SavingsPlansCoverageData' {} a -> s {totalCost = a} :: SavingsPlansCoverageData)

-- | The cost of your Amazon Web Services usage at the public On-Demand rate.
savingsPlansCoverageData_onDemandCost :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_onDemandCost = Lens.lens (\SavingsPlansCoverageData' {onDemandCost} -> onDemandCost) (\s@SavingsPlansCoverageData' {} a -> s {onDemandCost = a} :: SavingsPlansCoverageData)

instance Data.FromJSON SavingsPlansCoverageData where
  parseJSON =
    Data.withObject
      "SavingsPlansCoverageData"
      ( \x ->
          SavingsPlansCoverageData'
            Prelude.<$> (x Data..:? "SpendCoveredBySavingsPlans")
            Prelude.<*> (x Data..:? "CoveragePercentage")
            Prelude.<*> (x Data..:? "TotalCost")
            Prelude.<*> (x Data..:? "OnDemandCost")
      )

instance Prelude.Hashable SavingsPlansCoverageData where
  hashWithSalt _salt SavingsPlansCoverageData' {..} =
    _salt
      `Prelude.hashWithSalt` spendCoveredBySavingsPlans
      `Prelude.hashWithSalt` coveragePercentage
      `Prelude.hashWithSalt` totalCost
      `Prelude.hashWithSalt` onDemandCost

instance Prelude.NFData SavingsPlansCoverageData where
  rnf SavingsPlansCoverageData' {..} =
    Prelude.rnf spendCoveredBySavingsPlans
      `Prelude.seq` Prelude.rnf coveragePercentage
      `Prelude.seq` Prelude.rnf totalCost
      `Prelude.seq` Prelude.rnf onDemandCost
