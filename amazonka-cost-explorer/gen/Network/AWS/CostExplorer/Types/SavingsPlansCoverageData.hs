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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverageData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specific coverage percentage, On-Demand costs, and spend covered by
-- Savings Plans, and total Savings Plans costs for an account.
--
-- /See:/ 'newSavingsPlansCoverageData' smart constructor.
data SavingsPlansCoverageData = SavingsPlansCoverageData'
  { -- | The total cost of your AWS usage, regardless of your purchase option.
    totalCost :: Prelude.Maybe Prelude.Text,
    -- | The percentage of your existing Savings Plans covered usage, divided by
    -- all of your eligible Savings Plans usage in an account(or set of
    -- accounts).
    coveragePercentage :: Prelude.Maybe Prelude.Text,
    -- | The amount of your AWS usage that is covered by a Savings Plans.
    spendCoveredBySavingsPlans :: Prelude.Maybe Prelude.Text,
    -- | The cost of your AWS usage at the public On-Demand rate.
    onDemandCost :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansCoverageData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalCost', 'savingsPlansCoverageData_totalCost' - The total cost of your AWS usage, regardless of your purchase option.
--
-- 'coveragePercentage', 'savingsPlansCoverageData_coveragePercentage' - The percentage of your existing Savings Plans covered usage, divided by
-- all of your eligible Savings Plans usage in an account(or set of
-- accounts).
--
-- 'spendCoveredBySavingsPlans', 'savingsPlansCoverageData_spendCoveredBySavingsPlans' - The amount of your AWS usage that is covered by a Savings Plans.
--
-- 'onDemandCost', 'savingsPlansCoverageData_onDemandCost' - The cost of your AWS usage at the public On-Demand rate.
newSavingsPlansCoverageData ::
  SavingsPlansCoverageData
newSavingsPlansCoverageData =
  SavingsPlansCoverageData'
    { totalCost =
        Prelude.Nothing,
      coveragePercentage = Prelude.Nothing,
      spendCoveredBySavingsPlans = Prelude.Nothing,
      onDemandCost = Prelude.Nothing
    }

-- | The total cost of your AWS usage, regardless of your purchase option.
savingsPlansCoverageData_totalCost :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_totalCost = Lens.lens (\SavingsPlansCoverageData' {totalCost} -> totalCost) (\s@SavingsPlansCoverageData' {} a -> s {totalCost = a} :: SavingsPlansCoverageData)

-- | The percentage of your existing Savings Plans covered usage, divided by
-- all of your eligible Savings Plans usage in an account(or set of
-- accounts).
savingsPlansCoverageData_coveragePercentage :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_coveragePercentage = Lens.lens (\SavingsPlansCoverageData' {coveragePercentage} -> coveragePercentage) (\s@SavingsPlansCoverageData' {} a -> s {coveragePercentage = a} :: SavingsPlansCoverageData)

-- | The amount of your AWS usage that is covered by a Savings Plans.
savingsPlansCoverageData_spendCoveredBySavingsPlans :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_spendCoveredBySavingsPlans = Lens.lens (\SavingsPlansCoverageData' {spendCoveredBySavingsPlans} -> spendCoveredBySavingsPlans) (\s@SavingsPlansCoverageData' {} a -> s {spendCoveredBySavingsPlans = a} :: SavingsPlansCoverageData)

-- | The cost of your AWS usage at the public On-Demand rate.
savingsPlansCoverageData_onDemandCost :: Lens.Lens' SavingsPlansCoverageData (Prelude.Maybe Prelude.Text)
savingsPlansCoverageData_onDemandCost = Lens.lens (\SavingsPlansCoverageData' {onDemandCost} -> onDemandCost) (\s@SavingsPlansCoverageData' {} a -> s {onDemandCost = a} :: SavingsPlansCoverageData)

instance Prelude.FromJSON SavingsPlansCoverageData where
  parseJSON =
    Prelude.withObject
      "SavingsPlansCoverageData"
      ( \x ->
          SavingsPlansCoverageData'
            Prelude.<$> (x Prelude..:? "TotalCost")
            Prelude.<*> (x Prelude..:? "CoveragePercentage")
            Prelude.<*> (x Prelude..:? "SpendCoveredBySavingsPlans")
            Prelude.<*> (x Prelude..:? "OnDemandCost")
      )

instance Prelude.Hashable SavingsPlansCoverageData

instance Prelude.NFData SavingsPlansCoverageData
