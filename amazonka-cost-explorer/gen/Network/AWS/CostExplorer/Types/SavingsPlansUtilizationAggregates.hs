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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates where

import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The aggregated utilization metrics for your Savings Plans usage.
--
-- /See:/ 'newSavingsPlansUtilizationAggregates' smart constructor.
data SavingsPlansUtilizationAggregates = SavingsPlansUtilizationAggregates'
  { -- | The amount saved by using existing Savings Plans. Savings returns both
    -- net savings from Savings Plans, as well as the @onDemandCostEquivalent@
    -- of the Savings Plans when considering the utilization rate.
    savings :: Prelude.Maybe SavingsPlansSavings,
    -- | The total amortized commitment for a Savings Plans. This includes the
    -- sum of the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Prelude.Maybe SavingsPlansAmortizedCommitment,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply
    -- to workloads that are Savings Plans eligible.
    utilization :: SavingsPlansUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilizationAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savings', 'savingsPlansUtilizationAggregates_savings' - The amount saved by using existing Savings Plans. Savings returns both
-- net savings from Savings Plans, as well as the @onDemandCostEquivalent@
-- of the Savings Plans when considering the utilization rate.
--
-- 'amortizedCommitment', 'savingsPlansUtilizationAggregates_amortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
--
-- 'utilization', 'savingsPlansUtilizationAggregates_utilization' - A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
newSavingsPlansUtilizationAggregates ::
  -- | 'utilization'
  SavingsPlansUtilization ->
  SavingsPlansUtilizationAggregates
newSavingsPlansUtilizationAggregates pUtilization_ =
  SavingsPlansUtilizationAggregates'
    { savings =
        Prelude.Nothing,
      amortizedCommitment = Prelude.Nothing,
      utilization = pUtilization_
    }

-- | The amount saved by using existing Savings Plans. Savings returns both
-- net savings from Savings Plans, as well as the @onDemandCostEquivalent@
-- of the Savings Plans when considering the utilization rate.
savingsPlansUtilizationAggregates_savings :: Lens.Lens' SavingsPlansUtilizationAggregates (Prelude.Maybe SavingsPlansSavings)
savingsPlansUtilizationAggregates_savings = Lens.lens (\SavingsPlansUtilizationAggregates' {savings} -> savings) (\s@SavingsPlansUtilizationAggregates' {} a -> s {savings = a} :: SavingsPlansUtilizationAggregates)

-- | The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
savingsPlansUtilizationAggregates_amortizedCommitment :: Lens.Lens' SavingsPlansUtilizationAggregates (Prelude.Maybe SavingsPlansAmortizedCommitment)
savingsPlansUtilizationAggregates_amortizedCommitment = Lens.lens (\SavingsPlansUtilizationAggregates' {amortizedCommitment} -> amortizedCommitment) (\s@SavingsPlansUtilizationAggregates' {} a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationAggregates)

-- | A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
savingsPlansUtilizationAggregates_utilization :: Lens.Lens' SavingsPlansUtilizationAggregates SavingsPlansUtilization
savingsPlansUtilizationAggregates_utilization = Lens.lens (\SavingsPlansUtilizationAggregates' {utilization} -> utilization) (\s@SavingsPlansUtilizationAggregates' {} a -> s {utilization = a} :: SavingsPlansUtilizationAggregates)

instance
  Prelude.FromJSON
    SavingsPlansUtilizationAggregates
  where
  parseJSON =
    Prelude.withObject
      "SavingsPlansUtilizationAggregates"
      ( \x ->
          SavingsPlansUtilizationAggregates'
            Prelude.<$> (x Prelude..:? "Savings")
            Prelude.<*> (x Prelude..:? "AmortizedCommitment")
            Prelude.<*> (x Prelude..: "Utilization")
      )

instance
  Prelude.Hashable
    SavingsPlansUtilizationAggregates

instance
  Prelude.NFData
    SavingsPlansUtilizationAggregates
