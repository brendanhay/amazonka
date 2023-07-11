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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansUtilizationAggregates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansUtilizationAggregates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Amazonka.CostExplorer.Types.SavingsPlansSavings
import Amazonka.CostExplorer.Types.SavingsPlansUtilization
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The aggregated utilization metrics for your Savings Plans usage.
--
-- /See:/ 'newSavingsPlansUtilizationAggregates' smart constructor.
data SavingsPlansUtilizationAggregates = SavingsPlansUtilizationAggregates'
  { -- | The total amortized commitment for a Savings Plans. This includes the
    -- sum of the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Prelude.Maybe SavingsPlansAmortizedCommitment,
    -- | The amount that\'s saved by using existing Savings Plans. Savings
    -- returns both net savings from Savings Plans and also the
    -- @onDemandCostEquivalent@ of the Savings Plans when considering the
    -- utilization rate.
    savings :: Prelude.Maybe SavingsPlansSavings,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply
    -- to workloads that are Savings Plans eligible.
    utilization :: SavingsPlansUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilizationAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amortizedCommitment', 'savingsPlansUtilizationAggregates_amortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
--
-- 'savings', 'savingsPlansUtilizationAggregates_savings' - The amount that\'s saved by using existing Savings Plans. Savings
-- returns both net savings from Savings Plans and also the
-- @onDemandCostEquivalent@ of the Savings Plans when considering the
-- utilization rate.
--
-- 'utilization', 'savingsPlansUtilizationAggregates_utilization' - A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
newSavingsPlansUtilizationAggregates ::
  -- | 'utilization'
  SavingsPlansUtilization ->
  SavingsPlansUtilizationAggregates
newSavingsPlansUtilizationAggregates pUtilization_ =
  SavingsPlansUtilizationAggregates'
    { amortizedCommitment =
        Prelude.Nothing,
      savings = Prelude.Nothing,
      utilization = pUtilization_
    }

-- | The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
savingsPlansUtilizationAggregates_amortizedCommitment :: Lens.Lens' SavingsPlansUtilizationAggregates (Prelude.Maybe SavingsPlansAmortizedCommitment)
savingsPlansUtilizationAggregates_amortizedCommitment = Lens.lens (\SavingsPlansUtilizationAggregates' {amortizedCommitment} -> amortizedCommitment) (\s@SavingsPlansUtilizationAggregates' {} a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationAggregates)

-- | The amount that\'s saved by using existing Savings Plans. Savings
-- returns both net savings from Savings Plans and also the
-- @onDemandCostEquivalent@ of the Savings Plans when considering the
-- utilization rate.
savingsPlansUtilizationAggregates_savings :: Lens.Lens' SavingsPlansUtilizationAggregates (Prelude.Maybe SavingsPlansSavings)
savingsPlansUtilizationAggregates_savings = Lens.lens (\SavingsPlansUtilizationAggregates' {savings} -> savings) (\s@SavingsPlansUtilizationAggregates' {} a -> s {savings = a} :: SavingsPlansUtilizationAggregates)

-- | A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
savingsPlansUtilizationAggregates_utilization :: Lens.Lens' SavingsPlansUtilizationAggregates SavingsPlansUtilization
savingsPlansUtilizationAggregates_utilization = Lens.lens (\SavingsPlansUtilizationAggregates' {utilization} -> utilization) (\s@SavingsPlansUtilizationAggregates' {} a -> s {utilization = a} :: SavingsPlansUtilizationAggregates)

instance
  Data.FromJSON
    SavingsPlansUtilizationAggregates
  where
  parseJSON =
    Data.withObject
      "SavingsPlansUtilizationAggregates"
      ( \x ->
          SavingsPlansUtilizationAggregates'
            Prelude.<$> (x Data..:? "AmortizedCommitment")
            Prelude.<*> (x Data..:? "Savings")
            Prelude.<*> (x Data..: "Utilization")
      )

instance
  Prelude.Hashable
    SavingsPlansUtilizationAggregates
  where
  hashWithSalt
    _salt
    SavingsPlansUtilizationAggregates' {..} =
      _salt
        `Prelude.hashWithSalt` amortizedCommitment
        `Prelude.hashWithSalt` savings
        `Prelude.hashWithSalt` utilization

instance
  Prelude.NFData
    SavingsPlansUtilizationAggregates
  where
  rnf SavingsPlansUtilizationAggregates' {..} =
    Prelude.rnf amortizedCommitment
      `Prelude.seq` Prelude.rnf savings
      `Prelude.seq` Prelude.rnf utilization
