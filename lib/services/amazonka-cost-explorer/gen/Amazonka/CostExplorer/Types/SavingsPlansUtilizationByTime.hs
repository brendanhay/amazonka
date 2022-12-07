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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansUtilizationByTime
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansUtilizationByTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.DateInterval
import Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Amazonka.CostExplorer.Types.SavingsPlansSavings
import Amazonka.CostExplorer.Types.SavingsPlansUtilization
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amount of Savings Plans utilization (in hours).
--
-- /See:/ 'newSavingsPlansUtilizationByTime' smart constructor.
data SavingsPlansUtilizationByTime = SavingsPlansUtilizationByTime'
  { -- | The total amortized commitment for a Savings Plans. This includes the
    -- sum of the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Prelude.Maybe SavingsPlansAmortizedCommitment,
    -- | The amount that\'s saved by using existing Savings Plans. Savings
    -- returns both net savings from Savings Plans and also the
    -- @onDemandCostEquivalent@ of the Savings Plans when considering the
    -- utilization rate.
    savings :: Prelude.Maybe SavingsPlansSavings,
    timePeriod :: DateInterval,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply
    -- to workloads that are Savings Plans eligible.
    utilization :: SavingsPlansUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilizationByTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amortizedCommitment', 'savingsPlansUtilizationByTime_amortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
--
-- 'savings', 'savingsPlansUtilizationByTime_savings' - The amount that\'s saved by using existing Savings Plans. Savings
-- returns both net savings from Savings Plans and also the
-- @onDemandCostEquivalent@ of the Savings Plans when considering the
-- utilization rate.
--
-- 'timePeriod', 'savingsPlansUtilizationByTime_timePeriod' - Undocumented member.
--
-- 'utilization', 'savingsPlansUtilizationByTime_utilization' - A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
newSavingsPlansUtilizationByTime ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'utilization'
  SavingsPlansUtilization ->
  SavingsPlansUtilizationByTime
newSavingsPlansUtilizationByTime
  pTimePeriod_
  pUtilization_ =
    SavingsPlansUtilizationByTime'
      { amortizedCommitment =
          Prelude.Nothing,
        savings = Prelude.Nothing,
        timePeriod = pTimePeriod_,
        utilization = pUtilization_
      }

-- | The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
savingsPlansUtilizationByTime_amortizedCommitment :: Lens.Lens' SavingsPlansUtilizationByTime (Prelude.Maybe SavingsPlansAmortizedCommitment)
savingsPlansUtilizationByTime_amortizedCommitment = Lens.lens (\SavingsPlansUtilizationByTime' {amortizedCommitment} -> amortizedCommitment) (\s@SavingsPlansUtilizationByTime' {} a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationByTime)

-- | The amount that\'s saved by using existing Savings Plans. Savings
-- returns both net savings from Savings Plans and also the
-- @onDemandCostEquivalent@ of the Savings Plans when considering the
-- utilization rate.
savingsPlansUtilizationByTime_savings :: Lens.Lens' SavingsPlansUtilizationByTime (Prelude.Maybe SavingsPlansSavings)
savingsPlansUtilizationByTime_savings = Lens.lens (\SavingsPlansUtilizationByTime' {savings} -> savings) (\s@SavingsPlansUtilizationByTime' {} a -> s {savings = a} :: SavingsPlansUtilizationByTime)

-- | Undocumented member.
savingsPlansUtilizationByTime_timePeriod :: Lens.Lens' SavingsPlansUtilizationByTime DateInterval
savingsPlansUtilizationByTime_timePeriod = Lens.lens (\SavingsPlansUtilizationByTime' {timePeriod} -> timePeriod) (\s@SavingsPlansUtilizationByTime' {} a -> s {timePeriod = a} :: SavingsPlansUtilizationByTime)

-- | A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
savingsPlansUtilizationByTime_utilization :: Lens.Lens' SavingsPlansUtilizationByTime SavingsPlansUtilization
savingsPlansUtilizationByTime_utilization = Lens.lens (\SavingsPlansUtilizationByTime' {utilization} -> utilization) (\s@SavingsPlansUtilizationByTime' {} a -> s {utilization = a} :: SavingsPlansUtilizationByTime)

instance Data.FromJSON SavingsPlansUtilizationByTime where
  parseJSON =
    Data.withObject
      "SavingsPlansUtilizationByTime"
      ( \x ->
          SavingsPlansUtilizationByTime'
            Prelude.<$> (x Data..:? "AmortizedCommitment")
            Prelude.<*> (x Data..:? "Savings")
            Prelude.<*> (x Data..: "TimePeriod")
            Prelude.<*> (x Data..: "Utilization")
      )

instance
  Prelude.Hashable
    SavingsPlansUtilizationByTime
  where
  hashWithSalt _salt SavingsPlansUtilizationByTime' {..} =
    _salt `Prelude.hashWithSalt` amortizedCommitment
      `Prelude.hashWithSalt` savings
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` utilization

instance Prelude.NFData SavingsPlansUtilizationByTime where
  rnf SavingsPlansUtilizationByTime' {..} =
    Prelude.rnf amortizedCommitment
      `Prelude.seq` Prelude.rnf savings
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf utilization
