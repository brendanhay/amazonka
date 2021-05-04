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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The amount of Savings Plans utilization, in hours.
--
-- /See:/ 'newSavingsPlansUtilizationByTime' smart constructor.
data SavingsPlansUtilizationByTime = SavingsPlansUtilizationByTime'
  { -- | The amount saved by using existing Savings Plans. Savings returns both
    -- net savings from Savings Plans as well as the @onDemandCostEquivalent@
    -- of the Savings Plans when considering the utilization rate.
    savings :: Prelude.Maybe SavingsPlansSavings,
    -- | The total amortized commitment for a Savings Plans. This includes the
    -- sum of the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Prelude.Maybe SavingsPlansAmortizedCommitment,
    timePeriod :: DateInterval,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply
    -- to workloads that are Savings Plans eligible.
    utilization :: SavingsPlansUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilizationByTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savings', 'savingsPlansUtilizationByTime_savings' - The amount saved by using existing Savings Plans. Savings returns both
-- net savings from Savings Plans as well as the @onDemandCostEquivalent@
-- of the Savings Plans when considering the utilization rate.
--
-- 'amortizedCommitment', 'savingsPlansUtilizationByTime_amortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
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
      { savings =
          Prelude.Nothing,
        amortizedCommitment = Prelude.Nothing,
        timePeriod = pTimePeriod_,
        utilization = pUtilization_
      }

-- | The amount saved by using existing Savings Plans. Savings returns both
-- net savings from Savings Plans as well as the @onDemandCostEquivalent@
-- of the Savings Plans when considering the utilization rate.
savingsPlansUtilizationByTime_savings :: Lens.Lens' SavingsPlansUtilizationByTime (Prelude.Maybe SavingsPlansSavings)
savingsPlansUtilizationByTime_savings = Lens.lens (\SavingsPlansUtilizationByTime' {savings} -> savings) (\s@SavingsPlansUtilizationByTime' {} a -> s {savings = a} :: SavingsPlansUtilizationByTime)

-- | The total amortized commitment for a Savings Plans. This includes the
-- sum of the upfront and recurring Savings Plans fees.
savingsPlansUtilizationByTime_amortizedCommitment :: Lens.Lens' SavingsPlansUtilizationByTime (Prelude.Maybe SavingsPlansAmortizedCommitment)
savingsPlansUtilizationByTime_amortizedCommitment = Lens.lens (\SavingsPlansUtilizationByTime' {amortizedCommitment} -> amortizedCommitment) (\s@SavingsPlansUtilizationByTime' {} a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationByTime)

-- | Undocumented member.
savingsPlansUtilizationByTime_timePeriod :: Lens.Lens' SavingsPlansUtilizationByTime DateInterval
savingsPlansUtilizationByTime_timePeriod = Lens.lens (\SavingsPlansUtilizationByTime' {timePeriod} -> timePeriod) (\s@SavingsPlansUtilizationByTime' {} a -> s {timePeriod = a} :: SavingsPlansUtilizationByTime)

-- | A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
savingsPlansUtilizationByTime_utilization :: Lens.Lens' SavingsPlansUtilizationByTime SavingsPlansUtilization
savingsPlansUtilizationByTime_utilization = Lens.lens (\SavingsPlansUtilizationByTime' {utilization} -> utilization) (\s@SavingsPlansUtilizationByTime' {} a -> s {utilization = a} :: SavingsPlansUtilizationByTime)

instance
  Prelude.FromJSON
    SavingsPlansUtilizationByTime
  where
  parseJSON =
    Prelude.withObject
      "SavingsPlansUtilizationByTime"
      ( \x ->
          SavingsPlansUtilizationByTime'
            Prelude.<$> (x Prelude..:? "Savings")
            Prelude.<*> (x Prelude..:? "AmortizedCommitment")
            Prelude.<*> (x Prelude..: "TimePeriod")
            Prelude.<*> (x Prelude..: "Utilization")
      )

instance
  Prelude.Hashable
    SavingsPlansUtilizationByTime

instance Prelude.NFData SavingsPlansUtilizationByTime
