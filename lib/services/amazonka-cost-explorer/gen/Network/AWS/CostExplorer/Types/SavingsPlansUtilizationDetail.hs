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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A single daily or monthly Savings Plans utilization rate, and details
-- for your account. A management account in an organization have access to
-- member accounts. You can use @GetDimensionValues@ to determine the
-- possible dimension values.
--
-- /See:/ 'newSavingsPlansUtilizationDetail' smart constructor.
data SavingsPlansUtilizationDetail = SavingsPlansUtilizationDetail'
  { -- | The total amortized commitment for a Savings Plans. Includes the sum of
    -- the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Prelude.Maybe SavingsPlansAmortizedCommitment,
    -- | The amount saved by using existing Savings Plans. Savings returns both
    -- net savings from savings plans as well as the @onDemandCostEquivalent@
    -- of the Savings Plans when considering the utilization rate.
    savings :: Prelude.Maybe SavingsPlansSavings,
    -- | The attribute that applies to a specific @Dimension@.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A ratio of your effectiveness of using existing Savings Plans to apply
    -- to workloads that are Savings Plans eligible.
    utilization :: Prelude.Maybe SavingsPlansUtilization,
    -- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
    savingsPlanArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilizationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amortizedCommitment', 'savingsPlansUtilizationDetail_amortizedCommitment' - The total amortized commitment for a Savings Plans. Includes the sum of
-- the upfront and recurring Savings Plans fees.
--
-- 'savings', 'savingsPlansUtilizationDetail_savings' - The amount saved by using existing Savings Plans. Savings returns both
-- net savings from savings plans as well as the @onDemandCostEquivalent@
-- of the Savings Plans when considering the utilization rate.
--
-- 'attributes', 'savingsPlansUtilizationDetail_attributes' - The attribute that applies to a specific @Dimension@.
--
-- 'utilization', 'savingsPlansUtilizationDetail_utilization' - A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
--
-- 'savingsPlanArn', 'savingsPlansUtilizationDetail_savingsPlanArn' - The unique Amazon Resource Name (ARN) for a particular Savings Plan.
newSavingsPlansUtilizationDetail ::
  SavingsPlansUtilizationDetail
newSavingsPlansUtilizationDetail =
  SavingsPlansUtilizationDetail'
    { amortizedCommitment =
        Prelude.Nothing,
      savings = Prelude.Nothing,
      attributes = Prelude.Nothing,
      utilization = Prelude.Nothing,
      savingsPlanArn = Prelude.Nothing
    }

-- | The total amortized commitment for a Savings Plans. Includes the sum of
-- the upfront and recurring Savings Plans fees.
savingsPlansUtilizationDetail_amortizedCommitment :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe SavingsPlansAmortizedCommitment)
savingsPlansUtilizationDetail_amortizedCommitment = Lens.lens (\SavingsPlansUtilizationDetail' {amortizedCommitment} -> amortizedCommitment) (\s@SavingsPlansUtilizationDetail' {} a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationDetail)

-- | The amount saved by using existing Savings Plans. Savings returns both
-- net savings from savings plans as well as the @onDemandCostEquivalent@
-- of the Savings Plans when considering the utilization rate.
savingsPlansUtilizationDetail_savings :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe SavingsPlansSavings)
savingsPlansUtilizationDetail_savings = Lens.lens (\SavingsPlansUtilizationDetail' {savings} -> savings) (\s@SavingsPlansUtilizationDetail' {} a -> s {savings = a} :: SavingsPlansUtilizationDetail)

-- | The attribute that applies to a specific @Dimension@.
savingsPlansUtilizationDetail_attributes :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
savingsPlansUtilizationDetail_attributes = Lens.lens (\SavingsPlansUtilizationDetail' {attributes} -> attributes) (\s@SavingsPlansUtilizationDetail' {} a -> s {attributes = a} :: SavingsPlansUtilizationDetail) Prelude.. Lens.mapping Lens.coerced

-- | A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
savingsPlansUtilizationDetail_utilization :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe SavingsPlansUtilization)
savingsPlansUtilizationDetail_utilization = Lens.lens (\SavingsPlansUtilizationDetail' {utilization} -> utilization) (\s@SavingsPlansUtilizationDetail' {} a -> s {utilization = a} :: SavingsPlansUtilizationDetail)

-- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
savingsPlansUtilizationDetail_savingsPlanArn :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe Prelude.Text)
savingsPlansUtilizationDetail_savingsPlanArn = Lens.lens (\SavingsPlansUtilizationDetail' {savingsPlanArn} -> savingsPlanArn) (\s@SavingsPlansUtilizationDetail' {} a -> s {savingsPlanArn = a} :: SavingsPlansUtilizationDetail)

instance Core.FromJSON SavingsPlansUtilizationDetail where
  parseJSON =
    Core.withObject
      "SavingsPlansUtilizationDetail"
      ( \x ->
          SavingsPlansUtilizationDetail'
            Prelude.<$> (x Core..:? "AmortizedCommitment")
            Prelude.<*> (x Core..:? "Savings")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Utilization")
            Prelude.<*> (x Core..:? "SavingsPlanArn")
      )

instance
  Prelude.Hashable
    SavingsPlansUtilizationDetail

instance Prelude.NFData SavingsPlansUtilizationDetail
