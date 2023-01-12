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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansUtilizationDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansUtilizationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Amazonka.CostExplorer.Types.SavingsPlansSavings
import Amazonka.CostExplorer.Types.SavingsPlansUtilization
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single daily or monthly Savings Plans utilization rate and details for
-- your account. A management account in an organization have access to
-- member accounts. You can use @GetDimensionValues@ to determine the
-- possible dimension values.
--
-- /See:/ 'newSavingsPlansUtilizationDetail' smart constructor.
data SavingsPlansUtilizationDetail = SavingsPlansUtilizationDetail'
  { -- | The total amortized commitment for a Savings Plans. Includes the sum of
    -- the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Prelude.Maybe SavingsPlansAmortizedCommitment,
    -- | The attribute that applies to a specific @Dimension@.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The amount saved by using existing Savings Plans. Savings returns both
    -- net savings from savings plans and also the @onDemandCostEquivalent@ of
    -- the Savings Plans when considering the utilization rate.
    savings :: Prelude.Maybe SavingsPlansSavings,
    -- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
    savingsPlanArn :: Prelude.Maybe Prelude.Text,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply
    -- to workloads that are Savings Plans eligible.
    utilization :: Prelude.Maybe SavingsPlansUtilization
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
-- 'attributes', 'savingsPlansUtilizationDetail_attributes' - The attribute that applies to a specific @Dimension@.
--
-- 'savings', 'savingsPlansUtilizationDetail_savings' - The amount saved by using existing Savings Plans. Savings returns both
-- net savings from savings plans and also the @onDemandCostEquivalent@ of
-- the Savings Plans when considering the utilization rate.
--
-- 'savingsPlanArn', 'savingsPlansUtilizationDetail_savingsPlanArn' - The unique Amazon Resource Name (ARN) for a particular Savings Plan.
--
-- 'utilization', 'savingsPlansUtilizationDetail_utilization' - A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
newSavingsPlansUtilizationDetail ::
  SavingsPlansUtilizationDetail
newSavingsPlansUtilizationDetail =
  SavingsPlansUtilizationDetail'
    { amortizedCommitment =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      savings = Prelude.Nothing,
      savingsPlanArn = Prelude.Nothing,
      utilization = Prelude.Nothing
    }

-- | The total amortized commitment for a Savings Plans. Includes the sum of
-- the upfront and recurring Savings Plans fees.
savingsPlansUtilizationDetail_amortizedCommitment :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe SavingsPlansAmortizedCommitment)
savingsPlansUtilizationDetail_amortizedCommitment = Lens.lens (\SavingsPlansUtilizationDetail' {amortizedCommitment} -> amortizedCommitment) (\s@SavingsPlansUtilizationDetail' {} a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationDetail)

-- | The attribute that applies to a specific @Dimension@.
savingsPlansUtilizationDetail_attributes :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
savingsPlansUtilizationDetail_attributes = Lens.lens (\SavingsPlansUtilizationDetail' {attributes} -> attributes) (\s@SavingsPlansUtilizationDetail' {} a -> s {attributes = a} :: SavingsPlansUtilizationDetail) Prelude.. Lens.mapping Lens.coerced

-- | The amount saved by using existing Savings Plans. Savings returns both
-- net savings from savings plans and also the @onDemandCostEquivalent@ of
-- the Savings Plans when considering the utilization rate.
savingsPlansUtilizationDetail_savings :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe SavingsPlansSavings)
savingsPlansUtilizationDetail_savings = Lens.lens (\SavingsPlansUtilizationDetail' {savings} -> savings) (\s@SavingsPlansUtilizationDetail' {} a -> s {savings = a} :: SavingsPlansUtilizationDetail)

-- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
savingsPlansUtilizationDetail_savingsPlanArn :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe Prelude.Text)
savingsPlansUtilizationDetail_savingsPlanArn = Lens.lens (\SavingsPlansUtilizationDetail' {savingsPlanArn} -> savingsPlanArn) (\s@SavingsPlansUtilizationDetail' {} a -> s {savingsPlanArn = a} :: SavingsPlansUtilizationDetail)

-- | A ratio of your effectiveness of using existing Savings Plans to apply
-- to workloads that are Savings Plans eligible.
savingsPlansUtilizationDetail_utilization :: Lens.Lens' SavingsPlansUtilizationDetail (Prelude.Maybe SavingsPlansUtilization)
savingsPlansUtilizationDetail_utilization = Lens.lens (\SavingsPlansUtilizationDetail' {utilization} -> utilization) (\s@SavingsPlansUtilizationDetail' {} a -> s {utilization = a} :: SavingsPlansUtilizationDetail)

instance Data.FromJSON SavingsPlansUtilizationDetail where
  parseJSON =
    Data.withObject
      "SavingsPlansUtilizationDetail"
      ( \x ->
          SavingsPlansUtilizationDetail'
            Prelude.<$> (x Data..:? "AmortizedCommitment")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Savings")
            Prelude.<*> (x Data..:? "SavingsPlanArn")
            Prelude.<*> (x Data..:? "Utilization")
      )

instance
  Prelude.Hashable
    SavingsPlansUtilizationDetail
  where
  hashWithSalt _salt SavingsPlansUtilizationDetail' {..} =
    _salt `Prelude.hashWithSalt` amortizedCommitment
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` savings
      `Prelude.hashWithSalt` savingsPlanArn
      `Prelude.hashWithSalt` utilization

instance Prelude.NFData SavingsPlansUtilizationDetail where
  rnf SavingsPlansUtilizationDetail' {..} =
    Prelude.rnf amortizedCommitment
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf savings
      `Prelude.seq` Prelude.rnf savingsPlanArn
      `Prelude.seq` Prelude.rnf utilization
