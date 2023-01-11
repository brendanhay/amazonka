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
-- Module      : Amazonka.ComputeOptimizer.Types.SavingsOpportunity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.SavingsOpportunity where

import Amazonka.ComputeOptimizer.Types.EstimatedMonthlySavings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the savings opportunity for recommendations of a given
-- resource type or for the recommendation option of an individual
-- resource.
--
-- Savings opportunity represents the estimated monthly savings you can
-- achieve by implementing a given Compute Optimizer recommendation.
--
-- Savings opportunity data requires that you opt in to Cost Explorer, as
-- well as activate __Receive Amazon EC2 resource recommendations__ in the
-- Cost Explorer preferences page. That creates a connection between Cost
-- Explorer and Compute Optimizer. With this connection, Cost Explorer
-- generates savings estimates considering the price of existing resources,
-- the price of recommended resources, and historical usage data. Estimated
-- monthly savings reflects the projected dollar savings associated with
-- each of the recommendations generated. For more information, see
-- <https://docs.aws.amazon.com/cost-management/latest/userguide/ce-enable.html Enabling Cost Explorer>
-- and
-- <https://docs.aws.amazon.com/cost-management/latest/userguide/ce-rightsizing.html Optimizing your cost with Rightsizing Recommendations>
-- in the /Cost Management User Guide/.
--
-- /See:/ 'newSavingsOpportunity' smart constructor.
data SavingsOpportunity = SavingsOpportunity'
  { -- | An object that describes the estimated monthly savings amount possible,
    -- based on On-Demand instance pricing, by adopting Compute Optimizer
    -- recommendations for a given resource.
    estimatedMonthlySavings :: Prelude.Maybe EstimatedMonthlySavings,
    -- | The estimated monthly savings possible as a percentage of monthly cost
    -- by adopting Compute Optimizer recommendations for a given resource.
    savingsOpportunityPercentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsOpportunity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavings', 'savingsOpportunity_estimatedMonthlySavings' - An object that describes the estimated monthly savings amount possible,
-- based on On-Demand instance pricing, by adopting Compute Optimizer
-- recommendations for a given resource.
--
-- 'savingsOpportunityPercentage', 'savingsOpportunity_savingsOpportunityPercentage' - The estimated monthly savings possible as a percentage of monthly cost
-- by adopting Compute Optimizer recommendations for a given resource.
newSavingsOpportunity ::
  SavingsOpportunity
newSavingsOpportunity =
  SavingsOpportunity'
    { estimatedMonthlySavings =
        Prelude.Nothing,
      savingsOpportunityPercentage = Prelude.Nothing
    }

-- | An object that describes the estimated monthly savings amount possible,
-- based on On-Demand instance pricing, by adopting Compute Optimizer
-- recommendations for a given resource.
savingsOpportunity_estimatedMonthlySavings :: Lens.Lens' SavingsOpportunity (Prelude.Maybe EstimatedMonthlySavings)
savingsOpportunity_estimatedMonthlySavings = Lens.lens (\SavingsOpportunity' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@SavingsOpportunity' {} a -> s {estimatedMonthlySavings = a} :: SavingsOpportunity)

-- | The estimated monthly savings possible as a percentage of monthly cost
-- by adopting Compute Optimizer recommendations for a given resource.
savingsOpportunity_savingsOpportunityPercentage :: Lens.Lens' SavingsOpportunity (Prelude.Maybe Prelude.Double)
savingsOpportunity_savingsOpportunityPercentage = Lens.lens (\SavingsOpportunity' {savingsOpportunityPercentage} -> savingsOpportunityPercentage) (\s@SavingsOpportunity' {} a -> s {savingsOpportunityPercentage = a} :: SavingsOpportunity)

instance Data.FromJSON SavingsOpportunity where
  parseJSON =
    Data.withObject
      "SavingsOpportunity"
      ( \x ->
          SavingsOpportunity'
            Prelude.<$> (x Data..:? "estimatedMonthlySavings")
            Prelude.<*> (x Data..:? "savingsOpportunityPercentage")
      )

instance Prelude.Hashable SavingsOpportunity where
  hashWithSalt _salt SavingsOpportunity' {..} =
    _salt
      `Prelude.hashWithSalt` estimatedMonthlySavings
      `Prelude.hashWithSalt` savingsOpportunityPercentage

instance Prelude.NFData SavingsOpportunity where
  rnf SavingsOpportunity' {..} =
    Prelude.rnf estimatedMonthlySavings
      `Prelude.seq` Prelude.rnf savingsOpportunityPercentage
