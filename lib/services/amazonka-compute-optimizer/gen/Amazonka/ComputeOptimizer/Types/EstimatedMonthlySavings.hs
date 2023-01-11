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
-- Module      : Amazonka.ComputeOptimizer.Types.EstimatedMonthlySavings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.EstimatedMonthlySavings where

import Amazonka.ComputeOptimizer.Types.Currency
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the estimated monthly savings amount possible, based on
-- On-Demand instance pricing, by adopting Compute Optimizer
-- recommendations for a given resource.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/view-ec2-recommendations.html#ec2-savings-calculation Estimated monthly savings and savings opportunities>
-- in the /Compute Optimizer User Guide/.
--
-- /See:/ 'newEstimatedMonthlySavings' smart constructor.
data EstimatedMonthlySavings = EstimatedMonthlySavings'
  { -- | The currency of the estimated monthly savings.
    currency :: Prelude.Maybe Currency,
    -- | The value of the estimated monthly savings.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EstimatedMonthlySavings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currency', 'estimatedMonthlySavings_currency' - The currency of the estimated monthly savings.
--
-- 'value', 'estimatedMonthlySavings_value' - The value of the estimated monthly savings.
newEstimatedMonthlySavings ::
  EstimatedMonthlySavings
newEstimatedMonthlySavings =
  EstimatedMonthlySavings'
    { currency =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The currency of the estimated monthly savings.
estimatedMonthlySavings_currency :: Lens.Lens' EstimatedMonthlySavings (Prelude.Maybe Currency)
estimatedMonthlySavings_currency = Lens.lens (\EstimatedMonthlySavings' {currency} -> currency) (\s@EstimatedMonthlySavings' {} a -> s {currency = a} :: EstimatedMonthlySavings)

-- | The value of the estimated monthly savings.
estimatedMonthlySavings_value :: Lens.Lens' EstimatedMonthlySavings (Prelude.Maybe Prelude.Double)
estimatedMonthlySavings_value = Lens.lens (\EstimatedMonthlySavings' {value} -> value) (\s@EstimatedMonthlySavings' {} a -> s {value = a} :: EstimatedMonthlySavings)

instance Data.FromJSON EstimatedMonthlySavings where
  parseJSON =
    Data.withObject
      "EstimatedMonthlySavings"
      ( \x ->
          EstimatedMonthlySavings'
            Prelude.<$> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable EstimatedMonthlySavings where
  hashWithSalt _salt EstimatedMonthlySavings' {..} =
    _salt `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` value

instance Prelude.NFData EstimatedMonthlySavings where
  rnf EstimatedMonthlySavings' {..} =
    Prelude.rnf currency
      `Prelude.seq` Prelude.rnf value
