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
-- Module      : Amazonka.Forecast.Types.WeightedQuantileLoss
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.WeightedQuantileLoss where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The weighted loss value for a quantile. This object is part of the
-- Metrics object.
--
-- /See:/ 'newWeightedQuantileLoss' smart constructor.
data WeightedQuantileLoss = WeightedQuantileLoss'
  { -- | The quantile. Quantiles divide a probability distribution into regions
    -- of equal probability. For example, if the distribution was divided into
    -- 5 regions of equal probability, the quantiles would be 0.2, 0.4, 0.6,
    -- and 0.8.
    quantile :: Prelude.Maybe Prelude.Double,
    -- | The difference between the predicted value and the actual value over the
    -- quantile, weighted (normalized) by dividing by the sum over all
    -- quantiles.
    lossValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WeightedQuantileLoss' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantile', 'weightedQuantileLoss_quantile' - The quantile. Quantiles divide a probability distribution into regions
-- of equal probability. For example, if the distribution was divided into
-- 5 regions of equal probability, the quantiles would be 0.2, 0.4, 0.6,
-- and 0.8.
--
-- 'lossValue', 'weightedQuantileLoss_lossValue' - The difference between the predicted value and the actual value over the
-- quantile, weighted (normalized) by dividing by the sum over all
-- quantiles.
newWeightedQuantileLoss ::
  WeightedQuantileLoss
newWeightedQuantileLoss =
  WeightedQuantileLoss'
    { quantile = Prelude.Nothing,
      lossValue = Prelude.Nothing
    }

-- | The quantile. Quantiles divide a probability distribution into regions
-- of equal probability. For example, if the distribution was divided into
-- 5 regions of equal probability, the quantiles would be 0.2, 0.4, 0.6,
-- and 0.8.
weightedQuantileLoss_quantile :: Lens.Lens' WeightedQuantileLoss (Prelude.Maybe Prelude.Double)
weightedQuantileLoss_quantile = Lens.lens (\WeightedQuantileLoss' {quantile} -> quantile) (\s@WeightedQuantileLoss' {} a -> s {quantile = a} :: WeightedQuantileLoss)

-- | The difference between the predicted value and the actual value over the
-- quantile, weighted (normalized) by dividing by the sum over all
-- quantiles.
weightedQuantileLoss_lossValue :: Lens.Lens' WeightedQuantileLoss (Prelude.Maybe Prelude.Double)
weightedQuantileLoss_lossValue = Lens.lens (\WeightedQuantileLoss' {lossValue} -> lossValue) (\s@WeightedQuantileLoss' {} a -> s {lossValue = a} :: WeightedQuantileLoss)

instance Core.FromJSON WeightedQuantileLoss where
  parseJSON =
    Core.withObject
      "WeightedQuantileLoss"
      ( \x ->
          WeightedQuantileLoss'
            Prelude.<$> (x Core..:? "Quantile")
            Prelude.<*> (x Core..:? "LossValue")
      )

instance Prelude.Hashable WeightedQuantileLoss where
  hashWithSalt _salt WeightedQuantileLoss' {..} =
    _salt `Prelude.hashWithSalt` quantile
      `Prelude.hashWithSalt` lossValue

instance Prelude.NFData WeightedQuantileLoss where
  rnf WeightedQuantileLoss' {..} =
    Prelude.rnf quantile
      `Prelude.seq` Prelude.rnf lossValue
