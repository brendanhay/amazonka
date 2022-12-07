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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption where

import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryProjectedMetric
import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a recommendation option for an Lambda function.
--
-- /See:/ 'newLambdaFunctionMemoryRecommendationOption' smart constructor.
data LambdaFunctionMemoryRecommendationOption = LambdaFunctionMemoryRecommendationOption'
  { -- | The memory size, in MB, of the function recommendation option.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | An object that describes the savings opportunity for the Lambda function
    -- recommendation option. Savings opportunity includes the estimated
    -- monthly savings amount and percentage.
    savingsOpportunity :: Prelude.Maybe SavingsOpportunity,
    -- | The rank of the function recommendation option.
    --
    -- The top recommendation option is ranked as @1@.
    rank :: Prelude.Maybe Prelude.Int,
    -- | An array of objects that describe the projected utilization metrics of
    -- the function recommendation option.
    projectedUtilizationMetrics :: Prelude.Maybe [LambdaFunctionMemoryProjectedMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionMemoryRecommendationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memorySize', 'lambdaFunctionMemoryRecommendationOption_memorySize' - The memory size, in MB, of the function recommendation option.
--
-- 'savingsOpportunity', 'lambdaFunctionMemoryRecommendationOption_savingsOpportunity' - An object that describes the savings opportunity for the Lambda function
-- recommendation option. Savings opportunity includes the estimated
-- monthly savings amount and percentage.
--
-- 'rank', 'lambdaFunctionMemoryRecommendationOption_rank' - The rank of the function recommendation option.
--
-- The top recommendation option is ranked as @1@.
--
-- 'projectedUtilizationMetrics', 'lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics' - An array of objects that describe the projected utilization metrics of
-- the function recommendation option.
newLambdaFunctionMemoryRecommendationOption ::
  LambdaFunctionMemoryRecommendationOption
newLambdaFunctionMemoryRecommendationOption =
  LambdaFunctionMemoryRecommendationOption'
    { memorySize =
        Prelude.Nothing,
      savingsOpportunity =
        Prelude.Nothing,
      rank = Prelude.Nothing,
      projectedUtilizationMetrics =
        Prelude.Nothing
    }

-- | The memory size, in MB, of the function recommendation option.
lambdaFunctionMemoryRecommendationOption_memorySize :: Lens.Lens' LambdaFunctionMemoryRecommendationOption (Prelude.Maybe Prelude.Int)
lambdaFunctionMemoryRecommendationOption_memorySize = Lens.lens (\LambdaFunctionMemoryRecommendationOption' {memorySize} -> memorySize) (\s@LambdaFunctionMemoryRecommendationOption' {} a -> s {memorySize = a} :: LambdaFunctionMemoryRecommendationOption)

-- | An object that describes the savings opportunity for the Lambda function
-- recommendation option. Savings opportunity includes the estimated
-- monthly savings amount and percentage.
lambdaFunctionMemoryRecommendationOption_savingsOpportunity :: Lens.Lens' LambdaFunctionMemoryRecommendationOption (Prelude.Maybe SavingsOpportunity)
lambdaFunctionMemoryRecommendationOption_savingsOpportunity = Lens.lens (\LambdaFunctionMemoryRecommendationOption' {savingsOpportunity} -> savingsOpportunity) (\s@LambdaFunctionMemoryRecommendationOption' {} a -> s {savingsOpportunity = a} :: LambdaFunctionMemoryRecommendationOption)

-- | The rank of the function recommendation option.
--
-- The top recommendation option is ranked as @1@.
lambdaFunctionMemoryRecommendationOption_rank :: Lens.Lens' LambdaFunctionMemoryRecommendationOption (Prelude.Maybe Prelude.Int)
lambdaFunctionMemoryRecommendationOption_rank = Lens.lens (\LambdaFunctionMemoryRecommendationOption' {rank} -> rank) (\s@LambdaFunctionMemoryRecommendationOption' {} a -> s {rank = a} :: LambdaFunctionMemoryRecommendationOption)

-- | An array of objects that describe the projected utilization metrics of
-- the function recommendation option.
lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics :: Lens.Lens' LambdaFunctionMemoryRecommendationOption (Prelude.Maybe [LambdaFunctionMemoryProjectedMetric])
lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics = Lens.lens (\LambdaFunctionMemoryRecommendationOption' {projectedUtilizationMetrics} -> projectedUtilizationMetrics) (\s@LambdaFunctionMemoryRecommendationOption' {} a -> s {projectedUtilizationMetrics = a} :: LambdaFunctionMemoryRecommendationOption) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    LambdaFunctionMemoryRecommendationOption
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionMemoryRecommendationOption"
      ( \x ->
          LambdaFunctionMemoryRecommendationOption'
            Prelude.<$> (x Data..:? "memorySize")
            Prelude.<*> (x Data..:? "savingsOpportunity")
            Prelude.<*> (x Data..:? "rank")
            Prelude.<*> ( x Data..:? "projectedUtilizationMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    LambdaFunctionMemoryRecommendationOption
  where
  hashWithSalt
    _salt
    LambdaFunctionMemoryRecommendationOption' {..} =
      _salt `Prelude.hashWithSalt` memorySize
        `Prelude.hashWithSalt` savingsOpportunity
        `Prelude.hashWithSalt` rank
        `Prelude.hashWithSalt` projectedUtilizationMetrics

instance
  Prelude.NFData
    LambdaFunctionMemoryRecommendationOption
  where
  rnf LambdaFunctionMemoryRecommendationOption' {..} =
    Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf savingsOpportunity
      `Prelude.seq` Prelude.rnf rank
      `Prelude.seq` Prelude.rnf projectedUtilizationMetrics
