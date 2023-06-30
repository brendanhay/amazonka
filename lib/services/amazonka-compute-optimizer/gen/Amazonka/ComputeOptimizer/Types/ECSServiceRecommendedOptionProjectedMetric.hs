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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendedOptionProjectedMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendedOptionProjectedMetric where

import Amazonka.ComputeOptimizer.Types.ECSServiceProjectedMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the projected metrics of an Amazon ECS service recommendation
-- option.
--
-- To determine the performance difference between your current ECS service
-- and the recommended option, compare the metric data of your service
-- against its projected metric data.
--
-- /See:/ 'newECSServiceRecommendedOptionProjectedMetric' smart constructor.
data ECSServiceRecommendedOptionProjectedMetric = ECSServiceRecommendedOptionProjectedMetric'
  { -- | An array of objects that describe the projected metric.
    projectedMetrics :: Prelude.Maybe [ECSServiceProjectedMetric],
    -- | The recommended CPU size for the ECS service.
    recommendedCpuUnits :: Prelude.Maybe Prelude.Int,
    -- | The recommended memory size for the ECS service.
    recommendedMemorySize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceRecommendedOptionProjectedMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectedMetrics', 'eCSServiceRecommendedOptionProjectedMetric_projectedMetrics' - An array of objects that describe the projected metric.
--
-- 'recommendedCpuUnits', 'eCSServiceRecommendedOptionProjectedMetric_recommendedCpuUnits' - The recommended CPU size for the ECS service.
--
-- 'recommendedMemorySize', 'eCSServiceRecommendedOptionProjectedMetric_recommendedMemorySize' - The recommended memory size for the ECS service.
newECSServiceRecommendedOptionProjectedMetric ::
  ECSServiceRecommendedOptionProjectedMetric
newECSServiceRecommendedOptionProjectedMetric =
  ECSServiceRecommendedOptionProjectedMetric'
    { projectedMetrics =
        Prelude.Nothing,
      recommendedCpuUnits =
        Prelude.Nothing,
      recommendedMemorySize =
        Prelude.Nothing
    }

-- | An array of objects that describe the projected metric.
eCSServiceRecommendedOptionProjectedMetric_projectedMetrics :: Lens.Lens' ECSServiceRecommendedOptionProjectedMetric (Prelude.Maybe [ECSServiceProjectedMetric])
eCSServiceRecommendedOptionProjectedMetric_projectedMetrics = Lens.lens (\ECSServiceRecommendedOptionProjectedMetric' {projectedMetrics} -> projectedMetrics) (\s@ECSServiceRecommendedOptionProjectedMetric' {} a -> s {projectedMetrics = a} :: ECSServiceRecommendedOptionProjectedMetric) Prelude.. Lens.mapping Lens.coerced

-- | The recommended CPU size for the ECS service.
eCSServiceRecommendedOptionProjectedMetric_recommendedCpuUnits :: Lens.Lens' ECSServiceRecommendedOptionProjectedMetric (Prelude.Maybe Prelude.Int)
eCSServiceRecommendedOptionProjectedMetric_recommendedCpuUnits = Lens.lens (\ECSServiceRecommendedOptionProjectedMetric' {recommendedCpuUnits} -> recommendedCpuUnits) (\s@ECSServiceRecommendedOptionProjectedMetric' {} a -> s {recommendedCpuUnits = a} :: ECSServiceRecommendedOptionProjectedMetric)

-- | The recommended memory size for the ECS service.
eCSServiceRecommendedOptionProjectedMetric_recommendedMemorySize :: Lens.Lens' ECSServiceRecommendedOptionProjectedMetric (Prelude.Maybe Prelude.Int)
eCSServiceRecommendedOptionProjectedMetric_recommendedMemorySize = Lens.lens (\ECSServiceRecommendedOptionProjectedMetric' {recommendedMemorySize} -> recommendedMemorySize) (\s@ECSServiceRecommendedOptionProjectedMetric' {} a -> s {recommendedMemorySize = a} :: ECSServiceRecommendedOptionProjectedMetric)

instance
  Data.FromJSON
    ECSServiceRecommendedOptionProjectedMetric
  where
  parseJSON =
    Data.withObject
      "ECSServiceRecommendedOptionProjectedMetric"
      ( \x ->
          ECSServiceRecommendedOptionProjectedMetric'
            Prelude.<$> ( x
                            Data..:? "projectedMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "recommendedCpuUnits")
            Prelude.<*> (x Data..:? "recommendedMemorySize")
      )

instance
  Prelude.Hashable
    ECSServiceRecommendedOptionProjectedMetric
  where
  hashWithSalt
    _salt
    ECSServiceRecommendedOptionProjectedMetric' {..} =
      _salt
        `Prelude.hashWithSalt` projectedMetrics
        `Prelude.hashWithSalt` recommendedCpuUnits
        `Prelude.hashWithSalt` recommendedMemorySize

instance
  Prelude.NFData
    ECSServiceRecommendedOptionProjectedMetric
  where
  rnf ECSServiceRecommendedOptionProjectedMetric' {..} =
    Prelude.rnf projectedMetrics
      `Prelude.seq` Prelude.rnf recommendedCpuUnits
      `Prelude.seq` Prelude.rnf recommendedMemorySize
