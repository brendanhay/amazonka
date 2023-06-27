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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationOption where

import Amazonka.ComputeOptimizer.Types.ContainerRecommendation
import Amazonka.ComputeOptimizer.Types.ECSServiceProjectedUtilizationMetric
import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the recommendation options for an Amazon ECS service.
--
-- /See:/ 'newECSServiceRecommendationOption' smart constructor.
data ECSServiceRecommendationOption = ECSServiceRecommendationOption'
  { -- | The CPU and memory size recommendations for the containers within the
    -- task of your Amazon ECS service.
    containerRecommendations :: Prelude.Maybe [ContainerRecommendation],
    -- | The CPU size of the Amazon ECS service recommendation option.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The memory size of the Amazon ECS service recommendation option.
    memory :: Prelude.Maybe Prelude.Int,
    -- | An array of objects that describe the projected utilization metrics of
    -- the Amazon ECS service recommendation option.
    projectedUtilizationMetrics :: Prelude.Maybe [ECSServiceProjectedUtilizationMetric],
    savingsOpportunity :: Prelude.Maybe SavingsOpportunity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceRecommendationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerRecommendations', 'eCSServiceRecommendationOption_containerRecommendations' - The CPU and memory size recommendations for the containers within the
-- task of your Amazon ECS service.
--
-- 'cpu', 'eCSServiceRecommendationOption_cpu' - The CPU size of the Amazon ECS service recommendation option.
--
-- 'memory', 'eCSServiceRecommendationOption_memory' - The memory size of the Amazon ECS service recommendation option.
--
-- 'projectedUtilizationMetrics', 'eCSServiceRecommendationOption_projectedUtilizationMetrics' - An array of objects that describe the projected utilization metrics of
-- the Amazon ECS service recommendation option.
--
-- 'savingsOpportunity', 'eCSServiceRecommendationOption_savingsOpportunity' - Undocumented member.
newECSServiceRecommendationOption ::
  ECSServiceRecommendationOption
newECSServiceRecommendationOption =
  ECSServiceRecommendationOption'
    { containerRecommendations =
        Prelude.Nothing,
      cpu = Prelude.Nothing,
      memory = Prelude.Nothing,
      projectedUtilizationMetrics =
        Prelude.Nothing,
      savingsOpportunity = Prelude.Nothing
    }

-- | The CPU and memory size recommendations for the containers within the
-- task of your Amazon ECS service.
eCSServiceRecommendationOption_containerRecommendations :: Lens.Lens' ECSServiceRecommendationOption (Prelude.Maybe [ContainerRecommendation])
eCSServiceRecommendationOption_containerRecommendations = Lens.lens (\ECSServiceRecommendationOption' {containerRecommendations} -> containerRecommendations) (\s@ECSServiceRecommendationOption' {} a -> s {containerRecommendations = a} :: ECSServiceRecommendationOption) Prelude.. Lens.mapping Lens.coerced

-- | The CPU size of the Amazon ECS service recommendation option.
eCSServiceRecommendationOption_cpu :: Lens.Lens' ECSServiceRecommendationOption (Prelude.Maybe Prelude.Int)
eCSServiceRecommendationOption_cpu = Lens.lens (\ECSServiceRecommendationOption' {cpu} -> cpu) (\s@ECSServiceRecommendationOption' {} a -> s {cpu = a} :: ECSServiceRecommendationOption)

-- | The memory size of the Amazon ECS service recommendation option.
eCSServiceRecommendationOption_memory :: Lens.Lens' ECSServiceRecommendationOption (Prelude.Maybe Prelude.Int)
eCSServiceRecommendationOption_memory = Lens.lens (\ECSServiceRecommendationOption' {memory} -> memory) (\s@ECSServiceRecommendationOption' {} a -> s {memory = a} :: ECSServiceRecommendationOption)

-- | An array of objects that describe the projected utilization metrics of
-- the Amazon ECS service recommendation option.
eCSServiceRecommendationOption_projectedUtilizationMetrics :: Lens.Lens' ECSServiceRecommendationOption (Prelude.Maybe [ECSServiceProjectedUtilizationMetric])
eCSServiceRecommendationOption_projectedUtilizationMetrics = Lens.lens (\ECSServiceRecommendationOption' {projectedUtilizationMetrics} -> projectedUtilizationMetrics) (\s@ECSServiceRecommendationOption' {} a -> s {projectedUtilizationMetrics = a} :: ECSServiceRecommendationOption) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
eCSServiceRecommendationOption_savingsOpportunity :: Lens.Lens' ECSServiceRecommendationOption (Prelude.Maybe SavingsOpportunity)
eCSServiceRecommendationOption_savingsOpportunity = Lens.lens (\ECSServiceRecommendationOption' {savingsOpportunity} -> savingsOpportunity) (\s@ECSServiceRecommendationOption' {} a -> s {savingsOpportunity = a} :: ECSServiceRecommendationOption)

instance Data.FromJSON ECSServiceRecommendationOption where
  parseJSON =
    Data.withObject
      "ECSServiceRecommendationOption"
      ( \x ->
          ECSServiceRecommendationOption'
            Prelude.<$> ( x
                            Data..:? "containerRecommendations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> ( x
                            Data..:? "projectedUtilizationMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "savingsOpportunity")
      )

instance
  Prelude.Hashable
    ECSServiceRecommendationOption
  where
  hashWithSalt
    _salt
    ECSServiceRecommendationOption' {..} =
      _salt
        `Prelude.hashWithSalt` containerRecommendations
        `Prelude.hashWithSalt` cpu
        `Prelude.hashWithSalt` memory
        `Prelude.hashWithSalt` projectedUtilizationMetrics
        `Prelude.hashWithSalt` savingsOpportunity

instance
  Prelude.NFData
    ECSServiceRecommendationOption
  where
  rnf ECSServiceRecommendationOption' {..} =
    Prelude.rnf containerRecommendations
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf projectedUtilizationMetrics
      `Prelude.seq` Prelude.rnf savingsOpportunity
