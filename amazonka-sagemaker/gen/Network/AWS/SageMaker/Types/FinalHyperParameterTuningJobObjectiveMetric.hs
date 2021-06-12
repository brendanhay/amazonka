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
-- Module      : Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Shows the final value for the objective metric for a training job that
-- was launched by a hyperparameter tuning job. You define the objective
-- metric in the @HyperParameterTuningJobObjective@ parameter of
-- HyperParameterTuningJobConfig.
--
-- /See:/ 'newFinalHyperParameterTuningJobObjectiveMetric' smart constructor.
data FinalHyperParameterTuningJobObjectiveMetric = FinalHyperParameterTuningJobObjectiveMetric'
  { -- | Whether to minimize or maximize the objective metric. Valid values are
    -- Minimize and Maximize.
    type' :: Core.Maybe HyperParameterTuningJobObjectiveType,
    -- | The name of the objective metric.
    metricName :: Core.Text,
    -- | The value of the objective metric.
    value :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FinalHyperParameterTuningJobObjectiveMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'finalHyperParameterTuningJobObjectiveMetric_type' - Whether to minimize or maximize the objective metric. Valid values are
-- Minimize and Maximize.
--
-- 'metricName', 'finalHyperParameterTuningJobObjectiveMetric_metricName' - The name of the objective metric.
--
-- 'value', 'finalHyperParameterTuningJobObjectiveMetric_value' - The value of the objective metric.
newFinalHyperParameterTuningJobObjectiveMetric ::
  -- | 'metricName'
  Core.Text ->
  -- | 'value'
  Core.Double ->
  FinalHyperParameterTuningJobObjectiveMetric
newFinalHyperParameterTuningJobObjectiveMetric
  pMetricName_
  pValue_ =
    FinalHyperParameterTuningJobObjectiveMetric'
      { type' =
          Core.Nothing,
        metricName = pMetricName_,
        value = pValue_
      }

-- | Whether to minimize or maximize the objective metric. Valid values are
-- Minimize and Maximize.
finalHyperParameterTuningJobObjectiveMetric_type :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric (Core.Maybe HyperParameterTuningJobObjectiveType)
finalHyperParameterTuningJobObjectiveMetric_type = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {type'} -> type') (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {type' = a} :: FinalHyperParameterTuningJobObjectiveMetric)

-- | The name of the objective metric.
finalHyperParameterTuningJobObjectiveMetric_metricName :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Core.Text
finalHyperParameterTuningJobObjectiveMetric_metricName = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {metricName} -> metricName) (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {metricName = a} :: FinalHyperParameterTuningJobObjectiveMetric)

-- | The value of the objective metric.
finalHyperParameterTuningJobObjectiveMetric_value :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Core.Double
finalHyperParameterTuningJobObjectiveMetric_value = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {value} -> value) (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {value = a} :: FinalHyperParameterTuningJobObjectiveMetric)

instance
  Core.FromJSON
    FinalHyperParameterTuningJobObjectiveMetric
  where
  parseJSON =
    Core.withObject
      "FinalHyperParameterTuningJobObjectiveMetric"
      ( \x ->
          FinalHyperParameterTuningJobObjectiveMetric'
            Core.<$> (x Core..:? "Type")
            Core.<*> (x Core..: "MetricName")
            Core.<*> (x Core..: "Value")
      )

instance
  Core.Hashable
    FinalHyperParameterTuningJobObjectiveMetric

instance
  Core.NFData
    FinalHyperParameterTuningJobObjectiveMetric
