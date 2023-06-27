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
-- Module      : Amazonka.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Shows the latest objective metric emitted by a training job that was
-- launched by a hyperparameter tuning job. You define the objective metric
-- in the @HyperParameterTuningJobObjective@ parameter of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTuningJobConfig.html HyperParameterTuningJobConfig>.
--
-- /See:/ 'newFinalHyperParameterTuningJobObjectiveMetric' smart constructor.
data FinalHyperParameterTuningJobObjectiveMetric = FinalHyperParameterTuningJobObjectiveMetric'
  { -- | Select if you want to minimize or maximize the objective metric during
    -- hyperparameter tuning.
    type' :: Prelude.Maybe HyperParameterTuningJobObjectiveType,
    -- | The name of the objective metric. For SageMaker built-in algorithms,
    -- metrics are defined per algorithm. See the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/xgboost-tuning.html metrics for XGBoost>
    -- as an example. You can also use a custom algorithm for training and
    -- define your own metrics. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics-variables.html Define metrics and environment variables>.
    metricName :: Prelude.Text,
    -- | The value of the objective metric.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FinalHyperParameterTuningJobObjectiveMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'finalHyperParameterTuningJobObjectiveMetric_type' - Select if you want to minimize or maximize the objective metric during
-- hyperparameter tuning.
--
-- 'metricName', 'finalHyperParameterTuningJobObjectiveMetric_metricName' - The name of the objective metric. For SageMaker built-in algorithms,
-- metrics are defined per algorithm. See the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/xgboost-tuning.html metrics for XGBoost>
-- as an example. You can also use a custom algorithm for training and
-- define your own metrics. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics-variables.html Define metrics and environment variables>.
--
-- 'value', 'finalHyperParameterTuningJobObjectiveMetric_value' - The value of the objective metric.
newFinalHyperParameterTuningJobObjectiveMetric ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'value'
  Prelude.Double ->
  FinalHyperParameterTuningJobObjectiveMetric
newFinalHyperParameterTuningJobObjectiveMetric
  pMetricName_
  pValue_ =
    FinalHyperParameterTuningJobObjectiveMetric'
      { type' =
          Prelude.Nothing,
        metricName = pMetricName_,
        value = pValue_
      }

-- | Select if you want to minimize or maximize the objective metric during
-- hyperparameter tuning.
finalHyperParameterTuningJobObjectiveMetric_type :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric (Prelude.Maybe HyperParameterTuningJobObjectiveType)
finalHyperParameterTuningJobObjectiveMetric_type = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {type'} -> type') (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {type' = a} :: FinalHyperParameterTuningJobObjectiveMetric)

-- | The name of the objective metric. For SageMaker built-in algorithms,
-- metrics are defined per algorithm. See the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/xgboost-tuning.html metrics for XGBoost>
-- as an example. You can also use a custom algorithm for training and
-- define your own metrics. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics-variables.html Define metrics and environment variables>.
finalHyperParameterTuningJobObjectiveMetric_metricName :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Prelude.Text
finalHyperParameterTuningJobObjectiveMetric_metricName = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {metricName} -> metricName) (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {metricName = a} :: FinalHyperParameterTuningJobObjectiveMetric)

-- | The value of the objective metric.
finalHyperParameterTuningJobObjectiveMetric_value :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Prelude.Double
finalHyperParameterTuningJobObjectiveMetric_value = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {value} -> value) (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {value = a} :: FinalHyperParameterTuningJobObjectiveMetric)

instance
  Data.FromJSON
    FinalHyperParameterTuningJobObjectiveMetric
  where
  parseJSON =
    Data.withObject
      "FinalHyperParameterTuningJobObjectiveMetric"
      ( \x ->
          FinalHyperParameterTuningJobObjectiveMetric'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..: "MetricName")
            Prelude.<*> (x Data..: "Value")
      )

instance
  Prelude.Hashable
    FinalHyperParameterTuningJobObjectiveMetric
  where
  hashWithSalt
    _salt
    FinalHyperParameterTuningJobObjectiveMetric' {..} =
      _salt
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    FinalHyperParameterTuningJobObjectiveMetric
  where
  rnf FinalHyperParameterTuningJobObjectiveMetric' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf value
