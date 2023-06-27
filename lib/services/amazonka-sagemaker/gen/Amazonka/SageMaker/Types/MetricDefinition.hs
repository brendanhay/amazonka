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
-- Module      : Amazonka.SageMaker.Types.MetricDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MetricDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a metric that the training algorithm writes to @stderr@ or
-- @stdout@. You can view these logs to understand how your training job
-- performs and check for any errors encountered during training. SageMaker
-- hyperparameter tuning captures all defined metrics. Specify one of the
-- defined metrics to use as an objective metric using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTrainingJobDefinition.html#sagemaker-Type-HyperParameterTrainingJobDefinition-TuningObjective TuningObjective>
-- parameter in the @HyperParameterTrainingJobDefinition@ API to evaluate
-- job performance during hyperparameter tuning.
--
-- /See:/ 'newMetricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { -- | The name of the metric.
    name :: Prelude.Text,
    -- | A regular expression that searches the output of a training job and gets
    -- the value of the metric. For more information about using regular
    -- expressions to define metrics, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics-variables.html Defining metrics and environment variables>.
    regex :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metricDefinition_name' - The name of the metric.
--
-- 'regex', 'metricDefinition_regex' - A regular expression that searches the output of a training job and gets
-- the value of the metric. For more information about using regular
-- expressions to define metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics-variables.html Defining metrics and environment variables>.
newMetricDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'regex'
  Prelude.Text ->
  MetricDefinition
newMetricDefinition pName_ pRegex_ =
  MetricDefinition' {name = pName_, regex = pRegex_}

-- | The name of the metric.
metricDefinition_name :: Lens.Lens' MetricDefinition Prelude.Text
metricDefinition_name = Lens.lens (\MetricDefinition' {name} -> name) (\s@MetricDefinition' {} a -> s {name = a} :: MetricDefinition)

-- | A regular expression that searches the output of a training job and gets
-- the value of the metric. For more information about using regular
-- expressions to define metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics-variables.html Defining metrics and environment variables>.
metricDefinition_regex :: Lens.Lens' MetricDefinition Prelude.Text
metricDefinition_regex = Lens.lens (\MetricDefinition' {regex} -> regex) (\s@MetricDefinition' {} a -> s {regex = a} :: MetricDefinition)

instance Data.FromJSON MetricDefinition where
  parseJSON =
    Data.withObject
      "MetricDefinition"
      ( \x ->
          MetricDefinition'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Regex")
      )

instance Prelude.Hashable MetricDefinition where
  hashWithSalt _salt MetricDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regex

instance Prelude.NFData MetricDefinition where
  rnf MetricDefinition' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf regex

instance Data.ToJSON MetricDefinition where
  toJSON MetricDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Regex" Data..= regex)
          ]
      )
