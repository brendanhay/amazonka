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
-- Module      : Network.AWS.SageMaker.Types.MetricDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies a metric that the training algorithm writes to @stderr@ or
-- @stdout@ . Amazon SageMakerhyperparameter tuning captures all defined
-- metrics. You specify one metric that a hyperparameter tuning job uses as
-- its objective metric to choose the best training job.
--
-- /See:/ 'newMetricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { -- | The name of the metric.
    name :: Core.Text,
    -- | A regular expression that searches the output of a training job and gets
    -- the value of the metric. For more information about using regular
    -- expressions to define metrics, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics>.
    regex :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics>.
newMetricDefinition ::
  -- | 'name'
  Core.Text ->
  -- | 'regex'
  Core.Text ->
  MetricDefinition
newMetricDefinition pName_ pRegex_ =
  MetricDefinition' {name = pName_, regex = pRegex_}

-- | The name of the metric.
metricDefinition_name :: Lens.Lens' MetricDefinition Core.Text
metricDefinition_name = Lens.lens (\MetricDefinition' {name} -> name) (\s@MetricDefinition' {} a -> s {name = a} :: MetricDefinition)

-- | A regular expression that searches the output of a training job and gets
-- the value of the metric. For more information about using regular
-- expressions to define metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics>.
metricDefinition_regex :: Lens.Lens' MetricDefinition Core.Text
metricDefinition_regex = Lens.lens (\MetricDefinition' {regex} -> regex) (\s@MetricDefinition' {} a -> s {regex = a} :: MetricDefinition)

instance Core.FromJSON MetricDefinition where
  parseJSON =
    Core.withObject
      "MetricDefinition"
      ( \x ->
          MetricDefinition'
            Core.<$> (x Core..: "Name") Core.<*> (x Core..: "Regex")
      )

instance Core.Hashable MetricDefinition

instance Core.NFData MetricDefinition

instance Core.ToJSON MetricDefinition where
  toJSON MetricDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Regex" Core..= regex)
          ]
      )
