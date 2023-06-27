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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobObjective
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobObjective where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLMetricEnum

-- | Specifies a metric to minimize or maximize as the objective of a job.
--
-- /See:/ 'newAutoMLJobObjective' smart constructor.
data AutoMLJobObjective = AutoMLJobObjective'
  { -- | The name of the objective metric used to measure the predictive quality
    -- of a machine learning system. During training, the model\'s parameters
    -- are updated iteratively to optimize its performance based on the
    -- feedback provided by the objective metric when evaluating the model on
    -- the validation dataset.
    --
    -- For the list of all available metrics supported by Autopilot, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html#autopilot-metrics Autopilot metrics>.
    --
    -- If you do not specify a metric explicitly, the default behavior is to
    -- automatically use:
    --
    -- -   For tabular problem types:
    --
    --     -   Regression: @MSE@.
    --
    --     -   Binary classification: @F1@.
    --
    --     -   Multiclass classification: @Accuracy@.
    --
    -- -   For image or text classification problem types: @Accuracy@
    metricName :: AutoMLMetricEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobObjective' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'autoMLJobObjective_metricName' - The name of the objective metric used to measure the predictive quality
-- of a machine learning system. During training, the model\'s parameters
-- are updated iteratively to optimize its performance based on the
-- feedback provided by the objective metric when evaluating the model on
-- the validation dataset.
--
-- For the list of all available metrics supported by Autopilot, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html#autopilot-metrics Autopilot metrics>.
--
-- If you do not specify a metric explicitly, the default behavior is to
-- automatically use:
--
-- -   For tabular problem types:
--
--     -   Regression: @MSE@.
--
--     -   Binary classification: @F1@.
--
--     -   Multiclass classification: @Accuracy@.
--
-- -   For image or text classification problem types: @Accuracy@
newAutoMLJobObjective ::
  -- | 'metricName'
  AutoMLMetricEnum ->
  AutoMLJobObjective
newAutoMLJobObjective pMetricName_ =
  AutoMLJobObjective' {metricName = pMetricName_}

-- | The name of the objective metric used to measure the predictive quality
-- of a machine learning system. During training, the model\'s parameters
-- are updated iteratively to optimize its performance based on the
-- feedback provided by the objective metric when evaluating the model on
-- the validation dataset.
--
-- For the list of all available metrics supported by Autopilot, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html#autopilot-metrics Autopilot metrics>.
--
-- If you do not specify a metric explicitly, the default behavior is to
-- automatically use:
--
-- -   For tabular problem types:
--
--     -   Regression: @MSE@.
--
--     -   Binary classification: @F1@.
--
--     -   Multiclass classification: @Accuracy@.
--
-- -   For image or text classification problem types: @Accuracy@
autoMLJobObjective_metricName :: Lens.Lens' AutoMLJobObjective AutoMLMetricEnum
autoMLJobObjective_metricName = Lens.lens (\AutoMLJobObjective' {metricName} -> metricName) (\s@AutoMLJobObjective' {} a -> s {metricName = a} :: AutoMLJobObjective)

instance Data.FromJSON AutoMLJobObjective where
  parseJSON =
    Data.withObject
      "AutoMLJobObjective"
      ( \x ->
          AutoMLJobObjective'
            Prelude.<$> (x Data..: "MetricName")
      )

instance Prelude.Hashable AutoMLJobObjective where
  hashWithSalt _salt AutoMLJobObjective' {..} =
    _salt `Prelude.hashWithSalt` metricName

instance Prelude.NFData AutoMLJobObjective where
  rnf AutoMLJobObjective' {..} = Prelude.rnf metricName

instance Data.ToJSON AutoMLJobObjective where
  toJSON AutoMLJobObjective' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MetricName" Data..= metricName)]
      )
