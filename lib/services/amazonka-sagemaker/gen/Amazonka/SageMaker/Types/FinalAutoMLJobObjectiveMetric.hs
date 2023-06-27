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
-- Module      : Amazonka.SageMaker.Types.FinalAutoMLJobObjectiveMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FinalAutoMLJobObjectiveMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobObjectiveType
import Amazonka.SageMaker.Types.AutoMLMetricEnum

-- | The best candidate result from an AutoML training job.
--
-- /See:/ 'newFinalAutoMLJobObjectiveMetric' smart constructor.
data FinalAutoMLJobObjectiveMetric = FinalAutoMLJobObjectiveMetric'
  { -- | The name of the standard metric. For a description of the standard
    -- metrics, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html#autopilot-metrics Autopilot candidate metrics>.
    standardMetricName :: Prelude.Maybe AutoMLMetricEnum,
    -- | The type of metric with the best result.
    type' :: Prelude.Maybe AutoMLJobObjectiveType,
    -- | The name of the metric with the best result. For a description of the
    -- possible objective metrics, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobObjective.html AutoMLJobObjective$MetricName>.
    metricName :: AutoMLMetricEnum,
    -- | The value of the metric with the best result.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FinalAutoMLJobObjectiveMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardMetricName', 'finalAutoMLJobObjectiveMetric_standardMetricName' - The name of the standard metric. For a description of the standard
-- metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html#autopilot-metrics Autopilot candidate metrics>.
--
-- 'type'', 'finalAutoMLJobObjectiveMetric_type' - The type of metric with the best result.
--
-- 'metricName', 'finalAutoMLJobObjectiveMetric_metricName' - The name of the metric with the best result. For a description of the
-- possible objective metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobObjective.html AutoMLJobObjective$MetricName>.
--
-- 'value', 'finalAutoMLJobObjectiveMetric_value' - The value of the metric with the best result.
newFinalAutoMLJobObjectiveMetric ::
  -- | 'metricName'
  AutoMLMetricEnum ->
  -- | 'value'
  Prelude.Double ->
  FinalAutoMLJobObjectiveMetric
newFinalAutoMLJobObjectiveMetric pMetricName_ pValue_ =
  FinalAutoMLJobObjectiveMetric'
    { standardMetricName =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      metricName = pMetricName_,
      value = pValue_
    }

-- | The name of the standard metric. For a description of the standard
-- metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html#autopilot-metrics Autopilot candidate metrics>.
finalAutoMLJobObjectiveMetric_standardMetricName :: Lens.Lens' FinalAutoMLJobObjectiveMetric (Prelude.Maybe AutoMLMetricEnum)
finalAutoMLJobObjectiveMetric_standardMetricName = Lens.lens (\FinalAutoMLJobObjectiveMetric' {standardMetricName} -> standardMetricName) (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {standardMetricName = a} :: FinalAutoMLJobObjectiveMetric)

-- | The type of metric with the best result.
finalAutoMLJobObjectiveMetric_type :: Lens.Lens' FinalAutoMLJobObjectiveMetric (Prelude.Maybe AutoMLJobObjectiveType)
finalAutoMLJobObjectiveMetric_type = Lens.lens (\FinalAutoMLJobObjectiveMetric' {type'} -> type') (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {type' = a} :: FinalAutoMLJobObjectiveMetric)

-- | The name of the metric with the best result. For a description of the
-- possible objective metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobObjective.html AutoMLJobObjective$MetricName>.
finalAutoMLJobObjectiveMetric_metricName :: Lens.Lens' FinalAutoMLJobObjectiveMetric AutoMLMetricEnum
finalAutoMLJobObjectiveMetric_metricName = Lens.lens (\FinalAutoMLJobObjectiveMetric' {metricName} -> metricName) (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {metricName = a} :: FinalAutoMLJobObjectiveMetric)

-- | The value of the metric with the best result.
finalAutoMLJobObjectiveMetric_value :: Lens.Lens' FinalAutoMLJobObjectiveMetric Prelude.Double
finalAutoMLJobObjectiveMetric_value = Lens.lens (\FinalAutoMLJobObjectiveMetric' {value} -> value) (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {value = a} :: FinalAutoMLJobObjectiveMetric)

instance Data.FromJSON FinalAutoMLJobObjectiveMetric where
  parseJSON =
    Data.withObject
      "FinalAutoMLJobObjectiveMetric"
      ( \x ->
          FinalAutoMLJobObjectiveMetric'
            Prelude.<$> (x Data..:? "StandardMetricName")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "MetricName")
            Prelude.<*> (x Data..: "Value")
      )

instance
  Prelude.Hashable
    FinalAutoMLJobObjectiveMetric
  where
  hashWithSalt _salt FinalAutoMLJobObjectiveMetric' {..} =
    _salt
      `Prelude.hashWithSalt` standardMetricName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` value

instance Prelude.NFData FinalAutoMLJobObjectiveMetric where
  rnf FinalAutoMLJobObjectiveMetric' {..} =
    Prelude.rnf standardMetricName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf value
