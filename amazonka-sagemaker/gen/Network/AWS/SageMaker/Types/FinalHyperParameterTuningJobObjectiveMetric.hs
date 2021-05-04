{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    type' :: Prelude.Maybe HyperParameterTuningJobObjectiveType,
    -- | The name of the objective metric.
    metricName :: Prelude.Text,
    -- | The value of the objective metric.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | Whether to minimize or maximize the objective metric. Valid values are
-- Minimize and Maximize.
finalHyperParameterTuningJobObjectiveMetric_type :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric (Prelude.Maybe HyperParameterTuningJobObjectiveType)
finalHyperParameterTuningJobObjectiveMetric_type = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {type'} -> type') (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {type' = a} :: FinalHyperParameterTuningJobObjectiveMetric)

-- | The name of the objective metric.
finalHyperParameterTuningJobObjectiveMetric_metricName :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Prelude.Text
finalHyperParameterTuningJobObjectiveMetric_metricName = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {metricName} -> metricName) (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {metricName = a} :: FinalHyperParameterTuningJobObjectiveMetric)

-- | The value of the objective metric.
finalHyperParameterTuningJobObjectiveMetric_value :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Prelude.Double
finalHyperParameterTuningJobObjectiveMetric_value = Lens.lens (\FinalHyperParameterTuningJobObjectiveMetric' {value} -> value) (\s@FinalHyperParameterTuningJobObjectiveMetric' {} a -> s {value = a} :: FinalHyperParameterTuningJobObjectiveMetric)

instance
  Prelude.FromJSON
    FinalHyperParameterTuningJobObjectiveMetric
  where
  parseJSON =
    Prelude.withObject
      "FinalHyperParameterTuningJobObjectiveMetric"
      ( \x ->
          FinalHyperParameterTuningJobObjectiveMetric'
            Prelude.<$> (x Prelude..:? "Type")
              Prelude.<*> (x Prelude..: "MetricName")
              Prelude.<*> (x Prelude..: "Value")
      )

instance
  Prelude.Hashable
    FinalHyperParameterTuningJobObjectiveMetric

instance
  Prelude.NFData
    FinalHyperParameterTuningJobObjectiveMetric
