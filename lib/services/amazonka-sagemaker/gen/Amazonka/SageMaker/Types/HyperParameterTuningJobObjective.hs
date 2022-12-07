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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobObjective where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Defines the objective metric for a hyperparameter tuning job.
-- Hyperparameter tuning uses the value of this metric to evaluate the
-- training jobs it launches, and returns the training job that results in
-- either the highest or lowest value for this metric, depending on the
-- value you specify for the @Type@ parameter.
--
-- /See:/ 'newHyperParameterTuningJobObjective' smart constructor.
data HyperParameterTuningJobObjective = HyperParameterTuningJobObjective'
  { -- | Whether to minimize or maximize the objective metric.
    type' :: HyperParameterTuningJobObjectiveType,
    -- | The name of the metric to use for the objective metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobObjective' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'hyperParameterTuningJobObjective_type' - Whether to minimize or maximize the objective metric.
--
-- 'metricName', 'hyperParameterTuningJobObjective_metricName' - The name of the metric to use for the objective metric.
newHyperParameterTuningJobObjective ::
  -- | 'type''
  HyperParameterTuningJobObjectiveType ->
  -- | 'metricName'
  Prelude.Text ->
  HyperParameterTuningJobObjective
newHyperParameterTuningJobObjective
  pType_
  pMetricName_ =
    HyperParameterTuningJobObjective'
      { type' = pType_,
        metricName = pMetricName_
      }

-- | Whether to minimize or maximize the objective metric.
hyperParameterTuningJobObjective_type :: Lens.Lens' HyperParameterTuningJobObjective HyperParameterTuningJobObjectiveType
hyperParameterTuningJobObjective_type = Lens.lens (\HyperParameterTuningJobObjective' {type'} -> type') (\s@HyperParameterTuningJobObjective' {} a -> s {type' = a} :: HyperParameterTuningJobObjective)

-- | The name of the metric to use for the objective metric.
hyperParameterTuningJobObjective_metricName :: Lens.Lens' HyperParameterTuningJobObjective Prelude.Text
hyperParameterTuningJobObjective_metricName = Lens.lens (\HyperParameterTuningJobObjective' {metricName} -> metricName) (\s@HyperParameterTuningJobObjective' {} a -> s {metricName = a} :: HyperParameterTuningJobObjective)

instance
  Data.FromJSON
    HyperParameterTuningJobObjective
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobObjective"
      ( \x ->
          HyperParameterTuningJobObjective'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "MetricName")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobObjective
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobObjective' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` metricName

instance
  Prelude.NFData
    HyperParameterTuningJobObjective
  where
  rnf HyperParameterTuningJobObjective' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToJSON HyperParameterTuningJobObjective where
  toJSON HyperParameterTuningJobObjective' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("MetricName" Data..= metricName)
          ]
      )
