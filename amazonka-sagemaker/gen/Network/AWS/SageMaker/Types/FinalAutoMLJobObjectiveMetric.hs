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
-- Module      : Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
import Network.AWS.SageMaker.Types.AutoMLMetricEnum

-- | The best candidate result from an AutoML training job.
--
-- /See:/ 'newFinalAutoMLJobObjectiveMetric' smart constructor.
data FinalAutoMLJobObjectiveMetric = FinalAutoMLJobObjectiveMetric'
  { -- | The type of metric with the best result.
    type' :: Prelude.Maybe AutoMLJobObjectiveType,
    -- | The name of the metric with the best result. For a description of the
    -- possible objective metrics, see AutoMLJobObjective$MetricName.
    metricName :: AutoMLMetricEnum,
    -- | The value of the metric with the best result.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FinalAutoMLJobObjectiveMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'finalAutoMLJobObjectiveMetric_type' - The type of metric with the best result.
--
-- 'metricName', 'finalAutoMLJobObjectiveMetric_metricName' - The name of the metric with the best result. For a description of the
-- possible objective metrics, see AutoMLJobObjective$MetricName.
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
    { type' =
        Prelude.Nothing,
      metricName = pMetricName_,
      value = pValue_
    }

-- | The type of metric with the best result.
finalAutoMLJobObjectiveMetric_type :: Lens.Lens' FinalAutoMLJobObjectiveMetric (Prelude.Maybe AutoMLJobObjectiveType)
finalAutoMLJobObjectiveMetric_type = Lens.lens (\FinalAutoMLJobObjectiveMetric' {type'} -> type') (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {type' = a} :: FinalAutoMLJobObjectiveMetric)

-- | The name of the metric with the best result. For a description of the
-- possible objective metrics, see AutoMLJobObjective$MetricName.
finalAutoMLJobObjectiveMetric_metricName :: Lens.Lens' FinalAutoMLJobObjectiveMetric AutoMLMetricEnum
finalAutoMLJobObjectiveMetric_metricName = Lens.lens (\FinalAutoMLJobObjectiveMetric' {metricName} -> metricName) (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {metricName = a} :: FinalAutoMLJobObjectiveMetric)

-- | The value of the metric with the best result.
finalAutoMLJobObjectiveMetric_value :: Lens.Lens' FinalAutoMLJobObjectiveMetric Prelude.Double
finalAutoMLJobObjectiveMetric_value = Lens.lens (\FinalAutoMLJobObjectiveMetric' {value} -> value) (\s@FinalAutoMLJobObjectiveMetric' {} a -> s {value = a} :: FinalAutoMLJobObjectiveMetric)

instance
  Prelude.FromJSON
    FinalAutoMLJobObjectiveMetric
  where
  parseJSON =
    Prelude.withObject
      "FinalAutoMLJobObjectiveMetric"
      ( \x ->
          FinalAutoMLJobObjectiveMetric'
            Prelude.<$> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..: "MetricName")
            Prelude.<*> (x Prelude..: "Value")
      )

instance
  Prelude.Hashable
    FinalAutoMLJobObjectiveMetric

instance Prelude.NFData FinalAutoMLJobObjectiveMetric
