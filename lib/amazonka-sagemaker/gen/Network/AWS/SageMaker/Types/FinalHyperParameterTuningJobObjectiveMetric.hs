{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
  ( FinalHyperParameterTuningJobObjectiveMetric (..)
  -- * Smart constructor
  , mkFinalHyperParameterTuningJobObjectiveMetric
  -- * Lenses
  , fhptjomMetricName
  , fhptjomValue
  , fhptjomType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType as Types
import qualified Network.AWS.SageMaker.Types.MetricName as Types

-- | Shows the final value for the objective metric for a training job that was launched by a hyperparameter tuning job. You define the objective metric in the @HyperParameterTuningJobObjective@ parameter of 'HyperParameterTuningJobConfig' .
--
-- /See:/ 'mkFinalHyperParameterTuningJobObjectiveMetric' smart constructor.
data FinalHyperParameterTuningJobObjectiveMetric = FinalHyperParameterTuningJobObjectiveMetric'
  { metricName :: Types.MetricName
    -- ^ The name of the objective metric.
  , value :: Core.Double
    -- ^ The value of the objective metric.
  , type' :: Core.Maybe Types.HyperParameterTuningJobObjectiveType
    -- ^ Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FinalHyperParameterTuningJobObjectiveMetric' value with any optional fields omitted.
mkFinalHyperParameterTuningJobObjectiveMetric
    :: Types.MetricName -- ^ 'metricName'
    -> Core.Double -- ^ 'value'
    -> FinalHyperParameterTuningJobObjectiveMetric
mkFinalHyperParameterTuningJobObjectiveMetric metricName value
  = FinalHyperParameterTuningJobObjectiveMetric'{metricName, value,
                                                 type' = Core.Nothing}

-- | The name of the objective metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhptjomMetricName :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Types.MetricName
fhptjomMetricName = Lens.field @"metricName"
{-# INLINEABLE fhptjomMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The value of the objective metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhptjomValue :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Core.Double
fhptjomValue = Lens.field @"value"
{-# INLINEABLE fhptjomValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhptjomType :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric (Core.Maybe Types.HyperParameterTuningJobObjectiveType)
fhptjomType = Lens.field @"type'"
{-# INLINEABLE fhptjomType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON FinalHyperParameterTuningJobObjectiveMetric
         where
        parseJSON
          = Core.withObject "FinalHyperParameterTuningJobObjectiveMetric"
              Core.$
              \ x ->
                FinalHyperParameterTuningJobObjectiveMetric' Core.<$>
                  (x Core..: "MetricName") Core.<*> x Core..: "Value" Core.<*>
                    x Core..:? "Type"
