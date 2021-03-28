{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
  ( HyperParameterTuningJobObjective (..)
  -- * Smart constructor
  , mkHyperParameterTuningJobObjective
  -- * Lenses
  , hptjoType
  , hptjoMetricName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType as Types
import qualified Network.AWS.SageMaker.Types.MetricName as Types

-- | Defines the objective metric for a hyperparameter tuning job. Hyperparameter tuning uses the value of this metric to evaluate the training jobs it launches, and returns the training job that results in either the highest or lowest value for this metric, depending on the value you specify for the @Type@ parameter.
--
-- /See:/ 'mkHyperParameterTuningJobObjective' smart constructor.
data HyperParameterTuningJobObjective = HyperParameterTuningJobObjective'
  { type' :: Types.HyperParameterTuningJobObjectiveType
    -- ^ Whether to minimize or maximize the objective metric.
  , metricName :: Types.MetricName
    -- ^ The name of the metric to use for the objective metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterTuningJobObjective' value with any optional fields omitted.
mkHyperParameterTuningJobObjective
    :: Types.HyperParameterTuningJobObjectiveType -- ^ 'type\''
    -> Types.MetricName -- ^ 'metricName'
    -> HyperParameterTuningJobObjective
mkHyperParameterTuningJobObjective type' metricName
  = HyperParameterTuningJobObjective'{type', metricName}

-- | Whether to minimize or maximize the objective metric.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjoType :: Lens.Lens' HyperParameterTuningJobObjective Types.HyperParameterTuningJobObjectiveType
hptjoType = Lens.field @"type'"
{-# INLINEABLE hptjoType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The name of the metric to use for the objective metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjoMetricName :: Lens.Lens' HyperParameterTuningJobObjective Types.MetricName
hptjoMetricName = Lens.field @"metricName"
{-# INLINEABLE hptjoMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

instance Core.FromJSON HyperParameterTuningJobObjective where
        toJSON HyperParameterTuningJobObjective{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  Core.Just ("MetricName" Core..= metricName)])

instance Core.FromJSON HyperParameterTuningJobObjective where
        parseJSON
          = Core.withObject "HyperParameterTuningJobObjective" Core.$
              \ x ->
                HyperParameterTuningJobObjective' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..: "MetricName"
