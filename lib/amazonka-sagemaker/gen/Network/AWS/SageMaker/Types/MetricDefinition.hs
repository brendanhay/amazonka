{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MetricDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MetricDefinition
  ( MetricDefinition (..)
  -- * Smart constructor
  , mkMetricDefinition
  -- * Lenses
  , mdName
  , mdRegex
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MetricName as Types
import qualified Network.AWS.SageMaker.Types.MetricRegex as Types

-- | Specifies a metric that the training algorithm writes to @stderr@ or @stdout@ . Amazon SageMakerhyperparameter tuning captures all defined metrics. You specify one metric that a hyperparameter tuning job uses as its objective metric to choose the best training job.
--
-- /See:/ 'mkMetricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { name :: Types.MetricName
    -- ^ The name of the metric.
  , regex :: Types.MetricRegex
    -- ^ A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDefinition' value with any optional fields omitted.
mkMetricDefinition
    :: Types.MetricName -- ^ 'name'
    -> Types.MetricRegex -- ^ 'regex'
    -> MetricDefinition
mkMetricDefinition name regex = MetricDefinition'{name, regex}

-- | The name of the metric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdName :: Lens.Lens' MetricDefinition Types.MetricName
mdName = Lens.field @"name"
{-# INLINEABLE mdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
--
-- /Note:/ Consider using 'regex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdRegex :: Lens.Lens' MetricDefinition Types.MetricRegex
mdRegex = Lens.field @"regex"
{-# INLINEABLE mdRegex #-}
{-# DEPRECATED regex "Use generic-lens or generic-optics with 'regex' instead"  #-}

instance Core.FromJSON MetricDefinition where
        toJSON MetricDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Regex" Core..= regex)])

instance Core.FromJSON MetricDefinition where
        parseJSON
          = Core.withObject "MetricDefinition" Core.$
              \ x ->
                MetricDefinition' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Regex"
