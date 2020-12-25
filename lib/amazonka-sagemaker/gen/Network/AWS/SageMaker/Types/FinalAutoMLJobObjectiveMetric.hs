{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
  ( FinalAutoMLJobObjectiveMetric (..),

    -- * Smart constructor
    mkFinalAutoMLJobObjectiveMetric,

    -- * Lenses
    famljomMetricName,
    famljomValue,
    famljomType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLJobObjectiveType as Types
import qualified Network.AWS.SageMaker.Types.AutoMLMetricEnum as Types

-- | The best candidate result from an AutoML training job.
--
-- /See:/ 'mkFinalAutoMLJobObjectiveMetric' smart constructor.
data FinalAutoMLJobObjectiveMetric = FinalAutoMLJobObjectiveMetric'
  { -- | The name of the metric with the best result. For a description of the possible objective metrics, see 'AutoMLJobObjective$MetricName' .
    metricName :: Types.AutoMLMetricEnum,
    -- | The value of the metric with the best result.
    value :: Core.Double,
    -- | The type of metric with the best result.
    type' :: Core.Maybe Types.AutoMLJobObjectiveType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FinalAutoMLJobObjectiveMetric' value with any optional fields omitted.
mkFinalAutoMLJobObjectiveMetric ::
  -- | 'metricName'
  Types.AutoMLMetricEnum ->
  -- | 'value'
  Core.Double ->
  FinalAutoMLJobObjectiveMetric
mkFinalAutoMLJobObjectiveMetric metricName value =
  FinalAutoMLJobObjectiveMetric'
    { metricName,
      value,
      type' = Core.Nothing
    }

-- | The name of the metric with the best result. For a description of the possible objective metrics, see 'AutoMLJobObjective$MetricName' .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
famljomMetricName :: Lens.Lens' FinalAutoMLJobObjectiveMetric Types.AutoMLMetricEnum
famljomMetricName = Lens.field @"metricName"
{-# DEPRECATED famljomMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The value of the metric with the best result.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
famljomValue :: Lens.Lens' FinalAutoMLJobObjectiveMetric Core.Double
famljomValue = Lens.field @"value"
{-# DEPRECATED famljomValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of metric with the best result.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
famljomType :: Lens.Lens' FinalAutoMLJobObjectiveMetric (Core.Maybe Types.AutoMLJobObjectiveType)
famljomType = Lens.field @"type'"
{-# DEPRECATED famljomType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON FinalAutoMLJobObjectiveMetric where
  parseJSON =
    Core.withObject "FinalAutoMLJobObjectiveMetric" Core.$
      \x ->
        FinalAutoMLJobObjectiveMetric'
          Core.<$> (x Core..: "MetricName")
          Core.<*> (x Core..: "Value")
          Core.<*> (x Core..:? "Type")
