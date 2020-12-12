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
    famljomType,
    famljomMetricName,
    famljomValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
import Network.AWS.SageMaker.Types.AutoMLMetricEnum

-- | The best candidate result from an AutoML training job.
--
-- /See:/ 'mkFinalAutoMLJobObjectiveMetric' smart constructor.
data FinalAutoMLJobObjectiveMetric = FinalAutoMLJobObjectiveMetric'
  { type' ::
      Lude.Maybe
        AutoMLJobObjectiveType,
    metricName :: AutoMLMetricEnum,
    value :: Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FinalAutoMLJobObjectiveMetric' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric with the best result. For a description of the possible objective metrics, see 'AutoMLJobObjective$MetricName' .
-- * 'type'' - The type of metric with the best result.
-- * 'value' - The value of the metric with the best result.
mkFinalAutoMLJobObjectiveMetric ::
  -- | 'metricName'
  AutoMLMetricEnum ->
  -- | 'value'
  Lude.Double ->
  FinalAutoMLJobObjectiveMetric
mkFinalAutoMLJobObjectiveMetric pMetricName_ pValue_ =
  FinalAutoMLJobObjectiveMetric'
    { type' = Lude.Nothing,
      metricName = pMetricName_,
      value = pValue_
    }

-- | The type of metric with the best result.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
famljomType :: Lens.Lens' FinalAutoMLJobObjectiveMetric (Lude.Maybe AutoMLJobObjectiveType)
famljomType = Lens.lens (type' :: FinalAutoMLJobObjectiveMetric -> Lude.Maybe AutoMLJobObjectiveType) (\s a -> s {type' = a} :: FinalAutoMLJobObjectiveMetric)
{-# DEPRECATED famljomType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the metric with the best result. For a description of the possible objective metrics, see 'AutoMLJobObjective$MetricName' .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
famljomMetricName :: Lens.Lens' FinalAutoMLJobObjectiveMetric AutoMLMetricEnum
famljomMetricName = Lens.lens (metricName :: FinalAutoMLJobObjectiveMetric -> AutoMLMetricEnum) (\s a -> s {metricName = a} :: FinalAutoMLJobObjectiveMetric)
{-# DEPRECATED famljomMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The value of the metric with the best result.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
famljomValue :: Lens.Lens' FinalAutoMLJobObjectiveMetric Lude.Double
famljomValue = Lens.lens (value :: FinalAutoMLJobObjectiveMetric -> Lude.Double) (\s a -> s {value = a} :: FinalAutoMLJobObjectiveMetric)
{-# DEPRECATED famljomValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON FinalAutoMLJobObjectiveMetric where
  parseJSON =
    Lude.withObject
      "FinalAutoMLJobObjectiveMetric"
      ( \x ->
          FinalAutoMLJobObjectiveMetric'
            Lude.<$> (x Lude..:? "Type")
            Lude.<*> (x Lude..: "MetricName")
            Lude.<*> (x Lude..: "Value")
      )
