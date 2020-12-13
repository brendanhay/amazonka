{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
  ( FinalHyperParameterTuningJobObjectiveMetric (..),

    -- * Smart constructor
    mkFinalHyperParameterTuningJobObjectiveMetric,

    -- * Lenses
    fhptjomMetricName,
    fhptjomValue,
    fhptjomType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Shows the final value for the objective metric for a training job that was launched by a hyperparameter tuning job. You define the objective metric in the @HyperParameterTuningJobObjective@ parameter of 'HyperParameterTuningJobConfig' .
--
-- /See:/ 'mkFinalHyperParameterTuningJobObjectiveMetric' smart constructor.
data FinalHyperParameterTuningJobObjectiveMetric = FinalHyperParameterTuningJobObjectiveMetric'
  { -- | The name of the objective metric.
    metricName :: Lude.Text,
    -- | The value of the objective metric.
    value :: Lude.Double,
    -- | Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
    type' :: Lude.Maybe HyperParameterTuningJobObjectiveType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FinalHyperParameterTuningJobObjectiveMetric' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the objective metric.
-- * 'value' - The value of the objective metric.
-- * 'type'' - Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
mkFinalHyperParameterTuningJobObjectiveMetric ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'value'
  Lude.Double ->
  FinalHyperParameterTuningJobObjectiveMetric
mkFinalHyperParameterTuningJobObjectiveMetric pMetricName_ pValue_ =
  FinalHyperParameterTuningJobObjectiveMetric'
    { metricName =
        pMetricName_,
      value = pValue_,
      type' = Lude.Nothing
    }

-- | The name of the objective metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhptjomMetricName :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Lude.Text
fhptjomMetricName = Lens.lens (metricName :: FinalHyperParameterTuningJobObjectiveMetric -> Lude.Text) (\s a -> s {metricName = a} :: FinalHyperParameterTuningJobObjectiveMetric)
{-# DEPRECATED fhptjomMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The value of the objective metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhptjomValue :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric Lude.Double
fhptjomValue = Lens.lens (value :: FinalHyperParameterTuningJobObjectiveMetric -> Lude.Double) (\s a -> s {value = a} :: FinalHyperParameterTuningJobObjectiveMetric)
{-# DEPRECATED fhptjomValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhptjomType :: Lens.Lens' FinalHyperParameterTuningJobObjectiveMetric (Lude.Maybe HyperParameterTuningJobObjectiveType)
fhptjomType = Lens.lens (type' :: FinalHyperParameterTuningJobObjectiveMetric -> Lude.Maybe HyperParameterTuningJobObjectiveType) (\s a -> s {type' = a} :: FinalHyperParameterTuningJobObjectiveMetric)
{-# DEPRECATED fhptjomType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON FinalHyperParameterTuningJobObjectiveMetric where
  parseJSON =
    Lude.withObject
      "FinalHyperParameterTuningJobObjectiveMetric"
      ( \x ->
          FinalHyperParameterTuningJobObjectiveMetric'
            Lude.<$> (x Lude..: "MetricName")
            Lude.<*> (x Lude..: "Value")
            Lude.<*> (x Lude..:? "Type")
      )
