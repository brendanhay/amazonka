{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
  ( HyperParameterTuningJobObjective (..),

    -- * Smart constructor
    mkHyperParameterTuningJobObjective,

    -- * Lenses
    hptjoType,
    hptjoMetricName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Defines the objective metric for a hyperparameter tuning job. Hyperparameter tuning uses the value of this metric to evaluate the training jobs it launches, and returns the training job that results in either the highest or lowest value for this metric, depending on the value you specify for the @Type@ parameter.
--
-- /See:/ 'mkHyperParameterTuningJobObjective' smart constructor.
data HyperParameterTuningJobObjective = HyperParameterTuningJobObjective'
  { type' ::
      HyperParameterTuningJobObjectiveType,
    metricName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterTuningJobObjective' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric to use for the objective metric.
-- * 'type'' - Whether to minimize or maximize the objective metric.
mkHyperParameterTuningJobObjective ::
  -- | 'type''
  HyperParameterTuningJobObjectiveType ->
  -- | 'metricName'
  Lude.Text ->
  HyperParameterTuningJobObjective
mkHyperParameterTuningJobObjective pType_ pMetricName_ =
  HyperParameterTuningJobObjective'
    { type' = pType_,
      metricName = pMetricName_
    }

-- | Whether to minimize or maximize the objective metric.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjoType :: Lens.Lens' HyperParameterTuningJobObjective HyperParameterTuningJobObjectiveType
hptjoType = Lens.lens (type' :: HyperParameterTuningJobObjective -> HyperParameterTuningJobObjectiveType) (\s a -> s {type' = a} :: HyperParameterTuningJobObjective)
{-# DEPRECATED hptjoType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the metric to use for the objective metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjoMetricName :: Lens.Lens' HyperParameterTuningJobObjective Lude.Text
hptjoMetricName = Lens.lens (metricName :: HyperParameterTuningJobObjective -> Lude.Text) (\s a -> s {metricName = a} :: HyperParameterTuningJobObjective)
{-# DEPRECATED hptjoMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

instance Lude.FromJSON HyperParameterTuningJobObjective where
  parseJSON =
    Lude.withObject
      "HyperParameterTuningJobObjective"
      ( \x ->
          HyperParameterTuningJobObjective'
            Lude.<$> (x Lude..: "Type") Lude.<*> (x Lude..: "MetricName")
      )

instance Lude.ToJSON HyperParameterTuningJobObjective where
  toJSON HyperParameterTuningJobObjective' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Type" Lude..= type'),
            Lude.Just ("MetricName" Lude..= metricName)
          ]
      )
