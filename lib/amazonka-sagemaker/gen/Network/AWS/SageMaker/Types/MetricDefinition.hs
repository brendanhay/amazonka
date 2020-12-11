-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MetricDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricDefinition
  ( MetricDefinition (..),

    -- * Smart constructor
    mkMetricDefinition,

    -- * Lenses
    mdName,
    mdRegex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a metric that the training algorithm writes to @stderr@ or @stdout@ . Amazon SageMakerhyperparameter tuning captures all defined metrics. You specify one metric that a hyperparameter tuning job uses as its objective metric to choose the best training job.
--
-- /See:/ 'mkMetricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { name :: Lude.Text,
    regex :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDefinition' with the minimum fields required to make a request.
--
-- * 'name' - The name of the metric.
-- * 'regex' - A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
mkMetricDefinition ::
  -- | 'name'
  Lude.Text ->
  -- | 'regex'
  Lude.Text ->
  MetricDefinition
mkMetricDefinition pName_ pRegex_ =
  MetricDefinition' {name = pName_, regex = pRegex_}

-- | The name of the metric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdName :: Lens.Lens' MetricDefinition Lude.Text
mdName = Lens.lens (name :: MetricDefinition -> Lude.Text) (\s a -> s {name = a} :: MetricDefinition)
{-# DEPRECATED mdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
--
-- /Note:/ Consider using 'regex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdRegex :: Lens.Lens' MetricDefinition Lude.Text
mdRegex = Lens.lens (regex :: MetricDefinition -> Lude.Text) (\s a -> s {regex = a} :: MetricDefinition)
{-# DEPRECATED mdRegex "Use generic-lens or generic-optics with 'regex' instead." #-}

instance Lude.FromJSON MetricDefinition where
  parseJSON =
    Lude.withObject
      "MetricDefinition"
      ( \x ->
          MetricDefinition'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "Regex")
      )

instance Lude.ToJSON MetricDefinition where
  toJSON MetricDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Regex" Lude..= regex)
          ]
      )
