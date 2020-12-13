{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.PerformanceMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.PerformanceMetrics
  ( PerformanceMetrics (..),

    -- * Smart constructor
    mkPerformanceMetrics,

    -- * Lenses
    pmProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Measurements of how well the @MLModel@ performed on known observations. One of the following metrics is returned, based on the type of the @MLModel@ :
--
--
--     * BinaryAUC: The binary @MLModel@ uses the Area Under the Curve (AUC) technique to measure performance.
--
--
--     * RegressionRMSE: The regression @MLModel@ uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.
--
--
--     * MulticlassAvgFScore: The multiclass @MLModel@ uses the F1 score technique to measure performance.
--
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
--
-- /See:/ 'mkPerformanceMetrics' smart constructor.
newtype PerformanceMetrics = PerformanceMetrics'
  { properties :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PerformanceMetrics' with the minimum fields required to make a request.
--
-- * 'properties' -
mkPerformanceMetrics ::
  PerformanceMetrics
mkPerformanceMetrics =
  PerformanceMetrics' {properties = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmProperties :: Lens.Lens' PerformanceMetrics (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pmProperties = Lens.lens (properties :: PerformanceMetrics -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {properties = a} :: PerformanceMetrics)
{-# DEPRECATED pmProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.FromJSON PerformanceMetrics where
  parseJSON =
    Lude.withObject
      "PerformanceMetrics"
      ( \x ->
          PerformanceMetrics'
            Lude.<$> (x Lude..:? "Properties" Lude..!= Lude.mempty)
      )
