{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.PerformanceMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.PerformanceMetrics
  ( PerformanceMetrics (..)
  -- * Smart constructor
  , mkPerformanceMetrics
  -- * Lenses
  , pmProperties
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.PerformanceMetricsPropertyKey as Types
import qualified Network.AWS.MachineLearning.Types.PerformanceMetricsPropertyValue as Types
import qualified Network.AWS.Prelude as Core

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
  { properties :: Core.Maybe (Core.HashMap Types.PerformanceMetricsPropertyKey Types.PerformanceMetricsPropertyValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PerformanceMetrics' value with any optional fields omitted.
mkPerformanceMetrics
    :: PerformanceMetrics
mkPerformanceMetrics
  = PerformanceMetrics'{properties = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmProperties :: Lens.Lens' PerformanceMetrics (Core.Maybe (Core.HashMap Types.PerformanceMetricsPropertyKey Types.PerformanceMetricsPropertyValue))
pmProperties = Lens.field @"properties"
{-# INLINEABLE pmProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

instance Core.FromJSON PerformanceMetrics where
        parseJSON
          = Core.withObject "PerformanceMetrics" Core.$
              \ x -> PerformanceMetrics' Core.<$> (x Core..:? "Properties")
