{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
  ( TuningJobCompletionCriteria (..)
  -- * Smart constructor
  , mkTuningJobCompletionCriteria
  -- * Lenses
  , tjccTargetObjectiveMetricValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The job completion criteria.
--
-- /See:/ 'mkTuningJobCompletionCriteria' smart constructor.
newtype TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { targetObjectiveMetricValue :: Core.Double
    -- ^ The value of the objective metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TuningJobCompletionCriteria' value with any optional fields omitted.
mkTuningJobCompletionCriteria
    :: Core.Double -- ^ 'targetObjectiveMetricValue'
    -> TuningJobCompletionCriteria
mkTuningJobCompletionCriteria targetObjectiveMetricValue
  = TuningJobCompletionCriteria'{targetObjectiveMetricValue}

-- | The value of the objective metric.
--
-- /Note:/ Consider using 'targetObjectiveMetricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjccTargetObjectiveMetricValue :: Lens.Lens' TuningJobCompletionCriteria Core.Double
tjccTargetObjectiveMetricValue = Lens.field @"targetObjectiveMetricValue"
{-# INLINEABLE tjccTargetObjectiveMetricValue #-}
{-# DEPRECATED targetObjectiveMetricValue "Use generic-lens or generic-optics with 'targetObjectiveMetricValue' instead"  #-}

instance Core.FromJSON TuningJobCompletionCriteria where
        toJSON TuningJobCompletionCriteria{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("TargetObjectiveMetricValue" Core..= targetObjectiveMetricValue)])

instance Core.FromJSON TuningJobCompletionCriteria where
        parseJSON
          = Core.withObject "TuningJobCompletionCriteria" Core.$
              \ x ->
                TuningJobCompletionCriteria' Core.<$>
                  (x Core..: "TargetObjectiveMetricValue")
