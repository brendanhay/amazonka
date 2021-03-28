{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepStatus
  ( StepStatus (..)
  -- * Smart constructor
  , mkStepStatus
  -- * Lenses
  , ssFailureDetails
  , ssState
  , ssStateChangeReason
  , ssTimeline
  ) where

import qualified Network.AWS.EMR.Types.FailureDetails as Types
import qualified Network.AWS.EMR.Types.StepState as Types
import qualified Network.AWS.EMR.Types.StepStateChangeReason as Types
import qualified Network.AWS.EMR.Types.StepTimeline as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The execution status details of the cluster step.
--
-- /See:/ 'mkStepStatus' smart constructor.
data StepStatus = StepStatus'
  { failureDetails :: Core.Maybe Types.FailureDetails
    -- ^ The details for the step failure including reason, message, and log file path where the root cause was identified.
  , state :: Core.Maybe Types.StepState
    -- ^ The execution state of the cluster step.
  , stateChangeReason :: Core.Maybe Types.StepStateChangeReason
    -- ^ The reason for the step execution status change.
  , timeline :: Core.Maybe Types.StepTimeline
    -- ^ The timeline of the cluster step status over time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StepStatus' value with any optional fields omitted.
mkStepStatus
    :: StepStatus
mkStepStatus
  = StepStatus'{failureDetails = Core.Nothing, state = Core.Nothing,
                stateChangeReason = Core.Nothing, timeline = Core.Nothing}

-- | The details for the step failure including reason, message, and log file path where the root cause was identified.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssFailureDetails :: Lens.Lens' StepStatus (Core.Maybe Types.FailureDetails)
ssFailureDetails = Lens.field @"failureDetails"
{-# INLINEABLE ssFailureDetails #-}
{-# DEPRECATED failureDetails "Use generic-lens or generic-optics with 'failureDetails' instead"  #-}

-- | The execution state of the cluster step.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssState :: Lens.Lens' StepStatus (Core.Maybe Types.StepState)
ssState = Lens.field @"state"
{-# INLINEABLE ssState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason for the step execution status change.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStateChangeReason :: Lens.Lens' StepStatus (Core.Maybe Types.StepStateChangeReason)
ssStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE ssStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | The timeline of the cluster step status over time.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimeline :: Lens.Lens' StepStatus (Core.Maybe Types.StepTimeline)
ssTimeline = Lens.field @"timeline"
{-# INLINEABLE ssTimeline #-}
{-# DEPRECATED timeline "Use generic-lens or generic-optics with 'timeline' instead"  #-}

instance Core.FromJSON StepStatus where
        parseJSON
          = Core.withObject "StepStatus" Core.$
              \ x ->
                StepStatus' Core.<$>
                  (x Core..:? "FailureDetails") Core.<*> x Core..:? "State" Core.<*>
                    x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "Timeline"
