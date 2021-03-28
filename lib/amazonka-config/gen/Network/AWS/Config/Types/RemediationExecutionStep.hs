{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RemediationExecutionStep
  ( RemediationExecutionStep (..)
  -- * Smart constructor
  , mkRemediationExecutionStep
  -- * Lenses
  , resErrorMessage
  , resName
  , resStartTime
  , resState
  , resStopTime
  ) where

import qualified Network.AWS.Config.Types.RemediationExecutionStepState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Name of the step from the SSM document.
--
-- /See:/ 'mkRemediationExecutionStep' smart constructor.
data RemediationExecutionStep = RemediationExecutionStep'
  { errorMessage :: Core.Maybe Core.Text
    -- ^ An error message if the step was interrupted during execution.
  , name :: Core.Maybe Core.Text
    -- ^ The details of the step.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the step started.
  , state :: Core.Maybe Types.RemediationExecutionStepState
    -- ^ The valid status of the step.
  , stopTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the step stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RemediationExecutionStep' value with any optional fields omitted.
mkRemediationExecutionStep
    :: RemediationExecutionStep
mkRemediationExecutionStep
  = RemediationExecutionStep'{errorMessage = Core.Nothing,
                              name = Core.Nothing, startTime = Core.Nothing,
                              state = Core.Nothing, stopTime = Core.Nothing}

-- | An error message if the step was interrupted during execution.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resErrorMessage :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.Text)
resErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE resErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The details of the step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resName :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.Text)
resName = Lens.field @"name"
{-# INLINEABLE resName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The time when the step started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resStartTime :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.NominalDiffTime)
resStartTime = Lens.field @"startTime"
{-# INLINEABLE resStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The valid status of the step.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resState :: Lens.Lens' RemediationExecutionStep (Core.Maybe Types.RemediationExecutionStepState)
resState = Lens.field @"state"
{-# INLINEABLE resState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The time when the step stopped.
--
-- /Note:/ Consider using 'stopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resStopTime :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.NominalDiffTime)
resStopTime = Lens.field @"stopTime"
{-# INLINEABLE resStopTime #-}
{-# DEPRECATED stopTime "Use generic-lens or generic-optics with 'stopTime' instead"  #-}

instance Core.FromJSON RemediationExecutionStep where
        parseJSON
          = Core.withObject "RemediationExecutionStep" Core.$
              \ x ->
                RemediationExecutionStep' Core.<$>
                  (x Core..:? "ErrorMessage") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "StartTime"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StopTime"
