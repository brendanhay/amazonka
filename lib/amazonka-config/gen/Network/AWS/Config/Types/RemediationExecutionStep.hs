{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStep
  ( RemediationExecutionStep (..),

    -- * Smart constructor
    mkRemediationExecutionStep,

    -- * Lenses
    resErrorMessage,
    resName,
    resStartTime,
    resState,
    resStopTime,
  )
where

import qualified Network.AWS.Config.Types.ErrorMessage as Types
import qualified Network.AWS.Config.Types.Name as Types
import qualified Network.AWS.Config.Types.RemediationExecutionStepState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Name of the step from the SSM document.
--
-- /See:/ 'mkRemediationExecutionStep' smart constructor.
data RemediationExecutionStep = RemediationExecutionStep'
  { -- | An error message if the step was interrupted during execution.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The details of the step.
    name :: Core.Maybe Types.Name,
    -- | The time when the step started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The valid status of the step.
    state :: Core.Maybe Types.RemediationExecutionStepState,
    -- | The time when the step stopped.
    stopTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RemediationExecutionStep' value with any optional fields omitted.
mkRemediationExecutionStep ::
  RemediationExecutionStep
mkRemediationExecutionStep =
  RemediationExecutionStep'
    { errorMessage = Core.Nothing,
      name = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      stopTime = Core.Nothing
    }

-- | An error message if the step was interrupted during execution.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resErrorMessage :: Lens.Lens' RemediationExecutionStep (Core.Maybe Types.ErrorMessage)
resErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED resErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The details of the step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resName :: Lens.Lens' RemediationExecutionStep (Core.Maybe Types.Name)
resName = Lens.field @"name"
{-# DEPRECATED resName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time when the step started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resStartTime :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.NominalDiffTime)
resStartTime = Lens.field @"startTime"
{-# DEPRECATED resStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The valid status of the step.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resState :: Lens.Lens' RemediationExecutionStep (Core.Maybe Types.RemediationExecutionStepState)
resState = Lens.field @"state"
{-# DEPRECATED resState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time when the step stopped.
--
-- /Note:/ Consider using 'stopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resStopTime :: Lens.Lens' RemediationExecutionStep (Core.Maybe Core.NominalDiffTime)
resStopTime = Lens.field @"stopTime"
{-# DEPRECATED resStopTime "Use generic-lens or generic-optics with 'stopTime' instead." #-}

instance Core.FromJSON RemediationExecutionStep where
  parseJSON =
    Core.withObject "RemediationExecutionStep" Core.$
      \x ->
        RemediationExecutionStep'
          Core.<$> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "StopTime")
