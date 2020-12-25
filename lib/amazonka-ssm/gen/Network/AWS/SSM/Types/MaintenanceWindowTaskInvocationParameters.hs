{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
  ( MaintenanceWindowTaskInvocationParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowTaskInvocationParameters,

    -- * Lenses
    mwtipAutomation,
    mwtipLambda,
    mwtipRunCommand,
    mwtipStepFunctions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters as Types

-- | The parameters for task execution.
--
-- /See:/ 'mkMaintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { -- | The parameters for an AUTOMATION task type.
    automation :: Core.Maybe Types.MaintenanceWindowAutomationParameters,
    -- | The parameters for a LAMBDA task type.
    lambda :: Core.Maybe Types.MaintenanceWindowLambdaParameters,
    -- | The parameters for a RUN_COMMAND task type.
    runCommand :: Core.Maybe Types.MaintenanceWindowRunCommandParameters,
    -- | The parameters for a STEP_FUNCTIONS task type.
    stepFunctions :: Core.Maybe Types.MaintenanceWindowStepFunctionsParameters
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowTaskInvocationParameters' value with any optional fields omitted.
mkMaintenanceWindowTaskInvocationParameters ::
  MaintenanceWindowTaskInvocationParameters
mkMaintenanceWindowTaskInvocationParameters =
  MaintenanceWindowTaskInvocationParameters'
    { automation =
        Core.Nothing,
      lambda = Core.Nothing,
      runCommand = Core.Nothing,
      stepFunctions = Core.Nothing
    }

-- | The parameters for an AUTOMATION task type.
--
-- /Note:/ Consider using 'automation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipAutomation :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe Types.MaintenanceWindowAutomationParameters)
mwtipAutomation = Lens.field @"automation"
{-# DEPRECATED mwtipAutomation "Use generic-lens or generic-optics with 'automation' instead." #-}

-- | The parameters for a LAMBDA task type.
--
-- /Note:/ Consider using 'lambda' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipLambda :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe Types.MaintenanceWindowLambdaParameters)
mwtipLambda = Lens.field @"lambda"
{-# DEPRECATED mwtipLambda "Use generic-lens or generic-optics with 'lambda' instead." #-}

-- | The parameters for a RUN_COMMAND task type.
--
-- /Note:/ Consider using 'runCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipRunCommand :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe Types.MaintenanceWindowRunCommandParameters)
mwtipRunCommand = Lens.field @"runCommand"
{-# DEPRECATED mwtipRunCommand "Use generic-lens or generic-optics with 'runCommand' instead." #-}

-- | The parameters for a STEP_FUNCTIONS task type.
--
-- /Note:/ Consider using 'stepFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipStepFunctions :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe Types.MaintenanceWindowStepFunctionsParameters)
mwtipStepFunctions = Lens.field @"stepFunctions"
{-# DEPRECATED mwtipStepFunctions "Use generic-lens or generic-optics with 'stepFunctions' instead." #-}

instance Core.FromJSON MaintenanceWindowTaskInvocationParameters where
  toJSON MaintenanceWindowTaskInvocationParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("Automation" Core..=) Core.<$> automation,
            ("Lambda" Core..=) Core.<$> lambda,
            ("RunCommand" Core..=) Core.<$> runCommand,
            ("StepFunctions" Core..=) Core.<$> stepFunctions
          ]
      )

instance Core.FromJSON MaintenanceWindowTaskInvocationParameters where
  parseJSON =
    Core.withObject "MaintenanceWindowTaskInvocationParameters" Core.$
      \x ->
        MaintenanceWindowTaskInvocationParameters'
          Core.<$> (x Core..:? "Automation")
          Core.<*> (x Core..:? "Lambda")
          Core.<*> (x Core..:? "RunCommand")
          Core.<*> (x Core..:? "StepFunctions")
