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
    mwtipStepFunctions,
    mwtipRunCommand,
    mwtipLambda,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters

-- | The parameters for task execution.
--
-- /See:/ 'mkMaintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { -- | The parameters for an AUTOMATION task type.
    automation :: Lude.Maybe MaintenanceWindowAutomationParameters,
    -- | The parameters for a STEP_FUNCTIONS task type.
    stepFunctions :: Lude.Maybe MaintenanceWindowStepFunctionsParameters,
    -- | The parameters for a RUN_COMMAND task type.
    runCommand :: Lude.Maybe MaintenanceWindowRunCommandParameters,
    -- | The parameters for a LAMBDA task type.
    lambda :: Lude.Maybe MaintenanceWindowLambdaParameters
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowTaskInvocationParameters' with the minimum fields required to make a request.
--
-- * 'automation' - The parameters for an AUTOMATION task type.
-- * 'stepFunctions' - The parameters for a STEP_FUNCTIONS task type.
-- * 'runCommand' - The parameters for a RUN_COMMAND task type.
-- * 'lambda' - The parameters for a LAMBDA task type.
mkMaintenanceWindowTaskInvocationParameters ::
  MaintenanceWindowTaskInvocationParameters
mkMaintenanceWindowTaskInvocationParameters =
  MaintenanceWindowTaskInvocationParameters'
    { automation =
        Lude.Nothing,
      stepFunctions = Lude.Nothing,
      runCommand = Lude.Nothing,
      lambda = Lude.Nothing
    }

-- | The parameters for an AUTOMATION task type.
--
-- /Note:/ Consider using 'automation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipAutomation :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Lude.Maybe MaintenanceWindowAutomationParameters)
mwtipAutomation = Lens.lens (automation :: MaintenanceWindowTaskInvocationParameters -> Lude.Maybe MaintenanceWindowAutomationParameters) (\s a -> s {automation = a} :: MaintenanceWindowTaskInvocationParameters)
{-# DEPRECATED mwtipAutomation "Use generic-lens or generic-optics with 'automation' instead." #-}

-- | The parameters for a STEP_FUNCTIONS task type.
--
-- /Note:/ Consider using 'stepFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipStepFunctions :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Lude.Maybe MaintenanceWindowStepFunctionsParameters)
mwtipStepFunctions = Lens.lens (stepFunctions :: MaintenanceWindowTaskInvocationParameters -> Lude.Maybe MaintenanceWindowStepFunctionsParameters) (\s a -> s {stepFunctions = a} :: MaintenanceWindowTaskInvocationParameters)
{-# DEPRECATED mwtipStepFunctions "Use generic-lens or generic-optics with 'stepFunctions' instead." #-}

-- | The parameters for a RUN_COMMAND task type.
--
-- /Note:/ Consider using 'runCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipRunCommand :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Lude.Maybe MaintenanceWindowRunCommandParameters)
mwtipRunCommand = Lens.lens (runCommand :: MaintenanceWindowTaskInvocationParameters -> Lude.Maybe MaintenanceWindowRunCommandParameters) (\s a -> s {runCommand = a} :: MaintenanceWindowTaskInvocationParameters)
{-# DEPRECATED mwtipRunCommand "Use generic-lens or generic-optics with 'runCommand' instead." #-}

-- | The parameters for a LAMBDA task type.
--
-- /Note:/ Consider using 'lambda' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtipLambda :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Lude.Maybe MaintenanceWindowLambdaParameters)
mwtipLambda = Lens.lens (lambda :: MaintenanceWindowTaskInvocationParameters -> Lude.Maybe MaintenanceWindowLambdaParameters) (\s a -> s {lambda = a} :: MaintenanceWindowTaskInvocationParameters)
{-# DEPRECATED mwtipLambda "Use generic-lens or generic-optics with 'lambda' instead." #-}

instance Lude.FromJSON MaintenanceWindowTaskInvocationParameters where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowTaskInvocationParameters"
      ( \x ->
          MaintenanceWindowTaskInvocationParameters'
            Lude.<$> (x Lude..:? "Automation")
            Lude.<*> (x Lude..:? "StepFunctions")
            Lude.<*> (x Lude..:? "RunCommand")
            Lude.<*> (x Lude..:? "Lambda")
      )

instance Lude.ToJSON MaintenanceWindowTaskInvocationParameters where
  toJSON MaintenanceWindowTaskInvocationParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Automation" Lude..=) Lude.<$> automation,
            ("StepFunctions" Lude..=) Lude.<$> stepFunctions,
            ("RunCommand" Lude..=) Lude.<$> runCommand,
            ("Lambda" Lude..=) Lude.<$> lambda
          ]
      )
