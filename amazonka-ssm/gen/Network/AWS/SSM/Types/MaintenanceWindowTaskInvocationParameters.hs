{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters

-- | The parameters for task execution.
--
-- /See:/ 'newMaintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { -- | The parameters for an AUTOMATION task type.
    automation :: Core.Maybe MaintenanceWindowAutomationParameters,
    -- | The parameters for a LAMBDA task type.
    lambda :: Core.Maybe MaintenanceWindowLambdaParameters,
    -- | The parameters for a RUN_COMMAND task type.
    runCommand :: Core.Maybe MaintenanceWindowRunCommandParameters,
    -- | The parameters for a STEP_FUNCTIONS task type.
    stepFunctions :: Core.Maybe MaintenanceWindowStepFunctionsParameters
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaintenanceWindowTaskInvocationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automation', 'maintenanceWindowTaskInvocationParameters_automation' - The parameters for an AUTOMATION task type.
--
-- 'lambda', 'maintenanceWindowTaskInvocationParameters_lambda' - The parameters for a LAMBDA task type.
--
-- 'runCommand', 'maintenanceWindowTaskInvocationParameters_runCommand' - The parameters for a RUN_COMMAND task type.
--
-- 'stepFunctions', 'maintenanceWindowTaskInvocationParameters_stepFunctions' - The parameters for a STEP_FUNCTIONS task type.
newMaintenanceWindowTaskInvocationParameters ::
  MaintenanceWindowTaskInvocationParameters
newMaintenanceWindowTaskInvocationParameters =
  MaintenanceWindowTaskInvocationParameters'
    { automation =
        Core.Nothing,
      lambda = Core.Nothing,
      runCommand = Core.Nothing,
      stepFunctions = Core.Nothing
    }

-- | The parameters for an AUTOMATION task type.
maintenanceWindowTaskInvocationParameters_automation :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe MaintenanceWindowAutomationParameters)
maintenanceWindowTaskInvocationParameters_automation = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {automation} -> automation) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {automation = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a LAMBDA task type.
maintenanceWindowTaskInvocationParameters_lambda :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe MaintenanceWindowLambdaParameters)
maintenanceWindowTaskInvocationParameters_lambda = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {lambda} -> lambda) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {lambda = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a RUN_COMMAND task type.
maintenanceWindowTaskInvocationParameters_runCommand :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe MaintenanceWindowRunCommandParameters)
maintenanceWindowTaskInvocationParameters_runCommand = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {runCommand} -> runCommand) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {runCommand = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a STEP_FUNCTIONS task type.
maintenanceWindowTaskInvocationParameters_stepFunctions :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Core.Maybe MaintenanceWindowStepFunctionsParameters)
maintenanceWindowTaskInvocationParameters_stepFunctions = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {stepFunctions} -> stepFunctions) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {stepFunctions = a} :: MaintenanceWindowTaskInvocationParameters)

instance
  Core.FromJSON
    MaintenanceWindowTaskInvocationParameters
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTaskInvocationParameters"
      ( \x ->
          MaintenanceWindowTaskInvocationParameters'
            Core.<$> (x Core..:? "Automation")
            Core.<*> (x Core..:? "Lambda")
            Core.<*> (x Core..:? "RunCommand")
            Core.<*> (x Core..:? "StepFunctions")
      )

instance
  Core.Hashable
    MaintenanceWindowTaskInvocationParameters

instance
  Core.NFData
    MaintenanceWindowTaskInvocationParameters

instance
  Core.ToJSON
    MaintenanceWindowTaskInvocationParameters
  where
  toJSON MaintenanceWindowTaskInvocationParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Automation" Core..=) Core.<$> automation,
            ("Lambda" Core..=) Core.<$> lambda,
            ("RunCommand" Core..=) Core.<$> runCommand,
            ("StepFunctions" Core..=) Core.<$> stepFunctions
          ]
      )
