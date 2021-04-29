{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters

-- | The parameters for task execution.
--
-- /See:/ 'newMaintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { -- | The parameters for an AUTOMATION task type.
    automation :: Prelude.Maybe MaintenanceWindowAutomationParameters,
    -- | The parameters for a LAMBDA task type.
    lambda :: Prelude.Maybe MaintenanceWindowLambdaParameters,
    -- | The parameters for a RUN_COMMAND task type.
    runCommand :: Prelude.Maybe MaintenanceWindowRunCommandParameters,
    -- | The parameters for a STEP_FUNCTIONS task type.
    stepFunctions :: Prelude.Maybe MaintenanceWindowStepFunctionsParameters
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      lambda = Prelude.Nothing,
      runCommand = Prelude.Nothing,
      stepFunctions = Prelude.Nothing
    }

-- | The parameters for an AUTOMATION task type.
maintenanceWindowTaskInvocationParameters_automation :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowAutomationParameters)
maintenanceWindowTaskInvocationParameters_automation = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {automation} -> automation) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {automation = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a LAMBDA task type.
maintenanceWindowTaskInvocationParameters_lambda :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowLambdaParameters)
maintenanceWindowTaskInvocationParameters_lambda = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {lambda} -> lambda) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {lambda = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a RUN_COMMAND task type.
maintenanceWindowTaskInvocationParameters_runCommand :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowRunCommandParameters)
maintenanceWindowTaskInvocationParameters_runCommand = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {runCommand} -> runCommand) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {runCommand = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a STEP_FUNCTIONS task type.
maintenanceWindowTaskInvocationParameters_stepFunctions :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowStepFunctionsParameters)
maintenanceWindowTaskInvocationParameters_stepFunctions = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {stepFunctions} -> stepFunctions) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {stepFunctions = a} :: MaintenanceWindowTaskInvocationParameters)

instance
  Prelude.FromJSON
    MaintenanceWindowTaskInvocationParameters
  where
  parseJSON =
    Prelude.withObject
      "MaintenanceWindowTaskInvocationParameters"
      ( \x ->
          MaintenanceWindowTaskInvocationParameters'
            Prelude.<$> (x Prelude..:? "Automation")
              Prelude.<*> (x Prelude..:? "Lambda")
              Prelude.<*> (x Prelude..:? "RunCommand")
              Prelude.<*> (x Prelude..:? "StepFunctions")
      )

instance
  Prelude.Hashable
    MaintenanceWindowTaskInvocationParameters

instance
  Prelude.NFData
    MaintenanceWindowTaskInvocationParameters

instance
  Prelude.ToJSON
    MaintenanceWindowTaskInvocationParameters
  where
  toJSON MaintenanceWindowTaskInvocationParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Automation" Prelude..=) Prelude.<$> automation,
            ("Lambda" Prelude..=) Prelude.<$> lambda,
            ("RunCommand" Prelude..=) Prelude.<$> runCommand,
            ("StepFunctions" Prelude..=)
              Prelude.<$> stepFunctions
          ]
      )
