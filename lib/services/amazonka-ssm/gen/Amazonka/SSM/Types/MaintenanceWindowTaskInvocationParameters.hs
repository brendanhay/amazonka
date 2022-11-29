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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTaskInvocationParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTaskInvocationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.MaintenanceWindowAutomationParameters
import Amazonka.SSM.Types.MaintenanceWindowLambdaParameters
import Amazonka.SSM.Types.MaintenanceWindowRunCommandParameters
import Amazonka.SSM.Types.MaintenanceWindowStepFunctionsParameters

-- | The parameters for task execution.
--
-- /See:/ 'newMaintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { -- | The parameters for an @AUTOMATION@ task type.
    automation :: Prelude.Maybe MaintenanceWindowAutomationParameters,
    -- | The parameters for a @LAMBDA@ task type.
    lambda :: Prelude.Maybe MaintenanceWindowLambdaParameters,
    -- | The parameters for a @STEP_FUNCTIONS@ task type.
    stepFunctions :: Prelude.Maybe MaintenanceWindowStepFunctionsParameters,
    -- | The parameters for a @RUN_COMMAND@ task type.
    runCommand :: Prelude.Maybe MaintenanceWindowRunCommandParameters
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowTaskInvocationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automation', 'maintenanceWindowTaskInvocationParameters_automation' - The parameters for an @AUTOMATION@ task type.
--
-- 'lambda', 'maintenanceWindowTaskInvocationParameters_lambda' - The parameters for a @LAMBDA@ task type.
--
-- 'stepFunctions', 'maintenanceWindowTaskInvocationParameters_stepFunctions' - The parameters for a @STEP_FUNCTIONS@ task type.
--
-- 'runCommand', 'maintenanceWindowTaskInvocationParameters_runCommand' - The parameters for a @RUN_COMMAND@ task type.
newMaintenanceWindowTaskInvocationParameters ::
  MaintenanceWindowTaskInvocationParameters
newMaintenanceWindowTaskInvocationParameters =
  MaintenanceWindowTaskInvocationParameters'
    { automation =
        Prelude.Nothing,
      lambda = Prelude.Nothing,
      stepFunctions = Prelude.Nothing,
      runCommand = Prelude.Nothing
    }

-- | The parameters for an @AUTOMATION@ task type.
maintenanceWindowTaskInvocationParameters_automation :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowAutomationParameters)
maintenanceWindowTaskInvocationParameters_automation = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {automation} -> automation) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {automation = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a @LAMBDA@ task type.
maintenanceWindowTaskInvocationParameters_lambda :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowLambdaParameters)
maintenanceWindowTaskInvocationParameters_lambda = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {lambda} -> lambda) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {lambda = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a @STEP_FUNCTIONS@ task type.
maintenanceWindowTaskInvocationParameters_stepFunctions :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowStepFunctionsParameters)
maintenanceWindowTaskInvocationParameters_stepFunctions = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {stepFunctions} -> stepFunctions) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {stepFunctions = a} :: MaintenanceWindowTaskInvocationParameters)

-- | The parameters for a @RUN_COMMAND@ task type.
maintenanceWindowTaskInvocationParameters_runCommand :: Lens.Lens' MaintenanceWindowTaskInvocationParameters (Prelude.Maybe MaintenanceWindowRunCommandParameters)
maintenanceWindowTaskInvocationParameters_runCommand = Lens.lens (\MaintenanceWindowTaskInvocationParameters' {runCommand} -> runCommand) (\s@MaintenanceWindowTaskInvocationParameters' {} a -> s {runCommand = a} :: MaintenanceWindowTaskInvocationParameters)

instance
  Core.FromJSON
    MaintenanceWindowTaskInvocationParameters
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTaskInvocationParameters"
      ( \x ->
          MaintenanceWindowTaskInvocationParameters'
            Prelude.<$> (x Core..:? "Automation")
              Prelude.<*> (x Core..:? "Lambda")
              Prelude.<*> (x Core..:? "StepFunctions")
              Prelude.<*> (x Core..:? "RunCommand")
      )

instance
  Prelude.Hashable
    MaintenanceWindowTaskInvocationParameters
  where
  hashWithSalt
    _salt
    MaintenanceWindowTaskInvocationParameters' {..} =
      _salt `Prelude.hashWithSalt` automation
        `Prelude.hashWithSalt` lambda
        `Prelude.hashWithSalt` stepFunctions
        `Prelude.hashWithSalt` runCommand

instance
  Prelude.NFData
    MaintenanceWindowTaskInvocationParameters
  where
  rnf MaintenanceWindowTaskInvocationParameters' {..} =
    Prelude.rnf automation
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf stepFunctions
      `Prelude.seq` Prelude.rnf runCommand

instance
  Core.ToJSON
    MaintenanceWindowTaskInvocationParameters
  where
  toJSON MaintenanceWindowTaskInvocationParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Automation" Core..=) Prelude.<$> automation,
            ("Lambda" Core..=) Prelude.<$> lambda,
            ("StepFunctions" Core..=) Prelude.<$> stepFunctions,
            ("RunCommand" Core..=) Prelude.<$> runCommand
          ]
      )
