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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The parameters for a STEP_FUNCTIONS task.
--
-- For information about specifying and updating task parameters, see
-- RegisterTaskWithMaintenanceWindow and UpdateMaintenanceWindowTask.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- For Step Functions tasks, Systems Manager ignores any values specified
-- for @TaskParameters@ and @LoggingInfo@.
--
-- /See:/ 'newMaintenanceWindowStepFunctionsParameters' smart constructor.
data MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters'
  { -- | The inputs for the STEP_FUNCTIONS task.
    input :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The name of the STEP_FUNCTIONS task.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaintenanceWindowStepFunctionsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'maintenanceWindowStepFunctionsParameters_input' - The inputs for the STEP_FUNCTIONS task.
--
-- 'name', 'maintenanceWindowStepFunctionsParameters_name' - The name of the STEP_FUNCTIONS task.
newMaintenanceWindowStepFunctionsParameters ::
  MaintenanceWindowStepFunctionsParameters
newMaintenanceWindowStepFunctionsParameters =
  MaintenanceWindowStepFunctionsParameters'
    { input =
        Core.Nothing,
      name = Core.Nothing
    }

-- | The inputs for the STEP_FUNCTIONS task.
maintenanceWindowStepFunctionsParameters_input :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Core.Maybe Core.Text)
maintenanceWindowStepFunctionsParameters_input = Lens.lens (\MaintenanceWindowStepFunctionsParameters' {input} -> input) (\s@MaintenanceWindowStepFunctionsParameters' {} a -> s {input = a} :: MaintenanceWindowStepFunctionsParameters) Core.. Lens.mapping Core._Sensitive

-- | The name of the STEP_FUNCTIONS task.
maintenanceWindowStepFunctionsParameters_name :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Core.Maybe Core.Text)
maintenanceWindowStepFunctionsParameters_name = Lens.lens (\MaintenanceWindowStepFunctionsParameters' {name} -> name) (\s@MaintenanceWindowStepFunctionsParameters' {} a -> s {name = a} :: MaintenanceWindowStepFunctionsParameters)

instance
  Core.FromJSON
    MaintenanceWindowStepFunctionsParameters
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowStepFunctionsParameters"
      ( \x ->
          MaintenanceWindowStepFunctionsParameters'
            Core.<$> (x Core..:? "Input") Core.<*> (x Core..:? "Name")
      )

instance
  Core.Hashable
    MaintenanceWindowStepFunctionsParameters

instance
  Core.NFData
    MaintenanceWindowStepFunctionsParameters

instance
  Core.ToJSON
    MaintenanceWindowStepFunctionsParameters
  where
  toJSON MaintenanceWindowStepFunctionsParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Input" Core..=) Core.<$> input,
            ("Name" Core..=) Core.<$> name
          ]
      )
