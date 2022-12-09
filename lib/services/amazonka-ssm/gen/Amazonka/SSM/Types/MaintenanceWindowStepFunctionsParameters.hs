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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowStepFunctionsParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowStepFunctionsParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for a @STEP_FUNCTIONS@ task.
--
-- For information about specifying and updating task parameters, see
-- RegisterTaskWithMaintenanceWindow and UpdateMaintenanceWindowTask.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
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
  { -- | The inputs for the @STEP_FUNCTIONS@ task.
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the @STEP_FUNCTIONS@ task.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowStepFunctionsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'maintenanceWindowStepFunctionsParameters_input' - The inputs for the @STEP_FUNCTIONS@ task.
--
-- 'name', 'maintenanceWindowStepFunctionsParameters_name' - The name of the @STEP_FUNCTIONS@ task.
newMaintenanceWindowStepFunctionsParameters ::
  MaintenanceWindowStepFunctionsParameters
newMaintenanceWindowStepFunctionsParameters =
  MaintenanceWindowStepFunctionsParameters'
    { input =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The inputs for the @STEP_FUNCTIONS@ task.
maintenanceWindowStepFunctionsParameters_input :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowStepFunctionsParameters_input = Lens.lens (\MaintenanceWindowStepFunctionsParameters' {input} -> input) (\s@MaintenanceWindowStepFunctionsParameters' {} a -> s {input = a} :: MaintenanceWindowStepFunctionsParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the @STEP_FUNCTIONS@ task.
maintenanceWindowStepFunctionsParameters_name :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowStepFunctionsParameters_name = Lens.lens (\MaintenanceWindowStepFunctionsParameters' {name} -> name) (\s@MaintenanceWindowStepFunctionsParameters' {} a -> s {name = a} :: MaintenanceWindowStepFunctionsParameters)

instance
  Data.FromJSON
    MaintenanceWindowStepFunctionsParameters
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowStepFunctionsParameters"
      ( \x ->
          MaintenanceWindowStepFunctionsParameters'
            Prelude.<$> (x Data..:? "Input") Prelude.<*> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    MaintenanceWindowStepFunctionsParameters
  where
  hashWithSalt
    _salt
    MaintenanceWindowStepFunctionsParameters' {..} =
      _salt `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    MaintenanceWindowStepFunctionsParameters
  where
  rnf MaintenanceWindowStepFunctionsParameters' {..} =
    Prelude.rnf input `Prelude.seq` Prelude.rnf name

instance
  Data.ToJSON
    MaintenanceWindowStepFunctionsParameters
  where
  toJSON MaintenanceWindowStepFunctionsParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Input" Data..=) Prelude.<$> input,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
