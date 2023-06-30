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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowAutomationParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowAutomationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for an @AUTOMATION@ task type.
--
-- /See:/ 'newMaintenanceWindowAutomationParameters' smart constructor.
data MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters'
  { -- | The version of an Automation runbook to use during task execution.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the @AUTOMATION@ task.
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
    -- For @AUTOMATION@ task types, Amazon Web Services Systems Manager ignores
    -- any values specified for these parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowAutomationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersion', 'maintenanceWindowAutomationParameters_documentVersion' - The version of an Automation runbook to use during task execution.
--
-- 'parameters', 'maintenanceWindowAutomationParameters_parameters' - The parameters for the @AUTOMATION@ task.
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
-- For @AUTOMATION@ task types, Amazon Web Services Systems Manager ignores
-- any values specified for these parameters.
newMaintenanceWindowAutomationParameters ::
  MaintenanceWindowAutomationParameters
newMaintenanceWindowAutomationParameters =
  MaintenanceWindowAutomationParameters'
    { documentVersion =
        Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The version of an Automation runbook to use during task execution.
maintenanceWindowAutomationParameters_documentVersion :: Lens.Lens' MaintenanceWindowAutomationParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowAutomationParameters_documentVersion = Lens.lens (\MaintenanceWindowAutomationParameters' {documentVersion} -> documentVersion) (\s@MaintenanceWindowAutomationParameters' {} a -> s {documentVersion = a} :: MaintenanceWindowAutomationParameters)

-- | The parameters for the @AUTOMATION@ task.
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
-- For @AUTOMATION@ task types, Amazon Web Services Systems Manager ignores
-- any values specified for these parameters.
maintenanceWindowAutomationParameters_parameters :: Lens.Lens' MaintenanceWindowAutomationParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
maintenanceWindowAutomationParameters_parameters = Lens.lens (\MaintenanceWindowAutomationParameters' {parameters} -> parameters) (\s@MaintenanceWindowAutomationParameters' {} a -> s {parameters = a} :: MaintenanceWindowAutomationParameters) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    MaintenanceWindowAutomationParameters
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowAutomationParameters"
      ( \x ->
          MaintenanceWindowAutomationParameters'
            Prelude.<$> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    MaintenanceWindowAutomationParameters
  where
  hashWithSalt
    _salt
    MaintenanceWindowAutomationParameters' {..} =
      _salt
        `Prelude.hashWithSalt` documentVersion
        `Prelude.hashWithSalt` parameters

instance
  Prelude.NFData
    MaintenanceWindowAutomationParameters
  where
  rnf MaintenanceWindowAutomationParameters' {..} =
    Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf parameters

instance
  Data.ToJSON
    MaintenanceWindowAutomationParameters
  where
  toJSON MaintenanceWindowAutomationParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("Parameters" Data..=) Prelude.<$> parameters
          ]
      )
