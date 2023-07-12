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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.WorkflowStepAutomationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.WorkflowStepAutomationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.PlatformCommand
import Amazonka.MigrationHubOrchestrator.Types.PlatformScriptKey
import Amazonka.MigrationHubOrchestrator.Types.RunEnvironment
import Amazonka.MigrationHubOrchestrator.Types.TargetType
import qualified Amazonka.Prelude as Prelude

-- | The custom script to run tests on source or target environments.
--
-- /See:/ 'newWorkflowStepAutomationConfiguration' smart constructor.
data WorkflowStepAutomationConfiguration = WorkflowStepAutomationConfiguration'
  { -- | The command required to run the script.
    command :: Prelude.Maybe PlatformCommand,
    -- | The source or target environment.
    runEnvironment :: Prelude.Maybe RunEnvironment,
    -- | The Amazon S3 bucket where the script is located.
    scriptLocationS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key for the script location.
    scriptLocationS3Key :: Prelude.Maybe PlatformScriptKey,
    -- | The servers on which to run the script.
    targetType :: Prelude.Maybe TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStepAutomationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'workflowStepAutomationConfiguration_command' - The command required to run the script.
--
-- 'runEnvironment', 'workflowStepAutomationConfiguration_runEnvironment' - The source or target environment.
--
-- 'scriptLocationS3Bucket', 'workflowStepAutomationConfiguration_scriptLocationS3Bucket' - The Amazon S3 bucket where the script is located.
--
-- 'scriptLocationS3Key', 'workflowStepAutomationConfiguration_scriptLocationS3Key' - The Amazon S3 key for the script location.
--
-- 'targetType', 'workflowStepAutomationConfiguration_targetType' - The servers on which to run the script.
newWorkflowStepAutomationConfiguration ::
  WorkflowStepAutomationConfiguration
newWorkflowStepAutomationConfiguration =
  WorkflowStepAutomationConfiguration'
    { command =
        Prelude.Nothing,
      runEnvironment = Prelude.Nothing,
      scriptLocationS3Bucket =
        Prelude.Nothing,
      scriptLocationS3Key = Prelude.Nothing,
      targetType = Prelude.Nothing
    }

-- | The command required to run the script.
workflowStepAutomationConfiguration_command :: Lens.Lens' WorkflowStepAutomationConfiguration (Prelude.Maybe PlatformCommand)
workflowStepAutomationConfiguration_command = Lens.lens (\WorkflowStepAutomationConfiguration' {command} -> command) (\s@WorkflowStepAutomationConfiguration' {} a -> s {command = a} :: WorkflowStepAutomationConfiguration)

-- | The source or target environment.
workflowStepAutomationConfiguration_runEnvironment :: Lens.Lens' WorkflowStepAutomationConfiguration (Prelude.Maybe RunEnvironment)
workflowStepAutomationConfiguration_runEnvironment = Lens.lens (\WorkflowStepAutomationConfiguration' {runEnvironment} -> runEnvironment) (\s@WorkflowStepAutomationConfiguration' {} a -> s {runEnvironment = a} :: WorkflowStepAutomationConfiguration)

-- | The Amazon S3 bucket where the script is located.
workflowStepAutomationConfiguration_scriptLocationS3Bucket :: Lens.Lens' WorkflowStepAutomationConfiguration (Prelude.Maybe Prelude.Text)
workflowStepAutomationConfiguration_scriptLocationS3Bucket = Lens.lens (\WorkflowStepAutomationConfiguration' {scriptLocationS3Bucket} -> scriptLocationS3Bucket) (\s@WorkflowStepAutomationConfiguration' {} a -> s {scriptLocationS3Bucket = a} :: WorkflowStepAutomationConfiguration)

-- | The Amazon S3 key for the script location.
workflowStepAutomationConfiguration_scriptLocationS3Key :: Lens.Lens' WorkflowStepAutomationConfiguration (Prelude.Maybe PlatformScriptKey)
workflowStepAutomationConfiguration_scriptLocationS3Key = Lens.lens (\WorkflowStepAutomationConfiguration' {scriptLocationS3Key} -> scriptLocationS3Key) (\s@WorkflowStepAutomationConfiguration' {} a -> s {scriptLocationS3Key = a} :: WorkflowStepAutomationConfiguration)

-- | The servers on which to run the script.
workflowStepAutomationConfiguration_targetType :: Lens.Lens' WorkflowStepAutomationConfiguration (Prelude.Maybe TargetType)
workflowStepAutomationConfiguration_targetType = Lens.lens (\WorkflowStepAutomationConfiguration' {targetType} -> targetType) (\s@WorkflowStepAutomationConfiguration' {} a -> s {targetType = a} :: WorkflowStepAutomationConfiguration)

instance
  Data.FromJSON
    WorkflowStepAutomationConfiguration
  where
  parseJSON =
    Data.withObject
      "WorkflowStepAutomationConfiguration"
      ( \x ->
          WorkflowStepAutomationConfiguration'
            Prelude.<$> (x Data..:? "command")
            Prelude.<*> (x Data..:? "runEnvironment")
            Prelude.<*> (x Data..:? "scriptLocationS3Bucket")
            Prelude.<*> (x Data..:? "scriptLocationS3Key")
            Prelude.<*> (x Data..:? "targetType")
      )

instance
  Prelude.Hashable
    WorkflowStepAutomationConfiguration
  where
  hashWithSalt
    _salt
    WorkflowStepAutomationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` command
        `Prelude.hashWithSalt` runEnvironment
        `Prelude.hashWithSalt` scriptLocationS3Bucket
        `Prelude.hashWithSalt` scriptLocationS3Key
        `Prelude.hashWithSalt` targetType

instance
  Prelude.NFData
    WorkflowStepAutomationConfiguration
  where
  rnf WorkflowStepAutomationConfiguration' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf runEnvironment
      `Prelude.seq` Prelude.rnf scriptLocationS3Bucket
      `Prelude.seq` Prelude.rnf scriptLocationS3Key
      `Prelude.seq` Prelude.rnf targetType

instance
  Data.ToJSON
    WorkflowStepAutomationConfiguration
  where
  toJSON WorkflowStepAutomationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("command" Data..=) Prelude.<$> command,
            ("runEnvironment" Data..=)
              Prelude.<$> runEnvironment,
            ("scriptLocationS3Bucket" Data..=)
              Prelude.<$> scriptLocationS3Bucket,
            ("scriptLocationS3Key" Data..=)
              Prelude.<$> scriptLocationS3Key,
            ("targetType" Data..=) Prelude.<$> targetType
          ]
      )
