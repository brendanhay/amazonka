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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.StepAutomationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.StepAutomationConfiguration where

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
-- /See:/ 'newStepAutomationConfiguration' smart constructor.
data StepAutomationConfiguration = StepAutomationConfiguration'
  { -- | The command to run the script.
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
-- Create a value of 'StepAutomationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'stepAutomationConfiguration_command' - The command to run the script.
--
-- 'runEnvironment', 'stepAutomationConfiguration_runEnvironment' - The source or target environment.
--
-- 'scriptLocationS3Bucket', 'stepAutomationConfiguration_scriptLocationS3Bucket' - The Amazon S3 bucket where the script is located.
--
-- 'scriptLocationS3Key', 'stepAutomationConfiguration_scriptLocationS3Key' - The Amazon S3 key for the script location.
--
-- 'targetType', 'stepAutomationConfiguration_targetType' - The servers on which to run the script.
newStepAutomationConfiguration ::
  StepAutomationConfiguration
newStepAutomationConfiguration =
  StepAutomationConfiguration'
    { command =
        Prelude.Nothing,
      runEnvironment = Prelude.Nothing,
      scriptLocationS3Bucket = Prelude.Nothing,
      scriptLocationS3Key = Prelude.Nothing,
      targetType = Prelude.Nothing
    }

-- | The command to run the script.
stepAutomationConfiguration_command :: Lens.Lens' StepAutomationConfiguration (Prelude.Maybe PlatformCommand)
stepAutomationConfiguration_command = Lens.lens (\StepAutomationConfiguration' {command} -> command) (\s@StepAutomationConfiguration' {} a -> s {command = a} :: StepAutomationConfiguration)

-- | The source or target environment.
stepAutomationConfiguration_runEnvironment :: Lens.Lens' StepAutomationConfiguration (Prelude.Maybe RunEnvironment)
stepAutomationConfiguration_runEnvironment = Lens.lens (\StepAutomationConfiguration' {runEnvironment} -> runEnvironment) (\s@StepAutomationConfiguration' {} a -> s {runEnvironment = a} :: StepAutomationConfiguration)

-- | The Amazon S3 bucket where the script is located.
stepAutomationConfiguration_scriptLocationS3Bucket :: Lens.Lens' StepAutomationConfiguration (Prelude.Maybe Prelude.Text)
stepAutomationConfiguration_scriptLocationS3Bucket = Lens.lens (\StepAutomationConfiguration' {scriptLocationS3Bucket} -> scriptLocationS3Bucket) (\s@StepAutomationConfiguration' {} a -> s {scriptLocationS3Bucket = a} :: StepAutomationConfiguration)

-- | The Amazon S3 key for the script location.
stepAutomationConfiguration_scriptLocationS3Key :: Lens.Lens' StepAutomationConfiguration (Prelude.Maybe PlatformScriptKey)
stepAutomationConfiguration_scriptLocationS3Key = Lens.lens (\StepAutomationConfiguration' {scriptLocationS3Key} -> scriptLocationS3Key) (\s@StepAutomationConfiguration' {} a -> s {scriptLocationS3Key = a} :: StepAutomationConfiguration)

-- | The servers on which to run the script.
stepAutomationConfiguration_targetType :: Lens.Lens' StepAutomationConfiguration (Prelude.Maybe TargetType)
stepAutomationConfiguration_targetType = Lens.lens (\StepAutomationConfiguration' {targetType} -> targetType) (\s@StepAutomationConfiguration' {} a -> s {targetType = a} :: StepAutomationConfiguration)

instance Data.FromJSON StepAutomationConfiguration where
  parseJSON =
    Data.withObject
      "StepAutomationConfiguration"
      ( \x ->
          StepAutomationConfiguration'
            Prelude.<$> (x Data..:? "command")
            Prelude.<*> (x Data..:? "runEnvironment")
            Prelude.<*> (x Data..:? "scriptLocationS3Bucket")
            Prelude.<*> (x Data..:? "scriptLocationS3Key")
            Prelude.<*> (x Data..:? "targetType")
      )

instance Prelude.Hashable StepAutomationConfiguration where
  hashWithSalt _salt StepAutomationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` runEnvironment
      `Prelude.hashWithSalt` scriptLocationS3Bucket
      `Prelude.hashWithSalt` scriptLocationS3Key
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData StepAutomationConfiguration where
  rnf StepAutomationConfiguration' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf runEnvironment
      `Prelude.seq` Prelude.rnf scriptLocationS3Bucket
      `Prelude.seq` Prelude.rnf scriptLocationS3Key
      `Prelude.seq` Prelude.rnf targetType
