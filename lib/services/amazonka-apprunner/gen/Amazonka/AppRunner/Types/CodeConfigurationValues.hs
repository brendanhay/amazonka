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
-- Module      : Amazonka.AppRunner.Types.CodeConfigurationValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CodeConfigurationValues where

import Amazonka.AppRunner.Types.Runtime
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the basic configuration needed for building and running an App
-- Runner service. This type doesn\'t support the full set of possible
-- configuration options. Fur full configuration capabilities, use a
-- @apprunner.yaml@ file in the source code repository.
--
-- /See:/ 'newCodeConfigurationValues' smart constructor.
data CodeConfigurationValues = CodeConfigurationValues'
  { -- | The command App Runner runs to start your application.
    startCommand :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The environment variables that are available to your running App Runner
    -- service. An array of key-value pairs. Keys with a prefix of
    -- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
    runtimeEnvironmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)),
    -- | The command App Runner runs to build your application.
    buildCommand :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The port that your application listens to in the container.
    --
    -- Default: @8080@
    port :: Prelude.Maybe Prelude.Text,
    -- | A runtime environment type for building and running an App Runner
    -- service. It represents a programming language runtime.
    runtime :: Runtime
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeConfigurationValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startCommand', 'codeConfigurationValues_startCommand' - The command App Runner runs to start your application.
--
-- 'runtimeEnvironmentVariables', 'codeConfigurationValues_runtimeEnvironmentVariables' - The environment variables that are available to your running App Runner
-- service. An array of key-value pairs. Keys with a prefix of
-- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
--
-- 'buildCommand', 'codeConfigurationValues_buildCommand' - The command App Runner runs to build your application.
--
-- 'port', 'codeConfigurationValues_port' - The port that your application listens to in the container.
--
-- Default: @8080@
--
-- 'runtime', 'codeConfigurationValues_runtime' - A runtime environment type for building and running an App Runner
-- service. It represents a programming language runtime.
newCodeConfigurationValues ::
  -- | 'runtime'
  Runtime ->
  CodeConfigurationValues
newCodeConfigurationValues pRuntime_ =
  CodeConfigurationValues'
    { startCommand =
        Prelude.Nothing,
      runtimeEnvironmentVariables = Prelude.Nothing,
      buildCommand = Prelude.Nothing,
      port = Prelude.Nothing,
      runtime = pRuntime_
    }

-- | The command App Runner runs to start your application.
codeConfigurationValues_startCommand :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_startCommand = Lens.lens (\CodeConfigurationValues' {startCommand} -> startCommand) (\s@CodeConfigurationValues' {} a -> s {startCommand = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Core._Sensitive

-- | The environment variables that are available to your running App Runner
-- service. An array of key-value pairs. Keys with a prefix of
-- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
codeConfigurationValues_runtimeEnvironmentVariables :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
codeConfigurationValues_runtimeEnvironmentVariables = Lens.lens (\CodeConfigurationValues' {runtimeEnvironmentVariables} -> runtimeEnvironmentVariables) (\s@CodeConfigurationValues' {} a -> s {runtimeEnvironmentVariables = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Lens.coerced

-- | The command App Runner runs to build your application.
codeConfigurationValues_buildCommand :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_buildCommand = Lens.lens (\CodeConfigurationValues' {buildCommand} -> buildCommand) (\s@CodeConfigurationValues' {} a -> s {buildCommand = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Core._Sensitive

-- | The port that your application listens to in the container.
--
-- Default: @8080@
codeConfigurationValues_port :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_port = Lens.lens (\CodeConfigurationValues' {port} -> port) (\s@CodeConfigurationValues' {} a -> s {port = a} :: CodeConfigurationValues)

-- | A runtime environment type for building and running an App Runner
-- service. It represents a programming language runtime.
codeConfigurationValues_runtime :: Lens.Lens' CodeConfigurationValues Runtime
codeConfigurationValues_runtime = Lens.lens (\CodeConfigurationValues' {runtime} -> runtime) (\s@CodeConfigurationValues' {} a -> s {runtime = a} :: CodeConfigurationValues)

instance Core.FromJSON CodeConfigurationValues where
  parseJSON =
    Core.withObject
      "CodeConfigurationValues"
      ( \x ->
          CodeConfigurationValues'
            Prelude.<$> (x Core..:? "StartCommand")
            Prelude.<*> ( x Core..:? "RuntimeEnvironmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BuildCommand")
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> (x Core..: "Runtime")
      )

instance Prelude.Hashable CodeConfigurationValues where
  hashWithSalt salt' CodeConfigurationValues' {..} =
    salt' `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` buildCommand
      `Prelude.hashWithSalt` runtimeEnvironmentVariables
      `Prelude.hashWithSalt` startCommand

instance Prelude.NFData CodeConfigurationValues where
  rnf CodeConfigurationValues' {..} =
    Prelude.rnf startCommand
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf buildCommand
      `Prelude.seq` Prelude.rnf runtimeEnvironmentVariables

instance Core.ToJSON CodeConfigurationValues where
  toJSON CodeConfigurationValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartCommand" Core..=) Prelude.<$> startCommand,
            ("RuntimeEnvironmentVariables" Core..=)
              Prelude.<$> runtimeEnvironmentVariables,
            ("BuildCommand" Core..=) Prelude.<$> buildCommand,
            ("Port" Core..=) Prelude.<$> port,
            Prelude.Just ("Runtime" Core..= runtime)
          ]
      )
