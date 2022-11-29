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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CodeConfigurationValues where

import Amazonka.AppRunner.Types.Runtime
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the basic configuration needed for building and running an App
-- Runner service. This type doesn\'t support the full set of possible
-- configuration options. Fur full configuration capabilities, use a
-- @apprunner.yaml@ file in the source code repository.
--
-- /See:/ 'newCodeConfigurationValues' smart constructor.
data CodeConfigurationValues = CodeConfigurationValues'
  { -- | The port that your application listens to in the container.
    --
    -- Default: @8080@
    port :: Prelude.Maybe Prelude.Text,
    -- | The command App Runner runs to start your application.
    startCommand :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The command App Runner runs to build your application.
    buildCommand :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The environment variables that are available to your running App Runner
    -- service. An array of key-value pairs. Keys with a prefix of
    -- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
    runtimeEnvironmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)),
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
-- 'port', 'codeConfigurationValues_port' - The port that your application listens to in the container.
--
-- Default: @8080@
--
-- 'startCommand', 'codeConfigurationValues_startCommand' - The command App Runner runs to start your application.
--
-- 'buildCommand', 'codeConfigurationValues_buildCommand' - The command App Runner runs to build your application.
--
-- 'runtimeEnvironmentVariables', 'codeConfigurationValues_runtimeEnvironmentVariables' - The environment variables that are available to your running App Runner
-- service. An array of key-value pairs. Keys with a prefix of
-- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
--
-- 'runtime', 'codeConfigurationValues_runtime' - A runtime environment type for building and running an App Runner
-- service. It represents a programming language runtime.
newCodeConfigurationValues ::
  -- | 'runtime'
  Runtime ->
  CodeConfigurationValues
newCodeConfigurationValues pRuntime_ =
  CodeConfigurationValues'
    { port = Prelude.Nothing,
      startCommand = Prelude.Nothing,
      buildCommand = Prelude.Nothing,
      runtimeEnvironmentVariables = Prelude.Nothing,
      runtime = pRuntime_
    }

-- | The port that your application listens to in the container.
--
-- Default: @8080@
codeConfigurationValues_port :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_port = Lens.lens (\CodeConfigurationValues' {port} -> port) (\s@CodeConfigurationValues' {} a -> s {port = a} :: CodeConfigurationValues)

-- | The command App Runner runs to start your application.
codeConfigurationValues_startCommand :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_startCommand = Lens.lens (\CodeConfigurationValues' {startCommand} -> startCommand) (\s@CodeConfigurationValues' {} a -> s {startCommand = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Core._Sensitive

-- | The command App Runner runs to build your application.
codeConfigurationValues_buildCommand :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_buildCommand = Lens.lens (\CodeConfigurationValues' {buildCommand} -> buildCommand) (\s@CodeConfigurationValues' {} a -> s {buildCommand = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Core._Sensitive

-- | The environment variables that are available to your running App Runner
-- service. An array of key-value pairs. Keys with a prefix of
-- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
codeConfigurationValues_runtimeEnvironmentVariables :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
codeConfigurationValues_runtimeEnvironmentVariables = Lens.lens (\CodeConfigurationValues' {runtimeEnvironmentVariables} -> runtimeEnvironmentVariables) (\s@CodeConfigurationValues' {} a -> s {runtimeEnvironmentVariables = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "StartCommand")
            Prelude.<*> (x Core..:? "BuildCommand")
            Prelude.<*> ( x Core..:? "RuntimeEnvironmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Runtime")
      )

instance Prelude.Hashable CodeConfigurationValues where
  hashWithSalt _salt CodeConfigurationValues' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` startCommand
      `Prelude.hashWithSalt` buildCommand
      `Prelude.hashWithSalt` runtimeEnvironmentVariables
      `Prelude.hashWithSalt` runtime

instance Prelude.NFData CodeConfigurationValues where
  rnf CodeConfigurationValues' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf startCommand
      `Prelude.seq` Prelude.rnf buildCommand
      `Prelude.seq` Prelude.rnf runtimeEnvironmentVariables
      `Prelude.seq` Prelude.rnf runtime

instance Core.ToJSON CodeConfigurationValues where
  toJSON CodeConfigurationValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            ("StartCommand" Core..=) Prelude.<$> startCommand,
            ("BuildCommand" Core..=) Prelude.<$> buildCommand,
            ("RuntimeEnvironmentVariables" Core..=)
              Prelude.<$> runtimeEnvironmentVariables,
            Prelude.Just ("Runtime" Core..= runtime)
          ]
      )
