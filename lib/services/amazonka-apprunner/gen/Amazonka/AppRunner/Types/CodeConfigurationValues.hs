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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CodeConfigurationValues where

import Amazonka.AppRunner.Types.Runtime
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the basic configuration needed for building and running an App
-- Runner service. This type doesn\'t support the full set of possible
-- configuration options. Fur full configuration capabilities, use a
-- @apprunner.yaml@ file in the source code repository.
--
-- /See:/ 'newCodeConfigurationValues' smart constructor.
data CodeConfigurationValues = CodeConfigurationValues'
  { -- | The command App Runner runs to build your application.
    buildCommand :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The port that your application listens to in the container.
    --
    -- Default: @8080@
    port :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs representing the secrets and parameters that
    -- get referenced to your service as an environment variable. The supported
    -- values are either the full Amazon Resource Name (ARN) of the Secrets
    -- Manager secret or the full ARN of the parameter in the Amazon Web
    -- Services Systems Manager Parameter Store.
    --
    -- -   If the Amazon Web Services Systems Manager Parameter Store parameter
    --     exists in the same Amazon Web Services Region as the service that
    --     you\'re launching, you can use either the full ARN or name of the
    --     secret. If the parameter exists in a different Region, then the full
    --     ARN must be specified.
    --
    -- -   Currently, cross account referencing of Amazon Web Services Systems
    --     Manager Parameter Store parameter is not supported.
    runtimeEnvironmentSecrets :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The environment variables that are available to your running App Runner
    -- service. An array of key-value pairs.
    runtimeEnvironmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The command App Runner runs to start your application.
    startCommand :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
-- 'buildCommand', 'codeConfigurationValues_buildCommand' - The command App Runner runs to build your application.
--
-- 'port', 'codeConfigurationValues_port' - The port that your application listens to in the container.
--
-- Default: @8080@
--
-- 'runtimeEnvironmentSecrets', 'codeConfigurationValues_runtimeEnvironmentSecrets' - An array of key-value pairs representing the secrets and parameters that
-- get referenced to your service as an environment variable. The supported
-- values are either the full Amazon Resource Name (ARN) of the Secrets
-- Manager secret or the full ARN of the parameter in the Amazon Web
-- Services Systems Manager Parameter Store.
--
-- -   If the Amazon Web Services Systems Manager Parameter Store parameter
--     exists in the same Amazon Web Services Region as the service that
--     you\'re launching, you can use either the full ARN or name of the
--     secret. If the parameter exists in a different Region, then the full
--     ARN must be specified.
--
-- -   Currently, cross account referencing of Amazon Web Services Systems
--     Manager Parameter Store parameter is not supported.
--
-- 'runtimeEnvironmentVariables', 'codeConfigurationValues_runtimeEnvironmentVariables' - The environment variables that are available to your running App Runner
-- service. An array of key-value pairs.
--
-- 'startCommand', 'codeConfigurationValues_startCommand' - The command App Runner runs to start your application.
--
-- 'runtime', 'codeConfigurationValues_runtime' - A runtime environment type for building and running an App Runner
-- service. It represents a programming language runtime.
newCodeConfigurationValues ::
  -- | 'runtime'
  Runtime ->
  CodeConfigurationValues
newCodeConfigurationValues pRuntime_ =
  CodeConfigurationValues'
    { buildCommand =
        Prelude.Nothing,
      port = Prelude.Nothing,
      runtimeEnvironmentSecrets = Prelude.Nothing,
      runtimeEnvironmentVariables = Prelude.Nothing,
      startCommand = Prelude.Nothing,
      runtime = pRuntime_
    }

-- | The command App Runner runs to build your application.
codeConfigurationValues_buildCommand :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_buildCommand = Lens.lens (\CodeConfigurationValues' {buildCommand} -> buildCommand) (\s@CodeConfigurationValues' {} a -> s {buildCommand = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Data._Sensitive

-- | The port that your application listens to in the container.
--
-- Default: @8080@
codeConfigurationValues_port :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_port = Lens.lens (\CodeConfigurationValues' {port} -> port) (\s@CodeConfigurationValues' {} a -> s {port = a} :: CodeConfigurationValues)

-- | An array of key-value pairs representing the secrets and parameters that
-- get referenced to your service as an environment variable. The supported
-- values are either the full Amazon Resource Name (ARN) of the Secrets
-- Manager secret or the full ARN of the parameter in the Amazon Web
-- Services Systems Manager Parameter Store.
--
-- -   If the Amazon Web Services Systems Manager Parameter Store parameter
--     exists in the same Amazon Web Services Region as the service that
--     you\'re launching, you can use either the full ARN or name of the
--     secret. If the parameter exists in a different Region, then the full
--     ARN must be specified.
--
-- -   Currently, cross account referencing of Amazon Web Services Systems
--     Manager Parameter Store parameter is not supported.
codeConfigurationValues_runtimeEnvironmentSecrets :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
codeConfigurationValues_runtimeEnvironmentSecrets = Lens.lens (\CodeConfigurationValues' {runtimeEnvironmentSecrets} -> runtimeEnvironmentSecrets) (\s@CodeConfigurationValues' {} a -> s {runtimeEnvironmentSecrets = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables that are available to your running App Runner
-- service. An array of key-value pairs.
codeConfigurationValues_runtimeEnvironmentVariables :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
codeConfigurationValues_runtimeEnvironmentVariables = Lens.lens (\CodeConfigurationValues' {runtimeEnvironmentVariables} -> runtimeEnvironmentVariables) (\s@CodeConfigurationValues' {} a -> s {runtimeEnvironmentVariables = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Lens.coerced

-- | The command App Runner runs to start your application.
codeConfigurationValues_startCommand :: Lens.Lens' CodeConfigurationValues (Prelude.Maybe Prelude.Text)
codeConfigurationValues_startCommand = Lens.lens (\CodeConfigurationValues' {startCommand} -> startCommand) (\s@CodeConfigurationValues' {} a -> s {startCommand = a} :: CodeConfigurationValues) Prelude.. Lens.mapping Data._Sensitive

-- | A runtime environment type for building and running an App Runner
-- service. It represents a programming language runtime.
codeConfigurationValues_runtime :: Lens.Lens' CodeConfigurationValues Runtime
codeConfigurationValues_runtime = Lens.lens (\CodeConfigurationValues' {runtime} -> runtime) (\s@CodeConfigurationValues' {} a -> s {runtime = a} :: CodeConfigurationValues)

instance Data.FromJSON CodeConfigurationValues where
  parseJSON =
    Data.withObject
      "CodeConfigurationValues"
      ( \x ->
          CodeConfigurationValues'
            Prelude.<$> (x Data..:? "BuildCommand")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> ( x
                            Data..:? "RuntimeEnvironmentSecrets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "RuntimeEnvironmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StartCommand")
            Prelude.<*> (x Data..: "Runtime")
      )

instance Prelude.Hashable CodeConfigurationValues where
  hashWithSalt _salt CodeConfigurationValues' {..} =
    _salt
      `Prelude.hashWithSalt` buildCommand
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` runtimeEnvironmentSecrets
      `Prelude.hashWithSalt` runtimeEnvironmentVariables
      `Prelude.hashWithSalt` startCommand
      `Prelude.hashWithSalt` runtime

instance Prelude.NFData CodeConfigurationValues where
  rnf CodeConfigurationValues' {..} =
    Prelude.rnf buildCommand `Prelude.seq`
      Prelude.rnf port `Prelude.seq`
        Prelude.rnf runtimeEnvironmentSecrets `Prelude.seq`
          Prelude.rnf runtimeEnvironmentVariables `Prelude.seq`
            Prelude.rnf startCommand `Prelude.seq`
              Prelude.rnf runtime

instance Data.ToJSON CodeConfigurationValues where
  toJSON CodeConfigurationValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BuildCommand" Data..=) Prelude.<$> buildCommand,
            ("Port" Data..=) Prelude.<$> port,
            ("RuntimeEnvironmentSecrets" Data..=)
              Prelude.<$> runtimeEnvironmentSecrets,
            ("RuntimeEnvironmentVariables" Data..=)
              Prelude.<$> runtimeEnvironmentVariables,
            ("StartCommand" Data..=) Prelude.<$> startCommand,
            Prelude.Just ("Runtime" Data..= runtime)
          ]
      )
