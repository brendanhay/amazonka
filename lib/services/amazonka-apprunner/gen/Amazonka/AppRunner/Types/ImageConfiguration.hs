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
-- Module      : Amazonka.AppRunner.Types.ImageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ImageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration that App Runner uses to run an App Runner
-- service using an image pulled from a source image repository.
--
-- /See:/ 'newImageConfiguration' smart constructor.
data ImageConfiguration = ImageConfiguration'
  { -- | The port that your application listens to in the container.
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
    -- | Environment variables that are available to your running App Runner
    -- service. An array of key-value pairs.
    runtimeEnvironmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | An optional command that App Runner runs to start the application in the
    -- source image. If specified, this command overrides the Docker image’s
    -- default start command.
    startCommand :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'imageConfiguration_port' - The port that your application listens to in the container.
--
-- Default: @8080@
--
-- 'runtimeEnvironmentSecrets', 'imageConfiguration_runtimeEnvironmentSecrets' - An array of key-value pairs representing the secrets and parameters that
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
-- 'runtimeEnvironmentVariables', 'imageConfiguration_runtimeEnvironmentVariables' - Environment variables that are available to your running App Runner
-- service. An array of key-value pairs.
--
-- 'startCommand', 'imageConfiguration_startCommand' - An optional command that App Runner runs to start the application in the
-- source image. If specified, this command overrides the Docker image’s
-- default start command.
newImageConfiguration ::
  ImageConfiguration
newImageConfiguration =
  ImageConfiguration'
    { port = Prelude.Nothing,
      runtimeEnvironmentSecrets = Prelude.Nothing,
      runtimeEnvironmentVariables = Prelude.Nothing,
      startCommand = Prelude.Nothing
    }

-- | The port that your application listens to in the container.
--
-- Default: @8080@
imageConfiguration_port :: Lens.Lens' ImageConfiguration (Prelude.Maybe Prelude.Text)
imageConfiguration_port = Lens.lens (\ImageConfiguration' {port} -> port) (\s@ImageConfiguration' {} a -> s {port = a} :: ImageConfiguration)

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
imageConfiguration_runtimeEnvironmentSecrets :: Lens.Lens' ImageConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imageConfiguration_runtimeEnvironmentSecrets = Lens.lens (\ImageConfiguration' {runtimeEnvironmentSecrets} -> runtimeEnvironmentSecrets) (\s@ImageConfiguration' {} a -> s {runtimeEnvironmentSecrets = a} :: ImageConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Environment variables that are available to your running App Runner
-- service. An array of key-value pairs.
imageConfiguration_runtimeEnvironmentVariables :: Lens.Lens' ImageConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imageConfiguration_runtimeEnvironmentVariables = Lens.lens (\ImageConfiguration' {runtimeEnvironmentVariables} -> runtimeEnvironmentVariables) (\s@ImageConfiguration' {} a -> s {runtimeEnvironmentVariables = a} :: ImageConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional command that App Runner runs to start the application in the
-- source image. If specified, this command overrides the Docker image’s
-- default start command.
imageConfiguration_startCommand :: Lens.Lens' ImageConfiguration (Prelude.Maybe Prelude.Text)
imageConfiguration_startCommand = Lens.lens (\ImageConfiguration' {startCommand} -> startCommand) (\s@ImageConfiguration' {} a -> s {startCommand = a} :: ImageConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ImageConfiguration where
  parseJSON =
    Data.withObject
      "ImageConfiguration"
      ( \x ->
          ImageConfiguration'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> ( x
                            Data..:? "RuntimeEnvironmentSecrets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "RuntimeEnvironmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StartCommand")
      )

instance Prelude.Hashable ImageConfiguration where
  hashWithSalt _salt ImageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` runtimeEnvironmentSecrets
      `Prelude.hashWithSalt` runtimeEnvironmentVariables
      `Prelude.hashWithSalt` startCommand

instance Prelude.NFData ImageConfiguration where
  rnf ImageConfiguration' {..} =
    Prelude.rnf port `Prelude.seq`
      Prelude.rnf runtimeEnvironmentSecrets `Prelude.seq`
        Prelude.rnf runtimeEnvironmentVariables `Prelude.seq`
          Prelude.rnf startCommand

instance Data.ToJSON ImageConfiguration where
  toJSON ImageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("RuntimeEnvironmentSecrets" Data..=)
              Prelude.<$> runtimeEnvironmentSecrets,
            ("RuntimeEnvironmentVariables" Data..=)
              Prelude.<$> runtimeEnvironmentVariables,
            ("StartCommand" Data..=) Prelude.<$> startCommand
          ]
      )
