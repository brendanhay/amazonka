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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    -- | Environment variables that are available to your running App Runner
    -- service. An array of key-value pairs. Keys with a prefix of
    -- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
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
-- 'runtimeEnvironmentVariables', 'imageConfiguration_runtimeEnvironmentVariables' - Environment variables that are available to your running App Runner
-- service. An array of key-value pairs. Keys with a prefix of
-- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
--
-- 'startCommand', 'imageConfiguration_startCommand' - An optional command that App Runner runs to start the application in the
-- source image. If specified, this command overrides the Docker image’s
-- default start command.
newImageConfiguration ::
  ImageConfiguration
newImageConfiguration =
  ImageConfiguration'
    { port = Prelude.Nothing,
      runtimeEnvironmentVariables = Prelude.Nothing,
      startCommand = Prelude.Nothing
    }

-- | The port that your application listens to in the container.
--
-- Default: @8080@
imageConfiguration_port :: Lens.Lens' ImageConfiguration (Prelude.Maybe Prelude.Text)
imageConfiguration_port = Lens.lens (\ImageConfiguration' {port} -> port) (\s@ImageConfiguration' {} a -> s {port = a} :: ImageConfiguration)

-- | Environment variables that are available to your running App Runner
-- service. An array of key-value pairs. Keys with a prefix of
-- @AWSAPPRUNNER@ are reserved for system use and aren\'t valid.
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
            Prelude.<*> ( x Data..:? "RuntimeEnvironmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StartCommand")
      )

instance Prelude.Hashable ImageConfiguration where
  hashWithSalt _salt ImageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` runtimeEnvironmentVariables
      `Prelude.hashWithSalt` startCommand

instance Prelude.NFData ImageConfiguration where
  rnf ImageConfiguration' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf runtimeEnvironmentVariables
      `Prelude.seq` Prelude.rnf startCommand

instance Data.ToJSON ImageConfiguration where
  toJSON ImageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("RuntimeEnvironmentVariables" Data..=)
              Prelude.<$> runtimeEnvironmentVariables,
            ("StartCommand" Data..=) Prelude.<$> startCommand
          ]
      )
