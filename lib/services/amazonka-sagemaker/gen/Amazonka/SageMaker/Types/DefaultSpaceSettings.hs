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
-- Module      : Amazonka.SageMaker.Types.DefaultSpaceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DefaultSpaceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings

-- | A collection of settings that apply to spaces created in the Domain.
--
-- /See:/ 'newDefaultSpaceSettings' smart constructor.
data DefaultSpaceSettings = DefaultSpaceSettings'
  { -- | The execution role for the space.
    executionRole :: Prelude.Maybe Prelude.Text,
    jupyterServerAppSettings :: Prelude.Maybe JupyterServerAppSettings,
    kernelGatewayAppSettings :: Prelude.Maybe KernelGatewayAppSettings,
    -- | The security groups for the Amazon Virtual Private Cloud that the space
    -- uses for communication.
    securityGroups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultSpaceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionRole', 'defaultSpaceSettings_executionRole' - The execution role for the space.
--
-- 'jupyterServerAppSettings', 'defaultSpaceSettings_jupyterServerAppSettings' - Undocumented member.
--
-- 'kernelGatewayAppSettings', 'defaultSpaceSettings_kernelGatewayAppSettings' - Undocumented member.
--
-- 'securityGroups', 'defaultSpaceSettings_securityGroups' - The security groups for the Amazon Virtual Private Cloud that the space
-- uses for communication.
newDefaultSpaceSettings ::
  DefaultSpaceSettings
newDefaultSpaceSettings =
  DefaultSpaceSettings'
    { executionRole =
        Prelude.Nothing,
      jupyterServerAppSettings = Prelude.Nothing,
      kernelGatewayAppSettings = Prelude.Nothing,
      securityGroups = Prelude.Nothing
    }

-- | The execution role for the space.
defaultSpaceSettings_executionRole :: Lens.Lens' DefaultSpaceSettings (Prelude.Maybe Prelude.Text)
defaultSpaceSettings_executionRole = Lens.lens (\DefaultSpaceSettings' {executionRole} -> executionRole) (\s@DefaultSpaceSettings' {} a -> s {executionRole = a} :: DefaultSpaceSettings)

-- | Undocumented member.
defaultSpaceSettings_jupyterServerAppSettings :: Lens.Lens' DefaultSpaceSettings (Prelude.Maybe JupyterServerAppSettings)
defaultSpaceSettings_jupyterServerAppSettings = Lens.lens (\DefaultSpaceSettings' {jupyterServerAppSettings} -> jupyterServerAppSettings) (\s@DefaultSpaceSettings' {} a -> s {jupyterServerAppSettings = a} :: DefaultSpaceSettings)

-- | Undocumented member.
defaultSpaceSettings_kernelGatewayAppSettings :: Lens.Lens' DefaultSpaceSettings (Prelude.Maybe KernelGatewayAppSettings)
defaultSpaceSettings_kernelGatewayAppSettings = Lens.lens (\DefaultSpaceSettings' {kernelGatewayAppSettings} -> kernelGatewayAppSettings) (\s@DefaultSpaceSettings' {} a -> s {kernelGatewayAppSettings = a} :: DefaultSpaceSettings)

-- | The security groups for the Amazon Virtual Private Cloud that the space
-- uses for communication.
defaultSpaceSettings_securityGroups :: Lens.Lens' DefaultSpaceSettings (Prelude.Maybe [Prelude.Text])
defaultSpaceSettings_securityGroups = Lens.lens (\DefaultSpaceSettings' {securityGroups} -> securityGroups) (\s@DefaultSpaceSettings' {} a -> s {securityGroups = a} :: DefaultSpaceSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DefaultSpaceSettings where
  parseJSON =
    Data.withObject
      "DefaultSpaceSettings"
      ( \x ->
          DefaultSpaceSettings'
            Prelude.<$> (x Data..:? "ExecutionRole")
            Prelude.<*> (x Data..:? "JupyterServerAppSettings")
            Prelude.<*> (x Data..:? "KernelGatewayAppSettings")
            Prelude.<*> ( x
                            Data..:? "SecurityGroups"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DefaultSpaceSettings where
  hashWithSalt _salt DefaultSpaceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` jupyterServerAppSettings
      `Prelude.hashWithSalt` kernelGatewayAppSettings
      `Prelude.hashWithSalt` securityGroups

instance Prelude.NFData DefaultSpaceSettings where
  rnf DefaultSpaceSettings' {..} =
    Prelude.rnf executionRole `Prelude.seq`
      Prelude.rnf jupyterServerAppSettings `Prelude.seq`
        Prelude.rnf kernelGatewayAppSettings `Prelude.seq`
          Prelude.rnf securityGroups

instance Data.ToJSON DefaultSpaceSettings where
  toJSON DefaultSpaceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExecutionRole" Data..=) Prelude.<$> executionRole,
            ("JupyterServerAppSettings" Data..=)
              Prelude.<$> jupyterServerAppSettings,
            ("KernelGatewayAppSettings" Data..=)
              Prelude.<$> kernelGatewayAppSettings,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups
          ]
      )
