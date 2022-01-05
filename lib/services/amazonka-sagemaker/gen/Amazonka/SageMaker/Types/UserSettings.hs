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
-- Module      : Amazonka.SageMaker.Types.UserSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UserSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.TensorBoardAppSettings

-- | A collection of settings that apply to users of Amazon SageMaker Studio.
-- These settings are specified when the @CreateUserProfile@ API is called,
-- and as @DefaultUserSettings@ when the @CreateDomain@ API is called.
--
-- @SecurityGroups@ is aggregated when specified in both calls. For all
-- other settings in @UserSettings@, the values specified in
-- @CreateUserProfile@ take precedence over those specified in
-- @CreateDomain@.
--
-- /See:/ 'newUserSettings' smart constructor.
data UserSettings = UserSettings'
  { -- | The TensorBoard app settings.
    tensorBoardAppSettings :: Prelude.Maybe TensorBoardAppSettings,
    -- | The kernel gateway app settings.
    kernelGatewayAppSettings :: Prelude.Maybe KernelGatewayAppSettings,
    -- | The security groups for the Amazon Virtual Private Cloud (VPC) that
    -- Studio uses for communication.
    --
    -- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set
    -- to @PublicInternetOnly@.
    --
    -- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set
    -- to @VpcOnly@.
    --
    -- Amazon SageMaker adds a security group to allow NFS traffic from
    -- SageMaker Studio. Therefore, the number of security groups that you can
    -- specify is one less than the maximum number shown.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The Jupyter server\'s app settings.
    jupyterServerAppSettings :: Prelude.Maybe JupyterServerAppSettings,
    -- | Specifies options for sharing SageMaker Studio notebooks.
    sharingSettings :: Prelude.Maybe SharingSettings,
    -- | The execution role for the user.
    executionRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tensorBoardAppSettings', 'userSettings_tensorBoardAppSettings' - The TensorBoard app settings.
--
-- 'kernelGatewayAppSettings', 'userSettings_kernelGatewayAppSettings' - The kernel gateway app settings.
--
-- 'securityGroups', 'userSettings_securityGroups' - The security groups for the Amazon Virtual Private Cloud (VPC) that
-- Studio uses for communication.
--
-- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set
-- to @PublicInternetOnly@.
--
-- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set
-- to @VpcOnly@.
--
-- Amazon SageMaker adds a security group to allow NFS traffic from
-- SageMaker Studio. Therefore, the number of security groups that you can
-- specify is one less than the maximum number shown.
--
-- 'jupyterServerAppSettings', 'userSettings_jupyterServerAppSettings' - The Jupyter server\'s app settings.
--
-- 'sharingSettings', 'userSettings_sharingSettings' - Specifies options for sharing SageMaker Studio notebooks.
--
-- 'executionRole', 'userSettings_executionRole' - The execution role for the user.
newUserSettings ::
  UserSettings
newUserSettings =
  UserSettings'
    { tensorBoardAppSettings =
        Prelude.Nothing,
      kernelGatewayAppSettings = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      jupyterServerAppSettings = Prelude.Nothing,
      sharingSettings = Prelude.Nothing,
      executionRole = Prelude.Nothing
    }

-- | The TensorBoard app settings.
userSettings_tensorBoardAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe TensorBoardAppSettings)
userSettings_tensorBoardAppSettings = Lens.lens (\UserSettings' {tensorBoardAppSettings} -> tensorBoardAppSettings) (\s@UserSettings' {} a -> s {tensorBoardAppSettings = a} :: UserSettings)

-- | The kernel gateway app settings.
userSettings_kernelGatewayAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe KernelGatewayAppSettings)
userSettings_kernelGatewayAppSettings = Lens.lens (\UserSettings' {kernelGatewayAppSettings} -> kernelGatewayAppSettings) (\s@UserSettings' {} a -> s {kernelGatewayAppSettings = a} :: UserSettings)

-- | The security groups for the Amazon Virtual Private Cloud (VPC) that
-- Studio uses for communication.
--
-- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set
-- to @PublicInternetOnly@.
--
-- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set
-- to @VpcOnly@.
--
-- Amazon SageMaker adds a security group to allow NFS traffic from
-- SageMaker Studio. Therefore, the number of security groups that you can
-- specify is one less than the maximum number shown.
userSettings_securityGroups :: Lens.Lens' UserSettings (Prelude.Maybe [Prelude.Text])
userSettings_securityGroups = Lens.lens (\UserSettings' {securityGroups} -> securityGroups) (\s@UserSettings' {} a -> s {securityGroups = a} :: UserSettings) Prelude.. Lens.mapping Lens.coerced

-- | The Jupyter server\'s app settings.
userSettings_jupyterServerAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe JupyterServerAppSettings)
userSettings_jupyterServerAppSettings = Lens.lens (\UserSettings' {jupyterServerAppSettings} -> jupyterServerAppSettings) (\s@UserSettings' {} a -> s {jupyterServerAppSettings = a} :: UserSettings)

-- | Specifies options for sharing SageMaker Studio notebooks.
userSettings_sharingSettings :: Lens.Lens' UserSettings (Prelude.Maybe SharingSettings)
userSettings_sharingSettings = Lens.lens (\UserSettings' {sharingSettings} -> sharingSettings) (\s@UserSettings' {} a -> s {sharingSettings = a} :: UserSettings)

-- | The execution role for the user.
userSettings_executionRole :: Lens.Lens' UserSettings (Prelude.Maybe Prelude.Text)
userSettings_executionRole = Lens.lens (\UserSettings' {executionRole} -> executionRole) (\s@UserSettings' {} a -> s {executionRole = a} :: UserSettings)

instance Core.FromJSON UserSettings where
  parseJSON =
    Core.withObject
      "UserSettings"
      ( \x ->
          UserSettings'
            Prelude.<$> (x Core..:? "TensorBoardAppSettings")
            Prelude.<*> (x Core..:? "KernelGatewayAppSettings")
            Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "JupyterServerAppSettings")
            Prelude.<*> (x Core..:? "SharingSettings")
            Prelude.<*> (x Core..:? "ExecutionRole")
      )

instance Prelude.Hashable UserSettings where
  hashWithSalt _salt UserSettings' {..} =
    _salt `Prelude.hashWithSalt` tensorBoardAppSettings
      `Prelude.hashWithSalt` kernelGatewayAppSettings
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` jupyterServerAppSettings
      `Prelude.hashWithSalt` sharingSettings
      `Prelude.hashWithSalt` executionRole

instance Prelude.NFData UserSettings where
  rnf UserSettings' {..} =
    Prelude.rnf tensorBoardAppSettings
      `Prelude.seq` Prelude.rnf kernelGatewayAppSettings
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf jupyterServerAppSettings
      `Prelude.seq` Prelude.rnf sharingSettings
      `Prelude.seq` Prelude.rnf executionRole

instance Core.ToJSON UserSettings where
  toJSON UserSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TensorBoardAppSettings" Core..=)
              Prelude.<$> tensorBoardAppSettings,
            ("KernelGatewayAppSettings" Core..=)
              Prelude.<$> kernelGatewayAppSettings,
            ("SecurityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("JupyterServerAppSettings" Core..=)
              Prelude.<$> jupyterServerAppSettings,
            ("SharingSettings" Core..=)
              Prelude.<$> sharingSettings,
            ("ExecutionRole" Core..=) Prelude.<$> executionRole
          ]
      )
