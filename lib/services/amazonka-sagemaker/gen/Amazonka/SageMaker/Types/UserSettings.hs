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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UserSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CanvasAppSettings
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings
import Amazonka.SageMaker.Types.RSessionAppSettings
import Amazonka.SageMaker.Types.RStudioServerProAppSettings
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
  { -- | The Canvas app settings.
    canvasAppSettings :: Prelude.Maybe CanvasAppSettings,
    -- | The execution role for the user.
    executionRole :: Prelude.Maybe Prelude.Text,
    -- | The Jupyter server\'s app settings.
    jupyterServerAppSettings :: Prelude.Maybe JupyterServerAppSettings,
    -- | The kernel gateway app settings.
    kernelGatewayAppSettings :: Prelude.Maybe KernelGatewayAppSettings,
    -- | A collection of settings that configure the @RSessionGateway@ app.
    rSessionAppSettings :: Prelude.Maybe RSessionAppSettings,
    -- | A collection of settings that configure user interaction with the
    -- @RStudioServerPro@ app.
    rStudioServerProAppSettings :: Prelude.Maybe RStudioServerProAppSettings,
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
    -- | Specifies options for sharing SageMaker Studio notebooks.
    sharingSettings :: Prelude.Maybe SharingSettings,
    -- | The TensorBoard app settings.
    tensorBoardAppSettings :: Prelude.Maybe TensorBoardAppSettings
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
-- 'canvasAppSettings', 'userSettings_canvasAppSettings' - The Canvas app settings.
--
-- 'executionRole', 'userSettings_executionRole' - The execution role for the user.
--
-- 'jupyterServerAppSettings', 'userSettings_jupyterServerAppSettings' - The Jupyter server\'s app settings.
--
-- 'kernelGatewayAppSettings', 'userSettings_kernelGatewayAppSettings' - The kernel gateway app settings.
--
-- 'rSessionAppSettings', 'userSettings_rSessionAppSettings' - A collection of settings that configure the @RSessionGateway@ app.
--
-- 'rStudioServerProAppSettings', 'userSettings_rStudioServerProAppSettings' - A collection of settings that configure user interaction with the
-- @RStudioServerPro@ app.
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
-- 'sharingSettings', 'userSettings_sharingSettings' - Specifies options for sharing SageMaker Studio notebooks.
--
-- 'tensorBoardAppSettings', 'userSettings_tensorBoardAppSettings' - The TensorBoard app settings.
newUserSettings ::
  UserSettings
newUserSettings =
  UserSettings'
    { canvasAppSettings = Prelude.Nothing,
      executionRole = Prelude.Nothing,
      jupyterServerAppSettings = Prelude.Nothing,
      kernelGatewayAppSettings = Prelude.Nothing,
      rSessionAppSettings = Prelude.Nothing,
      rStudioServerProAppSettings = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      sharingSettings = Prelude.Nothing,
      tensorBoardAppSettings = Prelude.Nothing
    }

-- | The Canvas app settings.
userSettings_canvasAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe CanvasAppSettings)
userSettings_canvasAppSettings = Lens.lens (\UserSettings' {canvasAppSettings} -> canvasAppSettings) (\s@UserSettings' {} a -> s {canvasAppSettings = a} :: UserSettings)

-- | The execution role for the user.
userSettings_executionRole :: Lens.Lens' UserSettings (Prelude.Maybe Prelude.Text)
userSettings_executionRole = Lens.lens (\UserSettings' {executionRole} -> executionRole) (\s@UserSettings' {} a -> s {executionRole = a} :: UserSettings)

-- | The Jupyter server\'s app settings.
userSettings_jupyterServerAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe JupyterServerAppSettings)
userSettings_jupyterServerAppSettings = Lens.lens (\UserSettings' {jupyterServerAppSettings} -> jupyterServerAppSettings) (\s@UserSettings' {} a -> s {jupyterServerAppSettings = a} :: UserSettings)

-- | The kernel gateway app settings.
userSettings_kernelGatewayAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe KernelGatewayAppSettings)
userSettings_kernelGatewayAppSettings = Lens.lens (\UserSettings' {kernelGatewayAppSettings} -> kernelGatewayAppSettings) (\s@UserSettings' {} a -> s {kernelGatewayAppSettings = a} :: UserSettings)

-- | A collection of settings that configure the @RSessionGateway@ app.
userSettings_rSessionAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe RSessionAppSettings)
userSettings_rSessionAppSettings = Lens.lens (\UserSettings' {rSessionAppSettings} -> rSessionAppSettings) (\s@UserSettings' {} a -> s {rSessionAppSettings = a} :: UserSettings)

-- | A collection of settings that configure user interaction with the
-- @RStudioServerPro@ app.
userSettings_rStudioServerProAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe RStudioServerProAppSettings)
userSettings_rStudioServerProAppSettings = Lens.lens (\UserSettings' {rStudioServerProAppSettings} -> rStudioServerProAppSettings) (\s@UserSettings' {} a -> s {rStudioServerProAppSettings = a} :: UserSettings)

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

-- | Specifies options for sharing SageMaker Studio notebooks.
userSettings_sharingSettings :: Lens.Lens' UserSettings (Prelude.Maybe SharingSettings)
userSettings_sharingSettings = Lens.lens (\UserSettings' {sharingSettings} -> sharingSettings) (\s@UserSettings' {} a -> s {sharingSettings = a} :: UserSettings)

-- | The TensorBoard app settings.
userSettings_tensorBoardAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe TensorBoardAppSettings)
userSettings_tensorBoardAppSettings = Lens.lens (\UserSettings' {tensorBoardAppSettings} -> tensorBoardAppSettings) (\s@UserSettings' {} a -> s {tensorBoardAppSettings = a} :: UserSettings)

instance Data.FromJSON UserSettings where
  parseJSON =
    Data.withObject
      "UserSettings"
      ( \x ->
          UserSettings'
            Prelude.<$> (x Data..:? "CanvasAppSettings")
            Prelude.<*> (x Data..:? "ExecutionRole")
            Prelude.<*> (x Data..:? "JupyterServerAppSettings")
            Prelude.<*> (x Data..:? "KernelGatewayAppSettings")
            Prelude.<*> (x Data..:? "RSessionAppSettings")
            Prelude.<*> (x Data..:? "RStudioServerProAppSettings")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SharingSettings")
            Prelude.<*> (x Data..:? "TensorBoardAppSettings")
      )

instance Prelude.Hashable UserSettings where
  hashWithSalt _salt UserSettings' {..} =
    _salt `Prelude.hashWithSalt` canvasAppSettings
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` jupyterServerAppSettings
      `Prelude.hashWithSalt` kernelGatewayAppSettings
      `Prelude.hashWithSalt` rSessionAppSettings
      `Prelude.hashWithSalt` rStudioServerProAppSettings
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` sharingSettings
      `Prelude.hashWithSalt` tensorBoardAppSettings

instance Prelude.NFData UserSettings where
  rnf UserSettings' {..} =
    Prelude.rnf canvasAppSettings
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf jupyterServerAppSettings
      `Prelude.seq` Prelude.rnf kernelGatewayAppSettings
      `Prelude.seq` Prelude.rnf rSessionAppSettings
      `Prelude.seq` Prelude.rnf rStudioServerProAppSettings
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf sharingSettings
      `Prelude.seq` Prelude.rnf tensorBoardAppSettings

instance Data.ToJSON UserSettings where
  toJSON UserSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CanvasAppSettings" Data..=)
              Prelude.<$> canvasAppSettings,
            ("ExecutionRole" Data..=) Prelude.<$> executionRole,
            ("JupyterServerAppSettings" Data..=)
              Prelude.<$> jupyterServerAppSettings,
            ("KernelGatewayAppSettings" Data..=)
              Prelude.<$> kernelGatewayAppSettings,
            ("RSessionAppSettings" Data..=)
              Prelude.<$> rSessionAppSettings,
            ("RStudioServerProAppSettings" Data..=)
              Prelude.<$> rStudioServerProAppSettings,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("SharingSettings" Data..=)
              Prelude.<$> sharingSettings,
            ("TensorBoardAppSettings" Data..=)
              Prelude.<$> tensorBoardAppSettings
          ]
      )
