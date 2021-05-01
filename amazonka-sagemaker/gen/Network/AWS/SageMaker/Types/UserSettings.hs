{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.UserSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.JupyterServerAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayAppSettings
import Network.AWS.SageMaker.Types.SharingSettings
import Network.AWS.SageMaker.Types.TensorBoardAppSettings

-- | A collection of settings that apply to users of Amazon SageMaker Studio.
-- These settings are specified when the CreateUserProfile API is called,
-- and as @DefaultUserSettings@ when the CreateDomain API is called.
--
-- @SecurityGroups@ is aggregated when specified in both calls. For all
-- other settings in @UserSettings@, the values specified in
-- @CreateUserProfile@ take precedence over those specified in
-- @CreateDomain@.
--
-- /See:/ 'newUserSettings' smart constructor.
data UserSettings = UserSettings'
  { -- | The kernel gateway app settings.
    kernelGatewayAppSettings :: Prelude.Maybe KernelGatewayAppSettings,
    -- | The TensorBoard app settings.
    tensorBoardAppSettings :: Prelude.Maybe TensorBoardAppSettings,
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
    -- | The execution role for the user.
    executionRole :: Prelude.Maybe Prelude.Text,
    -- | The sharing settings.
    sharingSettings :: Prelude.Maybe SharingSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kernelGatewayAppSettings', 'userSettings_kernelGatewayAppSettings' - The kernel gateway app settings.
--
-- 'tensorBoardAppSettings', 'userSettings_tensorBoardAppSettings' - The TensorBoard app settings.
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
-- 'executionRole', 'userSettings_executionRole' - The execution role for the user.
--
-- 'sharingSettings', 'userSettings_sharingSettings' - The sharing settings.
newUserSettings ::
  UserSettings
newUserSettings =
  UserSettings'
    { kernelGatewayAppSettings =
        Prelude.Nothing,
      tensorBoardAppSettings = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      jupyterServerAppSettings = Prelude.Nothing,
      executionRole = Prelude.Nothing,
      sharingSettings = Prelude.Nothing
    }

-- | The kernel gateway app settings.
userSettings_kernelGatewayAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe KernelGatewayAppSettings)
userSettings_kernelGatewayAppSettings = Lens.lens (\UserSettings' {kernelGatewayAppSettings} -> kernelGatewayAppSettings) (\s@UserSettings' {} a -> s {kernelGatewayAppSettings = a} :: UserSettings)

-- | The TensorBoard app settings.
userSettings_tensorBoardAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe TensorBoardAppSettings)
userSettings_tensorBoardAppSettings = Lens.lens (\UserSettings' {tensorBoardAppSettings} -> tensorBoardAppSettings) (\s@UserSettings' {} a -> s {tensorBoardAppSettings = a} :: UserSettings)

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
userSettings_securityGroups = Lens.lens (\UserSettings' {securityGroups} -> securityGroups) (\s@UserSettings' {} a -> s {securityGroups = a} :: UserSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | The Jupyter server\'s app settings.
userSettings_jupyterServerAppSettings :: Lens.Lens' UserSettings (Prelude.Maybe JupyterServerAppSettings)
userSettings_jupyterServerAppSettings = Lens.lens (\UserSettings' {jupyterServerAppSettings} -> jupyterServerAppSettings) (\s@UserSettings' {} a -> s {jupyterServerAppSettings = a} :: UserSettings)

-- | The execution role for the user.
userSettings_executionRole :: Lens.Lens' UserSettings (Prelude.Maybe Prelude.Text)
userSettings_executionRole = Lens.lens (\UserSettings' {executionRole} -> executionRole) (\s@UserSettings' {} a -> s {executionRole = a} :: UserSettings)

-- | The sharing settings.
userSettings_sharingSettings :: Lens.Lens' UserSettings (Prelude.Maybe SharingSettings)
userSettings_sharingSettings = Lens.lens (\UserSettings' {sharingSettings} -> sharingSettings) (\s@UserSettings' {} a -> s {sharingSettings = a} :: UserSettings)

instance Prelude.FromJSON UserSettings where
  parseJSON =
    Prelude.withObject
      "UserSettings"
      ( \x ->
          UserSettings'
            Prelude.<$> (x Prelude..:? "KernelGatewayAppSettings")
            Prelude.<*> (x Prelude..:? "TensorBoardAppSettings")
            Prelude.<*> ( x Prelude..:? "SecurityGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "JupyterServerAppSettings")
            Prelude.<*> (x Prelude..:? "ExecutionRole")
            Prelude.<*> (x Prelude..:? "SharingSettings")
      )

instance Prelude.Hashable UserSettings

instance Prelude.NFData UserSettings

instance Prelude.ToJSON UserSettings where
  toJSON UserSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KernelGatewayAppSettings" Prelude..=)
              Prelude.<$> kernelGatewayAppSettings,
            ("TensorBoardAppSettings" Prelude..=)
              Prelude.<$> tensorBoardAppSettings,
            ("SecurityGroups" Prelude..=)
              Prelude.<$> securityGroups,
            ("JupyterServerAppSettings" Prelude..=)
              Prelude.<$> jupyterServerAppSettings,
            ("ExecutionRole" Prelude..=)
              Prelude.<$> executionRole,
            ("SharingSettings" Prelude..=)
              Prelude.<$> sharingSettings
          ]
      )
