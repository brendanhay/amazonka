{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserSettings
  ( UserSettings (..),

    -- * Smart constructor
    mkUserSettings,

    -- * Lenses
    usTensorBoardAppSettings,
    usKernelGatewayAppSettings,
    usSecurityGroups,
    usJupyterServerAppSettings,
    usSharingSettings,
    usExecutionRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.JupyterServerAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayAppSettings
import Network.AWS.SageMaker.Types.SharingSettings
import Network.AWS.SageMaker.Types.TensorBoardAppSettings

-- | A collection of settings.
--
-- /See:/ 'mkUserSettings' smart constructor.
data UserSettings = UserSettings'
  { tensorBoardAppSettings ::
      Lude.Maybe TensorBoardAppSettings,
    kernelGatewayAppSettings :: Lude.Maybe KernelGatewayAppSettings,
    securityGroups :: Lude.Maybe [Lude.Text],
    jupyterServerAppSettings :: Lude.Maybe JupyterServerAppSettings,
    sharingSettings :: Lude.Maybe SharingSettings,
    executionRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserSettings' with the minimum fields required to make a request.
--
-- * 'executionRole' - The execution role for the user.
-- * 'jupyterServerAppSettings' - The Jupyter server's app settings.
-- * 'kernelGatewayAppSettings' - The kernel gateway app settings.
-- * 'securityGroups' - The security groups for the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set to @PublicInternetOnly@ .
-- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set to @VpcOnly@ .
-- * 'sharingSettings' - The sharing settings.
-- * 'tensorBoardAppSettings' - The TensorBoard app settings.
mkUserSettings ::
  UserSettings
mkUserSettings =
  UserSettings'
    { tensorBoardAppSettings = Lude.Nothing,
      kernelGatewayAppSettings = Lude.Nothing,
      securityGroups = Lude.Nothing,
      jupyterServerAppSettings = Lude.Nothing,
      sharingSettings = Lude.Nothing,
      executionRole = Lude.Nothing
    }

-- | The TensorBoard app settings.
--
-- /Note:/ Consider using 'tensorBoardAppSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTensorBoardAppSettings :: Lens.Lens' UserSettings (Lude.Maybe TensorBoardAppSettings)
usTensorBoardAppSettings = Lens.lens (tensorBoardAppSettings :: UserSettings -> Lude.Maybe TensorBoardAppSettings) (\s a -> s {tensorBoardAppSettings = a} :: UserSettings)
{-# DEPRECATED usTensorBoardAppSettings "Use generic-lens or generic-optics with 'tensorBoardAppSettings' instead." #-}

-- | The kernel gateway app settings.
--
-- /Note:/ Consider using 'kernelGatewayAppSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usKernelGatewayAppSettings :: Lens.Lens' UserSettings (Lude.Maybe KernelGatewayAppSettings)
usKernelGatewayAppSettings = Lens.lens (kernelGatewayAppSettings :: UserSettings -> Lude.Maybe KernelGatewayAppSettings) (\s a -> s {kernelGatewayAppSettings = a} :: UserSettings)
{-# DEPRECATED usKernelGatewayAppSettings "Use generic-lens or generic-optics with 'kernelGatewayAppSettings' instead." #-}

-- | The security groups for the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set to @PublicInternetOnly@ .
-- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set to @VpcOnly@ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecurityGroups :: Lens.Lens' UserSettings (Lude.Maybe [Lude.Text])
usSecurityGroups = Lens.lens (securityGroups :: UserSettings -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: UserSettings)
{-# DEPRECATED usSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The Jupyter server's app settings.
--
-- /Note:/ Consider using 'jupyterServerAppSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usJupyterServerAppSettings :: Lens.Lens' UserSettings (Lude.Maybe JupyterServerAppSettings)
usJupyterServerAppSettings = Lens.lens (jupyterServerAppSettings :: UserSettings -> Lude.Maybe JupyterServerAppSettings) (\s a -> s {jupyterServerAppSettings = a} :: UserSettings)
{-# DEPRECATED usJupyterServerAppSettings "Use generic-lens or generic-optics with 'jupyterServerAppSettings' instead." #-}

-- | The sharing settings.
--
-- /Note:/ Consider using 'sharingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSharingSettings :: Lens.Lens' UserSettings (Lude.Maybe SharingSettings)
usSharingSettings = Lens.lens (sharingSettings :: UserSettings -> Lude.Maybe SharingSettings) (\s a -> s {sharingSettings = a} :: UserSettings)
{-# DEPRECATED usSharingSettings "Use generic-lens or generic-optics with 'sharingSettings' instead." #-}

-- | The execution role for the user.
--
-- /Note:/ Consider using 'executionRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usExecutionRole :: Lens.Lens' UserSettings (Lude.Maybe Lude.Text)
usExecutionRole = Lens.lens (executionRole :: UserSettings -> Lude.Maybe Lude.Text) (\s a -> s {executionRole = a} :: UserSettings)
{-# DEPRECATED usExecutionRole "Use generic-lens or generic-optics with 'executionRole' instead." #-}

instance Lude.FromJSON UserSettings where
  parseJSON =
    Lude.withObject
      "UserSettings"
      ( \x ->
          UserSettings'
            Lude.<$> (x Lude..:? "TensorBoardAppSettings")
            Lude.<*> (x Lude..:? "KernelGatewayAppSettings")
            Lude.<*> (x Lude..:? "SecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "JupyterServerAppSettings")
            Lude.<*> (x Lude..:? "SharingSettings")
            Lude.<*> (x Lude..:? "ExecutionRole")
      )

instance Lude.ToJSON UserSettings where
  toJSON UserSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TensorBoardAppSettings" Lude..=)
              Lude.<$> tensorBoardAppSettings,
            ("KernelGatewayAppSettings" Lude..=)
              Lude.<$> kernelGatewayAppSettings,
            ("SecurityGroups" Lude..=) Lude.<$> securityGroups,
            ("JupyterServerAppSettings" Lude..=)
              Lude.<$> jupyterServerAppSettings,
            ("SharingSettings" Lude..=) Lude.<$> sharingSettings,
            ("ExecutionRole" Lude..=) Lude.<$> executionRole
          ]
      )
