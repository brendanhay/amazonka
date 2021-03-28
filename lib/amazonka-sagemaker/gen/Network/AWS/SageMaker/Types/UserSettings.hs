{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.UserSettings
  ( UserSettings (..)
  -- * Smart constructor
  , mkUserSettings
  -- * Lenses
  , usExecutionRole
  , usJupyterServerAppSettings
  , usKernelGatewayAppSettings
  , usSecurityGroups
  , usSharingSettings
  , usTensorBoardAppSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ExecutionRole as Types
import qualified Network.AWS.SageMaker.Types.JupyterServerAppSettings as Types
import qualified Network.AWS.SageMaker.Types.KernelGatewayAppSettings as Types
import qualified Network.AWS.SageMaker.Types.SecurityGroupId as Types
import qualified Network.AWS.SageMaker.Types.SharingSettings as Types
import qualified Network.AWS.SageMaker.Types.TensorBoardAppSettings as Types

-- | A collection of settings.
--
-- /See:/ 'mkUserSettings' smart constructor.
data UserSettings = UserSettings'
  { executionRole :: Core.Maybe Types.ExecutionRole
    -- ^ The execution role for the user.
  , jupyterServerAppSettings :: Core.Maybe Types.JupyterServerAppSettings
    -- ^ The Jupyter server's app settings.
  , kernelGatewayAppSettings :: Core.Maybe Types.KernelGatewayAppSettings
    -- ^ The kernel gateway app settings.
  , securityGroups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The security groups for the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set to @PublicInternetOnly@ .
-- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set to @VpcOnly@ .
  , sharingSettings :: Core.Maybe Types.SharingSettings
    -- ^ The sharing settings.
  , tensorBoardAppSettings :: Core.Maybe Types.TensorBoardAppSettings
    -- ^ The TensorBoard app settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserSettings' value with any optional fields omitted.
mkUserSettings
    :: UserSettings
mkUserSettings
  = UserSettings'{executionRole = Core.Nothing,
                  jupyterServerAppSettings = Core.Nothing,
                  kernelGatewayAppSettings = Core.Nothing,
                  securityGroups = Core.Nothing, sharingSettings = Core.Nothing,
                  tensorBoardAppSettings = Core.Nothing}

-- | The execution role for the user.
--
-- /Note:/ Consider using 'executionRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usExecutionRole :: Lens.Lens' UserSettings (Core.Maybe Types.ExecutionRole)
usExecutionRole = Lens.field @"executionRole"
{-# INLINEABLE usExecutionRole #-}
{-# DEPRECATED executionRole "Use generic-lens or generic-optics with 'executionRole' instead"  #-}

-- | The Jupyter server's app settings.
--
-- /Note:/ Consider using 'jupyterServerAppSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usJupyterServerAppSettings :: Lens.Lens' UserSettings (Core.Maybe Types.JupyterServerAppSettings)
usJupyterServerAppSettings = Lens.field @"jupyterServerAppSettings"
{-# INLINEABLE usJupyterServerAppSettings #-}
{-# DEPRECATED jupyterServerAppSettings "Use generic-lens or generic-optics with 'jupyterServerAppSettings' instead"  #-}

-- | The kernel gateway app settings.
--
-- /Note:/ Consider using 'kernelGatewayAppSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usKernelGatewayAppSettings :: Lens.Lens' UserSettings (Core.Maybe Types.KernelGatewayAppSettings)
usKernelGatewayAppSettings = Lens.field @"kernelGatewayAppSettings"
{-# INLINEABLE usKernelGatewayAppSettings #-}
{-# DEPRECATED kernelGatewayAppSettings "Use generic-lens or generic-optics with 'kernelGatewayAppSettings' instead"  #-}

-- | The security groups for the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set to @PublicInternetOnly@ .
-- Required when the @CreateDomain.AppNetworkAccessType@ parameter is set to @VpcOnly@ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSecurityGroups :: Lens.Lens' UserSettings (Core.Maybe [Types.SecurityGroupId])
usSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE usSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The sharing settings.
--
-- /Note:/ Consider using 'sharingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSharingSettings :: Lens.Lens' UserSettings (Core.Maybe Types.SharingSettings)
usSharingSettings = Lens.field @"sharingSettings"
{-# INLINEABLE usSharingSettings #-}
{-# DEPRECATED sharingSettings "Use generic-lens or generic-optics with 'sharingSettings' instead"  #-}

-- | The TensorBoard app settings.
--
-- /Note:/ Consider using 'tensorBoardAppSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTensorBoardAppSettings :: Lens.Lens' UserSettings (Core.Maybe Types.TensorBoardAppSettings)
usTensorBoardAppSettings = Lens.field @"tensorBoardAppSettings"
{-# INLINEABLE usTensorBoardAppSettings #-}
{-# DEPRECATED tensorBoardAppSettings "Use generic-lens or generic-optics with 'tensorBoardAppSettings' instead"  #-}

instance Core.FromJSON UserSettings where
        toJSON UserSettings{..}
          = Core.object
              (Core.catMaybes
                 [("ExecutionRole" Core..=) Core.<$> executionRole,
                  ("JupyterServerAppSettings" Core..=) Core.<$>
                    jupyterServerAppSettings,
                  ("KernelGatewayAppSettings" Core..=) Core.<$>
                    kernelGatewayAppSettings,
                  ("SecurityGroups" Core..=) Core.<$> securityGroups,
                  ("SharingSettings" Core..=) Core.<$> sharingSettings,
                  ("TensorBoardAppSettings" Core..=) Core.<$>
                    tensorBoardAppSettings])

instance Core.FromJSON UserSettings where
        parseJSON
          = Core.withObject "UserSettings" Core.$
              \ x ->
                UserSettings' Core.<$>
                  (x Core..:? "ExecutionRole") Core.<*>
                    x Core..:? "JupyterServerAppSettings"
                    Core.<*> x Core..:? "KernelGatewayAppSettings"
                    Core.<*> x Core..:? "SecurityGroups"
                    Core.<*> x Core..:? "SharingSettings"
                    Core.<*> x Core..:? "TensorBoardAppSettings"
