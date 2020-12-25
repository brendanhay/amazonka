{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSVolumeConfiguration
  ( EFSVolumeConfiguration (..),

    -- * Smart constructor
    mkEFSVolumeConfiguration,

    -- * Lenses
    efsvcFileSystemId,
    efsvcAuthorizationConfig,
    efsvcRootDirectory,
    efsvcTransitEncryption,
    efsvcTransitEncryptionPort,
  )
where

import qualified Network.AWS.ECS.Types.EFSAuthorizationConfig as Types
import qualified Network.AWS.ECS.Types.EFSTransitEncryption as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html Amazon EFS Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkEFSVolumeConfiguration' smart constructor.
data EFSVolumeConfiguration = EFSVolumeConfiguration'
  { -- | The Amazon EFS file system ID to use.
    fileSystemId :: Types.String,
    -- | The authorization configuration details for the Amazon EFS file system.
    authorizationConfig :: Core.Maybe Types.EFSAuthorizationConfig,
    -- | The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter.
    --
    -- /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
    rootDirectory :: Core.Maybe Types.String,
    -- | Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
    transitEncryption :: Core.Maybe Types.EFSTransitEncryption,
    -- | The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
    transitEncryptionPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EFSVolumeConfiguration' value with any optional fields omitted.
mkEFSVolumeConfiguration ::
  -- | 'fileSystemId'
  Types.String ->
  EFSVolumeConfiguration
mkEFSVolumeConfiguration fileSystemId =
  EFSVolumeConfiguration'
    { fileSystemId,
      authorizationConfig = Core.Nothing,
      rootDirectory = Core.Nothing,
      transitEncryption = Core.Nothing,
      transitEncryptionPort = Core.Nothing
    }

-- | The Amazon EFS file system ID to use.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcFileSystemId :: Lens.Lens' EFSVolumeConfiguration Types.String
efsvcFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED efsvcFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The authorization configuration details for the Amazon EFS file system.
--
-- /Note:/ Consider using 'authorizationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcAuthorizationConfig :: Lens.Lens' EFSVolumeConfiguration (Core.Maybe Types.EFSAuthorizationConfig)
efsvcAuthorizationConfig = Lens.field @"authorizationConfig"
{-# DEPRECATED efsvcAuthorizationConfig "Use generic-lens or generic-optics with 'authorizationConfig' instead." #-}

-- | The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter.
--
-- /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcRootDirectory :: Lens.Lens' EFSVolumeConfiguration (Core.Maybe Types.String)
efsvcRootDirectory = Lens.field @"rootDirectory"
{-# DEPRECATED efsvcRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
--
-- /Note:/ Consider using 'transitEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcTransitEncryption :: Lens.Lens' EFSVolumeConfiguration (Core.Maybe Types.EFSTransitEncryption)
efsvcTransitEncryption = Lens.field @"transitEncryption"
{-# DEPRECATED efsvcTransitEncryption "Use generic-lens or generic-optics with 'transitEncryption' instead." #-}

-- | The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
--
-- /Note:/ Consider using 'transitEncryptionPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcTransitEncryptionPort :: Lens.Lens' EFSVolumeConfiguration (Core.Maybe Core.Int)
efsvcTransitEncryptionPort = Lens.field @"transitEncryptionPort"
{-# DEPRECATED efsvcTransitEncryptionPort "Use generic-lens or generic-optics with 'transitEncryptionPort' instead." #-}

instance Core.FromJSON EFSVolumeConfiguration where
  toJSON EFSVolumeConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("fileSystemId" Core..= fileSystemId),
            ("authorizationConfig" Core..=) Core.<$> authorizationConfig,
            ("rootDirectory" Core..=) Core.<$> rootDirectory,
            ("transitEncryption" Core..=) Core.<$> transitEncryption,
            ("transitEncryptionPort" Core..=) Core.<$> transitEncryptionPort
          ]
      )

instance Core.FromJSON EFSVolumeConfiguration where
  parseJSON =
    Core.withObject "EFSVolumeConfiguration" Core.$
      \x ->
        EFSVolumeConfiguration'
          Core.<$> (x Core..: "fileSystemId")
          Core.<*> (x Core..:? "authorizationConfig")
          Core.<*> (x Core..:? "rootDirectory")
          Core.<*> (x Core..:? "transitEncryption")
          Core.<*> (x Core..:? "transitEncryptionPort")
