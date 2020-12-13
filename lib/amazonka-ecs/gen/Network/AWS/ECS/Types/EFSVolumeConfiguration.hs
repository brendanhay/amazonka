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
    efsvcRootDirectory,
    efsvcTransitEncryption,
    efsvcFileSystemId,
    efsvcAuthorizationConfig,
    efsvcTransitEncryptionPort,
  )
where

import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSTransitEncryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html Amazon EFS Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkEFSVolumeConfiguration' smart constructor.
data EFSVolumeConfiguration = EFSVolumeConfiguration'
  { -- | The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter.
    --
    -- /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
    rootDirectory :: Lude.Maybe Lude.Text,
    -- | Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
    transitEncryption :: Lude.Maybe EFSTransitEncryption,
    -- | The Amazon EFS file system ID to use.
    fileSystemId :: Lude.Text,
    -- | The authorization configuration details for the Amazon EFS file system.
    authorizationConfig :: Lude.Maybe EFSAuthorizationConfig,
    -- | The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
    transitEncryptionPort :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EFSVolumeConfiguration' with the minimum fields required to make a request.
--
-- * 'rootDirectory' - The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter.
--
-- /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
-- * 'transitEncryption' - Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
-- * 'fileSystemId' - The Amazon EFS file system ID to use.
-- * 'authorizationConfig' - The authorization configuration details for the Amazon EFS file system.
-- * 'transitEncryptionPort' - The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
mkEFSVolumeConfiguration ::
  -- | 'fileSystemId'
  Lude.Text ->
  EFSVolumeConfiguration
mkEFSVolumeConfiguration pFileSystemId_ =
  EFSVolumeConfiguration'
    { rootDirectory = Lude.Nothing,
      transitEncryption = Lude.Nothing,
      fileSystemId = pFileSystemId_,
      authorizationConfig = Lude.Nothing,
      transitEncryptionPort = Lude.Nothing
    }

-- | The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter.
--
-- /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcRootDirectory :: Lens.Lens' EFSVolumeConfiguration (Lude.Maybe Lude.Text)
efsvcRootDirectory = Lens.lens (rootDirectory :: EFSVolumeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {rootDirectory = a} :: EFSVolumeConfiguration)
{-# DEPRECATED efsvcRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
--
-- /Note:/ Consider using 'transitEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcTransitEncryption :: Lens.Lens' EFSVolumeConfiguration (Lude.Maybe EFSTransitEncryption)
efsvcTransitEncryption = Lens.lens (transitEncryption :: EFSVolumeConfiguration -> Lude.Maybe EFSTransitEncryption) (\s a -> s {transitEncryption = a} :: EFSVolumeConfiguration)
{-# DEPRECATED efsvcTransitEncryption "Use generic-lens or generic-optics with 'transitEncryption' instead." #-}

-- | The Amazon EFS file system ID to use.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcFileSystemId :: Lens.Lens' EFSVolumeConfiguration Lude.Text
efsvcFileSystemId = Lens.lens (fileSystemId :: EFSVolumeConfiguration -> Lude.Text) (\s a -> s {fileSystemId = a} :: EFSVolumeConfiguration)
{-# DEPRECATED efsvcFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The authorization configuration details for the Amazon EFS file system.
--
-- /Note:/ Consider using 'authorizationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcAuthorizationConfig :: Lens.Lens' EFSVolumeConfiguration (Lude.Maybe EFSAuthorizationConfig)
efsvcAuthorizationConfig = Lens.lens (authorizationConfig :: EFSVolumeConfiguration -> Lude.Maybe EFSAuthorizationConfig) (\s a -> s {authorizationConfig = a} :: EFSVolumeConfiguration)
{-# DEPRECATED efsvcAuthorizationConfig "Use generic-lens or generic-optics with 'authorizationConfig' instead." #-}

-- | The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
--
-- /Note:/ Consider using 'transitEncryptionPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsvcTransitEncryptionPort :: Lens.Lens' EFSVolumeConfiguration (Lude.Maybe Lude.Int)
efsvcTransitEncryptionPort = Lens.lens (transitEncryptionPort :: EFSVolumeConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {transitEncryptionPort = a} :: EFSVolumeConfiguration)
{-# DEPRECATED efsvcTransitEncryptionPort "Use generic-lens or generic-optics with 'transitEncryptionPort' instead." #-}

instance Lude.FromJSON EFSVolumeConfiguration where
  parseJSON =
    Lude.withObject
      "EFSVolumeConfiguration"
      ( \x ->
          EFSVolumeConfiguration'
            Lude.<$> (x Lude..:? "rootDirectory")
            Lude.<*> (x Lude..:? "transitEncryption")
            Lude.<*> (x Lude..: "fileSystemId")
            Lude.<*> (x Lude..:? "authorizationConfig")
            Lude.<*> (x Lude..:? "transitEncryptionPort")
      )

instance Lude.ToJSON EFSVolumeConfiguration where
  toJSON EFSVolumeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rootDirectory" Lude..=) Lude.<$> rootDirectory,
            ("transitEncryption" Lude..=) Lude.<$> transitEncryption,
            Lude.Just ("fileSystemId" Lude..= fileSystemId),
            ("authorizationConfig" Lude..=) Lude.<$> authorizationConfig,
            ("transitEncryptionPort" Lude..=) Lude.<$> transitEncryptionPort
          ]
      )
