-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
  ( FSxWindowsFileServerVolumeConfiguration (..),

    -- * Smart constructor
    mkFSxWindowsFileServerVolumeConfiguration,

    -- * Lenses
    fswfsvcFileSystemId,
    fswfsvcRootDirectory,
    fswfsvcAuthorizationConfig,
  )
where

import Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This parameter is specified when you are using <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html Amazon FSx for Windows File Server> file system for task storage.
--
-- For more information and the input format, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFSxWindowsFileServerVolumeConfiguration' smart constructor.
data FSxWindowsFileServerVolumeConfiguration = FSxWindowsFileServerVolumeConfiguration'
  { fileSystemId ::
      Lude.Text,
    rootDirectory ::
      Lude.Text,
    authorizationConfig ::
      FSxWindowsFileServerAuthorizationConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FSxWindowsFileServerVolumeConfiguration' with the minimum fields required to make a request.
--
-- * 'authorizationConfig' - The authorization configuration details for the Amazon FSx for Windows File Server file system.
-- * 'fileSystemId' - The Amazon FSx for Windows File Server file system ID to use.
-- * 'rootDirectory' - The directory within the Amazon FSx for Windows File Server file system to mount as the root directory inside the host.
mkFSxWindowsFileServerVolumeConfiguration ::
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'rootDirectory'
  Lude.Text ->
  -- | 'authorizationConfig'
  FSxWindowsFileServerAuthorizationConfig ->
  FSxWindowsFileServerVolumeConfiguration
mkFSxWindowsFileServerVolumeConfiguration
  pFileSystemId_
  pRootDirectory_
  pAuthorizationConfig_ =
    FSxWindowsFileServerVolumeConfiguration'
      { fileSystemId =
          pFileSystemId_,
        rootDirectory = pRootDirectory_,
        authorizationConfig = pAuthorizationConfig_
      }

-- | The Amazon FSx for Windows File Server file system ID to use.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsvcFileSystemId :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Lude.Text
fswfsvcFileSystemId = Lens.lens (fileSystemId :: FSxWindowsFileServerVolumeConfiguration -> Lude.Text) (\s a -> s {fileSystemId = a} :: FSxWindowsFileServerVolumeConfiguration)
{-# DEPRECATED fswfsvcFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The directory within the Amazon FSx for Windows File Server file system to mount as the root directory inside the host.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsvcRootDirectory :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Lude.Text
fswfsvcRootDirectory = Lens.lens (rootDirectory :: FSxWindowsFileServerVolumeConfiguration -> Lude.Text) (\s a -> s {rootDirectory = a} :: FSxWindowsFileServerVolumeConfiguration)
{-# DEPRECATED fswfsvcRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | The authorization configuration details for the Amazon FSx for Windows File Server file system.
--
-- /Note:/ Consider using 'authorizationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsvcAuthorizationConfig :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration FSxWindowsFileServerAuthorizationConfig
fswfsvcAuthorizationConfig = Lens.lens (authorizationConfig :: FSxWindowsFileServerVolumeConfiguration -> FSxWindowsFileServerAuthorizationConfig) (\s a -> s {authorizationConfig = a} :: FSxWindowsFileServerVolumeConfiguration)
{-# DEPRECATED fswfsvcAuthorizationConfig "Use generic-lens or generic-optics with 'authorizationConfig' instead." #-}

instance Lude.FromJSON FSxWindowsFileServerVolumeConfiguration where
  parseJSON =
    Lude.withObject
      "FSxWindowsFileServerVolumeConfiguration"
      ( \x ->
          FSxWindowsFileServerVolumeConfiguration'
            Lude.<$> (x Lude..: "fileSystemId")
            Lude.<*> (x Lude..: "rootDirectory")
            Lude.<*> (x Lude..: "authorizationConfig")
      )

instance Lude.ToJSON FSxWindowsFileServerVolumeConfiguration where
  toJSON FSxWindowsFileServerVolumeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("fileSystemId" Lude..= fileSystemId),
            Lude.Just ("rootDirectory" Lude..= rootDirectory),
            Lude.Just ("authorizationConfig" Lude..= authorizationConfig)
          ]
      )
