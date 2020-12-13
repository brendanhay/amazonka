{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemConfig
  ( FileSystemConfig (..),

    -- * Smart constructor
    mkFileSystemConfig,

    -- * Lenses
    fscDefaultGid,
    fscMountPath,
    fscDefaultUid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
--
-- /See:/ 'mkFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { -- | The default POSIX group ID (GID). If not specified, defaults to @100@ .
    defaultGid :: Lude.Maybe Lude.Natural,
    -- | The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
    mountPath :: Lude.Maybe Lude.Text,
    -- | The default POSIX user ID (UID). If not specified, defaults to @1000@ .
    defaultUid :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSystemConfig' with the minimum fields required to make a request.
--
-- * 'defaultGid' - The default POSIX group ID (GID). If not specified, defaults to @100@ .
-- * 'mountPath' - The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
-- * 'defaultUid' - The default POSIX user ID (UID). If not specified, defaults to @1000@ .
mkFileSystemConfig ::
  FileSystemConfig
mkFileSystemConfig =
  FileSystemConfig'
    { defaultGid = Lude.Nothing,
      mountPath = Lude.Nothing,
      defaultUid = Lude.Nothing
    }

-- | The default POSIX group ID (GID). If not specified, defaults to @100@ .
--
-- /Note:/ Consider using 'defaultGid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscDefaultGid :: Lens.Lens' FileSystemConfig (Lude.Maybe Lude.Natural)
fscDefaultGid = Lens.lens (defaultGid :: FileSystemConfig -> Lude.Maybe Lude.Natural) (\s a -> s {defaultGid = a} :: FileSystemConfig)
{-# DEPRECATED fscDefaultGid "Use generic-lens or generic-optics with 'defaultGid' instead." #-}

-- | The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
--
-- /Note:/ Consider using 'mountPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscMountPath :: Lens.Lens' FileSystemConfig (Lude.Maybe Lude.Text)
fscMountPath = Lens.lens (mountPath :: FileSystemConfig -> Lude.Maybe Lude.Text) (\s a -> s {mountPath = a} :: FileSystemConfig)
{-# DEPRECATED fscMountPath "Use generic-lens or generic-optics with 'mountPath' instead." #-}

-- | The default POSIX user ID (UID). If not specified, defaults to @1000@ .
--
-- /Note:/ Consider using 'defaultUid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscDefaultUid :: Lens.Lens' FileSystemConfig (Lude.Maybe Lude.Natural)
fscDefaultUid = Lens.lens (defaultUid :: FileSystemConfig -> Lude.Maybe Lude.Natural) (\s a -> s {defaultUid = a} :: FileSystemConfig)
{-# DEPRECATED fscDefaultUid "Use generic-lens or generic-optics with 'defaultUid' instead." #-}

instance Lude.FromJSON FileSystemConfig where
  parseJSON =
    Lude.withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            Lude.<$> (x Lude..:? "DefaultGid")
            Lude.<*> (x Lude..:? "MountPath")
            Lude.<*> (x Lude..:? "DefaultUid")
      )

instance Lude.ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultGid" Lude..=) Lude.<$> defaultGid,
            ("MountPath" Lude..=) Lude.<$> mountPath,
            ("DefaultUid" Lude..=) Lude.<$> defaultUid
          ]
      )
