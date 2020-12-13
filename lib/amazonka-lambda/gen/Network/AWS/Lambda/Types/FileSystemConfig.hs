{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FileSystemConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FileSystemConfig
  ( FileSystemConfig (..),

    -- * Smart constructor
    mkFileSystemConfig,

    -- * Lenses
    fscARN,
    fscLocalMountPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the connection between a Lambda function and an Amazon EFS file system.
--
-- /See:/ 'mkFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { -- | The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
    arn :: Lude.Text,
    -- | The path where the function can access the file system, starting with @/mnt/@ .
    localMountPath :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSystemConfig' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
-- * 'localMountPath' - The path where the function can access the file system, starting with @/mnt/@ .
mkFileSystemConfig ::
  -- | 'arn'
  Lude.Text ->
  -- | 'localMountPath'
  Lude.Text ->
  FileSystemConfig
mkFileSystemConfig pARN_ pLocalMountPath_ =
  FileSystemConfig' {arn = pARN_, localMountPath = pLocalMountPath_}

-- | The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscARN :: Lens.Lens' FileSystemConfig Lude.Text
fscARN = Lens.lens (arn :: FileSystemConfig -> Lude.Text) (\s a -> s {arn = a} :: FileSystemConfig)
{-# DEPRECATED fscARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The path where the function can access the file system, starting with @/mnt/@ .
--
-- /Note:/ Consider using 'localMountPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscLocalMountPath :: Lens.Lens' FileSystemConfig Lude.Text
fscLocalMountPath = Lens.lens (localMountPath :: FileSystemConfig -> Lude.Text) (\s a -> s {localMountPath = a} :: FileSystemConfig)
{-# DEPRECATED fscLocalMountPath "Use generic-lens or generic-optics with 'localMountPath' instead." #-}

instance Lude.FromJSON FileSystemConfig where
  parseJSON =
    Lude.withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            Lude.<$> (x Lude..: "Arn") Lude.<*> (x Lude..: "LocalMountPath")
      )

instance Lude.ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Arn" Lude..= arn),
            Lude.Just ("LocalMountPath" Lude..= localMountPath)
          ]
      )
