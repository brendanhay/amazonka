{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemDataSource
  ( FileSystemDataSource (..),

    -- * Smart constructor
    mkFileSystemDataSource,

    -- * Lenses
    fsdsFileSystemAccessMode,
    fsdsFileSystemId,
    fsdsDirectoryPath,
    fsdsFileSystemType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.FileSystemAccessMode
import Network.AWS.SageMaker.Types.FileSystemType

-- | Specifies a file system data source for a channel.
--
-- /See:/ 'mkFileSystemDataSource' smart constructor.
data FileSystemDataSource = FileSystemDataSource'
  { -- | The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
    fileSystemAccessMode :: FileSystemAccessMode,
    -- | The file system id.
    fileSystemId :: Lude.Text,
    -- | The full path to the directory to associate with the channel.
    directoryPath :: Lude.Text,
    -- | The file system type.
    fileSystemType :: FileSystemType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSystemDataSource' with the minimum fields required to make a request.
--
-- * 'fileSystemAccessMode' - The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
-- * 'fileSystemId' - The file system id.
-- * 'directoryPath' - The full path to the directory to associate with the channel.
-- * 'fileSystemType' - The file system type.
mkFileSystemDataSource ::
  -- | 'fileSystemAccessMode'
  FileSystemAccessMode ->
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'directoryPath'
  Lude.Text ->
  -- | 'fileSystemType'
  FileSystemType ->
  FileSystemDataSource
mkFileSystemDataSource
  pFileSystemAccessMode_
  pFileSystemId_
  pDirectoryPath_
  pFileSystemType_ =
    FileSystemDataSource'
      { fileSystemAccessMode =
          pFileSystemAccessMode_,
        fileSystemId = pFileSystemId_,
        directoryPath = pDirectoryPath_,
        fileSystemType = pFileSystemType_
      }

-- | The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
--
-- /Note:/ Consider using 'fileSystemAccessMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsFileSystemAccessMode :: Lens.Lens' FileSystemDataSource FileSystemAccessMode
fsdsFileSystemAccessMode = Lens.lens (fileSystemAccessMode :: FileSystemDataSource -> FileSystemAccessMode) (\s a -> s {fileSystemAccessMode = a} :: FileSystemDataSource)
{-# DEPRECATED fsdsFileSystemAccessMode "Use generic-lens or generic-optics with 'fileSystemAccessMode' instead." #-}

-- | The file system id.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsFileSystemId :: Lens.Lens' FileSystemDataSource Lude.Text
fsdsFileSystemId = Lens.lens (fileSystemId :: FileSystemDataSource -> Lude.Text) (\s a -> s {fileSystemId = a} :: FileSystemDataSource)
{-# DEPRECATED fsdsFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The full path to the directory to associate with the channel.
--
-- /Note:/ Consider using 'directoryPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsDirectoryPath :: Lens.Lens' FileSystemDataSource Lude.Text
fsdsDirectoryPath = Lens.lens (directoryPath :: FileSystemDataSource -> Lude.Text) (\s a -> s {directoryPath = a} :: FileSystemDataSource)
{-# DEPRECATED fsdsDirectoryPath "Use generic-lens or generic-optics with 'directoryPath' instead." #-}

-- | The file system type.
--
-- /Note:/ Consider using 'fileSystemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsFileSystemType :: Lens.Lens' FileSystemDataSource FileSystemType
fsdsFileSystemType = Lens.lens (fileSystemType :: FileSystemDataSource -> FileSystemType) (\s a -> s {fileSystemType = a} :: FileSystemDataSource)
{-# DEPRECATED fsdsFileSystemType "Use generic-lens or generic-optics with 'fileSystemType' instead." #-}

instance Lude.FromJSON FileSystemDataSource where
  parseJSON =
    Lude.withObject
      "FileSystemDataSource"
      ( \x ->
          FileSystemDataSource'
            Lude.<$> (x Lude..: "FileSystemAccessMode")
            Lude.<*> (x Lude..: "FileSystemId")
            Lude.<*> (x Lude..: "DirectoryPath")
            Lude.<*> (x Lude..: "FileSystemType")
      )

instance Lude.ToJSON FileSystemDataSource where
  toJSON FileSystemDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FileSystemAccessMode" Lude..= fileSystemAccessMode),
            Lude.Just ("FileSystemId" Lude..= fileSystemId),
            Lude.Just ("DirectoryPath" Lude..= directoryPath),
            Lude.Just ("FileSystemType" Lude..= fileSystemType)
          ]
      )
