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
    fsdsFileSystemId,
    fsdsFileSystemAccessMode,
    fsdsFileSystemType,
    fsdsDirectoryPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DirectoryPath as Types
import qualified Network.AWS.SageMaker.Types.FileSystemAccessMode as Types
import qualified Network.AWS.SageMaker.Types.FileSystemId as Types
import qualified Network.AWS.SageMaker.Types.FileSystemType as Types

-- | Specifies a file system data source for a channel.
--
-- /See:/ 'mkFileSystemDataSource' smart constructor.
data FileSystemDataSource = FileSystemDataSource'
  { -- | The file system id.
    fileSystemId :: Types.FileSystemId,
    -- | The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
    fileSystemAccessMode :: Types.FileSystemAccessMode,
    -- | The file system type.
    fileSystemType :: Types.FileSystemType,
    -- | The full path to the directory to associate with the channel.
    directoryPath :: Types.DirectoryPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileSystemDataSource' value with any optional fields omitted.
mkFileSystemDataSource ::
  -- | 'fileSystemId'
  Types.FileSystemId ->
  -- | 'fileSystemAccessMode'
  Types.FileSystemAccessMode ->
  -- | 'fileSystemType'
  Types.FileSystemType ->
  -- | 'directoryPath'
  Types.DirectoryPath ->
  FileSystemDataSource
mkFileSystemDataSource
  fileSystemId
  fileSystemAccessMode
  fileSystemType
  directoryPath =
    FileSystemDataSource'
      { fileSystemId,
        fileSystemAccessMode,
        fileSystemType,
        directoryPath
      }

-- | The file system id.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsFileSystemId :: Lens.Lens' FileSystemDataSource Types.FileSystemId
fsdsFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED fsdsFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
--
-- /Note:/ Consider using 'fileSystemAccessMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsFileSystemAccessMode :: Lens.Lens' FileSystemDataSource Types.FileSystemAccessMode
fsdsFileSystemAccessMode = Lens.field @"fileSystemAccessMode"
{-# DEPRECATED fsdsFileSystemAccessMode "Use generic-lens or generic-optics with 'fileSystemAccessMode' instead." #-}

-- | The file system type.
--
-- /Note:/ Consider using 'fileSystemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsFileSystemType :: Lens.Lens' FileSystemDataSource Types.FileSystemType
fsdsFileSystemType = Lens.field @"fileSystemType"
{-# DEPRECATED fsdsFileSystemType "Use generic-lens or generic-optics with 'fileSystemType' instead." #-}

-- | The full path to the directory to associate with the channel.
--
-- /Note:/ Consider using 'directoryPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdsDirectoryPath :: Lens.Lens' FileSystemDataSource Types.DirectoryPath
fsdsDirectoryPath = Lens.field @"directoryPath"
{-# DEPRECATED fsdsDirectoryPath "Use generic-lens or generic-optics with 'directoryPath' instead." #-}

instance Core.FromJSON FileSystemDataSource where
  toJSON FileSystemDataSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FileSystemId" Core..= fileSystemId),
            Core.Just ("FileSystemAccessMode" Core..= fileSystemAccessMode),
            Core.Just ("FileSystemType" Core..= fileSystemType),
            Core.Just ("DirectoryPath" Core..= directoryPath)
          ]
      )

instance Core.FromJSON FileSystemDataSource where
  parseJSON =
    Core.withObject "FileSystemDataSource" Core.$
      \x ->
        FileSystemDataSource'
          Core.<$> (x Core..: "FileSystemId")
          Core.<*> (x Core..: "FileSystemAccessMode")
          Core.<*> (x Core..: "FileSystemType")
          Core.<*> (x Core..: "DirectoryPath")
