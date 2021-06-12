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
-- Module      : Network.AWS.SageMaker.Types.FileSystemDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemDataSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.FileSystemAccessMode
import Network.AWS.SageMaker.Types.FileSystemType

-- | Specifies a file system data source for a channel.
--
-- /See:/ 'newFileSystemDataSource' smart constructor.
data FileSystemDataSource = FileSystemDataSource'
  { -- | The file system id.
    fileSystemId :: Core.Text,
    -- | The access mode of the mount of the directory associated with the
    -- channel. A directory can be mounted either in @ro@ (read-only) or @rw@
    -- (read-write) mode.
    fileSystemAccessMode :: FileSystemAccessMode,
    -- | The file system type.
    fileSystemType :: FileSystemType,
    -- | The full path to the directory to associate with the channel.
    directoryPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FileSystemDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'fileSystemDataSource_fileSystemId' - The file system id.
--
-- 'fileSystemAccessMode', 'fileSystemDataSource_fileSystemAccessMode' - The access mode of the mount of the directory associated with the
-- channel. A directory can be mounted either in @ro@ (read-only) or @rw@
-- (read-write) mode.
--
-- 'fileSystemType', 'fileSystemDataSource_fileSystemType' - The file system type.
--
-- 'directoryPath', 'fileSystemDataSource_directoryPath' - The full path to the directory to associate with the channel.
newFileSystemDataSource ::
  -- | 'fileSystemId'
  Core.Text ->
  -- | 'fileSystemAccessMode'
  FileSystemAccessMode ->
  -- | 'fileSystemType'
  FileSystemType ->
  -- | 'directoryPath'
  Core.Text ->
  FileSystemDataSource
newFileSystemDataSource
  pFileSystemId_
  pFileSystemAccessMode_
  pFileSystemType_
  pDirectoryPath_ =
    FileSystemDataSource'
      { fileSystemId =
          pFileSystemId_,
        fileSystemAccessMode = pFileSystemAccessMode_,
        fileSystemType = pFileSystemType_,
        directoryPath = pDirectoryPath_
      }

-- | The file system id.
fileSystemDataSource_fileSystemId :: Lens.Lens' FileSystemDataSource Core.Text
fileSystemDataSource_fileSystemId = Lens.lens (\FileSystemDataSource' {fileSystemId} -> fileSystemId) (\s@FileSystemDataSource' {} a -> s {fileSystemId = a} :: FileSystemDataSource)

-- | The access mode of the mount of the directory associated with the
-- channel. A directory can be mounted either in @ro@ (read-only) or @rw@
-- (read-write) mode.
fileSystemDataSource_fileSystemAccessMode :: Lens.Lens' FileSystemDataSource FileSystemAccessMode
fileSystemDataSource_fileSystemAccessMode = Lens.lens (\FileSystemDataSource' {fileSystemAccessMode} -> fileSystemAccessMode) (\s@FileSystemDataSource' {} a -> s {fileSystemAccessMode = a} :: FileSystemDataSource)

-- | The file system type.
fileSystemDataSource_fileSystemType :: Lens.Lens' FileSystemDataSource FileSystemType
fileSystemDataSource_fileSystemType = Lens.lens (\FileSystemDataSource' {fileSystemType} -> fileSystemType) (\s@FileSystemDataSource' {} a -> s {fileSystemType = a} :: FileSystemDataSource)

-- | The full path to the directory to associate with the channel.
fileSystemDataSource_directoryPath :: Lens.Lens' FileSystemDataSource Core.Text
fileSystemDataSource_directoryPath = Lens.lens (\FileSystemDataSource' {directoryPath} -> directoryPath) (\s@FileSystemDataSource' {} a -> s {directoryPath = a} :: FileSystemDataSource)

instance Core.FromJSON FileSystemDataSource where
  parseJSON =
    Core.withObject
      "FileSystemDataSource"
      ( \x ->
          FileSystemDataSource'
            Core.<$> (x Core..: "FileSystemId")
            Core.<*> (x Core..: "FileSystemAccessMode")
            Core.<*> (x Core..: "FileSystemType")
            Core.<*> (x Core..: "DirectoryPath")
      )

instance Core.Hashable FileSystemDataSource

instance Core.NFData FileSystemDataSource

instance Core.ToJSON FileSystemDataSource where
  toJSON FileSystemDataSource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FileSystemId" Core..= fileSystemId),
            Core.Just
              ( "FileSystemAccessMode"
                  Core..= fileSystemAccessMode
              ),
            Core.Just ("FileSystemType" Core..= fileSystemType),
            Core.Just ("DirectoryPath" Core..= directoryPath)
          ]
      )
