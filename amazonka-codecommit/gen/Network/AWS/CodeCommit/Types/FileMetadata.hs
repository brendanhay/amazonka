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
-- Module      : Network.AWS.CodeCommit.Types.FileMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileMetadata where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A file to be added, updated, or deleted as part of a commit.
--
-- /See:/ 'newFileMetadata' smart constructor.
data FileMetadata = FileMetadata'
  { -- | The full path to the file to be added or updated, including the name of
    -- the file.
    absolutePath :: Core.Maybe Core.Text,
    -- | The blob ID that contains the file information.
    blobId :: Core.Maybe Core.Text,
    -- | The extrapolated file mode permissions for the file. Valid values
    -- include EXECUTABLE and NORMAL.
    fileMode :: Core.Maybe FileModeTypeEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FileMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absolutePath', 'fileMetadata_absolutePath' - The full path to the file to be added or updated, including the name of
-- the file.
--
-- 'blobId', 'fileMetadata_blobId' - The blob ID that contains the file information.
--
-- 'fileMode', 'fileMetadata_fileMode' - The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
newFileMetadata ::
  FileMetadata
newFileMetadata =
  FileMetadata'
    { absolutePath = Core.Nothing,
      blobId = Core.Nothing,
      fileMode = Core.Nothing
    }

-- | The full path to the file to be added or updated, including the name of
-- the file.
fileMetadata_absolutePath :: Lens.Lens' FileMetadata (Core.Maybe Core.Text)
fileMetadata_absolutePath = Lens.lens (\FileMetadata' {absolutePath} -> absolutePath) (\s@FileMetadata' {} a -> s {absolutePath = a} :: FileMetadata)

-- | The blob ID that contains the file information.
fileMetadata_blobId :: Lens.Lens' FileMetadata (Core.Maybe Core.Text)
fileMetadata_blobId = Lens.lens (\FileMetadata' {blobId} -> blobId) (\s@FileMetadata' {} a -> s {blobId = a} :: FileMetadata)

-- | The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
fileMetadata_fileMode :: Lens.Lens' FileMetadata (Core.Maybe FileModeTypeEnum)
fileMetadata_fileMode = Lens.lens (\FileMetadata' {fileMode} -> fileMode) (\s@FileMetadata' {} a -> s {fileMode = a} :: FileMetadata)

instance Core.FromJSON FileMetadata where
  parseJSON =
    Core.withObject
      "FileMetadata"
      ( \x ->
          FileMetadata'
            Core.<$> (x Core..:? "absolutePath")
            Core.<*> (x Core..:? "blobId")
            Core.<*> (x Core..:? "fileMode")
      )

instance Core.Hashable FileMetadata

instance Core.NFData FileMetadata
