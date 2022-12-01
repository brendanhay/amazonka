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
-- Module      : Amazonka.CodeCommit.Types.FileMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.FileMetadata where

import Amazonka.CodeCommit.Types.FileModeTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A file to be added, updated, or deleted as part of a commit.
--
-- /See:/ 'newFileMetadata' smart constructor.
data FileMetadata = FileMetadata'
  { -- | The extrapolated file mode permissions for the file. Valid values
    -- include EXECUTABLE and NORMAL.
    fileMode :: Prelude.Maybe FileModeTypeEnum,
    -- | The full path to the file to be added or updated, including the name of
    -- the file.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The blob ID that contains the file information.
    blobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileMode', 'fileMetadata_fileMode' - The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
--
-- 'absolutePath', 'fileMetadata_absolutePath' - The full path to the file to be added or updated, including the name of
-- the file.
--
-- 'blobId', 'fileMetadata_blobId' - The blob ID that contains the file information.
newFileMetadata ::
  FileMetadata
newFileMetadata =
  FileMetadata'
    { fileMode = Prelude.Nothing,
      absolutePath = Prelude.Nothing,
      blobId = Prelude.Nothing
    }

-- | The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
fileMetadata_fileMode :: Lens.Lens' FileMetadata (Prelude.Maybe FileModeTypeEnum)
fileMetadata_fileMode = Lens.lens (\FileMetadata' {fileMode} -> fileMode) (\s@FileMetadata' {} a -> s {fileMode = a} :: FileMetadata)

-- | The full path to the file to be added or updated, including the name of
-- the file.
fileMetadata_absolutePath :: Lens.Lens' FileMetadata (Prelude.Maybe Prelude.Text)
fileMetadata_absolutePath = Lens.lens (\FileMetadata' {absolutePath} -> absolutePath) (\s@FileMetadata' {} a -> s {absolutePath = a} :: FileMetadata)

-- | The blob ID that contains the file information.
fileMetadata_blobId :: Lens.Lens' FileMetadata (Prelude.Maybe Prelude.Text)
fileMetadata_blobId = Lens.lens (\FileMetadata' {blobId} -> blobId) (\s@FileMetadata' {} a -> s {blobId = a} :: FileMetadata)

instance Core.FromJSON FileMetadata where
  parseJSON =
    Core.withObject
      "FileMetadata"
      ( \x ->
          FileMetadata'
            Prelude.<$> (x Core..:? "fileMode")
            Prelude.<*> (x Core..:? "absolutePath")
            Prelude.<*> (x Core..:? "blobId")
      )

instance Prelude.Hashable FileMetadata where
  hashWithSalt _salt FileMetadata' {..} =
    _salt `Prelude.hashWithSalt` fileMode
      `Prelude.hashWithSalt` absolutePath
      `Prelude.hashWithSalt` blobId

instance Prelude.NFData FileMetadata where
  rnf FileMetadata' {..} =
    Prelude.rnf fileMode
      `Prelude.seq` Prelude.rnf absolutePath
      `Prelude.seq` Prelude.rnf blobId
