{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.Types.File
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.File where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a file in a repository.
--
-- /See:/ 'newFile' smart constructor.
data File = File'
  { -- | The fully qualified path to the file in the repository.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The relative path of the file from the folder where the query
    -- originated.
    relativePath :: Prelude.Maybe Prelude.Text,
    -- | The blob ID that contains the file information.
    blobId :: Prelude.Maybe Prelude.Text,
    -- | The extrapolated file mode permissions for the file. Valid values
    -- include EXECUTABLE and NORMAL.
    fileMode :: Prelude.Maybe FileModeTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'File' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absolutePath', 'file_absolutePath' - The fully qualified path to the file in the repository.
--
-- 'relativePath', 'file_relativePath' - The relative path of the file from the folder where the query
-- originated.
--
-- 'blobId', 'file_blobId' - The blob ID that contains the file information.
--
-- 'fileMode', 'file_fileMode' - The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
newFile ::
  File
newFile =
  File'
    { absolutePath = Prelude.Nothing,
      relativePath = Prelude.Nothing,
      blobId = Prelude.Nothing,
      fileMode = Prelude.Nothing
    }

-- | The fully qualified path to the file in the repository.
file_absolutePath :: Lens.Lens' File (Prelude.Maybe Prelude.Text)
file_absolutePath = Lens.lens (\File' {absolutePath} -> absolutePath) (\s@File' {} a -> s {absolutePath = a} :: File)

-- | The relative path of the file from the folder where the query
-- originated.
file_relativePath :: Lens.Lens' File (Prelude.Maybe Prelude.Text)
file_relativePath = Lens.lens (\File' {relativePath} -> relativePath) (\s@File' {} a -> s {relativePath = a} :: File)

-- | The blob ID that contains the file information.
file_blobId :: Lens.Lens' File (Prelude.Maybe Prelude.Text)
file_blobId = Lens.lens (\File' {blobId} -> blobId) (\s@File' {} a -> s {blobId = a} :: File)

-- | The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
file_fileMode :: Lens.Lens' File (Prelude.Maybe FileModeTypeEnum)
file_fileMode = Lens.lens (\File' {fileMode} -> fileMode) (\s@File' {} a -> s {fileMode = a} :: File)

instance Prelude.FromJSON File where
  parseJSON =
    Prelude.withObject
      "File"
      ( \x ->
          File'
            Prelude.<$> (x Prelude..:? "absolutePath")
            Prelude.<*> (x Prelude..:? "relativePath")
            Prelude.<*> (x Prelude..:? "blobId")
            Prelude.<*> (x Prelude..:? "fileMode")
      )

instance Prelude.Hashable File

instance Prelude.NFData File
