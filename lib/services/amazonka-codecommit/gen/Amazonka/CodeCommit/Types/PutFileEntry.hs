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
-- Module      : Amazonka.CodeCommit.Types.PutFileEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.PutFileEntry where

import Amazonka.CodeCommit.Types.FileModeTypeEnum
import Amazonka.CodeCommit.Types.SourceFileSpecifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a file added or updated as part of a commit.
--
-- /See:/ 'newPutFileEntry' smart constructor.
data PutFileEntry = PutFileEntry'
  { -- | The extrapolated file mode permissions for the file. Valid values
    -- include EXECUTABLE and NORMAL.
    fileMode :: Prelude.Maybe FileModeTypeEnum,
    -- | The name and full path of the file that contains the changes you want to
    -- make as part of the commit, if you are not providing the file content
    -- directly.
    sourceFile :: Prelude.Maybe SourceFileSpecifier,
    -- | The content of the file, if a source file is not specified.
    fileContent :: Prelude.Maybe Data.Base64,
    -- | The full path to the file in the repository, including the name of the
    -- file.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFileEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileMode', 'putFileEntry_fileMode' - The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
--
-- 'sourceFile', 'putFileEntry_sourceFile' - The name and full path of the file that contains the changes you want to
-- make as part of the commit, if you are not providing the file content
-- directly.
--
-- 'fileContent', 'putFileEntry_fileContent' - The content of the file, if a source file is not specified.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'filePath', 'putFileEntry_filePath' - The full path to the file in the repository, including the name of the
-- file.
newPutFileEntry ::
  -- | 'filePath'
  Prelude.Text ->
  PutFileEntry
newPutFileEntry pFilePath_ =
  PutFileEntry'
    { fileMode = Prelude.Nothing,
      sourceFile = Prelude.Nothing,
      fileContent = Prelude.Nothing,
      filePath = pFilePath_
    }

-- | The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
putFileEntry_fileMode :: Lens.Lens' PutFileEntry (Prelude.Maybe FileModeTypeEnum)
putFileEntry_fileMode = Lens.lens (\PutFileEntry' {fileMode} -> fileMode) (\s@PutFileEntry' {} a -> s {fileMode = a} :: PutFileEntry)

-- | The name and full path of the file that contains the changes you want to
-- make as part of the commit, if you are not providing the file content
-- directly.
putFileEntry_sourceFile :: Lens.Lens' PutFileEntry (Prelude.Maybe SourceFileSpecifier)
putFileEntry_sourceFile = Lens.lens (\PutFileEntry' {sourceFile} -> sourceFile) (\s@PutFileEntry' {} a -> s {sourceFile = a} :: PutFileEntry)

-- | The content of the file, if a source file is not specified.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putFileEntry_fileContent :: Lens.Lens' PutFileEntry (Prelude.Maybe Prelude.ByteString)
putFileEntry_fileContent = Lens.lens (\PutFileEntry' {fileContent} -> fileContent) (\s@PutFileEntry' {} a -> s {fileContent = a} :: PutFileEntry) Prelude.. Lens.mapping Data._Base64

-- | The full path to the file in the repository, including the name of the
-- file.
putFileEntry_filePath :: Lens.Lens' PutFileEntry Prelude.Text
putFileEntry_filePath = Lens.lens (\PutFileEntry' {filePath} -> filePath) (\s@PutFileEntry' {} a -> s {filePath = a} :: PutFileEntry)

instance Prelude.Hashable PutFileEntry where
  hashWithSalt _salt PutFileEntry' {..} =
    _salt `Prelude.hashWithSalt` fileMode
      `Prelude.hashWithSalt` sourceFile
      `Prelude.hashWithSalt` fileContent
      `Prelude.hashWithSalt` filePath

instance Prelude.NFData PutFileEntry where
  rnf PutFileEntry' {..} =
    Prelude.rnf fileMode
      `Prelude.seq` Prelude.rnf sourceFile
      `Prelude.seq` Prelude.rnf fileContent
      `Prelude.seq` Prelude.rnf filePath

instance Data.ToJSON PutFileEntry where
  toJSON PutFileEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fileMode" Data..=) Prelude.<$> fileMode,
            ("sourceFile" Data..=) Prelude.<$> sourceFile,
            ("fileContent" Data..=) Prelude.<$> fileContent,
            Prelude.Just ("filePath" Data..= filePath)
          ]
      )
