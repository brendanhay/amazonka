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
-- Module      : Network.AWS.CodeCommit.Types.PutFileEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PutFileEntry where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.CodeCommit.Types.SourceFileSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a file added or updated as part of a commit.
--
-- /See:/ 'newPutFileEntry' smart constructor.
data PutFileEntry = PutFileEntry'
  { -- | The content of the file, if a source file is not specified.
    fileContent :: Prelude.Maybe Prelude.Base64,
    -- | The name and full path of the file that contains the changes you want to
    -- make as part of the commit, if you are not providing the file content
    -- directly.
    sourceFile :: Prelude.Maybe SourceFileSpecifier,
    -- | The extrapolated file mode permissions for the file. Valid values
    -- include EXECUTABLE and NORMAL.
    fileMode :: Prelude.Maybe FileModeTypeEnum,
    -- | The full path to the file in the repository, including the name of the
    -- file.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutFileEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileContent', 'putFileEntry_fileContent' - The content of the file, if a source file is not specified.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'sourceFile', 'putFileEntry_sourceFile' - The name and full path of the file that contains the changes you want to
-- make as part of the commit, if you are not providing the file content
-- directly.
--
-- 'fileMode', 'putFileEntry_fileMode' - The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
--
-- 'filePath', 'putFileEntry_filePath' - The full path to the file in the repository, including the name of the
-- file.
newPutFileEntry ::
  -- | 'filePath'
  Prelude.Text ->
  PutFileEntry
newPutFileEntry pFilePath_ =
  PutFileEntry'
    { fileContent = Prelude.Nothing,
      sourceFile = Prelude.Nothing,
      fileMode = Prelude.Nothing,
      filePath = pFilePath_
    }

-- | The content of the file, if a source file is not specified.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putFileEntry_fileContent :: Lens.Lens' PutFileEntry (Prelude.Maybe Prelude.ByteString)
putFileEntry_fileContent = Lens.lens (\PutFileEntry' {fileContent} -> fileContent) (\s@PutFileEntry' {} a -> s {fileContent = a} :: PutFileEntry) Prelude.. Lens.mapping Prelude._Base64

-- | The name and full path of the file that contains the changes you want to
-- make as part of the commit, if you are not providing the file content
-- directly.
putFileEntry_sourceFile :: Lens.Lens' PutFileEntry (Prelude.Maybe SourceFileSpecifier)
putFileEntry_sourceFile = Lens.lens (\PutFileEntry' {sourceFile} -> sourceFile) (\s@PutFileEntry' {} a -> s {sourceFile = a} :: PutFileEntry)

-- | The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
putFileEntry_fileMode :: Lens.Lens' PutFileEntry (Prelude.Maybe FileModeTypeEnum)
putFileEntry_fileMode = Lens.lens (\PutFileEntry' {fileMode} -> fileMode) (\s@PutFileEntry' {} a -> s {fileMode = a} :: PutFileEntry)

-- | The full path to the file in the repository, including the name of the
-- file.
putFileEntry_filePath :: Lens.Lens' PutFileEntry Prelude.Text
putFileEntry_filePath = Lens.lens (\PutFileEntry' {filePath} -> filePath) (\s@PutFileEntry' {} a -> s {filePath = a} :: PutFileEntry)

instance Prelude.Hashable PutFileEntry

instance Prelude.NFData PutFileEntry

instance Prelude.ToJSON PutFileEntry where
  toJSON PutFileEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("fileContent" Prelude..=) Prelude.<$> fileContent,
            ("sourceFile" Prelude..=) Prelude.<$> sourceFile,
            ("fileMode" Prelude..=) Prelude.<$> fileMode,
            Prelude.Just ("filePath" Prelude..= filePath)
          ]
      )
