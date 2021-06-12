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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a file added or updated as part of a commit.
--
-- /See:/ 'newPutFileEntry' smart constructor.
data PutFileEntry = PutFileEntry'
  { -- | The content of the file, if a source file is not specified.
    fileContent :: Core.Maybe Core.Base64,
    -- | The name and full path of the file that contains the changes you want to
    -- make as part of the commit, if you are not providing the file content
    -- directly.
    sourceFile :: Core.Maybe SourceFileSpecifier,
    -- | The extrapolated file mode permissions for the file. Valid values
    -- include EXECUTABLE and NORMAL.
    fileMode :: Core.Maybe FileModeTypeEnum,
    -- | The full path to the file in the repository, including the name of the
    -- file.
    filePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  PutFileEntry
newPutFileEntry pFilePath_ =
  PutFileEntry'
    { fileContent = Core.Nothing,
      sourceFile = Core.Nothing,
      fileMode = Core.Nothing,
      filePath = pFilePath_
    }

-- | The content of the file, if a source file is not specified.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putFileEntry_fileContent :: Lens.Lens' PutFileEntry (Core.Maybe Core.ByteString)
putFileEntry_fileContent = Lens.lens (\PutFileEntry' {fileContent} -> fileContent) (\s@PutFileEntry' {} a -> s {fileContent = a} :: PutFileEntry) Core.. Lens.mapping Core._Base64

-- | The name and full path of the file that contains the changes you want to
-- make as part of the commit, if you are not providing the file content
-- directly.
putFileEntry_sourceFile :: Lens.Lens' PutFileEntry (Core.Maybe SourceFileSpecifier)
putFileEntry_sourceFile = Lens.lens (\PutFileEntry' {sourceFile} -> sourceFile) (\s@PutFileEntry' {} a -> s {sourceFile = a} :: PutFileEntry)

-- | The extrapolated file mode permissions for the file. Valid values
-- include EXECUTABLE and NORMAL.
putFileEntry_fileMode :: Lens.Lens' PutFileEntry (Core.Maybe FileModeTypeEnum)
putFileEntry_fileMode = Lens.lens (\PutFileEntry' {fileMode} -> fileMode) (\s@PutFileEntry' {} a -> s {fileMode = a} :: PutFileEntry)

-- | The full path to the file in the repository, including the name of the
-- file.
putFileEntry_filePath :: Lens.Lens' PutFileEntry Core.Text
putFileEntry_filePath = Lens.lens (\PutFileEntry' {filePath} -> filePath) (\s@PutFileEntry' {} a -> s {filePath = a} :: PutFileEntry)

instance Core.Hashable PutFileEntry

instance Core.NFData PutFileEntry

instance Core.ToJSON PutFileEntry where
  toJSON PutFileEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("fileContent" Core..=) Core.<$> fileContent,
            ("sourceFile" Core..=) Core.<$> sourceFile,
            ("fileMode" Core..=) Core.<$> fileMode,
            Core.Just ("filePath" Core..= filePath)
          ]
      )
