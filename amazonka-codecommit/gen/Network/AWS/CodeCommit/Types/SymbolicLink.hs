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
-- Module      : Network.AWS.CodeCommit.Types.SymbolicLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SymbolicLink where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a symbolic link in a repository folder.
--
-- /See:/ 'newSymbolicLink' smart constructor.
data SymbolicLink = SymbolicLink'
  { -- | The fully qualified path to the folder that contains the symbolic link.
    absolutePath :: Core.Maybe Core.Text,
    -- | The relative path of the symbolic link from the folder where the query
    -- originated.
    relativePath :: Core.Maybe Core.Text,
    -- | The blob ID that contains the information about the symbolic link.
    blobId :: Core.Maybe Core.Text,
    -- | The file mode permissions of the blob that cotains information about the
    -- symbolic link.
    fileMode :: Core.Maybe FileModeTypeEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SymbolicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absolutePath', 'symbolicLink_absolutePath' - The fully qualified path to the folder that contains the symbolic link.
--
-- 'relativePath', 'symbolicLink_relativePath' - The relative path of the symbolic link from the folder where the query
-- originated.
--
-- 'blobId', 'symbolicLink_blobId' - The blob ID that contains the information about the symbolic link.
--
-- 'fileMode', 'symbolicLink_fileMode' - The file mode permissions of the blob that cotains information about the
-- symbolic link.
newSymbolicLink ::
  SymbolicLink
newSymbolicLink =
  SymbolicLink'
    { absolutePath = Core.Nothing,
      relativePath = Core.Nothing,
      blobId = Core.Nothing,
      fileMode = Core.Nothing
    }

-- | The fully qualified path to the folder that contains the symbolic link.
symbolicLink_absolutePath :: Lens.Lens' SymbolicLink (Core.Maybe Core.Text)
symbolicLink_absolutePath = Lens.lens (\SymbolicLink' {absolutePath} -> absolutePath) (\s@SymbolicLink' {} a -> s {absolutePath = a} :: SymbolicLink)

-- | The relative path of the symbolic link from the folder where the query
-- originated.
symbolicLink_relativePath :: Lens.Lens' SymbolicLink (Core.Maybe Core.Text)
symbolicLink_relativePath = Lens.lens (\SymbolicLink' {relativePath} -> relativePath) (\s@SymbolicLink' {} a -> s {relativePath = a} :: SymbolicLink)

-- | The blob ID that contains the information about the symbolic link.
symbolicLink_blobId :: Lens.Lens' SymbolicLink (Core.Maybe Core.Text)
symbolicLink_blobId = Lens.lens (\SymbolicLink' {blobId} -> blobId) (\s@SymbolicLink' {} a -> s {blobId = a} :: SymbolicLink)

-- | The file mode permissions of the blob that cotains information about the
-- symbolic link.
symbolicLink_fileMode :: Lens.Lens' SymbolicLink (Core.Maybe FileModeTypeEnum)
symbolicLink_fileMode = Lens.lens (\SymbolicLink' {fileMode} -> fileMode) (\s@SymbolicLink' {} a -> s {fileMode = a} :: SymbolicLink)

instance Core.FromJSON SymbolicLink where
  parseJSON =
    Core.withObject
      "SymbolicLink"
      ( \x ->
          SymbolicLink'
            Core.<$> (x Core..:? "absolutePath")
            Core.<*> (x Core..:? "relativePath")
            Core.<*> (x Core..:? "blobId")
            Core.<*> (x Core..:? "fileMode")
      )

instance Core.Hashable SymbolicLink

instance Core.NFData SymbolicLink
