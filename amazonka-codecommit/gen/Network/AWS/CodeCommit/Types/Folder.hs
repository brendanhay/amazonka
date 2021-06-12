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
-- Module      : Network.AWS.CodeCommit.Types.Folder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Folder where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a folder in a repository.
--
-- /See:/ 'newFolder' smart constructor.
data Folder = Folder'
  { -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the folder.
    treeId :: Core.Maybe Core.Text,
    -- | The fully qualified path of the folder in the repository.
    absolutePath :: Core.Maybe Core.Text,
    -- | The relative path of the specified folder from the folder where the
    -- query originated.
    relativePath :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Folder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'treeId', 'folder_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
--
-- 'absolutePath', 'folder_absolutePath' - The fully qualified path of the folder in the repository.
--
-- 'relativePath', 'folder_relativePath' - The relative path of the specified folder from the folder where the
-- query originated.
newFolder ::
  Folder
newFolder =
  Folder'
    { treeId = Core.Nothing,
      absolutePath = Core.Nothing,
      relativePath = Core.Nothing
    }

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
folder_treeId :: Lens.Lens' Folder (Core.Maybe Core.Text)
folder_treeId = Lens.lens (\Folder' {treeId} -> treeId) (\s@Folder' {} a -> s {treeId = a} :: Folder)

-- | The fully qualified path of the folder in the repository.
folder_absolutePath :: Lens.Lens' Folder (Core.Maybe Core.Text)
folder_absolutePath = Lens.lens (\Folder' {absolutePath} -> absolutePath) (\s@Folder' {} a -> s {absolutePath = a} :: Folder)

-- | The relative path of the specified folder from the folder where the
-- query originated.
folder_relativePath :: Lens.Lens' Folder (Core.Maybe Core.Text)
folder_relativePath = Lens.lens (\Folder' {relativePath} -> relativePath) (\s@Folder' {} a -> s {relativePath = a} :: Folder)

instance Core.FromJSON Folder where
  parseJSON =
    Core.withObject
      "Folder"
      ( \x ->
          Folder'
            Core.<$> (x Core..:? "treeId")
            Core.<*> (x Core..:? "absolutePath")
            Core.<*> (x Core..:? "relativePath")
      )

instance Core.Hashable Folder

instance Core.NFData Folder
