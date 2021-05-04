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
-- Module      : Network.AWS.CodeCommit.Types.Folder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Folder where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a folder in a repository.
--
-- /See:/ 'newFolder' smart constructor.
data Folder = Folder'
  { -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the folder.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified path of the folder in the repository.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The relative path of the specified folder from the folder where the
    -- query originated.
    relativePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { treeId = Prelude.Nothing,
      absolutePath = Prelude.Nothing,
      relativePath = Prelude.Nothing
    }

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
folder_treeId :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_treeId = Lens.lens (\Folder' {treeId} -> treeId) (\s@Folder' {} a -> s {treeId = a} :: Folder)

-- | The fully qualified path of the folder in the repository.
folder_absolutePath :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_absolutePath = Lens.lens (\Folder' {absolutePath} -> absolutePath) (\s@Folder' {} a -> s {absolutePath = a} :: Folder)

-- | The relative path of the specified folder from the folder where the
-- query originated.
folder_relativePath :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_relativePath = Lens.lens (\Folder' {relativePath} -> relativePath) (\s@Folder' {} a -> s {relativePath = a} :: Folder)

instance Prelude.FromJSON Folder where
  parseJSON =
    Prelude.withObject
      "Folder"
      ( \x ->
          Folder'
            Prelude.<$> (x Prelude..:? "treeId")
            Prelude.<*> (x Prelude..:? "absolutePath")
            Prelude.<*> (x Prelude..:? "relativePath")
      )

instance Prelude.Hashable Folder

instance Prelude.NFData Folder
