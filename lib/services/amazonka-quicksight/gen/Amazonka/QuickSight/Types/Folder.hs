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
-- Module      : Amazonka.QuickSight.Types.Folder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Folder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FolderType

-- | A folder.
--
-- /See:/ 'newFolder' smart constructor.
data Folder = Folder'
  { -- | The time that the folder was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The folder Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that the folder was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The folder ID.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | A display name for the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of ancestor folder ARN strings.
    folderPath :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of the folder.
    folderType :: Prelude.Maybe FolderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Folder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'folder_lastUpdatedTime' - The time that the folder was last updated.
--
-- 'arn', 'folder_arn' - The folder Amazon Resource Name (ARN).
--
-- 'createdTime', 'folder_createdTime' - The time that the folder was created.
--
-- 'folderId', 'folder_folderId' - The folder ID.
--
-- 'name', 'folder_name' - A display name for the folder.
--
-- 'folderPath', 'folder_folderPath' - An array of ancestor folder ARN strings.
--
-- 'folderType', 'folder_folderType' - The type of the folder.
newFolder ::
  Folder
newFolder =
  Folder'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      folderId = Prelude.Nothing,
      name = Prelude.Nothing,
      folderPath = Prelude.Nothing,
      folderType = Prelude.Nothing
    }

-- | The time that the folder was last updated.
folder_lastUpdatedTime :: Lens.Lens' Folder (Prelude.Maybe Prelude.UTCTime)
folder_lastUpdatedTime = Lens.lens (\Folder' {lastUpdatedTime} -> lastUpdatedTime) (\s@Folder' {} a -> s {lastUpdatedTime = a} :: Folder) Prelude.. Lens.mapping Core._Time

-- | The folder Amazon Resource Name (ARN).
folder_arn :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_arn = Lens.lens (\Folder' {arn} -> arn) (\s@Folder' {} a -> s {arn = a} :: Folder)

-- | The time that the folder was created.
folder_createdTime :: Lens.Lens' Folder (Prelude.Maybe Prelude.UTCTime)
folder_createdTime = Lens.lens (\Folder' {createdTime} -> createdTime) (\s@Folder' {} a -> s {createdTime = a} :: Folder) Prelude.. Lens.mapping Core._Time

-- | The folder ID.
folder_folderId :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_folderId = Lens.lens (\Folder' {folderId} -> folderId) (\s@Folder' {} a -> s {folderId = a} :: Folder)

-- | A display name for the folder.
folder_name :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_name = Lens.lens (\Folder' {name} -> name) (\s@Folder' {} a -> s {name = a} :: Folder)

-- | An array of ancestor folder ARN strings.
folder_folderPath :: Lens.Lens' Folder (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
folder_folderPath = Lens.lens (\Folder' {folderPath} -> folderPath) (\s@Folder' {} a -> s {folderPath = a} :: Folder) Prelude.. Lens.mapping Lens.coerced

-- | The type of the folder.
folder_folderType :: Lens.Lens' Folder (Prelude.Maybe FolderType)
folder_folderType = Lens.lens (\Folder' {folderType} -> folderType) (\s@Folder' {} a -> s {folderType = a} :: Folder)

instance Core.FromJSON Folder where
  parseJSON =
    Core.withObject
      "Folder"
      ( \x ->
          Folder'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "FolderId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "FolderPath")
            Prelude.<*> (x Core..:? "FolderType")
      )

instance Prelude.Hashable Folder

instance Prelude.NFData Folder
