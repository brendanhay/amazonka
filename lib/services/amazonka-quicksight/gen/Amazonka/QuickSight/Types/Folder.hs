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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Folder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FolderType

-- | A folder in Amazon QuickSight.
--
-- /See:/ 'newFolder' smart constructor.
data Folder = Folder'
  { -- | The Amazon Resource Name (ARN) for the folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that the folder was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | An array of ancestor ARN strings for the folder.
    folderPath :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of folder it is.
    folderType :: Prelude.Maybe FolderType,
    -- | The time that the folder was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | A display name for the folder.
    name :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'folder_arn' - The Amazon Resource Name (ARN) for the folder.
--
-- 'createdTime', 'folder_createdTime' - The time that the folder was created.
--
-- 'folderId', 'folder_folderId' - The ID of the folder.
--
-- 'folderPath', 'folder_folderPath' - An array of ancestor ARN strings for the folder.
--
-- 'folderType', 'folder_folderType' - The type of folder it is.
--
-- 'lastUpdatedTime', 'folder_lastUpdatedTime' - The time that the folder was last updated.
--
-- 'name', 'folder_name' - A display name for the folder.
newFolder ::
  Folder
newFolder =
  Folder'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      folderId = Prelude.Nothing,
      folderPath = Prelude.Nothing,
      folderType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the folder.
folder_arn :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_arn = Lens.lens (\Folder' {arn} -> arn) (\s@Folder' {} a -> s {arn = a} :: Folder)

-- | The time that the folder was created.
folder_createdTime :: Lens.Lens' Folder (Prelude.Maybe Prelude.UTCTime)
folder_createdTime = Lens.lens (\Folder' {createdTime} -> createdTime) (\s@Folder' {} a -> s {createdTime = a} :: Folder) Prelude.. Lens.mapping Data._Time

-- | The ID of the folder.
folder_folderId :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_folderId = Lens.lens (\Folder' {folderId} -> folderId) (\s@Folder' {} a -> s {folderId = a} :: Folder)

-- | An array of ancestor ARN strings for the folder.
folder_folderPath :: Lens.Lens' Folder (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
folder_folderPath = Lens.lens (\Folder' {folderPath} -> folderPath) (\s@Folder' {} a -> s {folderPath = a} :: Folder) Prelude.. Lens.mapping Lens.coerced

-- | The type of folder it is.
folder_folderType :: Lens.Lens' Folder (Prelude.Maybe FolderType)
folder_folderType = Lens.lens (\Folder' {folderType} -> folderType) (\s@Folder' {} a -> s {folderType = a} :: Folder)

-- | The time that the folder was last updated.
folder_lastUpdatedTime :: Lens.Lens' Folder (Prelude.Maybe Prelude.UTCTime)
folder_lastUpdatedTime = Lens.lens (\Folder' {lastUpdatedTime} -> lastUpdatedTime) (\s@Folder' {} a -> s {lastUpdatedTime = a} :: Folder) Prelude.. Lens.mapping Data._Time

-- | A display name for the folder.
folder_name :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_name = Lens.lens (\Folder' {name} -> name) (\s@Folder' {} a -> s {name = a} :: Folder)

instance Data.FromJSON Folder where
  parseJSON =
    Data.withObject
      "Folder"
      ( \x ->
          Folder'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "FolderId")
            Prelude.<*> (x Data..:? "FolderPath")
            Prelude.<*> (x Data..:? "FolderType")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable Folder where
  hashWithSalt _salt Folder' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` folderPath
      `Prelude.hashWithSalt` folderType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData Folder where
  rnf Folder' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf folderPath
      `Prelude.seq` Prelude.rnf folderType
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
