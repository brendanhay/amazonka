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
-- Module      : Amazonka.QuickSight.Types.FolderSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FolderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FolderType

-- | A summary of information about an existing Amazon QuickSight folder.
--
-- /See:/ 'newFolderSummary' smart constructor.
data FolderSummary = FolderSummary'
  { -- | The Amazon Resource Name (ARN) of the folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that the folder was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The type of folder.
    folderType :: Prelude.Maybe FolderType,
    -- | The time that the folder was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The display name of the folder.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FolderSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'folderSummary_arn' - The Amazon Resource Name (ARN) of the folder.
--
-- 'createdTime', 'folderSummary_createdTime' - The time that the folder was created.
--
-- 'folderId', 'folderSummary_folderId' - The ID of the folder.
--
-- 'folderType', 'folderSummary_folderType' - The type of folder.
--
-- 'lastUpdatedTime', 'folderSummary_lastUpdatedTime' - The time that the folder was last updated.
--
-- 'name', 'folderSummary_name' - The display name of the folder.
newFolderSummary ::
  FolderSummary
newFolderSummary =
  FolderSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      folderId = Prelude.Nothing,
      folderType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the folder.
folderSummary_arn :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_arn = Lens.lens (\FolderSummary' {arn} -> arn) (\s@FolderSummary' {} a -> s {arn = a} :: FolderSummary)

-- | The time that the folder was created.
folderSummary_createdTime :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.UTCTime)
folderSummary_createdTime = Lens.lens (\FolderSummary' {createdTime} -> createdTime) (\s@FolderSummary' {} a -> s {createdTime = a} :: FolderSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the folder.
folderSummary_folderId :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_folderId = Lens.lens (\FolderSummary' {folderId} -> folderId) (\s@FolderSummary' {} a -> s {folderId = a} :: FolderSummary)

-- | The type of folder.
folderSummary_folderType :: Lens.Lens' FolderSummary (Prelude.Maybe FolderType)
folderSummary_folderType = Lens.lens (\FolderSummary' {folderType} -> folderType) (\s@FolderSummary' {} a -> s {folderType = a} :: FolderSummary)

-- | The time that the folder was last updated.
folderSummary_lastUpdatedTime :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.UTCTime)
folderSummary_lastUpdatedTime = Lens.lens (\FolderSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@FolderSummary' {} a -> s {lastUpdatedTime = a} :: FolderSummary) Prelude.. Lens.mapping Data._Time

-- | The display name of the folder.
folderSummary_name :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_name = Lens.lens (\FolderSummary' {name} -> name) (\s@FolderSummary' {} a -> s {name = a} :: FolderSummary)

instance Data.FromJSON FolderSummary where
  parseJSON =
    Data.withObject
      "FolderSummary"
      ( \x ->
          FolderSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "FolderId")
            Prelude.<*> (x Data..:? "FolderType")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable FolderSummary where
  hashWithSalt _salt FolderSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` folderType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData FolderSummary where
  rnf FolderSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf folderType
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
