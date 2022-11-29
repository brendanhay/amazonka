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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FolderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FolderType

-- | A summary of information about an existing Amazon QuickSight folder.
--
-- /See:/ 'newFolderSummary' smart constructor.
data FolderSummary = FolderSummary'
  { -- | The display name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time that the folder was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that the folder was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The type of folder.
    folderType :: Prelude.Maybe FolderType
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
-- 'name', 'folderSummary_name' - The display name of the folder.
--
-- 'createdTime', 'folderSummary_createdTime' - The time that the folder was created.
--
-- 'arn', 'folderSummary_arn' - The Amazon Resource Name (ARN) of the folder.
--
-- 'lastUpdatedTime', 'folderSummary_lastUpdatedTime' - The time that the folder was last updated.
--
-- 'folderId', 'folderSummary_folderId' - The ID of the folder.
--
-- 'folderType', 'folderSummary_folderType' - The type of folder.
newFolderSummary ::
  FolderSummary
newFolderSummary =
  FolderSummary'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      folderId = Prelude.Nothing,
      folderType = Prelude.Nothing
    }

-- | The display name of the folder.
folderSummary_name :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_name = Lens.lens (\FolderSummary' {name} -> name) (\s@FolderSummary' {} a -> s {name = a} :: FolderSummary)

-- | The time that the folder was created.
folderSummary_createdTime :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.UTCTime)
folderSummary_createdTime = Lens.lens (\FolderSummary' {createdTime} -> createdTime) (\s@FolderSummary' {} a -> s {createdTime = a} :: FolderSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the folder.
folderSummary_arn :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_arn = Lens.lens (\FolderSummary' {arn} -> arn) (\s@FolderSummary' {} a -> s {arn = a} :: FolderSummary)

-- | The time that the folder was last updated.
folderSummary_lastUpdatedTime :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.UTCTime)
folderSummary_lastUpdatedTime = Lens.lens (\FolderSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@FolderSummary' {} a -> s {lastUpdatedTime = a} :: FolderSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the folder.
folderSummary_folderId :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_folderId = Lens.lens (\FolderSummary' {folderId} -> folderId) (\s@FolderSummary' {} a -> s {folderId = a} :: FolderSummary)

-- | The type of folder.
folderSummary_folderType :: Lens.Lens' FolderSummary (Prelude.Maybe FolderType)
folderSummary_folderType = Lens.lens (\FolderSummary' {folderType} -> folderType) (\s@FolderSummary' {} a -> s {folderType = a} :: FolderSummary)

instance Core.FromJSON FolderSummary where
  parseJSON =
    Core.withObject
      "FolderSummary"
      ( \x ->
          FolderSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "FolderId")
            Prelude.<*> (x Core..:? "FolderType")
      )

instance Prelude.Hashable FolderSummary where
  hashWithSalt _salt FolderSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` folderType

instance Prelude.NFData FolderSummary where
  rnf FolderSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf folderType
