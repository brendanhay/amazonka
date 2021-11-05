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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FolderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FolderType

-- | A summary of the folder.
--
-- /See:/ 'newFolderSummary' smart constructor.
data FolderSummary = FolderSummary'
  { -- | The time that the folder was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that the folder was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The folder ID.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The display name of the folder.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'lastUpdatedTime', 'folderSummary_lastUpdatedTime' - The time that the folder was last updated.
--
-- 'arn', 'folderSummary_arn' - The Amazon Resource Name (ARN).
--
-- 'createdTime', 'folderSummary_createdTime' - The time that the folder was created.
--
-- 'folderId', 'folderSummary_folderId' - The folder ID.
--
-- 'name', 'folderSummary_name' - The display name of the folder.
--
-- 'folderType', 'folderSummary_folderType' - The type of folder.
newFolderSummary ::
  FolderSummary
newFolderSummary =
  FolderSummary'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      folderId = Prelude.Nothing,
      name = Prelude.Nothing,
      folderType = Prelude.Nothing
    }

-- | The time that the folder was last updated.
folderSummary_lastUpdatedTime :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.UTCTime)
folderSummary_lastUpdatedTime = Lens.lens (\FolderSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@FolderSummary' {} a -> s {lastUpdatedTime = a} :: FolderSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN).
folderSummary_arn :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_arn = Lens.lens (\FolderSummary' {arn} -> arn) (\s@FolderSummary' {} a -> s {arn = a} :: FolderSummary)

-- | The time that the folder was created.
folderSummary_createdTime :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.UTCTime)
folderSummary_createdTime = Lens.lens (\FolderSummary' {createdTime} -> createdTime) (\s@FolderSummary' {} a -> s {createdTime = a} :: FolderSummary) Prelude.. Lens.mapping Core._Time

-- | The folder ID.
folderSummary_folderId :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_folderId = Lens.lens (\FolderSummary' {folderId} -> folderId) (\s@FolderSummary' {} a -> s {folderId = a} :: FolderSummary)

-- | The display name of the folder.
folderSummary_name :: Lens.Lens' FolderSummary (Prelude.Maybe Prelude.Text)
folderSummary_name = Lens.lens (\FolderSummary' {name} -> name) (\s@FolderSummary' {} a -> s {name = a} :: FolderSummary)

-- | The type of folder.
folderSummary_folderType :: Lens.Lens' FolderSummary (Prelude.Maybe FolderType)
folderSummary_folderType = Lens.lens (\FolderSummary' {folderType} -> folderType) (\s@FolderSummary' {} a -> s {folderType = a} :: FolderSummary)

instance Core.FromJSON FolderSummary where
  parseJSON =
    Core.withObject
      "FolderSummary"
      ( \x ->
          FolderSummary'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "FolderId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "FolderType")
      )

instance Prelude.Hashable FolderSummary

instance Prelude.NFData FolderSummary
