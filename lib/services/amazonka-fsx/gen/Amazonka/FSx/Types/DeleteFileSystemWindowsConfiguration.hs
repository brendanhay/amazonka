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
-- Module      : Amazonka.FSx.Types.DeleteFileSystemWindowsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteFileSystemWindowsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for the Microsoft Windows file system used in
-- the @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystemWindowsConfiguration' smart constructor.
data DeleteFileSystemWindowsConfiguration = DeleteFileSystemWindowsConfiguration'
  { -- | A set of tags for your final backup.
    finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | By default, Amazon FSx for Windows takes a final backup on your behalf
    -- when the @DeleteFileSystem@ operation is invoked. Doing this helps
    -- protect you from data loss, and we highly recommend taking the final
    -- backup. If you want to skip this backup, use this flag to do so.
    skipFinalBackup :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemWindowsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupTags', 'deleteFileSystemWindowsConfiguration_finalBackupTags' - A set of tags for your final backup.
--
-- 'skipFinalBackup', 'deleteFileSystemWindowsConfiguration_skipFinalBackup' - By default, Amazon FSx for Windows takes a final backup on your behalf
-- when the @DeleteFileSystem@ operation is invoked. Doing this helps
-- protect you from data loss, and we highly recommend taking the final
-- backup. If you want to skip this backup, use this flag to do so.
newDeleteFileSystemWindowsConfiguration ::
  DeleteFileSystemWindowsConfiguration
newDeleteFileSystemWindowsConfiguration =
  DeleteFileSystemWindowsConfiguration'
    { finalBackupTags =
        Prelude.Nothing,
      skipFinalBackup = Prelude.Nothing
    }

-- | A set of tags for your final backup.
deleteFileSystemWindowsConfiguration_finalBackupTags :: Lens.Lens' DeleteFileSystemWindowsConfiguration (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteFileSystemWindowsConfiguration_finalBackupTags = Lens.lens (\DeleteFileSystemWindowsConfiguration' {finalBackupTags} -> finalBackupTags) (\s@DeleteFileSystemWindowsConfiguration' {} a -> s {finalBackupTags = a} :: DeleteFileSystemWindowsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | By default, Amazon FSx for Windows takes a final backup on your behalf
-- when the @DeleteFileSystem@ operation is invoked. Doing this helps
-- protect you from data loss, and we highly recommend taking the final
-- backup. If you want to skip this backup, use this flag to do so.
deleteFileSystemWindowsConfiguration_skipFinalBackup :: Lens.Lens' DeleteFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Bool)
deleteFileSystemWindowsConfiguration_skipFinalBackup = Lens.lens (\DeleteFileSystemWindowsConfiguration' {skipFinalBackup} -> skipFinalBackup) (\s@DeleteFileSystemWindowsConfiguration' {} a -> s {skipFinalBackup = a} :: DeleteFileSystemWindowsConfiguration)

instance
  Prelude.Hashable
    DeleteFileSystemWindowsConfiguration
  where
  hashWithSalt
    _salt
    DeleteFileSystemWindowsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` finalBackupTags
        `Prelude.hashWithSalt` skipFinalBackup

instance
  Prelude.NFData
    DeleteFileSystemWindowsConfiguration
  where
  rnf DeleteFileSystemWindowsConfiguration' {..} =
    Prelude.rnf finalBackupTags
      `Prelude.seq` Prelude.rnf skipFinalBackup

instance
  Data.ToJSON
    DeleteFileSystemWindowsConfiguration
  where
  toJSON DeleteFileSystemWindowsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FinalBackupTags" Data..=)
              Prelude.<$> finalBackupTags,
            ("SkipFinalBackup" Data..=)
              Prelude.<$> skipFinalBackup
          ]
      )
