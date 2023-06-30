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
-- Module      : Amazonka.FSx.Types.DeleteFileSystemOpenZFSConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteFileSystemOpenZFSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DeleteFileSystemOpenZFSOption
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for the Amazon FSx for OpenZFS file system used
-- in the @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystemOpenZFSConfiguration' smart constructor.
data DeleteFileSystemOpenZFSConfiguration = DeleteFileSystemOpenZFSConfiguration'
  { -- | A list of tags to apply to the file system\'s final backup.
    finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | To delete a file system if there are child volumes present below the
    -- root volume, use the string @DELETE_CHILD_VOLUMES_AND_SNAPSHOTS@. If
    -- your file system has child volumes and you don\'t use this option, the
    -- delete request will fail.
    options :: Prelude.Maybe [DeleteFileSystemOpenZFSOption],
    -- | By default, Amazon FSx for OpenZFS takes a final backup on your behalf
    -- when the @DeleteFileSystem@ operation is invoked. Doing this helps
    -- protect you from data loss, and we highly recommend taking the final
    -- backup. If you want to skip taking a final backup, set this value to
    -- @true@.
    skipFinalBackup :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemOpenZFSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupTags', 'deleteFileSystemOpenZFSConfiguration_finalBackupTags' - A list of tags to apply to the file system\'s final backup.
--
-- 'options', 'deleteFileSystemOpenZFSConfiguration_options' - To delete a file system if there are child volumes present below the
-- root volume, use the string @DELETE_CHILD_VOLUMES_AND_SNAPSHOTS@. If
-- your file system has child volumes and you don\'t use this option, the
-- delete request will fail.
--
-- 'skipFinalBackup', 'deleteFileSystemOpenZFSConfiguration_skipFinalBackup' - By default, Amazon FSx for OpenZFS takes a final backup on your behalf
-- when the @DeleteFileSystem@ operation is invoked. Doing this helps
-- protect you from data loss, and we highly recommend taking the final
-- backup. If you want to skip taking a final backup, set this value to
-- @true@.
newDeleteFileSystemOpenZFSConfiguration ::
  DeleteFileSystemOpenZFSConfiguration
newDeleteFileSystemOpenZFSConfiguration =
  DeleteFileSystemOpenZFSConfiguration'
    { finalBackupTags =
        Prelude.Nothing,
      options = Prelude.Nothing,
      skipFinalBackup = Prelude.Nothing
    }

-- | A list of tags to apply to the file system\'s final backup.
deleteFileSystemOpenZFSConfiguration_finalBackupTags :: Lens.Lens' DeleteFileSystemOpenZFSConfiguration (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteFileSystemOpenZFSConfiguration_finalBackupTags = Lens.lens (\DeleteFileSystemOpenZFSConfiguration' {finalBackupTags} -> finalBackupTags) (\s@DeleteFileSystemOpenZFSConfiguration' {} a -> s {finalBackupTags = a} :: DeleteFileSystemOpenZFSConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | To delete a file system if there are child volumes present below the
-- root volume, use the string @DELETE_CHILD_VOLUMES_AND_SNAPSHOTS@. If
-- your file system has child volumes and you don\'t use this option, the
-- delete request will fail.
deleteFileSystemOpenZFSConfiguration_options :: Lens.Lens' DeleteFileSystemOpenZFSConfiguration (Prelude.Maybe [DeleteFileSystemOpenZFSOption])
deleteFileSystemOpenZFSConfiguration_options = Lens.lens (\DeleteFileSystemOpenZFSConfiguration' {options} -> options) (\s@DeleteFileSystemOpenZFSConfiguration' {} a -> s {options = a} :: DeleteFileSystemOpenZFSConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | By default, Amazon FSx for OpenZFS takes a final backup on your behalf
-- when the @DeleteFileSystem@ operation is invoked. Doing this helps
-- protect you from data loss, and we highly recommend taking the final
-- backup. If you want to skip taking a final backup, set this value to
-- @true@.
deleteFileSystemOpenZFSConfiguration_skipFinalBackup :: Lens.Lens' DeleteFileSystemOpenZFSConfiguration (Prelude.Maybe Prelude.Bool)
deleteFileSystemOpenZFSConfiguration_skipFinalBackup = Lens.lens (\DeleteFileSystemOpenZFSConfiguration' {skipFinalBackup} -> skipFinalBackup) (\s@DeleteFileSystemOpenZFSConfiguration' {} a -> s {skipFinalBackup = a} :: DeleteFileSystemOpenZFSConfiguration)

instance
  Prelude.Hashable
    DeleteFileSystemOpenZFSConfiguration
  where
  hashWithSalt
    _salt
    DeleteFileSystemOpenZFSConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` finalBackupTags
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` skipFinalBackup

instance
  Prelude.NFData
    DeleteFileSystemOpenZFSConfiguration
  where
  rnf DeleteFileSystemOpenZFSConfiguration' {..} =
    Prelude.rnf finalBackupTags
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf skipFinalBackup

instance
  Data.ToJSON
    DeleteFileSystemOpenZFSConfiguration
  where
  toJSON DeleteFileSystemOpenZFSConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FinalBackupTags" Data..=)
              Prelude.<$> finalBackupTags,
            ("Options" Data..=) Prelude.<$> options,
            ("SkipFinalBackup" Data..=)
              Prelude.<$> skipFinalBackup
          ]
      )
