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
-- Module      : Amazonka.FSx.Types.DeleteFileSystemWindowsResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteFileSystemWindowsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The response object for the Microsoft Windows file system used in the
-- @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystemWindowsResponse' smart constructor.
data DeleteFileSystemWindowsResponse = DeleteFileSystemWindowsResponse'
  { -- | The set of tags applied to the final backup.
    finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of the final backup for this file system.
    finalBackupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemWindowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupTags', 'deleteFileSystemWindowsResponse_finalBackupTags' - The set of tags applied to the final backup.
--
-- 'finalBackupId', 'deleteFileSystemWindowsResponse_finalBackupId' - The ID of the final backup for this file system.
newDeleteFileSystemWindowsResponse ::
  DeleteFileSystemWindowsResponse
newDeleteFileSystemWindowsResponse =
  DeleteFileSystemWindowsResponse'
    { finalBackupTags =
        Prelude.Nothing,
      finalBackupId = Prelude.Nothing
    }

-- | The set of tags applied to the final backup.
deleteFileSystemWindowsResponse_finalBackupTags :: Lens.Lens' DeleteFileSystemWindowsResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteFileSystemWindowsResponse_finalBackupTags = Lens.lens (\DeleteFileSystemWindowsResponse' {finalBackupTags} -> finalBackupTags) (\s@DeleteFileSystemWindowsResponse' {} a -> s {finalBackupTags = a} :: DeleteFileSystemWindowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the final backup for this file system.
deleteFileSystemWindowsResponse_finalBackupId :: Lens.Lens' DeleteFileSystemWindowsResponse (Prelude.Maybe Prelude.Text)
deleteFileSystemWindowsResponse_finalBackupId = Lens.lens (\DeleteFileSystemWindowsResponse' {finalBackupId} -> finalBackupId) (\s@DeleteFileSystemWindowsResponse' {} a -> s {finalBackupId = a} :: DeleteFileSystemWindowsResponse)

instance
  Data.FromJSON
    DeleteFileSystemWindowsResponse
  where
  parseJSON =
    Data.withObject
      "DeleteFileSystemWindowsResponse"
      ( \x ->
          DeleteFileSystemWindowsResponse'
            Prelude.<$> (x Data..:? "FinalBackupTags")
            Prelude.<*> (x Data..:? "FinalBackupId")
      )

instance
  Prelude.Hashable
    DeleteFileSystemWindowsResponse
  where
  hashWithSalt
    _salt
    DeleteFileSystemWindowsResponse' {..} =
      _salt `Prelude.hashWithSalt` finalBackupTags
        `Prelude.hashWithSalt` finalBackupId

instance
  Prelude.NFData
    DeleteFileSystemWindowsResponse
  where
  rnf DeleteFileSystemWindowsResponse' {..} =
    Prelude.rnf finalBackupTags
      `Prelude.seq` Prelude.rnf finalBackupId
