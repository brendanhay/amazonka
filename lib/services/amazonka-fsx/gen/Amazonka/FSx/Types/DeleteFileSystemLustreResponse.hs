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
-- Module      : Amazonka.FSx.Types.DeleteFileSystemLustreResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteFileSystemLustreResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The response object for the Amazon FSx for Lustre file system being
-- deleted in the @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystemLustreResponse' smart constructor.
data DeleteFileSystemLustreResponse = DeleteFileSystemLustreResponse'
  { -- | The ID of the final backup for this file system.
    finalBackupId :: Prelude.Maybe Prelude.Text,
    -- | The set of tags applied to the final backup.
    finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemLustreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupId', 'deleteFileSystemLustreResponse_finalBackupId' - The ID of the final backup for this file system.
--
-- 'finalBackupTags', 'deleteFileSystemLustreResponse_finalBackupTags' - The set of tags applied to the final backup.
newDeleteFileSystemLustreResponse ::
  DeleteFileSystemLustreResponse
newDeleteFileSystemLustreResponse =
  DeleteFileSystemLustreResponse'
    { finalBackupId =
        Prelude.Nothing,
      finalBackupTags = Prelude.Nothing
    }

-- | The ID of the final backup for this file system.
deleteFileSystemLustreResponse_finalBackupId :: Lens.Lens' DeleteFileSystemLustreResponse (Prelude.Maybe Prelude.Text)
deleteFileSystemLustreResponse_finalBackupId = Lens.lens (\DeleteFileSystemLustreResponse' {finalBackupId} -> finalBackupId) (\s@DeleteFileSystemLustreResponse' {} a -> s {finalBackupId = a} :: DeleteFileSystemLustreResponse)

-- | The set of tags applied to the final backup.
deleteFileSystemLustreResponse_finalBackupTags :: Lens.Lens' DeleteFileSystemLustreResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteFileSystemLustreResponse_finalBackupTags = Lens.lens (\DeleteFileSystemLustreResponse' {finalBackupTags} -> finalBackupTags) (\s@DeleteFileSystemLustreResponse' {} a -> s {finalBackupTags = a} :: DeleteFileSystemLustreResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DeleteFileSystemLustreResponse where
  parseJSON =
    Data.withObject
      "DeleteFileSystemLustreResponse"
      ( \x ->
          DeleteFileSystemLustreResponse'
            Prelude.<$> (x Data..:? "FinalBackupId")
            Prelude.<*> (x Data..:? "FinalBackupTags")
      )

instance
  Prelude.Hashable
    DeleteFileSystemLustreResponse
  where
  hashWithSalt
    _salt
    DeleteFileSystemLustreResponse' {..} =
      _salt
        `Prelude.hashWithSalt` finalBackupId
        `Prelude.hashWithSalt` finalBackupTags

instance
  Prelude.NFData
    DeleteFileSystemLustreResponse
  where
  rnf DeleteFileSystemLustreResponse' {..} =
    Prelude.rnf finalBackupId `Prelude.seq`
      Prelude.rnf finalBackupTags
