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
-- Module      : Amazonka.FSx.Types.DeleteFileSystemOpenZFSResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteFileSystemOpenZFSResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The response object for the Amazon FSx for OpenZFS file system that\'s
-- being deleted in the @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystemOpenZFSResponse' smart constructor.
data DeleteFileSystemOpenZFSResponse = DeleteFileSystemOpenZFSResponse'
  { finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    finalBackupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemOpenZFSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupTags', 'deleteFileSystemOpenZFSResponse_finalBackupTags' - Undocumented member.
--
-- 'finalBackupId', 'deleteFileSystemOpenZFSResponse_finalBackupId' - Undocumented member.
newDeleteFileSystemOpenZFSResponse ::
  DeleteFileSystemOpenZFSResponse
newDeleteFileSystemOpenZFSResponse =
  DeleteFileSystemOpenZFSResponse'
    { finalBackupTags =
        Prelude.Nothing,
      finalBackupId = Prelude.Nothing
    }

-- | Undocumented member.
deleteFileSystemOpenZFSResponse_finalBackupTags :: Lens.Lens' DeleteFileSystemOpenZFSResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteFileSystemOpenZFSResponse_finalBackupTags = Lens.lens (\DeleteFileSystemOpenZFSResponse' {finalBackupTags} -> finalBackupTags) (\s@DeleteFileSystemOpenZFSResponse' {} a -> s {finalBackupTags = a} :: DeleteFileSystemOpenZFSResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
deleteFileSystemOpenZFSResponse_finalBackupId :: Lens.Lens' DeleteFileSystemOpenZFSResponse (Prelude.Maybe Prelude.Text)
deleteFileSystemOpenZFSResponse_finalBackupId = Lens.lens (\DeleteFileSystemOpenZFSResponse' {finalBackupId} -> finalBackupId) (\s@DeleteFileSystemOpenZFSResponse' {} a -> s {finalBackupId = a} :: DeleteFileSystemOpenZFSResponse)

instance
  Core.FromJSON
    DeleteFileSystemOpenZFSResponse
  where
  parseJSON =
    Core.withObject
      "DeleteFileSystemOpenZFSResponse"
      ( \x ->
          DeleteFileSystemOpenZFSResponse'
            Prelude.<$> (x Core..:? "FinalBackupTags")
            Prelude.<*> (x Core..:? "FinalBackupId")
      )

instance
  Prelude.Hashable
    DeleteFileSystemOpenZFSResponse
  where
  hashWithSalt
    _salt
    DeleteFileSystemOpenZFSResponse' {..} =
      _salt `Prelude.hashWithSalt` finalBackupTags
        `Prelude.hashWithSalt` finalBackupId

instance
  Prelude.NFData
    DeleteFileSystemOpenZFSResponse
  where
  rnf DeleteFileSystemOpenZFSResponse' {..} =
    Prelude.rnf finalBackupTags
      `Prelude.seq` Prelude.rnf finalBackupId
