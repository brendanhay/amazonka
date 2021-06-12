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
-- Module      : Network.AWS.CodeCommit.Types.DeleteFileEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.DeleteFileEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A file that is deleted as part of a commit.
--
-- /See:/ 'newDeleteFileEntry' smart constructor.
data DeleteFileEntry = DeleteFileEntry'
  { -- | The full path of the file to be deleted, including the name of the file.
    filePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFileEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePath', 'deleteFileEntry_filePath' - The full path of the file to be deleted, including the name of the file.
newDeleteFileEntry ::
  -- | 'filePath'
  Core.Text ->
  DeleteFileEntry
newDeleteFileEntry pFilePath_ =
  DeleteFileEntry' {filePath = pFilePath_}

-- | The full path of the file to be deleted, including the name of the file.
deleteFileEntry_filePath :: Lens.Lens' DeleteFileEntry Core.Text
deleteFileEntry_filePath = Lens.lens (\DeleteFileEntry' {filePath} -> filePath) (\s@DeleteFileEntry' {} a -> s {filePath = a} :: DeleteFileEntry)

instance Core.Hashable DeleteFileEntry

instance Core.NFData DeleteFileEntry

instance Core.ToJSON DeleteFileEntry where
  toJSON DeleteFileEntry' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("filePath" Core..= filePath)]
      )
