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
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictResolution where

import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- /See:/ 'newConflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { -- | Files to be deleted as part of the merge conflict resolution.
    deleteFiles :: Core.Maybe [DeleteFileEntry],
    -- | File modes that are set as part of the merge conflict resolution.
    setFileModes :: Core.Maybe [SetFileModeEntry],
    -- | Files to have content replaced as part of the merge conflict resolution.
    replaceContents :: Core.Maybe [ReplaceContentEntry]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConflictResolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteFiles', 'conflictResolution_deleteFiles' - Files to be deleted as part of the merge conflict resolution.
--
-- 'setFileModes', 'conflictResolution_setFileModes' - File modes that are set as part of the merge conflict resolution.
--
-- 'replaceContents', 'conflictResolution_replaceContents' - Files to have content replaced as part of the merge conflict resolution.
newConflictResolution ::
  ConflictResolution
newConflictResolution =
  ConflictResolution'
    { deleteFiles = Core.Nothing,
      setFileModes = Core.Nothing,
      replaceContents = Core.Nothing
    }

-- | Files to be deleted as part of the merge conflict resolution.
conflictResolution_deleteFiles :: Lens.Lens' ConflictResolution (Core.Maybe [DeleteFileEntry])
conflictResolution_deleteFiles = Lens.lens (\ConflictResolution' {deleteFiles} -> deleteFiles) (\s@ConflictResolution' {} a -> s {deleteFiles = a} :: ConflictResolution) Core.. Lens.mapping Lens._Coerce

-- | File modes that are set as part of the merge conflict resolution.
conflictResolution_setFileModes :: Lens.Lens' ConflictResolution (Core.Maybe [SetFileModeEntry])
conflictResolution_setFileModes = Lens.lens (\ConflictResolution' {setFileModes} -> setFileModes) (\s@ConflictResolution' {} a -> s {setFileModes = a} :: ConflictResolution) Core.. Lens.mapping Lens._Coerce

-- | Files to have content replaced as part of the merge conflict resolution.
conflictResolution_replaceContents :: Lens.Lens' ConflictResolution (Core.Maybe [ReplaceContentEntry])
conflictResolution_replaceContents = Lens.lens (\ConflictResolution' {replaceContents} -> replaceContents) (\s@ConflictResolution' {} a -> s {replaceContents = a} :: ConflictResolution) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable ConflictResolution

instance Core.NFData ConflictResolution

instance Core.ToJSON ConflictResolution where
  toJSON ConflictResolution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deleteFiles" Core..=) Core.<$> deleteFiles,
            ("setFileModes" Core..=) Core.<$> setFileModes,
            ("replaceContents" Core..=)
              Core.<$> replaceContents
          ]
      )
