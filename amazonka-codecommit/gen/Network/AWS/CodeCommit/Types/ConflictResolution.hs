{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- /See:/ 'newConflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { -- | Files to be deleted as part of the merge conflict resolution.
    deleteFiles :: Prelude.Maybe [DeleteFileEntry],
    -- | File modes that are set as part of the merge conflict resolution.
    setFileModes :: Prelude.Maybe [SetFileModeEntry],
    -- | Files to have content replaced as part of the merge conflict resolution.
    replaceContents :: Prelude.Maybe [ReplaceContentEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { deleteFiles = Prelude.Nothing,
      setFileModes = Prelude.Nothing,
      replaceContents = Prelude.Nothing
    }

-- | Files to be deleted as part of the merge conflict resolution.
conflictResolution_deleteFiles :: Lens.Lens' ConflictResolution (Prelude.Maybe [DeleteFileEntry])
conflictResolution_deleteFiles = Lens.lens (\ConflictResolution' {deleteFiles} -> deleteFiles) (\s@ConflictResolution' {} a -> s {deleteFiles = a} :: ConflictResolution) Prelude.. Lens.mapping Prelude._Coerce

-- | File modes that are set as part of the merge conflict resolution.
conflictResolution_setFileModes :: Lens.Lens' ConflictResolution (Prelude.Maybe [SetFileModeEntry])
conflictResolution_setFileModes = Lens.lens (\ConflictResolution' {setFileModes} -> setFileModes) (\s@ConflictResolution' {} a -> s {setFileModes = a} :: ConflictResolution) Prelude.. Lens.mapping Prelude._Coerce

-- | Files to have content replaced as part of the merge conflict resolution.
conflictResolution_replaceContents :: Lens.Lens' ConflictResolution (Prelude.Maybe [ReplaceContentEntry])
conflictResolution_replaceContents = Lens.lens (\ConflictResolution' {replaceContents} -> replaceContents) (\s@ConflictResolution' {} a -> s {replaceContents = a} :: ConflictResolution) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable ConflictResolution

instance Prelude.NFData ConflictResolution

instance Prelude.ToJSON ConflictResolution where
  toJSON ConflictResolution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deleteFiles" Prelude..=) Prelude.<$> deleteFiles,
            ("setFileModes" Prelude..=) Prelude.<$> setFileModes,
            ("replaceContents" Prelude..=)
              Prelude.<$> replaceContents
          ]
      )
