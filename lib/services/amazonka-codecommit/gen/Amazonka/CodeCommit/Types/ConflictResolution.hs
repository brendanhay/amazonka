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
-- Module      : Amazonka.CodeCommit.Types.ConflictResolution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ConflictResolution where

import Amazonka.CodeCommit.Types.DeleteFileEntry
import Amazonka.CodeCommit.Types.ReplaceContentEntry
import Amazonka.CodeCommit.Types.SetFileModeEntry
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- /See:/ 'newConflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { -- | Files to be deleted as part of the merge conflict resolution.
    deleteFiles :: Prelude.Maybe [DeleteFileEntry],
    -- | Files to have content replaced as part of the merge conflict resolution.
    replaceContents :: Prelude.Maybe [ReplaceContentEntry],
    -- | File modes that are set as part of the merge conflict resolution.
    setFileModes :: Prelude.Maybe [SetFileModeEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'replaceContents', 'conflictResolution_replaceContents' - Files to have content replaced as part of the merge conflict resolution.
--
-- 'setFileModes', 'conflictResolution_setFileModes' - File modes that are set as part of the merge conflict resolution.
newConflictResolution ::
  ConflictResolution
newConflictResolution =
  ConflictResolution'
    { deleteFiles = Prelude.Nothing,
      replaceContents = Prelude.Nothing,
      setFileModes = Prelude.Nothing
    }

-- | Files to be deleted as part of the merge conflict resolution.
conflictResolution_deleteFiles :: Lens.Lens' ConflictResolution (Prelude.Maybe [DeleteFileEntry])
conflictResolution_deleteFiles = Lens.lens (\ConflictResolution' {deleteFiles} -> deleteFiles) (\s@ConflictResolution' {} a -> s {deleteFiles = a} :: ConflictResolution) Prelude.. Lens.mapping Lens.coerced

-- | Files to have content replaced as part of the merge conflict resolution.
conflictResolution_replaceContents :: Lens.Lens' ConflictResolution (Prelude.Maybe [ReplaceContentEntry])
conflictResolution_replaceContents = Lens.lens (\ConflictResolution' {replaceContents} -> replaceContents) (\s@ConflictResolution' {} a -> s {replaceContents = a} :: ConflictResolution) Prelude.. Lens.mapping Lens.coerced

-- | File modes that are set as part of the merge conflict resolution.
conflictResolution_setFileModes :: Lens.Lens' ConflictResolution (Prelude.Maybe [SetFileModeEntry])
conflictResolution_setFileModes = Lens.lens (\ConflictResolution' {setFileModes} -> setFileModes) (\s@ConflictResolution' {} a -> s {setFileModes = a} :: ConflictResolution) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ConflictResolution where
  hashWithSalt _salt ConflictResolution' {..} =
    _salt
      `Prelude.hashWithSalt` deleteFiles
      `Prelude.hashWithSalt` replaceContents
      `Prelude.hashWithSalt` setFileModes

instance Prelude.NFData ConflictResolution where
  rnf ConflictResolution' {..} =
    Prelude.rnf deleteFiles `Prelude.seq`
      Prelude.rnf replaceContents `Prelude.seq`
        Prelude.rnf setFileModes

instance Data.ToJSON ConflictResolution where
  toJSON ConflictResolution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deleteFiles" Data..=) Prelude.<$> deleteFiles,
            ("replaceContents" Data..=)
              Prelude.<$> replaceContents,
            ("setFileModes" Data..=) Prelude.<$> setFileModes
          ]
      )
