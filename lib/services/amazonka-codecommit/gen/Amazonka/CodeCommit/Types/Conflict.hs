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
-- Module      : Amazonka.CodeCommit.Types.Conflict
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Conflict where

import Amazonka.CodeCommit.Types.ConflictMetadata
import Amazonka.CodeCommit.Types.MergeHunk
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about conflicts in a merge operation.
--
-- /See:/ 'newConflict' smart constructor.
data Conflict = Conflict'
  { -- | A list of hunks that contain the differences between files or lines
    -- causing the conflict.
    mergeHunks :: Prelude.Maybe [MergeHunk],
    -- | Metadata about a conflict in a merge operation.
    conflictMetadata :: Prelude.Maybe ConflictMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Conflict' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mergeHunks', 'conflict_mergeHunks' - A list of hunks that contain the differences between files or lines
-- causing the conflict.
--
-- 'conflictMetadata', 'conflict_conflictMetadata' - Metadata about a conflict in a merge operation.
newConflict ::
  Conflict
newConflict =
  Conflict'
    { mergeHunks = Prelude.Nothing,
      conflictMetadata = Prelude.Nothing
    }

-- | A list of hunks that contain the differences between files or lines
-- causing the conflict.
conflict_mergeHunks :: Lens.Lens' Conflict (Prelude.Maybe [MergeHunk])
conflict_mergeHunks = Lens.lens (\Conflict' {mergeHunks} -> mergeHunks) (\s@Conflict' {} a -> s {mergeHunks = a} :: Conflict) Prelude.. Lens.mapping Lens.coerced

-- | Metadata about a conflict in a merge operation.
conflict_conflictMetadata :: Lens.Lens' Conflict (Prelude.Maybe ConflictMetadata)
conflict_conflictMetadata = Lens.lens (\Conflict' {conflictMetadata} -> conflictMetadata) (\s@Conflict' {} a -> s {conflictMetadata = a} :: Conflict)

instance Core.FromJSON Conflict where
  parseJSON =
    Core.withObject
      "Conflict"
      ( \x ->
          Conflict'
            Prelude.<$> (x Core..:? "mergeHunks" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "conflictMetadata")
      )

instance Prelude.Hashable Conflict where
  hashWithSalt _salt Conflict' {..} =
    _salt `Prelude.hashWithSalt` mergeHunks
      `Prelude.hashWithSalt` conflictMetadata

instance Prelude.NFData Conflict where
  rnf Conflict' {..} =
    Prelude.rnf mergeHunks
      `Prelude.seq` Prelude.rnf conflictMetadata
