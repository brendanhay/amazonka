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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Conflict where

import Amazonka.CodeCommit.Types.ConflictMetadata
import Amazonka.CodeCommit.Types.MergeHunk
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about conflicts in a merge operation.
--
-- /See:/ 'newConflict' smart constructor.
data Conflict = Conflict'
  { -- | Metadata about a conflict in a merge operation.
    conflictMetadata :: Prelude.Maybe ConflictMetadata,
    -- | A list of hunks that contain the differences between files or lines
    -- causing the conflict.
    mergeHunks :: Prelude.Maybe [MergeHunk]
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
-- 'conflictMetadata', 'conflict_conflictMetadata' - Metadata about a conflict in a merge operation.
--
-- 'mergeHunks', 'conflict_mergeHunks' - A list of hunks that contain the differences between files or lines
-- causing the conflict.
newConflict ::
  Conflict
newConflict =
  Conflict'
    { conflictMetadata = Prelude.Nothing,
      mergeHunks = Prelude.Nothing
    }

-- | Metadata about a conflict in a merge operation.
conflict_conflictMetadata :: Lens.Lens' Conflict (Prelude.Maybe ConflictMetadata)
conflict_conflictMetadata = Lens.lens (\Conflict' {conflictMetadata} -> conflictMetadata) (\s@Conflict' {} a -> s {conflictMetadata = a} :: Conflict)

-- | A list of hunks that contain the differences between files or lines
-- causing the conflict.
conflict_mergeHunks :: Lens.Lens' Conflict (Prelude.Maybe [MergeHunk])
conflict_mergeHunks = Lens.lens (\Conflict' {mergeHunks} -> mergeHunks) (\s@Conflict' {} a -> s {mergeHunks = a} :: Conflict) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Conflict where
  parseJSON =
    Data.withObject
      "Conflict"
      ( \x ->
          Conflict'
            Prelude.<$> (x Data..:? "conflictMetadata")
            Prelude.<*> (x Data..:? "mergeHunks" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Conflict where
  hashWithSalt _salt Conflict' {..} =
    _salt `Prelude.hashWithSalt` conflictMetadata
      `Prelude.hashWithSalt` mergeHunks

instance Prelude.NFData Conflict where
  rnf Conflict' {..} =
    Prelude.rnf conflictMetadata
      `Prelude.seq` Prelude.rnf mergeHunks
