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
-- Module      : Network.AWS.CodeCommit.Types.MergeHunk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeHunk where

import Network.AWS.CodeCommit.Types.MergeHunkDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about merge hunks in a merge or pull request operation.
--
-- /See:/ 'newMergeHunk' smart constructor.
data MergeHunk = MergeHunk'
  { -- | Information about the merge hunk in the source of a merge or pull
    -- request.
    source :: Prelude.Maybe MergeHunkDetail,
    -- | A Boolean value indicating whether a combination of hunks contains a
    -- conflict. Conflicts occur when the same file or the same lines in a file
    -- were modified in both the source and destination of a merge or pull
    -- request. Valid values include true, false, and null. True when the hunk
    -- represents a conflict and one or more files contains a line conflict.
    -- File mode conflicts in a merge do not set this to true.
    isConflict :: Prelude.Maybe Prelude.Bool,
    -- | Information about the merge hunk in the destination of a merge or pull
    -- request.
    destination :: Prelude.Maybe MergeHunkDetail,
    -- | Information about the merge hunk in the base of a merge or pull request.
    base :: Prelude.Maybe MergeHunkDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MergeHunk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'mergeHunk_source' - Information about the merge hunk in the source of a merge or pull
-- request.
--
-- 'isConflict', 'mergeHunk_isConflict' - A Boolean value indicating whether a combination of hunks contains a
-- conflict. Conflicts occur when the same file or the same lines in a file
-- were modified in both the source and destination of a merge or pull
-- request. Valid values include true, false, and null. True when the hunk
-- represents a conflict and one or more files contains a line conflict.
-- File mode conflicts in a merge do not set this to true.
--
-- 'destination', 'mergeHunk_destination' - Information about the merge hunk in the destination of a merge or pull
-- request.
--
-- 'base', 'mergeHunk_base' - Information about the merge hunk in the base of a merge or pull request.
newMergeHunk ::
  MergeHunk
newMergeHunk =
  MergeHunk'
    { source = Prelude.Nothing,
      isConflict = Prelude.Nothing,
      destination = Prelude.Nothing,
      base = Prelude.Nothing
    }

-- | Information about the merge hunk in the source of a merge or pull
-- request.
mergeHunk_source :: Lens.Lens' MergeHunk (Prelude.Maybe MergeHunkDetail)
mergeHunk_source = Lens.lens (\MergeHunk' {source} -> source) (\s@MergeHunk' {} a -> s {source = a} :: MergeHunk)

-- | A Boolean value indicating whether a combination of hunks contains a
-- conflict. Conflicts occur when the same file or the same lines in a file
-- were modified in both the source and destination of a merge or pull
-- request. Valid values include true, false, and null. True when the hunk
-- represents a conflict and one or more files contains a line conflict.
-- File mode conflicts in a merge do not set this to true.
mergeHunk_isConflict :: Lens.Lens' MergeHunk (Prelude.Maybe Prelude.Bool)
mergeHunk_isConflict = Lens.lens (\MergeHunk' {isConflict} -> isConflict) (\s@MergeHunk' {} a -> s {isConflict = a} :: MergeHunk)

-- | Information about the merge hunk in the destination of a merge or pull
-- request.
mergeHunk_destination :: Lens.Lens' MergeHunk (Prelude.Maybe MergeHunkDetail)
mergeHunk_destination = Lens.lens (\MergeHunk' {destination} -> destination) (\s@MergeHunk' {} a -> s {destination = a} :: MergeHunk)

-- | Information about the merge hunk in the base of a merge or pull request.
mergeHunk_base :: Lens.Lens' MergeHunk (Prelude.Maybe MergeHunkDetail)
mergeHunk_base = Lens.lens (\MergeHunk' {base} -> base) (\s@MergeHunk' {} a -> s {base = a} :: MergeHunk)

instance Prelude.FromJSON MergeHunk where
  parseJSON =
    Prelude.withObject
      "MergeHunk"
      ( \x ->
          MergeHunk'
            Prelude.<$> (x Prelude..:? "source")
            Prelude.<*> (x Prelude..:? "isConflict")
            Prelude.<*> (x Prelude..:? "destination")
            Prelude.<*> (x Prelude..:? "base")
      )

instance Prelude.Hashable MergeHunk

instance Prelude.NFData MergeHunk
