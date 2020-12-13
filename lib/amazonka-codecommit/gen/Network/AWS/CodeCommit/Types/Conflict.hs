{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Conflict
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Conflict
  ( Conflict (..),

    -- * Smart constructor
    mkConflict,

    -- * Lenses
    cMergeHunks,
    cConflictMetadata,
  )
where

import Network.AWS.CodeCommit.Types.ConflictMetadata
import Network.AWS.CodeCommit.Types.MergeHunk
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about conflicts in a merge operation.
--
-- /See:/ 'mkConflict' smart constructor.
data Conflict = Conflict'
  { -- | A list of hunks that contain the differences between files or lines causing the conflict.
    mergeHunks :: Lude.Maybe [MergeHunk],
    -- | Metadata about a conflict in a merge operation.
    conflictMetadata :: Lude.Maybe ConflictMetadata
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Conflict' with the minimum fields required to make a request.
--
-- * 'mergeHunks' - A list of hunks that contain the differences between files or lines causing the conflict.
-- * 'conflictMetadata' - Metadata about a conflict in a merge operation.
mkConflict ::
  Conflict
mkConflict =
  Conflict'
    { mergeHunks = Lude.Nothing,
      conflictMetadata = Lude.Nothing
    }

-- | A list of hunks that contain the differences between files or lines causing the conflict.
--
-- /Note:/ Consider using 'mergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMergeHunks :: Lens.Lens' Conflict (Lude.Maybe [MergeHunk])
cMergeHunks = Lens.lens (mergeHunks :: Conflict -> Lude.Maybe [MergeHunk]) (\s a -> s {mergeHunks = a} :: Conflict)
{-# DEPRECATED cMergeHunks "Use generic-lens or generic-optics with 'mergeHunks' instead." #-}

-- | Metadata about a conflict in a merge operation.
--
-- /Note:/ Consider using 'conflictMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConflictMetadata :: Lens.Lens' Conflict (Lude.Maybe ConflictMetadata)
cConflictMetadata = Lens.lens (conflictMetadata :: Conflict -> Lude.Maybe ConflictMetadata) (\s a -> s {conflictMetadata = a} :: Conflict)
{-# DEPRECATED cConflictMetadata "Use generic-lens or generic-optics with 'conflictMetadata' instead." #-}

instance Lude.FromJSON Conflict where
  parseJSON =
    Lude.withObject
      "Conflict"
      ( \x ->
          Conflict'
            Lude.<$> (x Lude..:? "mergeHunks" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "conflictMetadata")
      )
