{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeHunk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeHunk
  ( MergeHunk (..),

    -- * Smart constructor
    mkMergeHunk,

    -- * Lenses
    mhDestination,
    mhBase,
    mhIsConflict,
    mhSource,
  )
where

import Network.AWS.CodeCommit.Types.MergeHunkDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about merge hunks in a merge or pull request operation.
--
-- /See:/ 'mkMergeHunk' smart constructor.
data MergeHunk = MergeHunk'
  { destination ::
      Lude.Maybe MergeHunkDetail,
    base :: Lude.Maybe MergeHunkDetail,
    isConflict :: Lude.Maybe Lude.Bool,
    source :: Lude.Maybe MergeHunkDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeHunk' with the minimum fields required to make a request.
--
-- * 'base' - Information about the merge hunk in the base of a merge or pull request.
-- * 'destination' - Information about the merge hunk in the destination of a merge or pull request.
-- * 'isConflict' - A Boolean value indicating whether a combination of hunks contains a conflict. Conflicts occur when the same file or the same lines in a file were modified in both the source and destination of a merge or pull request. Valid values include true, false, and null. True when the hunk represents a conflict and one or more files contains a line conflict. File mode conflicts in a merge do not set this to true.
-- * 'source' - Information about the merge hunk in the source of a merge or pull request.
mkMergeHunk ::
  MergeHunk
mkMergeHunk =
  MergeHunk'
    { destination = Lude.Nothing,
      base = Lude.Nothing,
      isConflict = Lude.Nothing,
      source = Lude.Nothing
    }

-- | Information about the merge hunk in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhDestination :: Lens.Lens' MergeHunk (Lude.Maybe MergeHunkDetail)
mhDestination = Lens.lens (destination :: MergeHunk -> Lude.Maybe MergeHunkDetail) (\s a -> s {destination = a} :: MergeHunk)
{-# DEPRECATED mhDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Information about the merge hunk in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhBase :: Lens.Lens' MergeHunk (Lude.Maybe MergeHunkDetail)
mhBase = Lens.lens (base :: MergeHunk -> Lude.Maybe MergeHunkDetail) (\s a -> s {base = a} :: MergeHunk)
{-# DEPRECATED mhBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | A Boolean value indicating whether a combination of hunks contains a conflict. Conflicts occur when the same file or the same lines in a file were modified in both the source and destination of a merge or pull request. Valid values include true, false, and null. True when the hunk represents a conflict and one or more files contains a line conflict. File mode conflicts in a merge do not set this to true.
--
-- /Note:/ Consider using 'isConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhIsConflict :: Lens.Lens' MergeHunk (Lude.Maybe Lude.Bool)
mhIsConflict = Lens.lens (isConflict :: MergeHunk -> Lude.Maybe Lude.Bool) (\s a -> s {isConflict = a} :: MergeHunk)
{-# DEPRECATED mhIsConflict "Use generic-lens or generic-optics with 'isConflict' instead." #-}

-- | Information about the merge hunk in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhSource :: Lens.Lens' MergeHunk (Lude.Maybe MergeHunkDetail)
mhSource = Lens.lens (source :: MergeHunk -> Lude.Maybe MergeHunkDetail) (\s a -> s {source = a} :: MergeHunk)
{-# DEPRECATED mhSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON MergeHunk where
  parseJSON =
    Lude.withObject
      "MergeHunk"
      ( \x ->
          MergeHunk'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "base")
            Lude.<*> (x Lude..:? "isConflict")
            Lude.<*> (x Lude..:? "source")
      )
