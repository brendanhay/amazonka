{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeHunk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.MergeHunk
  ( MergeHunk (..)
  -- * Smart constructor
  , mkMergeHunk
  -- * Lenses
  , mhBase
  , mhDestination
  , mhIsConflict
  , mhSource
  ) where

import qualified Network.AWS.CodeCommit.Types.MergeHunkDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about merge hunks in a merge or pull request operation.
--
-- /See:/ 'mkMergeHunk' smart constructor.
data MergeHunk = MergeHunk'
  { base :: Core.Maybe Types.MergeHunkDetail
    -- ^ Information about the merge hunk in the base of a merge or pull request.
  , destination :: Core.Maybe Types.MergeHunkDetail
    -- ^ Information about the merge hunk in the destination of a merge or pull request.
  , isConflict :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether a combination of hunks contains a conflict. Conflicts occur when the same file or the same lines in a file were modified in both the source and destination of a merge or pull request. Valid values include true, false, and null. True when the hunk represents a conflict and one or more files contains a line conflict. File mode conflicts in a merge do not set this to true.
  , source :: Core.Maybe Types.MergeHunkDetail
    -- ^ Information about the merge hunk in the source of a merge or pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeHunk' value with any optional fields omitted.
mkMergeHunk
    :: MergeHunk
mkMergeHunk
  = MergeHunk'{base = Core.Nothing, destination = Core.Nothing,
               isConflict = Core.Nothing, source = Core.Nothing}

-- | Information about the merge hunk in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhBase :: Lens.Lens' MergeHunk (Core.Maybe Types.MergeHunkDetail)
mhBase = Lens.field @"base"
{-# INLINEABLE mhBase #-}
{-# DEPRECATED base "Use generic-lens or generic-optics with 'base' instead"  #-}

-- | Information about the merge hunk in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhDestination :: Lens.Lens' MergeHunk (Core.Maybe Types.MergeHunkDetail)
mhDestination = Lens.field @"destination"
{-# INLINEABLE mhDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | A Boolean value indicating whether a combination of hunks contains a conflict. Conflicts occur when the same file or the same lines in a file were modified in both the source and destination of a merge or pull request. Valid values include true, false, and null. True when the hunk represents a conflict and one or more files contains a line conflict. File mode conflicts in a merge do not set this to true.
--
-- /Note:/ Consider using 'isConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhIsConflict :: Lens.Lens' MergeHunk (Core.Maybe Core.Bool)
mhIsConflict = Lens.field @"isConflict"
{-# INLINEABLE mhIsConflict #-}
{-# DEPRECATED isConflict "Use generic-lens or generic-optics with 'isConflict' instead"  #-}

-- | Information about the merge hunk in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhSource :: Lens.Lens' MergeHunk (Core.Maybe Types.MergeHunkDetail)
mhSource = Lens.field @"source"
{-# INLINEABLE mhSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.FromJSON MergeHunk where
        parseJSON
          = Core.withObject "MergeHunk" Core.$
              \ x ->
                MergeHunk' Core.<$>
                  (x Core..:? "base") Core.<*> x Core..:? "destination" Core.<*>
                    x Core..:? "isConflict"
                    Core.<*> x Core..:? "source"
