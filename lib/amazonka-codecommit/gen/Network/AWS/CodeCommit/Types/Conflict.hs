{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Conflict
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.Conflict
  ( Conflict (..)
  -- * Smart constructor
  , mkConflict
  -- * Lenses
  , cConflictMetadata
  , cMergeHunks
  ) where

import qualified Network.AWS.CodeCommit.Types.ConflictMetadata as Types
import qualified Network.AWS.CodeCommit.Types.MergeHunk as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about conflicts in a merge operation.
--
-- /See:/ 'mkConflict' smart constructor.
data Conflict = Conflict'
  { conflictMetadata :: Core.Maybe Types.ConflictMetadata
    -- ^ Metadata about a conflict in a merge operation.
  , mergeHunks :: Core.Maybe [Types.MergeHunk]
    -- ^ A list of hunks that contain the differences between files or lines causing the conflict.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Conflict' value with any optional fields omitted.
mkConflict
    :: Conflict
mkConflict
  = Conflict'{conflictMetadata = Core.Nothing,
              mergeHunks = Core.Nothing}

-- | Metadata about a conflict in a merge operation.
--
-- /Note:/ Consider using 'conflictMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConflictMetadata :: Lens.Lens' Conflict (Core.Maybe Types.ConflictMetadata)
cConflictMetadata = Lens.field @"conflictMetadata"
{-# INLINEABLE cConflictMetadata #-}
{-# DEPRECATED conflictMetadata "Use generic-lens or generic-optics with 'conflictMetadata' instead"  #-}

-- | A list of hunks that contain the differences between files or lines causing the conflict.
--
-- /Note:/ Consider using 'mergeHunks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMergeHunks :: Lens.Lens' Conflict (Core.Maybe [Types.MergeHunk])
cMergeHunks = Lens.field @"mergeHunks"
{-# INLINEABLE cMergeHunks #-}
{-# DEPRECATED mergeHunks "Use generic-lens or generic-optics with 'mergeHunks' instead"  #-}

instance Core.FromJSON Conflict where
        parseJSON
          = Core.withObject "Conflict" Core.$
              \ x ->
                Conflict' Core.<$>
                  (x Core..:? "conflictMetadata") Core.<*> x Core..:? "mergeHunks"
