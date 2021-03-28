{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.MergeMetadata
  ( MergeMetadata (..)
  -- * Smart constructor
  , mkMergeMetadata
  -- * Lenses
  , mmIsMerged
  , mmMergeCommitId
  , mmMergeOption
  , mmMergedBy
  ) where

import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.MergeOptionTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.MergedBy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.
--
-- /See:/ 'mkMergeMetadata' smart constructor.
data MergeMetadata = MergeMetadata'
  { isMerged :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the merge has been made.
  , mergeCommitId :: Core.Maybe Types.CommitId
    -- ^ The commit ID for the merge commit, if any.
  , mergeOption :: Core.Maybe Types.MergeOptionTypeEnum
    -- ^ The merge strategy used in the merge.
  , mergedBy :: Core.Maybe Types.MergedBy
    -- ^ The Amazon Resource Name (ARN) of the user who merged the branches.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeMetadata' value with any optional fields omitted.
mkMergeMetadata
    :: MergeMetadata
mkMergeMetadata
  = MergeMetadata'{isMerged = Core.Nothing,
                   mergeCommitId = Core.Nothing, mergeOption = Core.Nothing,
                   mergedBy = Core.Nothing}

-- | A Boolean value indicating whether the merge has been made.
--
-- /Note:/ Consider using 'isMerged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmIsMerged :: Lens.Lens' MergeMetadata (Core.Maybe Core.Bool)
mmIsMerged = Lens.field @"isMerged"
{-# INLINEABLE mmIsMerged #-}
{-# DEPRECATED isMerged "Use generic-lens or generic-optics with 'isMerged' instead"  #-}

-- | The commit ID for the merge commit, if any.
--
-- /Note:/ Consider using 'mergeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmMergeCommitId :: Lens.Lens' MergeMetadata (Core.Maybe Types.CommitId)
mmMergeCommitId = Lens.field @"mergeCommitId"
{-# INLINEABLE mmMergeCommitId #-}
{-# DEPRECATED mergeCommitId "Use generic-lens or generic-optics with 'mergeCommitId' instead"  #-}

-- | The merge strategy used in the merge.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmMergeOption :: Lens.Lens' MergeMetadata (Core.Maybe Types.MergeOptionTypeEnum)
mmMergeOption = Lens.field @"mergeOption"
{-# INLINEABLE mmMergeOption #-}
{-# DEPRECATED mergeOption "Use generic-lens or generic-optics with 'mergeOption' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user who merged the branches.
--
-- /Note:/ Consider using 'mergedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmMergedBy :: Lens.Lens' MergeMetadata (Core.Maybe Types.MergedBy)
mmMergedBy = Lens.field @"mergedBy"
{-# INLINEABLE mmMergedBy #-}
{-# DEPRECATED mergedBy "Use generic-lens or generic-optics with 'mergedBy' instead"  #-}

instance Core.FromJSON MergeMetadata where
        parseJSON
          = Core.withObject "MergeMetadata" Core.$
              \ x ->
                MergeMetadata' Core.<$>
                  (x Core..:? "isMerged") Core.<*> x Core..:? "mergeCommitId"
                    Core.<*> x Core..:? "mergeOption"
                    Core.<*> x Core..:? "mergedBy"
