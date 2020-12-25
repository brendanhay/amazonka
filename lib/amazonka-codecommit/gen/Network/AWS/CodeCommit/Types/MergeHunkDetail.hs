{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeHunkDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeHunkDetail
  ( MergeHunkDetail (..),

    -- * Smart constructor
    mkMergeHunkDetail,

    -- * Lenses
    mhdEndLine,
    mhdHunkContent,
    mhdStartLine,
  )
where

import qualified Network.AWS.CodeCommit.Types.HunkContent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the details of a merge hunk that contains a conflict in a merge or pull request operation.
--
-- /See:/ 'mkMergeHunkDetail' smart constructor.
data MergeHunkDetail = MergeHunkDetail'
  { -- | The end position of the hunk in the merge result.
    endLine :: Core.Maybe Core.Int,
    -- | The base-64 encoded content of the hunk merged region that might contain a conflict.
    hunkContent :: Core.Maybe Types.HunkContent,
    -- | The start position of the hunk in the merge result.
    startLine :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeHunkDetail' value with any optional fields omitted.
mkMergeHunkDetail ::
  MergeHunkDetail
mkMergeHunkDetail =
  MergeHunkDetail'
    { endLine = Core.Nothing,
      hunkContent = Core.Nothing,
      startLine = Core.Nothing
    }

-- | The end position of the hunk in the merge result.
--
-- /Note:/ Consider using 'endLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhdEndLine :: Lens.Lens' MergeHunkDetail (Core.Maybe Core.Int)
mhdEndLine = Lens.field @"endLine"
{-# DEPRECATED mhdEndLine "Use generic-lens or generic-optics with 'endLine' instead." #-}

-- | The base-64 encoded content of the hunk merged region that might contain a conflict.
--
-- /Note:/ Consider using 'hunkContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhdHunkContent :: Lens.Lens' MergeHunkDetail (Core.Maybe Types.HunkContent)
mhdHunkContent = Lens.field @"hunkContent"
{-# DEPRECATED mhdHunkContent "Use generic-lens or generic-optics with 'hunkContent' instead." #-}

-- | The start position of the hunk in the merge result.
--
-- /Note:/ Consider using 'startLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhdStartLine :: Lens.Lens' MergeHunkDetail (Core.Maybe Core.Int)
mhdStartLine = Lens.field @"startLine"
{-# DEPRECATED mhdStartLine "Use generic-lens or generic-optics with 'startLine' instead." #-}

instance Core.FromJSON MergeHunkDetail where
  parseJSON =
    Core.withObject "MergeHunkDetail" Core.$
      \x ->
        MergeHunkDetail'
          Core.<$> (x Core..:? "endLine")
          Core.<*> (x Core..:? "hunkContent")
          Core.<*> (x Core..:? "startLine")
