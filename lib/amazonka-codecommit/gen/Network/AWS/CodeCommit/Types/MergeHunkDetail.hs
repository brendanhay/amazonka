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
    mhdStartLine,
    mhdEndLine,
    mhdHunkContent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the details of a merge hunk that contains a conflict in a merge or pull request operation.
--
-- /See:/ 'mkMergeHunkDetail' smart constructor.
data MergeHunkDetail = MergeHunkDetail'
  { startLine ::
      Lude.Maybe Lude.Int,
    endLine :: Lude.Maybe Lude.Int,
    hunkContent :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeHunkDetail' with the minimum fields required to make a request.
--
-- * 'endLine' - The end position of the hunk in the merge result.
-- * 'hunkContent' - The base-64 encoded content of the hunk merged region that might contain a conflict.
-- * 'startLine' - The start position of the hunk in the merge result.
mkMergeHunkDetail ::
  MergeHunkDetail
mkMergeHunkDetail =
  MergeHunkDetail'
    { startLine = Lude.Nothing,
      endLine = Lude.Nothing,
      hunkContent = Lude.Nothing
    }

-- | The start position of the hunk in the merge result.
--
-- /Note:/ Consider using 'startLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhdStartLine :: Lens.Lens' MergeHunkDetail (Lude.Maybe Lude.Int)
mhdStartLine = Lens.lens (startLine :: MergeHunkDetail -> Lude.Maybe Lude.Int) (\s a -> s {startLine = a} :: MergeHunkDetail)
{-# DEPRECATED mhdStartLine "Use generic-lens or generic-optics with 'startLine' instead." #-}

-- | The end position of the hunk in the merge result.
--
-- /Note:/ Consider using 'endLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhdEndLine :: Lens.Lens' MergeHunkDetail (Lude.Maybe Lude.Int)
mhdEndLine = Lens.lens (endLine :: MergeHunkDetail -> Lude.Maybe Lude.Int) (\s a -> s {endLine = a} :: MergeHunkDetail)
{-# DEPRECATED mhdEndLine "Use generic-lens or generic-optics with 'endLine' instead." #-}

-- | The base-64 encoded content of the hunk merged region that might contain a conflict.
--
-- /Note:/ Consider using 'hunkContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhdHunkContent :: Lens.Lens' MergeHunkDetail (Lude.Maybe Lude.Text)
mhdHunkContent = Lens.lens (hunkContent :: MergeHunkDetail -> Lude.Maybe Lude.Text) (\s a -> s {hunkContent = a} :: MergeHunkDetail)
{-# DEPRECATED mhdHunkContent "Use generic-lens or generic-optics with 'hunkContent' instead." #-}

instance Lude.FromJSON MergeHunkDetail where
  parseJSON =
    Lude.withObject
      "MergeHunkDetail"
      ( \x ->
          MergeHunkDetail'
            Lude.<$> (x Lude..:? "startLine")
            Lude.<*> (x Lude..:? "endLine")
            Lude.<*> (x Lude..:? "hunkContent")
      )
