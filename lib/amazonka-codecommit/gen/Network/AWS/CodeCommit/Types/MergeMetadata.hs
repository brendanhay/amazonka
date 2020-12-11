-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeMetadata
  ( MergeMetadata (..),

    -- * Smart constructor
    mkMergeMetadata,

    -- * Lenses
    mmMergedBy,
    mmMergeOption,
    mmIsMerged,
    mmMergeCommitId,
  )
where

import Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.
--
-- /See:/ 'mkMergeMetadata' smart constructor.
data MergeMetadata = MergeMetadata'
  { mergedBy ::
      Lude.Maybe Lude.Text,
    mergeOption :: Lude.Maybe MergeOptionTypeEnum,
    isMerged :: Lude.Maybe Lude.Bool,
    mergeCommitId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeMetadata' with the minimum fields required to make a request.
--
-- * 'isMerged' - A Boolean value indicating whether the merge has been made.
-- * 'mergeCommitId' - The commit ID for the merge commit, if any.
-- * 'mergeOption' - The merge strategy used in the merge.
-- * 'mergedBy' - The Amazon Resource Name (ARN) of the user who merged the branches.
mkMergeMetadata ::
  MergeMetadata
mkMergeMetadata =
  MergeMetadata'
    { mergedBy = Lude.Nothing,
      mergeOption = Lude.Nothing,
      isMerged = Lude.Nothing,
      mergeCommitId = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user who merged the branches.
--
-- /Note:/ Consider using 'mergedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmMergedBy :: Lens.Lens' MergeMetadata (Lude.Maybe Lude.Text)
mmMergedBy = Lens.lens (mergedBy :: MergeMetadata -> Lude.Maybe Lude.Text) (\s a -> s {mergedBy = a} :: MergeMetadata)
{-# DEPRECATED mmMergedBy "Use generic-lens or generic-optics with 'mergedBy' instead." #-}

-- | The merge strategy used in the merge.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmMergeOption :: Lens.Lens' MergeMetadata (Lude.Maybe MergeOptionTypeEnum)
mmMergeOption = Lens.lens (mergeOption :: MergeMetadata -> Lude.Maybe MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: MergeMetadata)
{-# DEPRECATED mmMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

-- | A Boolean value indicating whether the merge has been made.
--
-- /Note:/ Consider using 'isMerged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmIsMerged :: Lens.Lens' MergeMetadata (Lude.Maybe Lude.Bool)
mmIsMerged = Lens.lens (isMerged :: MergeMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {isMerged = a} :: MergeMetadata)
{-# DEPRECATED mmIsMerged "Use generic-lens or generic-optics with 'isMerged' instead." #-}

-- | The commit ID for the merge commit, if any.
--
-- /Note:/ Consider using 'mergeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmMergeCommitId :: Lens.Lens' MergeMetadata (Lude.Maybe Lude.Text)
mmMergeCommitId = Lens.lens (mergeCommitId :: MergeMetadata -> Lude.Maybe Lude.Text) (\s a -> s {mergeCommitId = a} :: MergeMetadata)
{-# DEPRECATED mmMergeCommitId "Use generic-lens or generic-optics with 'mergeCommitId' instead." #-}

instance Lude.FromJSON MergeMetadata where
  parseJSON =
    Lude.withObject
      "MergeMetadata"
      ( \x ->
          MergeMetadata'
            Lude.<$> (x Lude..:? "mergedBy")
            Lude.<*> (x Lude..:? "mergeOption")
            Lude.<*> (x Lude..:? "isMerged")
            Lude.<*> (x Lude..:? "mergeCommitId")
      )
