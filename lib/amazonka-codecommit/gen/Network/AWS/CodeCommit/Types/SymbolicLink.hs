-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SymbolicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SymbolicLink
  ( SymbolicLink (..),

    -- * Smart constructor
    mkSymbolicLink,

    -- * Lenses
    slAbsolutePath,
    slFileMode,
    slBlobId,
    slRelativePath,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a symbolic link in a repository folder.
--
-- /See:/ 'mkSymbolicLink' smart constructor.
data SymbolicLink = SymbolicLink'
  { absolutePath ::
      Lude.Maybe Lude.Text,
    fileMode :: Lude.Maybe FileModeTypeEnum,
    blobId :: Lude.Maybe Lude.Text,
    relativePath :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SymbolicLink' with the minimum fields required to make a request.
--
-- * 'absolutePath' - The fully qualified path to the folder that contains the symbolic link.
-- * 'blobId' - The blob ID that contains the information about the symbolic link.
-- * 'fileMode' - The file mode permissions of the blob that cotains information about the symbolic link.
-- * 'relativePath' - The relative path of the symbolic link from the folder where the query originated.
mkSymbolicLink ::
  SymbolicLink
mkSymbolicLink =
  SymbolicLink'
    { absolutePath = Lude.Nothing,
      fileMode = Lude.Nothing,
      blobId = Lude.Nothing,
      relativePath = Lude.Nothing
    }

-- | The fully qualified path to the folder that contains the symbolic link.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slAbsolutePath :: Lens.Lens' SymbolicLink (Lude.Maybe Lude.Text)
slAbsolutePath = Lens.lens (absolutePath :: SymbolicLink -> Lude.Maybe Lude.Text) (\s a -> s {absolutePath = a} :: SymbolicLink)
{-# DEPRECATED slAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The file mode permissions of the blob that cotains information about the symbolic link.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slFileMode :: Lens.Lens' SymbolicLink (Lude.Maybe FileModeTypeEnum)
slFileMode = Lens.lens (fileMode :: SymbolicLink -> Lude.Maybe FileModeTypeEnum) (\s a -> s {fileMode = a} :: SymbolicLink)
{-# DEPRECATED slFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The blob ID that contains the information about the symbolic link.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBlobId :: Lens.Lens' SymbolicLink (Lude.Maybe Lude.Text)
slBlobId = Lens.lens (blobId :: SymbolicLink -> Lude.Maybe Lude.Text) (\s a -> s {blobId = a} :: SymbolicLink)
{-# DEPRECATED slBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The relative path of the symbolic link from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slRelativePath :: Lens.Lens' SymbolicLink (Lude.Maybe Lude.Text)
slRelativePath = Lens.lens (relativePath :: SymbolicLink -> Lude.Maybe Lude.Text) (\s a -> s {relativePath = a} :: SymbolicLink)
{-# DEPRECATED slRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Lude.FromJSON SymbolicLink where
  parseJSON =
    Lude.withObject
      "SymbolicLink"
      ( \x ->
          SymbolicLink'
            Lude.<$> (x Lude..:? "absolutePath")
            Lude.<*> (x Lude..:? "fileMode")
            Lude.<*> (x Lude..:? "blobId")
            Lude.<*> (x Lude..:? "relativePath")
      )
