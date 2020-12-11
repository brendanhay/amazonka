-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Folder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Folder
  ( Folder (..),

    -- * Smart constructor
    mkFolder,

    -- * Lenses
    folAbsolutePath,
    folTreeId,
    folRelativePath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a folder in a repository.
--
-- /See:/ 'mkFolder' smart constructor.
data Folder = Folder'
  { absolutePath :: Lude.Maybe Lude.Text,
    treeId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'Folder' with the minimum fields required to make a request.
--
-- * 'absolutePath' - The fully qualified path of the folder in the repository.
-- * 'relativePath' - The relative path of the specified folder from the folder where the query originated.
-- * 'treeId' - The full SHA-1 pointer of the tree information for the commit that contains the folder.
mkFolder ::
  Folder
mkFolder =
  Folder'
    { absolutePath = Lude.Nothing,
      treeId = Lude.Nothing,
      relativePath = Lude.Nothing
    }

-- | The fully qualified path of the folder in the repository.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
folAbsolutePath :: Lens.Lens' Folder (Lude.Maybe Lude.Text)
folAbsolutePath = Lens.lens (absolutePath :: Folder -> Lude.Maybe Lude.Text) (\s a -> s {absolutePath = a} :: Folder)
{-# DEPRECATED folAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
folTreeId :: Lens.Lens' Folder (Lude.Maybe Lude.Text)
folTreeId = Lens.lens (treeId :: Folder -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: Folder)
{-# DEPRECATED folTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The relative path of the specified folder from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
folRelativePath :: Lens.Lens' Folder (Lude.Maybe Lude.Text)
folRelativePath = Lens.lens (relativePath :: Folder -> Lude.Maybe Lude.Text) (\s a -> s {relativePath = a} :: Folder)
{-# DEPRECATED folRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Lude.FromJSON Folder where
  parseJSON =
    Lude.withObject
      "Folder"
      ( \x ->
          Folder'
            Lude.<$> (x Lude..:? "absolutePath")
            Lude.<*> (x Lude..:? "treeId")
            Lude.<*> (x Lude..:? "relativePath")
      )
