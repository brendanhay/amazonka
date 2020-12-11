-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SubModule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SubModule
  ( SubModule (..),

    -- * Smart constructor
    mkSubModule,

    -- * Lenses
    smCommitId,
    smAbsolutePath,
    smRelativePath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a submodule reference in a repository folder.
--
-- /See:/ 'mkSubModule' smart constructor.
data SubModule = SubModule'
  { commitId :: Lude.Maybe Lude.Text,
    absolutePath :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SubModule' with the minimum fields required to make a request.
--
-- * 'absolutePath' - The fully qualified path to the folder that contains the reference to the submodule.
-- * 'commitId' - The commit ID that contains the reference to the submodule.
-- * 'relativePath' - The relative path of the submodule from the folder where the query originated.
mkSubModule ::
  SubModule
mkSubModule =
  SubModule'
    { commitId = Lude.Nothing,
      absolutePath = Lude.Nothing,
      relativePath = Lude.Nothing
    }

-- | The commit ID that contains the reference to the submodule.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smCommitId :: Lens.Lens' SubModule (Lude.Maybe Lude.Text)
smCommitId = Lens.lens (commitId :: SubModule -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: SubModule)
{-# DEPRECATED smCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The fully qualified path to the folder that contains the reference to the submodule.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smAbsolutePath :: Lens.Lens' SubModule (Lude.Maybe Lude.Text)
smAbsolutePath = Lens.lens (absolutePath :: SubModule -> Lude.Maybe Lude.Text) (\s a -> s {absolutePath = a} :: SubModule)
{-# DEPRECATED smAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The relative path of the submodule from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smRelativePath :: Lens.Lens' SubModule (Lude.Maybe Lude.Text)
smRelativePath = Lens.lens (relativePath :: SubModule -> Lude.Maybe Lude.Text) (\s a -> s {relativePath = a} :: SubModule)
{-# DEPRECATED smRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Lude.FromJSON SubModule where
  parseJSON =
    Lude.withObject
      "SubModule"
      ( \x ->
          SubModule'
            Lude.<$> (x Lude..:? "commitId")
            Lude.<*> (x Lude..:? "absolutePath")
            Lude.<*> (x Lude..:? "relativePath")
      )
