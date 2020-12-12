{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.DeleteFileEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.DeleteFileEntry
  ( DeleteFileEntry (..),

    -- * Smart constructor
    mkDeleteFileEntry,

    -- * Lenses
    dfeFilePath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A file that is deleted as part of a commit.
--
-- /See:/ 'mkDeleteFileEntry' smart constructor.
newtype DeleteFileEntry = DeleteFileEntry' {filePath :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileEntry' with the minimum fields required to make a request.
--
-- * 'filePath' - The full path of the file to be deleted, including the name of the file.
mkDeleteFileEntry ::
  -- | 'filePath'
  Lude.Text ->
  DeleteFileEntry
mkDeleteFileEntry pFilePath_ =
  DeleteFileEntry' {filePath = pFilePath_}

-- | The full path of the file to be deleted, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeFilePath :: Lens.Lens' DeleteFileEntry Lude.Text
dfeFilePath = Lens.lens (filePath :: DeleteFileEntry -> Lude.Text) (\s a -> s {filePath = a} :: DeleteFileEntry)
{-# DEPRECATED dfeFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

instance Lude.ToJSON DeleteFileEntry where
  toJSON DeleteFileEntry' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("filePath" Lude..= filePath)])
