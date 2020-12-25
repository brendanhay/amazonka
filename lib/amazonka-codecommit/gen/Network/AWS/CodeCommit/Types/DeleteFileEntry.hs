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

import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A file that is deleted as part of a commit.
--
-- /See:/ 'mkDeleteFileEntry' smart constructor.
newtype DeleteFileEntry = DeleteFileEntry'
  { -- | The full path of the file to be deleted, including the name of the file.
    filePath :: Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFileEntry' value with any optional fields omitted.
mkDeleteFileEntry ::
  -- | 'filePath'
  Types.Path ->
  DeleteFileEntry
mkDeleteFileEntry filePath = DeleteFileEntry' {filePath}

-- | The full path of the file to be deleted, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeFilePath :: Lens.Lens' DeleteFileEntry Types.Path
dfeFilePath = Lens.field @"filePath"
{-# DEPRECATED dfeFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

instance Core.FromJSON DeleteFileEntry where
  toJSON DeleteFileEntry {..} =
    Core.object
      (Core.catMaybes [Core.Just ("filePath" Core..= filePath)])
