{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ffAbsolutePath,
    ffRelativePath,
    ffTreeId,
  )
where

import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a folder in a repository.
--
-- /See:/ 'mkFolder' smart constructor.
data Folder = Folder'
  { -- | The fully qualified path of the folder in the repository.
    absolutePath :: Core.Maybe Types.Path,
    -- | The relative path of the specified folder from the folder where the query originated.
    relativePath :: Core.Maybe Types.Path,
    -- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
    treeId :: Core.Maybe Types.ObjectId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Folder' value with any optional fields omitted.
mkFolder ::
  Folder
mkFolder =
  Folder'
    { absolutePath = Core.Nothing,
      relativePath = Core.Nothing,
      treeId = Core.Nothing
    }

-- | The fully qualified path of the folder in the repository.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAbsolutePath :: Lens.Lens' Folder (Core.Maybe Types.Path)
ffAbsolutePath = Lens.field @"absolutePath"
{-# DEPRECATED ffAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The relative path of the specified folder from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffRelativePath :: Lens.Lens' Folder (Core.Maybe Types.Path)
ffRelativePath = Lens.field @"relativePath"
{-# DEPRECATED ffRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffTreeId :: Lens.Lens' Folder (Core.Maybe Types.ObjectId)
ffTreeId = Lens.field @"treeId"
{-# DEPRECATED ffTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

instance Core.FromJSON Folder where
  parseJSON =
    Core.withObject "Folder" Core.$
      \x ->
        Folder'
          Core.<$> (x Core..:? "absolutePath")
          Core.<*> (x Core..:? "relativePath")
          Core.<*> (x Core..:? "treeId")
