{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    slBlobId,
    slFileMode,
    slRelativePath,
  )
where

import qualified Network.AWS.CodeCommit.Types.AbsolutePath as Types
import qualified Network.AWS.CodeCommit.Types.BlobId as Types
import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.RelativePath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a symbolic link in a repository folder.
--
-- /See:/ 'mkSymbolicLink' smart constructor.
data SymbolicLink = SymbolicLink'
  { -- | The fully qualified path to the folder that contains the symbolic link.
    absolutePath :: Core.Maybe Types.AbsolutePath,
    -- | The blob ID that contains the information about the symbolic link.
    blobId :: Core.Maybe Types.BlobId,
    -- | The file mode permissions of the blob that cotains information about the symbolic link.
    fileMode :: Core.Maybe Types.FileModeTypeEnum,
    -- | The relative path of the symbolic link from the folder where the query originated.
    relativePath :: Core.Maybe Types.RelativePath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SymbolicLink' value with any optional fields omitted.
mkSymbolicLink ::
  SymbolicLink
mkSymbolicLink =
  SymbolicLink'
    { absolutePath = Core.Nothing,
      blobId = Core.Nothing,
      fileMode = Core.Nothing,
      relativePath = Core.Nothing
    }

-- | The fully qualified path to the folder that contains the symbolic link.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slAbsolutePath :: Lens.Lens' SymbolicLink (Core.Maybe Types.AbsolutePath)
slAbsolutePath = Lens.field @"absolutePath"
{-# DEPRECATED slAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The blob ID that contains the information about the symbolic link.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBlobId :: Lens.Lens' SymbolicLink (Core.Maybe Types.BlobId)
slBlobId = Lens.field @"blobId"
{-# DEPRECATED slBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The file mode permissions of the blob that cotains information about the symbolic link.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slFileMode :: Lens.Lens' SymbolicLink (Core.Maybe Types.FileModeTypeEnum)
slFileMode = Lens.field @"fileMode"
{-# DEPRECATED slFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The relative path of the symbolic link from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slRelativePath :: Lens.Lens' SymbolicLink (Core.Maybe Types.RelativePath)
slRelativePath = Lens.field @"relativePath"
{-# DEPRECATED slRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Core.FromJSON SymbolicLink where
  parseJSON =
    Core.withObject "SymbolicLink" Core.$
      \x ->
        SymbolicLink'
          Core.<$> (x Core..:? "absolutePath")
          Core.<*> (x Core..:? "blobId")
          Core.<*> (x Core..:? "fileMode")
          Core.<*> (x Core..:? "relativePath")
