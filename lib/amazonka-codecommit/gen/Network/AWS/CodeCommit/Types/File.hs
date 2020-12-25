{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.File
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.File
  ( File (..),

    -- * Smart constructor
    mkFile,

    -- * Lenses
    fAbsolutePath,
    fBlobId,
    fFileMode,
    fRelativePath,
  )
where

import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a file in a repository.
--
-- /See:/ 'mkFile' smart constructor.
data File = File'
  { -- | The fully qualified path to the file in the repository.
    absolutePath :: Core.Maybe Types.Path,
    -- | The blob ID that contains the file information.
    blobId :: Core.Maybe Types.ObjectId,
    -- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
    fileMode :: Core.Maybe Types.FileModeTypeEnum,
    -- | The relative path of the file from the folder where the query originated.
    relativePath :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'File' value with any optional fields omitted.
mkFile ::
  File
mkFile =
  File'
    { absolutePath = Core.Nothing,
      blobId = Core.Nothing,
      fileMode = Core.Nothing,
      relativePath = Core.Nothing
    }

-- | The fully qualified path to the file in the repository.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAbsolutePath :: Lens.Lens' File (Core.Maybe Types.Path)
fAbsolutePath = Lens.field @"absolutePath"
{-# DEPRECATED fAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The blob ID that contains the file information.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fBlobId :: Lens.Lens' File (Core.Maybe Types.ObjectId)
fBlobId = Lens.field @"blobId"
{-# DEPRECATED fBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFileMode :: Lens.Lens' File (Core.Maybe Types.FileModeTypeEnum)
fFileMode = Lens.field @"fileMode"
{-# DEPRECATED fFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The relative path of the file from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRelativePath :: Lens.Lens' File (Core.Maybe Types.Path)
fRelativePath = Lens.field @"relativePath"
{-# DEPRECATED fRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Core.FromJSON File where
  parseJSON =
    Core.withObject "File" Core.$
      \x ->
        File'
          Core.<$> (x Core..:? "absolutePath")
          Core.<*> (x Core..:? "blobId")
          Core.<*> (x Core..:? "fileMode")
          Core.<*> (x Core..:? "relativePath")
