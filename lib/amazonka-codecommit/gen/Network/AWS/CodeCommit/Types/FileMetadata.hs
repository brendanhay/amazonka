{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileMetadata
  ( FileMetadata (..),

    -- * Smart constructor
    mkFileMetadata,

    -- * Lenses
    fmAbsolutePath,
    fmBlobId,
    fmFileMode,
  )
where

import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A file to be added, updated, or deleted as part of a commit.
--
-- /See:/ 'mkFileMetadata' smart constructor.
data FileMetadata = FileMetadata'
  { -- | The full path to the file to be added or updated, including the name of the file.
    absolutePath :: Core.Maybe Types.Path,
    -- | The blob ID that contains the file information.
    blobId :: Core.Maybe Types.ObjectId,
    -- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
    fileMode :: Core.Maybe Types.FileModeTypeEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileMetadata' value with any optional fields omitted.
mkFileMetadata ::
  FileMetadata
mkFileMetadata =
  FileMetadata'
    { absolutePath = Core.Nothing,
      blobId = Core.Nothing,
      fileMode = Core.Nothing
    }

-- | The full path to the file to be added or updated, including the name of the file.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmAbsolutePath :: Lens.Lens' FileMetadata (Core.Maybe Types.Path)
fmAbsolutePath = Lens.field @"absolutePath"
{-# DEPRECATED fmAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The blob ID that contains the file information.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmBlobId :: Lens.Lens' FileMetadata (Core.Maybe Types.ObjectId)
fmBlobId = Lens.field @"blobId"
{-# DEPRECATED fmBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmFileMode :: Lens.Lens' FileMetadata (Core.Maybe Types.FileModeTypeEnum)
fmFileMode = Lens.field @"fileMode"
{-# DEPRECATED fmFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

instance Core.FromJSON FileMetadata where
  parseJSON =
    Core.withObject "FileMetadata" Core.$
      \x ->
        FileMetadata'
          Core.<$> (x Core..:? "absolutePath")
          Core.<*> (x Core..:? "blobId")
          Core.<*> (x Core..:? "fileMode")
