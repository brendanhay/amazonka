{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PutFileEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PutFileEntry
  ( PutFileEntry (..),

    -- * Smart constructor
    mkPutFileEntry,

    -- * Lenses
    pfeFilePath,
    pfeFileContent,
    pfeFileMode,
    pfeSourceFile,
  )
where

import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.CodeCommit.Types.SourceFileSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a file added or updated as part of a commit.
--
-- /See:/ 'mkPutFileEntry' smart constructor.
data PutFileEntry = PutFileEntry'
  { -- | The full path to the file in the repository, including the name of the file.
    filePath :: Types.Path,
    -- | The content of the file, if a source file is not specified.
    fileContent :: Core.Maybe Core.Base64,
    -- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
    fileMode :: Core.Maybe Types.FileModeTypeEnum,
    -- | The name and full path of the file that contains the changes you want to make as part of the commit, if you are not providing the file content directly.
    sourceFile :: Core.Maybe Types.SourceFileSpecifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFileEntry' value with any optional fields omitted.
mkPutFileEntry ::
  -- | 'filePath'
  Types.Path ->
  PutFileEntry
mkPutFileEntry filePath =
  PutFileEntry'
    { filePath,
      fileContent = Core.Nothing,
      fileMode = Core.Nothing,
      sourceFile = Core.Nothing
    }

-- | The full path to the file in the repository, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeFilePath :: Lens.Lens' PutFileEntry Types.Path
pfeFilePath = Lens.field @"filePath"
{-# DEPRECATED pfeFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The content of the file, if a source file is not specified.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeFileContent :: Lens.Lens' PutFileEntry (Core.Maybe Core.Base64)
pfeFileContent = Lens.field @"fileContent"
{-# DEPRECATED pfeFileContent "Use generic-lens or generic-optics with 'fileContent' instead." #-}

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeFileMode :: Lens.Lens' PutFileEntry (Core.Maybe Types.FileModeTypeEnum)
pfeFileMode = Lens.field @"fileMode"
{-# DEPRECATED pfeFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The name and full path of the file that contains the changes you want to make as part of the commit, if you are not providing the file content directly.
--
-- /Note:/ Consider using 'sourceFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeSourceFile :: Lens.Lens' PutFileEntry (Core.Maybe Types.SourceFileSpecifier)
pfeSourceFile = Lens.field @"sourceFile"
{-# DEPRECATED pfeSourceFile "Use generic-lens or generic-optics with 'sourceFile' instead." #-}

instance Core.FromJSON PutFileEntry where
  toJSON PutFileEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("filePath" Core..= filePath),
            ("fileContent" Core..=) Core.<$> fileContent,
            ("fileMode" Core..=) Core.<$> fileMode,
            ("sourceFile" Core..=) Core.<$> sourceFile
          ]
      )
