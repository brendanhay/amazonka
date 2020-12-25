{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictMetadata
  ( ConflictMetadata (..),

    -- * Smart constructor
    mkConflictMetadata,

    -- * Lenses
    cmContentConflict,
    cmFileModeConflict,
    cmFileModes,
    cmFilePath,
    cmFileSizes,
    cmIsBinaryFile,
    cmMergeOperations,
    cmNumberOfConflicts,
    cmObjectTypeConflict,
    cmObjectTypes,
  )
where

import qualified Network.AWS.CodeCommit.Types.FileModes as Types
import qualified Network.AWS.CodeCommit.Types.FileSizes as Types
import qualified Network.AWS.CodeCommit.Types.IsBinaryFile as Types
import qualified Network.AWS.CodeCommit.Types.MergeOperations as Types
import qualified Network.AWS.CodeCommit.Types.ObjectTypes as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the metadata for a conflict in a merge operation.
--
-- /See:/ 'mkConflictMetadata' smart constructor.
data ConflictMetadata = ConflictMetadata'
  { -- | A boolean value indicating whether there are conflicts in the content of a file.
    contentConflict :: Core.Maybe Core.Bool,
    -- | A boolean value indicating whether there are conflicts in the file mode of a file.
    fileModeConflict :: Core.Maybe Core.Bool,
    -- | The file modes of the file in the source, destination, and base of the merge.
    fileModes :: Core.Maybe Types.FileModes,
    -- | The path of the file that contains conflicts.
    filePath :: Core.Maybe Types.Path,
    -- | The file sizes of the file in the source, destination, and base of the merge.
    fileSizes :: Core.Maybe Types.FileSizes,
    -- | A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
    isBinaryFile :: Core.Maybe Types.IsBinaryFile,
    -- | Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
    mergeOperations :: Core.Maybe Types.MergeOperations,
    -- | The number of conflicts, including both hunk conflicts and metadata conflicts.
    numberOfConflicts :: Core.Maybe Core.Int,
    -- | A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
    objectTypeConflict :: Core.Maybe Core.Bool,
    -- | Information about any object type conflicts in a merge operation.
    objectTypes :: Core.Maybe Types.ObjectTypes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConflictMetadata' value with any optional fields omitted.
mkConflictMetadata ::
  ConflictMetadata
mkConflictMetadata =
  ConflictMetadata'
    { contentConflict = Core.Nothing,
      fileModeConflict = Core.Nothing,
      fileModes = Core.Nothing,
      filePath = Core.Nothing,
      fileSizes = Core.Nothing,
      isBinaryFile = Core.Nothing,
      mergeOperations = Core.Nothing,
      numberOfConflicts = Core.Nothing,
      objectTypeConflict = Core.Nothing,
      objectTypes = Core.Nothing
    }

-- | A boolean value indicating whether there are conflicts in the content of a file.
--
-- /Note:/ Consider using 'contentConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContentConflict :: Lens.Lens' ConflictMetadata (Core.Maybe Core.Bool)
cmContentConflict = Lens.field @"contentConflict"
{-# DEPRECATED cmContentConflict "Use generic-lens or generic-optics with 'contentConflict' instead." #-}

-- | A boolean value indicating whether there are conflicts in the file mode of a file.
--
-- /Note:/ Consider using 'fileModeConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFileModeConflict :: Lens.Lens' ConflictMetadata (Core.Maybe Core.Bool)
cmFileModeConflict = Lens.field @"fileModeConflict"
{-# DEPRECATED cmFileModeConflict "Use generic-lens or generic-optics with 'fileModeConflict' instead." #-}

-- | The file modes of the file in the source, destination, and base of the merge.
--
-- /Note:/ Consider using 'fileModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFileModes :: Lens.Lens' ConflictMetadata (Core.Maybe Types.FileModes)
cmFileModes = Lens.field @"fileModes"
{-# DEPRECATED cmFileModes "Use generic-lens or generic-optics with 'fileModes' instead." #-}

-- | The path of the file that contains conflicts.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFilePath :: Lens.Lens' ConflictMetadata (Core.Maybe Types.Path)
cmFilePath = Lens.field @"filePath"
{-# DEPRECATED cmFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The file sizes of the file in the source, destination, and base of the merge.
--
-- /Note:/ Consider using 'fileSizes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFileSizes :: Lens.Lens' ConflictMetadata (Core.Maybe Types.FileSizes)
cmFileSizes = Lens.field @"fileSizes"
{-# DEPRECATED cmFileSizes "Use generic-lens or generic-optics with 'fileSizes' instead." #-}

-- | A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
--
-- /Note:/ Consider using 'isBinaryFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmIsBinaryFile :: Lens.Lens' ConflictMetadata (Core.Maybe Types.IsBinaryFile)
cmIsBinaryFile = Lens.field @"isBinaryFile"
{-# DEPRECATED cmIsBinaryFile "Use generic-lens or generic-optics with 'isBinaryFile' instead." #-}

-- | Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
--
-- /Note:/ Consider using 'mergeOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMergeOperations :: Lens.Lens' ConflictMetadata (Core.Maybe Types.MergeOperations)
cmMergeOperations = Lens.field @"mergeOperations"
{-# DEPRECATED cmMergeOperations "Use generic-lens or generic-optics with 'mergeOperations' instead." #-}

-- | The number of conflicts, including both hunk conflicts and metadata conflicts.
--
-- /Note:/ Consider using 'numberOfConflicts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfConflicts :: Lens.Lens' ConflictMetadata (Core.Maybe Core.Int)
cmNumberOfConflicts = Lens.field @"numberOfConflicts"
{-# DEPRECATED cmNumberOfConflicts "Use generic-lens or generic-optics with 'numberOfConflicts' instead." #-}

-- | A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
--
-- /Note:/ Consider using 'objectTypeConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmObjectTypeConflict :: Lens.Lens' ConflictMetadata (Core.Maybe Core.Bool)
cmObjectTypeConflict = Lens.field @"objectTypeConflict"
{-# DEPRECATED cmObjectTypeConflict "Use generic-lens or generic-optics with 'objectTypeConflict' instead." #-}

-- | Information about any object type conflicts in a merge operation.
--
-- /Note:/ Consider using 'objectTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmObjectTypes :: Lens.Lens' ConflictMetadata (Core.Maybe Types.ObjectTypes)
cmObjectTypes = Lens.field @"objectTypes"
{-# DEPRECATED cmObjectTypes "Use generic-lens or generic-optics with 'objectTypes' instead." #-}

instance Core.FromJSON ConflictMetadata where
  parseJSON =
    Core.withObject "ConflictMetadata" Core.$
      \x ->
        ConflictMetadata'
          Core.<$> (x Core..:? "contentConflict")
          Core.<*> (x Core..:? "fileModeConflict")
          Core.<*> (x Core..:? "fileModes")
          Core.<*> (x Core..:? "filePath")
          Core.<*> (x Core..:? "fileSizes")
          Core.<*> (x Core..:? "isBinaryFile")
          Core.<*> (x Core..:? "mergeOperations")
          Core.<*> (x Core..:? "numberOfConflicts")
          Core.<*> (x Core..:? "objectTypeConflict")
          Core.<*> (x Core..:? "objectTypes")
