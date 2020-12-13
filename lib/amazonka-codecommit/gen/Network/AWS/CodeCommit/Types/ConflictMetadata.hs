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
    cmNumberOfConflicts,
    cmContentConflict,
    cmFileSizes,
    cmFilePath,
    cmIsBinaryFile,
    cmFileModeConflict,
    cmObjectTypeConflict,
    cmMergeOperations,
    cmObjectTypes,
    cmFileModes,
  )
where

import Network.AWS.CodeCommit.Types.FileModes
import Network.AWS.CodeCommit.Types.FileSizes
import Network.AWS.CodeCommit.Types.IsBinaryFile
import Network.AWS.CodeCommit.Types.MergeOperations
import Network.AWS.CodeCommit.Types.ObjectTypes
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the metadata for a conflict in a merge operation.
--
-- /See:/ 'mkConflictMetadata' smart constructor.
data ConflictMetadata = ConflictMetadata'
  { -- | The number of conflicts, including both hunk conflicts and metadata conflicts.
    numberOfConflicts :: Lude.Maybe Lude.Int,
    -- | A boolean value indicating whether there are conflicts in the content of a file.
    contentConflict :: Lude.Maybe Lude.Bool,
    -- | The file sizes of the file in the source, destination, and base of the merge.
    fileSizes :: Lude.Maybe FileSizes,
    -- | The path of the file that contains conflicts.
    filePath :: Lude.Maybe Lude.Text,
    -- | A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
    isBinaryFile :: Lude.Maybe IsBinaryFile,
    -- | A boolean value indicating whether there are conflicts in the file mode of a file.
    fileModeConflict :: Lude.Maybe Lude.Bool,
    -- | A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
    objectTypeConflict :: Lude.Maybe Lude.Bool,
    -- | Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
    mergeOperations :: Lude.Maybe MergeOperations,
    -- | Information about any object type conflicts in a merge operation.
    objectTypes :: Lude.Maybe ObjectTypes,
    -- | The file modes of the file in the source, destination, and base of the merge.
    fileModes :: Lude.Maybe FileModes
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConflictMetadata' with the minimum fields required to make a request.
--
-- * 'numberOfConflicts' - The number of conflicts, including both hunk conflicts and metadata conflicts.
-- * 'contentConflict' - A boolean value indicating whether there are conflicts in the content of a file.
-- * 'fileSizes' - The file sizes of the file in the source, destination, and base of the merge.
-- * 'filePath' - The path of the file that contains conflicts.
-- * 'isBinaryFile' - A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
-- * 'fileModeConflict' - A boolean value indicating whether there are conflicts in the file mode of a file.
-- * 'objectTypeConflict' - A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
-- * 'mergeOperations' - Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
-- * 'objectTypes' - Information about any object type conflicts in a merge operation.
-- * 'fileModes' - The file modes of the file in the source, destination, and base of the merge.
mkConflictMetadata ::
  ConflictMetadata
mkConflictMetadata =
  ConflictMetadata'
    { numberOfConflicts = Lude.Nothing,
      contentConflict = Lude.Nothing,
      fileSizes = Lude.Nothing,
      filePath = Lude.Nothing,
      isBinaryFile = Lude.Nothing,
      fileModeConflict = Lude.Nothing,
      objectTypeConflict = Lude.Nothing,
      mergeOperations = Lude.Nothing,
      objectTypes = Lude.Nothing,
      fileModes = Lude.Nothing
    }

-- | The number of conflicts, including both hunk conflicts and metadata conflicts.
--
-- /Note:/ Consider using 'numberOfConflicts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfConflicts :: Lens.Lens' ConflictMetadata (Lude.Maybe Lude.Int)
cmNumberOfConflicts = Lens.lens (numberOfConflicts :: ConflictMetadata -> Lude.Maybe Lude.Int) (\s a -> s {numberOfConflicts = a} :: ConflictMetadata)
{-# DEPRECATED cmNumberOfConflicts "Use generic-lens or generic-optics with 'numberOfConflicts' instead." #-}

-- | A boolean value indicating whether there are conflicts in the content of a file.
--
-- /Note:/ Consider using 'contentConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContentConflict :: Lens.Lens' ConflictMetadata (Lude.Maybe Lude.Bool)
cmContentConflict = Lens.lens (contentConflict :: ConflictMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {contentConflict = a} :: ConflictMetadata)
{-# DEPRECATED cmContentConflict "Use generic-lens or generic-optics with 'contentConflict' instead." #-}

-- | The file sizes of the file in the source, destination, and base of the merge.
--
-- /Note:/ Consider using 'fileSizes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFileSizes :: Lens.Lens' ConflictMetadata (Lude.Maybe FileSizes)
cmFileSizes = Lens.lens (fileSizes :: ConflictMetadata -> Lude.Maybe FileSizes) (\s a -> s {fileSizes = a} :: ConflictMetadata)
{-# DEPRECATED cmFileSizes "Use generic-lens or generic-optics with 'fileSizes' instead." #-}

-- | The path of the file that contains conflicts.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFilePath :: Lens.Lens' ConflictMetadata (Lude.Maybe Lude.Text)
cmFilePath = Lens.lens (filePath :: ConflictMetadata -> Lude.Maybe Lude.Text) (\s a -> s {filePath = a} :: ConflictMetadata)
{-# DEPRECATED cmFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
--
-- /Note:/ Consider using 'isBinaryFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmIsBinaryFile :: Lens.Lens' ConflictMetadata (Lude.Maybe IsBinaryFile)
cmIsBinaryFile = Lens.lens (isBinaryFile :: ConflictMetadata -> Lude.Maybe IsBinaryFile) (\s a -> s {isBinaryFile = a} :: ConflictMetadata)
{-# DEPRECATED cmIsBinaryFile "Use generic-lens or generic-optics with 'isBinaryFile' instead." #-}

-- | A boolean value indicating whether there are conflicts in the file mode of a file.
--
-- /Note:/ Consider using 'fileModeConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFileModeConflict :: Lens.Lens' ConflictMetadata (Lude.Maybe Lude.Bool)
cmFileModeConflict = Lens.lens (fileModeConflict :: ConflictMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {fileModeConflict = a} :: ConflictMetadata)
{-# DEPRECATED cmFileModeConflict "Use generic-lens or generic-optics with 'fileModeConflict' instead." #-}

-- | A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
--
-- /Note:/ Consider using 'objectTypeConflict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmObjectTypeConflict :: Lens.Lens' ConflictMetadata (Lude.Maybe Lude.Bool)
cmObjectTypeConflict = Lens.lens (objectTypeConflict :: ConflictMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {objectTypeConflict = a} :: ConflictMetadata)
{-# DEPRECATED cmObjectTypeConflict "Use generic-lens or generic-optics with 'objectTypeConflict' instead." #-}

-- | Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
--
-- /Note:/ Consider using 'mergeOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMergeOperations :: Lens.Lens' ConflictMetadata (Lude.Maybe MergeOperations)
cmMergeOperations = Lens.lens (mergeOperations :: ConflictMetadata -> Lude.Maybe MergeOperations) (\s a -> s {mergeOperations = a} :: ConflictMetadata)
{-# DEPRECATED cmMergeOperations "Use generic-lens or generic-optics with 'mergeOperations' instead." #-}

-- | Information about any object type conflicts in a merge operation.
--
-- /Note:/ Consider using 'objectTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmObjectTypes :: Lens.Lens' ConflictMetadata (Lude.Maybe ObjectTypes)
cmObjectTypes = Lens.lens (objectTypes :: ConflictMetadata -> Lude.Maybe ObjectTypes) (\s a -> s {objectTypes = a} :: ConflictMetadata)
{-# DEPRECATED cmObjectTypes "Use generic-lens or generic-optics with 'objectTypes' instead." #-}

-- | The file modes of the file in the source, destination, and base of the merge.
--
-- /Note:/ Consider using 'fileModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmFileModes :: Lens.Lens' ConflictMetadata (Lude.Maybe FileModes)
cmFileModes = Lens.lens (fileModes :: ConflictMetadata -> Lude.Maybe FileModes) (\s a -> s {fileModes = a} :: ConflictMetadata)
{-# DEPRECATED cmFileModes "Use generic-lens or generic-optics with 'fileModes' instead." #-}

instance Lude.FromJSON ConflictMetadata where
  parseJSON =
    Lude.withObject
      "ConflictMetadata"
      ( \x ->
          ConflictMetadata'
            Lude.<$> (x Lude..:? "numberOfConflicts")
            Lude.<*> (x Lude..:? "contentConflict")
            Lude.<*> (x Lude..:? "fileSizes")
            Lude.<*> (x Lude..:? "filePath")
            Lude.<*> (x Lude..:? "isBinaryFile")
            Lude.<*> (x Lude..:? "fileModeConflict")
            Lude.<*> (x Lude..:? "objectTypeConflict")
            Lude.<*> (x Lude..:? "mergeOperations")
            Lude.<*> (x Lude..:? "objectTypes")
            Lude.<*> (x Lude..:? "fileModes")
      )
