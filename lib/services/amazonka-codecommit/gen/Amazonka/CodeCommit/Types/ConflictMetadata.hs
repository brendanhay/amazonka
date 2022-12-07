{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeCommit.Types.ConflictMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ConflictMetadata where

import Amazonka.CodeCommit.Types.FileModes
import Amazonka.CodeCommit.Types.FileSizes
import Amazonka.CodeCommit.Types.IsBinaryFile
import Amazonka.CodeCommit.Types.MergeOperations
import Amazonka.CodeCommit.Types.ObjectTypes
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the metadata for a conflict in a merge operation.
--
-- /See:/ 'newConflictMetadata' smart constructor.
data ConflictMetadata = ConflictMetadata'
  { -- | The path of the file that contains conflicts.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | A boolean value indicating whether there are conflicts in the content of
    -- a file.
    contentConflict :: Prelude.Maybe Prelude.Bool,
    -- | The file sizes of the file in the source, destination, and base of the
    -- merge.
    fileSizes :: Prelude.Maybe FileSizes,
    -- | The number of conflicts, including both hunk conflicts and metadata
    -- conflicts.
    numberOfConflicts :: Prelude.Maybe Prelude.Int,
    -- | Whether an add, modify, or delete operation caused the conflict between
    -- the source and destination of the merge.
    mergeOperations :: Prelude.Maybe MergeOperations,
    -- | The file modes of the file in the source, destination, and base of the
    -- merge.
    fileModes :: Prelude.Maybe FileModes,
    -- | A boolean value (true or false) indicating whether the file is binary or
    -- textual in the source, destination, and base of the merge.
    isBinaryFile :: Prelude.Maybe IsBinaryFile,
    -- | A boolean value indicating whether there are conflicts in the file mode
    -- of a file.
    fileModeConflict :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value (true or false) indicating whether there are conflicts
    -- between the branches in the object type of a file, folder, or submodule.
    objectTypeConflict :: Prelude.Maybe Prelude.Bool,
    -- | Information about any object type conflicts in a merge operation.
    objectTypes :: Prelude.Maybe ObjectTypes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConflictMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePath', 'conflictMetadata_filePath' - The path of the file that contains conflicts.
--
-- 'contentConflict', 'conflictMetadata_contentConflict' - A boolean value indicating whether there are conflicts in the content of
-- a file.
--
-- 'fileSizes', 'conflictMetadata_fileSizes' - The file sizes of the file in the source, destination, and base of the
-- merge.
--
-- 'numberOfConflicts', 'conflictMetadata_numberOfConflicts' - The number of conflicts, including both hunk conflicts and metadata
-- conflicts.
--
-- 'mergeOperations', 'conflictMetadata_mergeOperations' - Whether an add, modify, or delete operation caused the conflict between
-- the source and destination of the merge.
--
-- 'fileModes', 'conflictMetadata_fileModes' - The file modes of the file in the source, destination, and base of the
-- merge.
--
-- 'isBinaryFile', 'conflictMetadata_isBinaryFile' - A boolean value (true or false) indicating whether the file is binary or
-- textual in the source, destination, and base of the merge.
--
-- 'fileModeConflict', 'conflictMetadata_fileModeConflict' - A boolean value indicating whether there are conflicts in the file mode
-- of a file.
--
-- 'objectTypeConflict', 'conflictMetadata_objectTypeConflict' - A boolean value (true or false) indicating whether there are conflicts
-- between the branches in the object type of a file, folder, or submodule.
--
-- 'objectTypes', 'conflictMetadata_objectTypes' - Information about any object type conflicts in a merge operation.
newConflictMetadata ::
  ConflictMetadata
newConflictMetadata =
  ConflictMetadata'
    { filePath = Prelude.Nothing,
      contentConflict = Prelude.Nothing,
      fileSizes = Prelude.Nothing,
      numberOfConflicts = Prelude.Nothing,
      mergeOperations = Prelude.Nothing,
      fileModes = Prelude.Nothing,
      isBinaryFile = Prelude.Nothing,
      fileModeConflict = Prelude.Nothing,
      objectTypeConflict = Prelude.Nothing,
      objectTypes = Prelude.Nothing
    }

-- | The path of the file that contains conflicts.
conflictMetadata_filePath :: Lens.Lens' ConflictMetadata (Prelude.Maybe Prelude.Text)
conflictMetadata_filePath = Lens.lens (\ConflictMetadata' {filePath} -> filePath) (\s@ConflictMetadata' {} a -> s {filePath = a} :: ConflictMetadata)

-- | A boolean value indicating whether there are conflicts in the content of
-- a file.
conflictMetadata_contentConflict :: Lens.Lens' ConflictMetadata (Prelude.Maybe Prelude.Bool)
conflictMetadata_contentConflict = Lens.lens (\ConflictMetadata' {contentConflict} -> contentConflict) (\s@ConflictMetadata' {} a -> s {contentConflict = a} :: ConflictMetadata)

-- | The file sizes of the file in the source, destination, and base of the
-- merge.
conflictMetadata_fileSizes :: Lens.Lens' ConflictMetadata (Prelude.Maybe FileSizes)
conflictMetadata_fileSizes = Lens.lens (\ConflictMetadata' {fileSizes} -> fileSizes) (\s@ConflictMetadata' {} a -> s {fileSizes = a} :: ConflictMetadata)

-- | The number of conflicts, including both hunk conflicts and metadata
-- conflicts.
conflictMetadata_numberOfConflicts :: Lens.Lens' ConflictMetadata (Prelude.Maybe Prelude.Int)
conflictMetadata_numberOfConflicts = Lens.lens (\ConflictMetadata' {numberOfConflicts} -> numberOfConflicts) (\s@ConflictMetadata' {} a -> s {numberOfConflicts = a} :: ConflictMetadata)

-- | Whether an add, modify, or delete operation caused the conflict between
-- the source and destination of the merge.
conflictMetadata_mergeOperations :: Lens.Lens' ConflictMetadata (Prelude.Maybe MergeOperations)
conflictMetadata_mergeOperations = Lens.lens (\ConflictMetadata' {mergeOperations} -> mergeOperations) (\s@ConflictMetadata' {} a -> s {mergeOperations = a} :: ConflictMetadata)

-- | The file modes of the file in the source, destination, and base of the
-- merge.
conflictMetadata_fileModes :: Lens.Lens' ConflictMetadata (Prelude.Maybe FileModes)
conflictMetadata_fileModes = Lens.lens (\ConflictMetadata' {fileModes} -> fileModes) (\s@ConflictMetadata' {} a -> s {fileModes = a} :: ConflictMetadata)

-- | A boolean value (true or false) indicating whether the file is binary or
-- textual in the source, destination, and base of the merge.
conflictMetadata_isBinaryFile :: Lens.Lens' ConflictMetadata (Prelude.Maybe IsBinaryFile)
conflictMetadata_isBinaryFile = Lens.lens (\ConflictMetadata' {isBinaryFile} -> isBinaryFile) (\s@ConflictMetadata' {} a -> s {isBinaryFile = a} :: ConflictMetadata)

-- | A boolean value indicating whether there are conflicts in the file mode
-- of a file.
conflictMetadata_fileModeConflict :: Lens.Lens' ConflictMetadata (Prelude.Maybe Prelude.Bool)
conflictMetadata_fileModeConflict = Lens.lens (\ConflictMetadata' {fileModeConflict} -> fileModeConflict) (\s@ConflictMetadata' {} a -> s {fileModeConflict = a} :: ConflictMetadata)

-- | A boolean value (true or false) indicating whether there are conflicts
-- between the branches in the object type of a file, folder, or submodule.
conflictMetadata_objectTypeConflict :: Lens.Lens' ConflictMetadata (Prelude.Maybe Prelude.Bool)
conflictMetadata_objectTypeConflict = Lens.lens (\ConflictMetadata' {objectTypeConflict} -> objectTypeConflict) (\s@ConflictMetadata' {} a -> s {objectTypeConflict = a} :: ConflictMetadata)

-- | Information about any object type conflicts in a merge operation.
conflictMetadata_objectTypes :: Lens.Lens' ConflictMetadata (Prelude.Maybe ObjectTypes)
conflictMetadata_objectTypes = Lens.lens (\ConflictMetadata' {objectTypes} -> objectTypes) (\s@ConflictMetadata' {} a -> s {objectTypes = a} :: ConflictMetadata)

instance Data.FromJSON ConflictMetadata where
  parseJSON =
    Data.withObject
      "ConflictMetadata"
      ( \x ->
          ConflictMetadata'
            Prelude.<$> (x Data..:? "filePath")
            Prelude.<*> (x Data..:? "contentConflict")
            Prelude.<*> (x Data..:? "fileSizes")
            Prelude.<*> (x Data..:? "numberOfConflicts")
            Prelude.<*> (x Data..:? "mergeOperations")
            Prelude.<*> (x Data..:? "fileModes")
            Prelude.<*> (x Data..:? "isBinaryFile")
            Prelude.<*> (x Data..:? "fileModeConflict")
            Prelude.<*> (x Data..:? "objectTypeConflict")
            Prelude.<*> (x Data..:? "objectTypes")
      )

instance Prelude.Hashable ConflictMetadata where
  hashWithSalt _salt ConflictMetadata' {..} =
    _salt `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` contentConflict
      `Prelude.hashWithSalt` fileSizes
      `Prelude.hashWithSalt` numberOfConflicts
      `Prelude.hashWithSalt` mergeOperations
      `Prelude.hashWithSalt` fileModes
      `Prelude.hashWithSalt` isBinaryFile
      `Prelude.hashWithSalt` fileModeConflict
      `Prelude.hashWithSalt` objectTypeConflict
      `Prelude.hashWithSalt` objectTypes

instance Prelude.NFData ConflictMetadata where
  rnf ConflictMetadata' {..} =
    Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf contentConflict
      `Prelude.seq` Prelude.rnf fileSizes
      `Prelude.seq` Prelude.rnf numberOfConflicts
      `Prelude.seq` Prelude.rnf mergeOperations
      `Prelude.seq` Prelude.rnf fileModes
      `Prelude.seq` Prelude.rnf isBinaryFile
      `Prelude.seq` Prelude.rnf fileModeConflict
      `Prelude.seq` Prelude.rnf objectTypeConflict
      `Prelude.seq` Prelude.rnf objectTypes
