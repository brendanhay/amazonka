{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictMetadata where

import Network.AWS.CodeCommit.Types.FileModes
import Network.AWS.CodeCommit.Types.FileSizes
import Network.AWS.CodeCommit.Types.IsBinaryFile
import Network.AWS.CodeCommit.Types.MergeOperations
import Network.AWS.CodeCommit.Types.ObjectTypes
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the metadata for a conflict in a merge operation.
--
--
--
-- /See:/ 'conflictMetadata' smart constructor.
data ConflictMetadata = ConflictMetadata'
  { _cmNumberOfConflicts ::
      !(Maybe Int),
    _cmContentConflict :: !(Maybe Bool),
    _cmFileSizes :: !(Maybe FileSizes),
    _cmFilePath :: !(Maybe Text),
    _cmIsBinaryFile :: !(Maybe IsBinaryFile),
    _cmFileModeConflict :: !(Maybe Bool),
    _cmObjectTypeConflict :: !(Maybe Bool),
    _cmMergeOperations :: !(Maybe MergeOperations),
    _cmObjectTypes :: !(Maybe ObjectTypes),
    _cmFileModes :: !(Maybe FileModes)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConflictMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmNumberOfConflicts' - The number of conflicts, including both hunk conflicts and metadata conflicts.
--
-- * 'cmContentConflict' - A boolean value indicating whether there are conflicts in the content of a file.
--
-- * 'cmFileSizes' - The file sizes of the file in the source, destination, and base of the merge.
--
-- * 'cmFilePath' - The path of the file that contains conflicts.
--
-- * 'cmIsBinaryFile' - A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
--
-- * 'cmFileModeConflict' - A boolean value indicating whether there are conflicts in the file mode of a file.
--
-- * 'cmObjectTypeConflict' - A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
--
-- * 'cmMergeOperations' - Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
--
-- * 'cmObjectTypes' - Information about any object type conflicts in a merge operation.
--
-- * 'cmFileModes' - The file modes of the file in the source, destination, and base of the merge.
conflictMetadata ::
  ConflictMetadata
conflictMetadata =
  ConflictMetadata'
    { _cmNumberOfConflicts = Nothing,
      _cmContentConflict = Nothing,
      _cmFileSizes = Nothing,
      _cmFilePath = Nothing,
      _cmIsBinaryFile = Nothing,
      _cmFileModeConflict = Nothing,
      _cmObjectTypeConflict = Nothing,
      _cmMergeOperations = Nothing,
      _cmObjectTypes = Nothing,
      _cmFileModes = Nothing
    }

-- | The number of conflicts, including both hunk conflicts and metadata conflicts.
cmNumberOfConflicts :: Lens' ConflictMetadata (Maybe Int)
cmNumberOfConflicts = lens _cmNumberOfConflicts (\s a -> s {_cmNumberOfConflicts = a})

-- | A boolean value indicating whether there are conflicts in the content of a file.
cmContentConflict :: Lens' ConflictMetadata (Maybe Bool)
cmContentConflict = lens _cmContentConflict (\s a -> s {_cmContentConflict = a})

-- | The file sizes of the file in the source, destination, and base of the merge.
cmFileSizes :: Lens' ConflictMetadata (Maybe FileSizes)
cmFileSizes = lens _cmFileSizes (\s a -> s {_cmFileSizes = a})

-- | The path of the file that contains conflicts.
cmFilePath :: Lens' ConflictMetadata (Maybe Text)
cmFilePath = lens _cmFilePath (\s a -> s {_cmFilePath = a})

-- | A boolean value (true or false) indicating whether the file is binary or textual in the source, destination, and base of the merge.
cmIsBinaryFile :: Lens' ConflictMetadata (Maybe IsBinaryFile)
cmIsBinaryFile = lens _cmIsBinaryFile (\s a -> s {_cmIsBinaryFile = a})

-- | A boolean value indicating whether there are conflicts in the file mode of a file.
cmFileModeConflict :: Lens' ConflictMetadata (Maybe Bool)
cmFileModeConflict = lens _cmFileModeConflict (\s a -> s {_cmFileModeConflict = a})

-- | A boolean value (true or false) indicating whether there are conflicts between the branches in the object type of a file, folder, or submodule.
cmObjectTypeConflict :: Lens' ConflictMetadata (Maybe Bool)
cmObjectTypeConflict = lens _cmObjectTypeConflict (\s a -> s {_cmObjectTypeConflict = a})

-- | Whether an add, modify, or delete operation caused the conflict between the source and destination of the merge.
cmMergeOperations :: Lens' ConflictMetadata (Maybe MergeOperations)
cmMergeOperations = lens _cmMergeOperations (\s a -> s {_cmMergeOperations = a})

-- | Information about any object type conflicts in a merge operation.
cmObjectTypes :: Lens' ConflictMetadata (Maybe ObjectTypes)
cmObjectTypes = lens _cmObjectTypes (\s a -> s {_cmObjectTypes = a})

-- | The file modes of the file in the source, destination, and base of the merge.
cmFileModes :: Lens' ConflictMetadata (Maybe FileModes)
cmFileModes = lens _cmFileModes (\s a -> s {_cmFileModes = a})

instance FromJSON ConflictMetadata where
  parseJSON =
    withObject
      "ConflictMetadata"
      ( \x ->
          ConflictMetadata'
            <$> (x .:? "numberOfConflicts")
            <*> (x .:? "contentConflict")
            <*> (x .:? "fileSizes")
            <*> (x .:? "filePath")
            <*> (x .:? "isBinaryFile")
            <*> (x .:? "fileModeConflict")
            <*> (x .:? "objectTypeConflict")
            <*> (x .:? "mergeOperations")
            <*> (x .:? "objectTypes")
            <*> (x .:? "fileModes")
      )

instance Hashable ConflictMetadata

instance NFData ConflictMetadata
