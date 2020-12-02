{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileMetadata where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A file to be added, updated, or deleted as part of a commit.
--
--
--
-- /See:/ 'fileMetadata' smart constructor.
data FileMetadata = FileMetadata'
  { _fmAbsolutePath :: !(Maybe Text),
    _fmFileMode :: !(Maybe FileModeTypeEnum),
    _fmBlobId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmAbsolutePath' - The full path to the file to be added or updated, including the name of the file.
--
-- * 'fmFileMode' - The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- * 'fmBlobId' - The blob ID that contains the file information.
fileMetadata ::
  FileMetadata
fileMetadata =
  FileMetadata'
    { _fmAbsolutePath = Nothing,
      _fmFileMode = Nothing,
      _fmBlobId = Nothing
    }

-- | The full path to the file to be added or updated, including the name of the file.
fmAbsolutePath :: Lens' FileMetadata (Maybe Text)
fmAbsolutePath = lens _fmAbsolutePath (\s a -> s {_fmAbsolutePath = a})

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
fmFileMode :: Lens' FileMetadata (Maybe FileModeTypeEnum)
fmFileMode = lens _fmFileMode (\s a -> s {_fmFileMode = a})

-- | The blob ID that contains the file information.
fmBlobId :: Lens' FileMetadata (Maybe Text)
fmBlobId = lens _fmBlobId (\s a -> s {_fmBlobId = a})

instance FromJSON FileMetadata where
  parseJSON =
    withObject
      "FileMetadata"
      ( \x ->
          FileMetadata'
            <$> (x .:? "absolutePath") <*> (x .:? "fileMode") <*> (x .:? "blobId")
      )

instance Hashable FileMetadata

instance NFData FileMetadata
