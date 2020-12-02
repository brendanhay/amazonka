{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.File
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.File where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a file in a repository.
--
--
--
-- /See:/ 'file' smart constructor.
data File = File'
  { _fAbsolutePath :: !(Maybe Text),
    _fFileMode :: !(Maybe FileModeTypeEnum),
    _fBlobId :: !(Maybe Text),
    _fRelativePath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'File' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fAbsolutePath' - The fully qualified path to the file in the repository.
--
-- * 'fFileMode' - The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- * 'fBlobId' - The blob ID that contains the file information.
--
-- * 'fRelativePath' - The relative path of the file from the folder where the query originated.
file ::
  File
file =
  File'
    { _fAbsolutePath = Nothing,
      _fFileMode = Nothing,
      _fBlobId = Nothing,
      _fRelativePath = Nothing
    }

-- | The fully qualified path to the file in the repository.
fAbsolutePath :: Lens' File (Maybe Text)
fAbsolutePath = lens _fAbsolutePath (\s a -> s {_fAbsolutePath = a})

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
fFileMode :: Lens' File (Maybe FileModeTypeEnum)
fFileMode = lens _fFileMode (\s a -> s {_fFileMode = a})

-- | The blob ID that contains the file information.
fBlobId :: Lens' File (Maybe Text)
fBlobId = lens _fBlobId (\s a -> s {_fBlobId = a})

-- | The relative path of the file from the folder where the query originated.
fRelativePath :: Lens' File (Maybe Text)
fRelativePath = lens _fRelativePath (\s a -> s {_fRelativePath = a})

instance FromJSON File where
  parseJSON =
    withObject
      "File"
      ( \x ->
          File'
            <$> (x .:? "absolutePath")
            <*> (x .:? "fileMode")
            <*> (x .:? "blobId")
            <*> (x .:? "relativePath")
      )

instance Hashable File

instance NFData File
