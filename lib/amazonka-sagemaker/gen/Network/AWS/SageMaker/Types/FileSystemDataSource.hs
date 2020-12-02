{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.FileSystemAccessMode
import Network.AWS.SageMaker.Types.FileSystemType

-- | Specifies a file system data source for a channel.
--
--
--
-- /See:/ 'fileSystemDataSource' smart constructor.
data FileSystemDataSource = FileSystemDataSource'
  { _fsdsFileSystemId ::
      !Text,
    _fsdsFileSystemAccessMode ::
      !FileSystemAccessMode,
    _fsdsFileSystemType :: !FileSystemType,
    _fsdsDirectoryPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSystemDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsdsFileSystemId' - The file system id.
--
-- * 'fsdsFileSystemAccessMode' - The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
--
-- * 'fsdsFileSystemType' - The file system type.
--
-- * 'fsdsDirectoryPath' - The full path to the directory to associate with the channel.
fileSystemDataSource ::
  -- | 'fsdsFileSystemId'
  Text ->
  -- | 'fsdsFileSystemAccessMode'
  FileSystemAccessMode ->
  -- | 'fsdsFileSystemType'
  FileSystemType ->
  -- | 'fsdsDirectoryPath'
  Text ->
  FileSystemDataSource
fileSystemDataSource
  pFileSystemId_
  pFileSystemAccessMode_
  pFileSystemType_
  pDirectoryPath_ =
    FileSystemDataSource'
      { _fsdsFileSystemId = pFileSystemId_,
        _fsdsFileSystemAccessMode = pFileSystemAccessMode_,
        _fsdsFileSystemType = pFileSystemType_,
        _fsdsDirectoryPath = pDirectoryPath_
      }

-- | The file system id.
fsdsFileSystemId :: Lens' FileSystemDataSource Text
fsdsFileSystemId = lens _fsdsFileSystemId (\s a -> s {_fsdsFileSystemId = a})

-- | The access mode of the mount of the directory associated with the channel. A directory can be mounted either in @ro@ (read-only) or @rw@ (read-write) mode.
fsdsFileSystemAccessMode :: Lens' FileSystemDataSource FileSystemAccessMode
fsdsFileSystemAccessMode = lens _fsdsFileSystemAccessMode (\s a -> s {_fsdsFileSystemAccessMode = a})

-- | The file system type.
fsdsFileSystemType :: Lens' FileSystemDataSource FileSystemType
fsdsFileSystemType = lens _fsdsFileSystemType (\s a -> s {_fsdsFileSystemType = a})

-- | The full path to the directory to associate with the channel.
fsdsDirectoryPath :: Lens' FileSystemDataSource Text
fsdsDirectoryPath = lens _fsdsDirectoryPath (\s a -> s {_fsdsDirectoryPath = a})

instance FromJSON FileSystemDataSource where
  parseJSON =
    withObject
      "FileSystemDataSource"
      ( \x ->
          FileSystemDataSource'
            <$> (x .: "FileSystemId")
            <*> (x .: "FileSystemAccessMode")
            <*> (x .: "FileSystemType")
            <*> (x .: "DirectoryPath")
      )

instance Hashable FileSystemDataSource

instance NFData FileSystemDataSource

instance ToJSON FileSystemDataSource where
  toJSON FileSystemDataSource' {..} =
    object
      ( catMaybes
          [ Just ("FileSystemId" .= _fsdsFileSystemId),
            Just ("FileSystemAccessMode" .= _fsdsFileSystemAccessMode),
            Just ("FileSystemType" .= _fsdsFileSystemType),
            Just ("DirectoryPath" .= _fsdsDirectoryPath)
          ]
      )
