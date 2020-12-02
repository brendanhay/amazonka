{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.FileSystemDataSource
import Network.AWS.SageMaker.Types.S3DataSource

-- | Describes the location of the channel data.
--
--
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
  { _dsS3DataSource ::
      !(Maybe S3DataSource),
    _dsFileSystemDataSource :: !(Maybe FileSystemDataSource)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsS3DataSource' - The S3 location of the data source that is associated with a channel.
--
-- * 'dsFileSystemDataSource' - The file system that is associated with a channel.
dataSource ::
  DataSource
dataSource =
  DataSource'
    { _dsS3DataSource = Nothing,
      _dsFileSystemDataSource = Nothing
    }

-- | The S3 location of the data source that is associated with a channel.
dsS3DataSource :: Lens' DataSource (Maybe S3DataSource)
dsS3DataSource = lens _dsS3DataSource (\s a -> s {_dsS3DataSource = a})

-- | The file system that is associated with a channel.
dsFileSystemDataSource :: Lens' DataSource (Maybe FileSystemDataSource)
dsFileSystemDataSource = lens _dsFileSystemDataSource (\s a -> s {_dsFileSystemDataSource = a})

instance FromJSON DataSource where
  parseJSON =
    withObject
      "DataSource"
      ( \x ->
          DataSource'
            <$> (x .:? "S3DataSource") <*> (x .:? "FileSystemDataSource")
      )

instance Hashable DataSource

instance NFData DataSource

instance ToJSON DataSource where
  toJSON DataSource' {..} =
    object
      ( catMaybes
          [ ("S3DataSource" .=) <$> _dsS3DataSource,
            ("FileSystemDataSource" .=) <$> _dsFileSystemDataSource
          ]
      )
