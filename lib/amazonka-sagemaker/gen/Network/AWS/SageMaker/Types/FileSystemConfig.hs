{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
--
--
--
-- /See:/ 'fileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { _fscDefaultGid ::
      !(Maybe Nat),
    _fscMountPath :: !(Maybe Text),
    _fscDefaultUid :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSystemConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fscDefaultGid' - The default POSIX group ID (GID). If not specified, defaults to @100@ .
--
-- * 'fscMountPath' - The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
--
-- * 'fscDefaultUid' - The default POSIX user ID (UID). If not specified, defaults to @1000@ .
fileSystemConfig ::
  FileSystemConfig
fileSystemConfig =
  FileSystemConfig'
    { _fscDefaultGid = Nothing,
      _fscMountPath = Nothing,
      _fscDefaultUid = Nothing
    }

-- | The default POSIX group ID (GID). If not specified, defaults to @100@ .
fscDefaultGid :: Lens' FileSystemConfig (Maybe Natural)
fscDefaultGid = lens _fscDefaultGid (\s a -> s {_fscDefaultGid = a}) . mapping _Nat

-- | The path within the image to mount the user's EFS home directory. The directory should be empty. If not specified, defaults to /\/home\/sagemaker-user/ .
fscMountPath :: Lens' FileSystemConfig (Maybe Text)
fscMountPath = lens _fscMountPath (\s a -> s {_fscMountPath = a})

-- | The default POSIX user ID (UID). If not specified, defaults to @1000@ .
fscDefaultUid :: Lens' FileSystemConfig (Maybe Natural)
fscDefaultUid = lens _fscDefaultUid (\s a -> s {_fscDefaultUid = a}) . mapping _Nat

instance FromJSON FileSystemConfig where
  parseJSON =
    withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            <$> (x .:? "DefaultGid")
            <*> (x .:? "MountPath")
            <*> (x .:? "DefaultUid")
      )

instance Hashable FileSystemConfig

instance NFData FileSystemConfig

instance ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    object
      ( catMaybes
          [ ("DefaultGid" .=) <$> _fscDefaultGid,
            ("MountPath" .=) <$> _fscMountPath,
            ("DefaultUid" .=) <$> _fscDefaultUid
          ]
      )
