{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FileSystemConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FileSystemConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the connection between a Lambda function and an Amazon EFS file system.
--
--
--
-- /See:/ 'fileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { _fscARN :: !Text,
    _fscLocalMountPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSystemConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fscARN' - The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
--
-- * 'fscLocalMountPath' - The path where the function can access the file system, starting with @/mnt/@ .
fileSystemConfig ::
  -- | 'fscARN'
  Text ->
  -- | 'fscLocalMountPath'
  Text ->
  FileSystemConfig
fileSystemConfig pARN_ pLocalMountPath_ =
  FileSystemConfig'
    { _fscARN = pARN_,
      _fscLocalMountPath = pLocalMountPath_
    }

-- | The Amazon Resource Name (ARN) of the Amazon EFS access point that provides access to the file system.
fscARN :: Lens' FileSystemConfig Text
fscARN = lens _fscARN (\s a -> s {_fscARN = a})

-- | The path where the function can access the file system, starting with @/mnt/@ .
fscLocalMountPath :: Lens' FileSystemConfig Text
fscLocalMountPath = lens _fscLocalMountPath (\s a -> s {_fscLocalMountPath = a})

instance FromJSON FileSystemConfig where
  parseJSON =
    withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig' <$> (x .: "Arn") <*> (x .: "LocalMountPath")
      )

instance Hashable FileSystemConfig

instance NFData FileSystemConfig

instance ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    object
      ( catMaybes
          [ Just ("Arn" .= _fscARN),
            Just ("LocalMountPath" .= _fscLocalMountPath)
          ]
      )
