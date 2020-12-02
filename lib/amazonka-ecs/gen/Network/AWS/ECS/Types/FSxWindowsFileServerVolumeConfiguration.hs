{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration where

import Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This parameter is specified when you are using <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html Amazon FSx for Windows File Server> file system for task storage.
--
--
-- For more information and the input format, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
-- /See:/ 'fSxWindowsFileServerVolumeConfiguration' smart constructor.
data FSxWindowsFileServerVolumeConfiguration = FSxWindowsFileServerVolumeConfiguration'
  { _fswfsvcFileSystemId ::
      !Text,
    _fswfsvcRootDirectory ::
      !Text,
    _fswfsvcAuthorizationConfig ::
      !FSxWindowsFileServerAuthorizationConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FSxWindowsFileServerVolumeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fswfsvcFileSystemId' - The Amazon FSx for Windows File Server file system ID to use.
--
-- * 'fswfsvcRootDirectory' - The directory within the Amazon FSx for Windows File Server file system to mount as the root directory inside the host.
--
-- * 'fswfsvcAuthorizationConfig' - The authorization configuration details for the Amazon FSx for Windows File Server file system.
fSxWindowsFileServerVolumeConfiguration ::
  -- | 'fswfsvcFileSystemId'
  Text ->
  -- | 'fswfsvcRootDirectory'
  Text ->
  -- | 'fswfsvcAuthorizationConfig'
  FSxWindowsFileServerAuthorizationConfig ->
  FSxWindowsFileServerVolumeConfiguration
fSxWindowsFileServerVolumeConfiguration
  pFileSystemId_
  pRootDirectory_
  pAuthorizationConfig_ =
    FSxWindowsFileServerVolumeConfiguration'
      { _fswfsvcFileSystemId =
          pFileSystemId_,
        _fswfsvcRootDirectory = pRootDirectory_,
        _fswfsvcAuthorizationConfig = pAuthorizationConfig_
      }

-- | The Amazon FSx for Windows File Server file system ID to use.
fswfsvcFileSystemId :: Lens' FSxWindowsFileServerVolumeConfiguration Text
fswfsvcFileSystemId = lens _fswfsvcFileSystemId (\s a -> s {_fswfsvcFileSystemId = a})

-- | The directory within the Amazon FSx for Windows File Server file system to mount as the root directory inside the host.
fswfsvcRootDirectory :: Lens' FSxWindowsFileServerVolumeConfiguration Text
fswfsvcRootDirectory = lens _fswfsvcRootDirectory (\s a -> s {_fswfsvcRootDirectory = a})

-- | The authorization configuration details for the Amazon FSx for Windows File Server file system.
fswfsvcAuthorizationConfig :: Lens' FSxWindowsFileServerVolumeConfiguration FSxWindowsFileServerAuthorizationConfig
fswfsvcAuthorizationConfig = lens _fswfsvcAuthorizationConfig (\s a -> s {_fswfsvcAuthorizationConfig = a})

instance FromJSON FSxWindowsFileServerVolumeConfiguration where
  parseJSON =
    withObject
      "FSxWindowsFileServerVolumeConfiguration"
      ( \x ->
          FSxWindowsFileServerVolumeConfiguration'
            <$> (x .: "fileSystemId")
            <*> (x .: "rootDirectory")
            <*> (x .: "authorizationConfig")
      )

instance Hashable FSxWindowsFileServerVolumeConfiguration

instance NFData FSxWindowsFileServerVolumeConfiguration

instance ToJSON FSxWindowsFileServerVolumeConfiguration where
  toJSON FSxWindowsFileServerVolumeConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("fileSystemId" .= _fswfsvcFileSystemId),
            Just ("rootDirectory" .= _fswfsvcRootDirectory),
            Just ("authorizationConfig" .= _fswfsvcAuthorizationConfig)
          ]
      )
