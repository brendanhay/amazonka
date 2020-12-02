{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSVolumeConfiguration where

import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSTransitEncryption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This parameter is specified when you are using an Amazon Elastic File System file system for task storage. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html Amazon EFS Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'eFSVolumeConfiguration' smart constructor.
data EFSVolumeConfiguration = EFSVolumeConfiguration'
  { _efsvcRootDirectory ::
      !(Maybe Text),
    _efsvcTransitEncryption ::
      !(Maybe EFSTransitEncryption),
    _efsvcAuthorizationConfig ::
      !(Maybe EFSAuthorizationConfig),
    _efsvcTransitEncryptionPort :: !(Maybe Int),
    _efsvcFileSystemId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EFSVolumeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsvcRootDirectory' - The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter. /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
--
-- * 'efsvcTransitEncryption' - Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
--
-- * 'efsvcAuthorizationConfig' - The authorization configuration details for the Amazon EFS file system.
--
-- * 'efsvcTransitEncryptionPort' - The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
--
-- * 'efsvcFileSystemId' - The Amazon EFS file system ID to use.
eFSVolumeConfiguration ::
  -- | 'efsvcFileSystemId'
  Text ->
  EFSVolumeConfiguration
eFSVolumeConfiguration pFileSystemId_ =
  EFSVolumeConfiguration'
    { _efsvcRootDirectory = Nothing,
      _efsvcTransitEncryption = Nothing,
      _efsvcAuthorizationConfig = Nothing,
      _efsvcTransitEncryptionPort = Nothing,
      _efsvcFileSystemId = pFileSystemId_
    }

-- | The directory within the Amazon EFS file system to mount as the root directory inside the host. If this parameter is omitted, the root of the Amazon EFS volume will be used. Specifying @/@ will have the same effect as omitting this parameter. /Important:/ If an EFS access point is specified in the @authorizationConfig@ , the root directory parameter must either be omitted or set to @/@ which will enforce the path set on the EFS access point.
efsvcRootDirectory :: Lens' EFSVolumeConfiguration (Maybe Text)
efsvcRootDirectory = lens _efsvcRootDirectory (\s a -> s {_efsvcRootDirectory = a})

-- | Whether or not to enable encryption for Amazon EFS data in transit between the Amazon ECS host and the Amazon EFS server. Transit encryption must be enabled if Amazon EFS IAM authorization is used. If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting Data in Transit> in the /Amazon Elastic File System User Guide/ .
efsvcTransitEncryption :: Lens' EFSVolumeConfiguration (Maybe EFSTransitEncryption)
efsvcTransitEncryption = lens _efsvcTransitEncryption (\s a -> s {_efsvcTransitEncryption = a})

-- | The authorization configuration details for the Amazon EFS file system.
efsvcAuthorizationConfig :: Lens' EFSVolumeConfiguration (Maybe EFSAuthorizationConfig)
efsvcAuthorizationConfig = lens _efsvcAuthorizationConfig (\s a -> s {_efsvcAuthorizationConfig = a})

-- | The port to use when sending encrypted data between the Amazon ECS host and the Amazon EFS server. If you do not specify a transit encryption port, it will use the port selection strategy that the Amazon EFS mount helper uses. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS Mount Helper> in the /Amazon Elastic File System User Guide/ .
efsvcTransitEncryptionPort :: Lens' EFSVolumeConfiguration (Maybe Int)
efsvcTransitEncryptionPort = lens _efsvcTransitEncryptionPort (\s a -> s {_efsvcTransitEncryptionPort = a})

-- | The Amazon EFS file system ID to use.
efsvcFileSystemId :: Lens' EFSVolumeConfiguration Text
efsvcFileSystemId = lens _efsvcFileSystemId (\s a -> s {_efsvcFileSystemId = a})

instance FromJSON EFSVolumeConfiguration where
  parseJSON =
    withObject
      "EFSVolumeConfiguration"
      ( \x ->
          EFSVolumeConfiguration'
            <$> (x .:? "rootDirectory")
            <*> (x .:? "transitEncryption")
            <*> (x .:? "authorizationConfig")
            <*> (x .:? "transitEncryptionPort")
            <*> (x .: "fileSystemId")
      )

instance Hashable EFSVolumeConfiguration

instance NFData EFSVolumeConfiguration

instance ToJSON EFSVolumeConfiguration where
  toJSON EFSVolumeConfiguration' {..} =
    object
      ( catMaybes
          [ ("rootDirectory" .=) <$> _efsvcRootDirectory,
            ("transitEncryption" .=) <$> _efsvcTransitEncryption,
            ("authorizationConfig" .=) <$> _efsvcAuthorizationConfig,
            ("transitEncryptionPort" .=) <$> _efsvcTransitEncryptionPort,
            Just ("fileSystemId" .= _efsvcFileSystemId)
          ]
      )
