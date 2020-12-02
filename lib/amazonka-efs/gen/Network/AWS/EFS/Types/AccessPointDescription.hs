{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.AccessPointDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.AccessPointDescription where

import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.PosixUser
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of an EFS file system access point.
--
--
--
-- /See:/ 'accessPointDescription' smart constructor.
data AccessPointDescription = AccessPointDescription'
  { _apdPosixUser ::
      !(Maybe PosixUser),
    _apdRootDirectory :: !(Maybe RootDirectory),
    _apdClientToken :: !(Maybe Text),
    _apdAccessPointId :: !(Maybe Text),
    _apdFileSystemId :: !(Maybe Text),
    _apdOwnerId :: !(Maybe Text),
    _apdName :: !(Maybe Text),
    _apdAccessPointARN :: !(Maybe Text),
    _apdLifeCycleState :: !(Maybe LifeCycleState),
    _apdTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessPointDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apdPosixUser' - The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
--
-- * 'apdRootDirectory' - The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
--
-- * 'apdClientToken' - The opaque string specified in the request to ensure idempotent creation.
--
-- * 'apdAccessPointId' - The ID of the access point, assigned by Amazon EFS.
--
-- * 'apdFileSystemId' - The ID of the EFS file system that the access point applies to.
--
-- * 'apdOwnerId' - Identified the AWS account that owns the access point resource.
--
-- * 'apdName' - The name of the access point. This is the value of the @Name@ tag.
--
-- * 'apdAccessPointARN' - The unique Amazon Resource Name (ARN) associated with the access point.
--
-- * 'apdLifeCycleState' - Identifies the lifecycle phase of the access point.
--
-- * 'apdTags' - The tags associated with the access point, presented as an array of Tag objects.
accessPointDescription ::
  AccessPointDescription
accessPointDescription =
  AccessPointDescription'
    { _apdPosixUser = Nothing,
      _apdRootDirectory = Nothing,
      _apdClientToken = Nothing,
      _apdAccessPointId = Nothing,
      _apdFileSystemId = Nothing,
      _apdOwnerId = Nothing,
      _apdName = Nothing,
      _apdAccessPointARN = Nothing,
      _apdLifeCycleState = Nothing,
      _apdTags = Nothing
    }

-- | The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
apdPosixUser :: Lens' AccessPointDescription (Maybe PosixUser)
apdPosixUser = lens _apdPosixUser (\s a -> s {_apdPosixUser = a})

-- | The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
apdRootDirectory :: Lens' AccessPointDescription (Maybe RootDirectory)
apdRootDirectory = lens _apdRootDirectory (\s a -> s {_apdRootDirectory = a})

-- | The opaque string specified in the request to ensure idempotent creation.
apdClientToken :: Lens' AccessPointDescription (Maybe Text)
apdClientToken = lens _apdClientToken (\s a -> s {_apdClientToken = a})

-- | The ID of the access point, assigned by Amazon EFS.
apdAccessPointId :: Lens' AccessPointDescription (Maybe Text)
apdAccessPointId = lens _apdAccessPointId (\s a -> s {_apdAccessPointId = a})

-- | The ID of the EFS file system that the access point applies to.
apdFileSystemId :: Lens' AccessPointDescription (Maybe Text)
apdFileSystemId = lens _apdFileSystemId (\s a -> s {_apdFileSystemId = a})

-- | Identified the AWS account that owns the access point resource.
apdOwnerId :: Lens' AccessPointDescription (Maybe Text)
apdOwnerId = lens _apdOwnerId (\s a -> s {_apdOwnerId = a})

-- | The name of the access point. This is the value of the @Name@ tag.
apdName :: Lens' AccessPointDescription (Maybe Text)
apdName = lens _apdName (\s a -> s {_apdName = a})

-- | The unique Amazon Resource Name (ARN) associated with the access point.
apdAccessPointARN :: Lens' AccessPointDescription (Maybe Text)
apdAccessPointARN = lens _apdAccessPointARN (\s a -> s {_apdAccessPointARN = a})

-- | Identifies the lifecycle phase of the access point.
apdLifeCycleState :: Lens' AccessPointDescription (Maybe LifeCycleState)
apdLifeCycleState = lens _apdLifeCycleState (\s a -> s {_apdLifeCycleState = a})

-- | The tags associated with the access point, presented as an array of Tag objects.
apdTags :: Lens' AccessPointDescription [Tag]
apdTags = lens _apdTags (\s a -> s {_apdTags = a}) . _Default . _Coerce

instance FromJSON AccessPointDescription where
  parseJSON =
    withObject
      "AccessPointDescription"
      ( \x ->
          AccessPointDescription'
            <$> (x .:? "PosixUser")
            <*> (x .:? "RootDirectory")
            <*> (x .:? "ClientToken")
            <*> (x .:? "AccessPointId")
            <*> (x .:? "FileSystemId")
            <*> (x .:? "OwnerId")
            <*> (x .:? "Name")
            <*> (x .:? "AccessPointArn")
            <*> (x .:? "LifeCycleState")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable AccessPointDescription

instance NFData AccessPointDescription
