{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.MountTargetDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.MountTargetDescription where

import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of a mount target.
--
--
--
-- /See:/ 'mountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { _mtdIPAddress ::
      !(Maybe Text),
    _mtdAvailabilityZoneId :: !(Maybe Text),
    _mtdVPCId :: !(Maybe Text),
    _mtdAvailabilityZoneName :: !(Maybe Text),
    _mtdNetworkInterfaceId :: !(Maybe Text),
    _mtdOwnerId :: !(Maybe Text),
    _mtdMountTargetId :: !Text,
    _mtdFileSystemId :: !Text,
    _mtdSubnetId :: !Text,
    _mtdLifeCycleState :: !LifeCycleState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MountTargetDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtdIPAddress' - Address at which the file system can be mounted by using the mount target.
--
-- * 'mtdAvailabilityZoneId' - The unique and consistent identifier of the Availability Zone (AZ) that the mount target resides in. For example, @use1-az1@ is an AZ ID for the us-east-1 Region and it has the same location in every AWS account.
--
-- * 'mtdVPCId' - The Virtual Private Cloud (VPC) ID that the mount target is configured in.
--
-- * 'mtdAvailabilityZoneName' - The name of the Availability Zone (AZ) that the mount target resides in. AZs are independently mapped to names for each AWS account. For example, the Availability Zone @us-east-1a@ for your AWS account might not be the same location as @us-east-1a@ for another AWS account.
--
-- * 'mtdNetworkInterfaceId' - The ID of the network interface that Amazon EFS created when it created the mount target.
--
-- * 'mtdOwnerId' - AWS account ID that owns the resource.
--
-- * 'mtdMountTargetId' - System-assigned mount target ID.
--
-- * 'mtdFileSystemId' - The ID of the file system for which the mount target is intended.
--
-- * 'mtdSubnetId' - The ID of the mount target's subnet.
--
-- * 'mtdLifeCycleState' - Lifecycle state of the mount target.
mountTargetDescription ::
  -- | 'mtdMountTargetId'
  Text ->
  -- | 'mtdFileSystemId'
  Text ->
  -- | 'mtdSubnetId'
  Text ->
  -- | 'mtdLifeCycleState'
  LifeCycleState ->
  MountTargetDescription
mountTargetDescription
  pMountTargetId_
  pFileSystemId_
  pSubnetId_
  pLifeCycleState_ =
    MountTargetDescription'
      { _mtdIPAddress = Nothing,
        _mtdAvailabilityZoneId = Nothing,
        _mtdVPCId = Nothing,
        _mtdAvailabilityZoneName = Nothing,
        _mtdNetworkInterfaceId = Nothing,
        _mtdOwnerId = Nothing,
        _mtdMountTargetId = pMountTargetId_,
        _mtdFileSystemId = pFileSystemId_,
        _mtdSubnetId = pSubnetId_,
        _mtdLifeCycleState = pLifeCycleState_
      }

-- | Address at which the file system can be mounted by using the mount target.
mtdIPAddress :: Lens' MountTargetDescription (Maybe Text)
mtdIPAddress = lens _mtdIPAddress (\s a -> s {_mtdIPAddress = a})

-- | The unique and consistent identifier of the Availability Zone (AZ) that the mount target resides in. For example, @use1-az1@ is an AZ ID for the us-east-1 Region and it has the same location in every AWS account.
mtdAvailabilityZoneId :: Lens' MountTargetDescription (Maybe Text)
mtdAvailabilityZoneId = lens _mtdAvailabilityZoneId (\s a -> s {_mtdAvailabilityZoneId = a})

-- | The Virtual Private Cloud (VPC) ID that the mount target is configured in.
mtdVPCId :: Lens' MountTargetDescription (Maybe Text)
mtdVPCId = lens _mtdVPCId (\s a -> s {_mtdVPCId = a})

-- | The name of the Availability Zone (AZ) that the mount target resides in. AZs are independently mapped to names for each AWS account. For example, the Availability Zone @us-east-1a@ for your AWS account might not be the same location as @us-east-1a@ for another AWS account.
mtdAvailabilityZoneName :: Lens' MountTargetDescription (Maybe Text)
mtdAvailabilityZoneName = lens _mtdAvailabilityZoneName (\s a -> s {_mtdAvailabilityZoneName = a})

-- | The ID of the network interface that Amazon EFS created when it created the mount target.
mtdNetworkInterfaceId :: Lens' MountTargetDescription (Maybe Text)
mtdNetworkInterfaceId = lens _mtdNetworkInterfaceId (\s a -> s {_mtdNetworkInterfaceId = a})

-- | AWS account ID that owns the resource.
mtdOwnerId :: Lens' MountTargetDescription (Maybe Text)
mtdOwnerId = lens _mtdOwnerId (\s a -> s {_mtdOwnerId = a})

-- | System-assigned mount target ID.
mtdMountTargetId :: Lens' MountTargetDescription Text
mtdMountTargetId = lens _mtdMountTargetId (\s a -> s {_mtdMountTargetId = a})

-- | The ID of the file system for which the mount target is intended.
mtdFileSystemId :: Lens' MountTargetDescription Text
mtdFileSystemId = lens _mtdFileSystemId (\s a -> s {_mtdFileSystemId = a})

-- | The ID of the mount target's subnet.
mtdSubnetId :: Lens' MountTargetDescription Text
mtdSubnetId = lens _mtdSubnetId (\s a -> s {_mtdSubnetId = a})

-- | Lifecycle state of the mount target.
mtdLifeCycleState :: Lens' MountTargetDescription LifeCycleState
mtdLifeCycleState = lens _mtdLifeCycleState (\s a -> s {_mtdLifeCycleState = a})

instance FromJSON MountTargetDescription where
  parseJSON =
    withObject
      "MountTargetDescription"
      ( \x ->
          MountTargetDescription'
            <$> (x .:? "IpAddress")
            <*> (x .:? "AvailabilityZoneId")
            <*> (x .:? "VpcId")
            <*> (x .:? "AvailabilityZoneName")
            <*> (x .:? "NetworkInterfaceId")
            <*> (x .:? "OwnerId")
            <*> (x .: "MountTargetId")
            <*> (x .: "FileSystemId")
            <*> (x .: "SubnetId")
            <*> (x .: "LifeCycleState")
      )

instance Hashable MountTargetDescription

instance NFData MountTargetDescription
