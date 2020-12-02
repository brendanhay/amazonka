{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile
import Network.AWS.EC2.Types.ScheduledInstancesMonitoring
import Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
import Network.AWS.EC2.Types.ScheduledInstancesPlacement
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the launch specification for a Scheduled Instance.
--
--
-- If you are launching the Scheduled Instance in EC2-VPC, you must specify the ID of the subnet. You can specify the subnet using either @SubnetId@ or @NetworkInterface@ .
--
--
-- /See:/ 'scheduledInstancesLaunchSpecification' smart constructor.
data ScheduledInstancesLaunchSpecification = ScheduledInstancesLaunchSpecification'
  { _silsSecurityGroupIds ::
      !(Maybe [Text]),
    _silsKeyName ::
      !(Maybe Text),
    _silsNetworkInterfaces ::
      !( Maybe
           [ScheduledInstancesNetworkInterface]
       ),
    _silsRAMDiskId ::
      !(Maybe Text),
    _silsSubnetId ::
      !(Maybe Text),
    _silsKernelId ::
      !(Maybe Text),
    _silsInstanceType ::
      !(Maybe Text),
    _silsEBSOptimized ::
      !(Maybe Bool),
    _silsUserData ::
      !(Maybe Text),
    _silsMonitoring ::
      !( Maybe
           ScheduledInstancesMonitoring
       ),
    _silsIAMInstanceProfile ::
      !( Maybe
           ScheduledInstancesIAMInstanceProfile
       ),
    _silsBlockDeviceMappings ::
      !( Maybe
           [ScheduledInstancesBlockDeviceMapping]
       ),
    _silsPlacement ::
      !( Maybe
           ScheduledInstancesPlacement
       ),
    _silsImageId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesLaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'silsSecurityGroupIds' - The IDs of the security groups.
--
-- * 'silsKeyName' - The name of the key pair.
--
-- * 'silsNetworkInterfaces' - The network interfaces.
--
-- * 'silsRAMDiskId' - The ID of the RAM disk.
--
-- * 'silsSubnetId' - The ID of the subnet in which to launch the instances.
--
-- * 'silsKernelId' - The ID of the kernel.
--
-- * 'silsInstanceType' - The instance type.
--
-- * 'silsEBSOptimized' - Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
--
-- * 'silsUserData' - The base64-encoded MIME user data.
--
-- * 'silsMonitoring' - Enable or disable monitoring for the instances.
--
-- * 'silsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'silsBlockDeviceMappings' - The block device mapping entries.
--
-- * 'silsPlacement' - The placement information.
--
-- * 'silsImageId' - The ID of the Amazon Machine Image (AMI).
scheduledInstancesLaunchSpecification ::
  -- | 'silsImageId'
  Text ->
  ScheduledInstancesLaunchSpecification
scheduledInstancesLaunchSpecification pImageId_ =
  ScheduledInstancesLaunchSpecification'
    { _silsSecurityGroupIds =
        Nothing,
      _silsKeyName = Nothing,
      _silsNetworkInterfaces = Nothing,
      _silsRAMDiskId = Nothing,
      _silsSubnetId = Nothing,
      _silsKernelId = Nothing,
      _silsInstanceType = Nothing,
      _silsEBSOptimized = Nothing,
      _silsUserData = Nothing,
      _silsMonitoring = Nothing,
      _silsIAMInstanceProfile = Nothing,
      _silsBlockDeviceMappings = Nothing,
      _silsPlacement = Nothing,
      _silsImageId = pImageId_
    }

-- | The IDs of the security groups.
silsSecurityGroupIds :: Lens' ScheduledInstancesLaunchSpecification [Text]
silsSecurityGroupIds = lens _silsSecurityGroupIds (\s a -> s {_silsSecurityGroupIds = a}) . _Default . _Coerce

-- | The name of the key pair.
silsKeyName :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsKeyName = lens _silsKeyName (\s a -> s {_silsKeyName = a})

-- | The network interfaces.
silsNetworkInterfaces :: Lens' ScheduledInstancesLaunchSpecification [ScheduledInstancesNetworkInterface]
silsNetworkInterfaces = lens _silsNetworkInterfaces (\s a -> s {_silsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
silsRAMDiskId :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsRAMDiskId = lens _silsRAMDiskId (\s a -> s {_silsRAMDiskId = a})

-- | The ID of the subnet in which to launch the instances.
silsSubnetId :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsSubnetId = lens _silsSubnetId (\s a -> s {_silsSubnetId = a})

-- | The ID of the kernel.
silsKernelId :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsKernelId = lens _silsKernelId (\s a -> s {_silsKernelId = a})

-- | The instance type.
silsInstanceType :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsInstanceType = lens _silsInstanceType (\s a -> s {_silsInstanceType = a})

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
silsEBSOptimized :: Lens' ScheduledInstancesLaunchSpecification (Maybe Bool)
silsEBSOptimized = lens _silsEBSOptimized (\s a -> s {_silsEBSOptimized = a})

-- | The base64-encoded MIME user data.
silsUserData :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsUserData = lens _silsUserData (\s a -> s {_silsUserData = a})

-- | Enable or disable monitoring for the instances.
silsMonitoring :: Lens' ScheduledInstancesLaunchSpecification (Maybe ScheduledInstancesMonitoring)
silsMonitoring = lens _silsMonitoring (\s a -> s {_silsMonitoring = a})

-- | The IAM instance profile.
silsIAMInstanceProfile :: Lens' ScheduledInstancesLaunchSpecification (Maybe ScheduledInstancesIAMInstanceProfile)
silsIAMInstanceProfile = lens _silsIAMInstanceProfile (\s a -> s {_silsIAMInstanceProfile = a})

-- | The block device mapping entries.
silsBlockDeviceMappings :: Lens' ScheduledInstancesLaunchSpecification [ScheduledInstancesBlockDeviceMapping]
silsBlockDeviceMappings = lens _silsBlockDeviceMappings (\s a -> s {_silsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information.
silsPlacement :: Lens' ScheduledInstancesLaunchSpecification (Maybe ScheduledInstancesPlacement)
silsPlacement = lens _silsPlacement (\s a -> s {_silsPlacement = a})

-- | The ID of the Amazon Machine Image (AMI).
silsImageId :: Lens' ScheduledInstancesLaunchSpecification Text
silsImageId = lens _silsImageId (\s a -> s {_silsImageId = a})

instance Hashable ScheduledInstancesLaunchSpecification

instance NFData ScheduledInstancesLaunchSpecification

instance ToQuery ScheduledInstancesLaunchSpecification where
  toQuery ScheduledInstancesLaunchSpecification' {..} =
    mconcat
      [ toQuery (toQueryList "SecurityGroupId" <$> _silsSecurityGroupIds),
        "KeyName" =: _silsKeyName,
        toQuery
          (toQueryList "NetworkInterface" <$> _silsNetworkInterfaces),
        "RamdiskId" =: _silsRAMDiskId,
        "SubnetId" =: _silsSubnetId,
        "KernelId" =: _silsKernelId,
        "InstanceType" =: _silsInstanceType,
        "EbsOptimized" =: _silsEBSOptimized,
        "UserData" =: _silsUserData,
        "Monitoring" =: _silsMonitoring,
        "IamInstanceProfile" =: _silsIAMInstanceProfile,
        toQuery
          (toQueryList "BlockDeviceMapping" <$> _silsBlockDeviceMappings),
        "Placement" =: _silsPlacement,
        "ImageId" =: _silsImageId
      ]
