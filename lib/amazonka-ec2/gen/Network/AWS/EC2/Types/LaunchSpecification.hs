{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.IAMInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.SpotPlacement
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the launch specification for an instance.
--
--
--
-- /See:/ 'launchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { _lsSecurityGroups ::
      !(Maybe [GroupIdentifier]),
    _lsKeyName :: !(Maybe Text),
    _lsNetworkInterfaces ::
      !(Maybe [InstanceNetworkInterfaceSpecification]),
    _lsRAMDiskId :: !(Maybe Text),
    _lsSubnetId :: !(Maybe Text),
    _lsKernelId :: !(Maybe Text),
    _lsInstanceType :: !(Maybe InstanceType),
    _lsEBSOptimized :: !(Maybe Bool),
    _lsUserData :: !(Maybe Text),
    _lsMonitoring ::
      !(Maybe RunInstancesMonitoringEnabled),
    _lsIAMInstanceProfile ::
      !(Maybe IAMInstanceProfileSpecification),
    _lsImageId :: !(Maybe Text),
    _lsAddressingType :: !(Maybe Text),
    _lsBlockDeviceMappings ::
      !(Maybe [BlockDeviceMapping]),
    _lsPlacement :: !(Maybe SpotPlacement)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsSecurityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- * 'lsKeyName' - The name of the key pair.
--
-- * 'lsNetworkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- * 'lsRAMDiskId' - The ID of the RAM disk.
--
-- * 'lsSubnetId' - The ID of the subnet in which to launch the instance.
--
-- * 'lsKernelId' - The ID of the kernel.
--
-- * 'lsInstanceType' - The instance type.
--
-- * 'lsEBSOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
--
-- * 'lsUserData' - The Base64-encoded user data for the instance.
--
-- * 'lsMonitoring' - Undocumented member.
--
-- * 'lsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'lsImageId' - The ID of the AMI.
--
-- * 'lsAddressingType' - Deprecated.
--
-- * 'lsBlockDeviceMappings' - One or more block device mapping entries.
--
-- * 'lsPlacement' - The placement information for the instance.
launchSpecification ::
  LaunchSpecification
launchSpecification =
  LaunchSpecification'
    { _lsSecurityGroups = Nothing,
      _lsKeyName = Nothing,
      _lsNetworkInterfaces = Nothing,
      _lsRAMDiskId = Nothing,
      _lsSubnetId = Nothing,
      _lsKernelId = Nothing,
      _lsInstanceType = Nothing,
      _lsEBSOptimized = Nothing,
      _lsUserData = Nothing,
      _lsMonitoring = Nothing,
      _lsIAMInstanceProfile = Nothing,
      _lsImageId = Nothing,
      _lsAddressingType = Nothing,
      _lsBlockDeviceMappings = Nothing,
      _lsPlacement = Nothing
    }

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
lsSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
lsSecurityGroups = lens _lsSecurityGroups (\s a -> s {_lsSecurityGroups = a}) . _Default . _Coerce

-- | The name of the key pair.
lsKeyName :: Lens' LaunchSpecification (Maybe Text)
lsKeyName = lens _lsKeyName (\s a -> s {_lsKeyName = a})

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
lsNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
lsNetworkInterfaces = lens _lsNetworkInterfaces (\s a -> s {_lsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
lsRAMDiskId :: Lens' LaunchSpecification (Maybe Text)
lsRAMDiskId = lens _lsRAMDiskId (\s a -> s {_lsRAMDiskId = a})

-- | The ID of the subnet in which to launch the instance.
lsSubnetId :: Lens' LaunchSpecification (Maybe Text)
lsSubnetId = lens _lsSubnetId (\s a -> s {_lsSubnetId = a})

-- | The ID of the kernel.
lsKernelId :: Lens' LaunchSpecification (Maybe Text)
lsKernelId = lens _lsKernelId (\s a -> s {_lsKernelId = a})

-- | The instance type.
lsInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
lsInstanceType = lens _lsInstanceType (\s a -> s {_lsInstanceType = a})

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
lsEBSOptimized :: Lens' LaunchSpecification (Maybe Bool)
lsEBSOptimized = lens _lsEBSOptimized (\s a -> s {_lsEBSOptimized = a})

-- | The Base64-encoded user data for the instance.
lsUserData :: Lens' LaunchSpecification (Maybe Text)
lsUserData = lens _lsUserData (\s a -> s {_lsUserData = a})

-- | Undocumented member.
lsMonitoring :: Lens' LaunchSpecification (Maybe RunInstancesMonitoringEnabled)
lsMonitoring = lens _lsMonitoring (\s a -> s {_lsMonitoring = a})

-- | The IAM instance profile.
lsIAMInstanceProfile :: Lens' LaunchSpecification (Maybe IAMInstanceProfileSpecification)
lsIAMInstanceProfile = lens _lsIAMInstanceProfile (\s a -> s {_lsIAMInstanceProfile = a})

-- | The ID of the AMI.
lsImageId :: Lens' LaunchSpecification (Maybe Text)
lsImageId = lens _lsImageId (\s a -> s {_lsImageId = a})

-- | Deprecated.
lsAddressingType :: Lens' LaunchSpecification (Maybe Text)
lsAddressingType = lens _lsAddressingType (\s a -> s {_lsAddressingType = a})

-- | One or more block device mapping entries.
lsBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
lsBlockDeviceMappings = lens _lsBlockDeviceMappings (\s a -> s {_lsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information for the instance.
lsPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
lsPlacement = lens _lsPlacement (\s a -> s {_lsPlacement = a})

instance FromXML LaunchSpecification where
  parseXML x =
    LaunchSpecification'
      <$> (x .@? "groupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "keyName")
      <*> ( x .@? "networkInterfaceSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "ramdiskId")
      <*> (x .@? "subnetId")
      <*> (x .@? "kernelId")
      <*> (x .@? "instanceType")
      <*> (x .@? "ebsOptimized")
      <*> (x .@? "userData")
      <*> (x .@? "monitoring")
      <*> (x .@? "iamInstanceProfile")
      <*> (x .@? "imageId")
      <*> (x .@? "addressingType")
      <*> ( x .@? "blockDeviceMapping" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "placement")

instance Hashable LaunchSpecification

instance NFData LaunchSpecification
