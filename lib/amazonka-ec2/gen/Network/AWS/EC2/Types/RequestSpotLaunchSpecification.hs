{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RequestSpotLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RequestSpotLaunchSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BlockDeviceMapping
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
-- /See:/ 'requestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { _rslsSecurityGroupIds ::
      !(Maybe [Text]),
    _rslsSecurityGroups ::
      !(Maybe [Text]),
    _rslsKeyName :: !(Maybe Text),
    _rslsNetworkInterfaces ::
      !( Maybe
           [InstanceNetworkInterfaceSpecification]
       ),
    _rslsRAMDiskId ::
      !(Maybe Text),
    _rslsSubnetId ::
      !(Maybe Text),
    _rslsKernelId ::
      !(Maybe Text),
    _rslsInstanceType ::
      !(Maybe InstanceType),
    _rslsEBSOptimized ::
      !(Maybe Bool),
    _rslsUserData ::
      !(Maybe Text),
    _rslsMonitoring ::
      !( Maybe
           RunInstancesMonitoringEnabled
       ),
    _rslsIAMInstanceProfile ::
      !( Maybe
           IAMInstanceProfileSpecification
       ),
    _rslsImageId :: !(Maybe Text),
    _rslsAddressingType ::
      !(Maybe Text),
    _rslsBlockDeviceMappings ::
      !(Maybe [BlockDeviceMapping]),
    _rslsPlacement ::
      !(Maybe SpotPlacement)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestSpotLaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rslsSecurityGroupIds' - One or more security group IDs.
--
-- * 'rslsSecurityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- * 'rslsKeyName' - The name of the key pair.
--
-- * 'rslsNetworkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- * 'rslsRAMDiskId' - The ID of the RAM disk.
--
-- * 'rslsSubnetId' - The IDs of the subnets in which to launch the instance. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
--
-- * 'rslsKernelId' - The ID of the kernel.
--
-- * 'rslsInstanceType' - The instance type.
--
-- * 'rslsEBSOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
--
-- * 'rslsUserData' - The Base64-encoded user data for the instance. User data is limited to 16 KB.
--
-- * 'rslsMonitoring' - Indicates whether basic or detailed monitoring is enabled for the instance. Default: Disabled
--
-- * 'rslsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rslsImageId' - The ID of the AMI.
--
-- * 'rslsAddressingType' - Deprecated.
--
-- * 'rslsBlockDeviceMappings' - One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- * 'rslsPlacement' - The placement information for the instance.
requestSpotLaunchSpecification ::
  RequestSpotLaunchSpecification
requestSpotLaunchSpecification =
  RequestSpotLaunchSpecification'
    { _rslsSecurityGroupIds = Nothing,
      _rslsSecurityGroups = Nothing,
      _rslsKeyName = Nothing,
      _rslsNetworkInterfaces = Nothing,
      _rslsRAMDiskId = Nothing,
      _rslsSubnetId = Nothing,
      _rslsKernelId = Nothing,
      _rslsInstanceType = Nothing,
      _rslsEBSOptimized = Nothing,
      _rslsUserData = Nothing,
      _rslsMonitoring = Nothing,
      _rslsIAMInstanceProfile = Nothing,
      _rslsImageId = Nothing,
      _rslsAddressingType = Nothing,
      _rslsBlockDeviceMappings = Nothing,
      _rslsPlacement = Nothing
    }

-- | One or more security group IDs.
rslsSecurityGroupIds :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroupIds = lens _rslsSecurityGroupIds (\s a -> s {_rslsSecurityGroupIds = a}) . _Default . _Coerce

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
rslsSecurityGroups :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroups = lens _rslsSecurityGroups (\s a -> s {_rslsSecurityGroups = a}) . _Default . _Coerce

-- | The name of the key pair.
rslsKeyName :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKeyName = lens _rslsKeyName (\s a -> s {_rslsKeyName = a})

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
rslsNetworkInterfaces :: Lens' RequestSpotLaunchSpecification [InstanceNetworkInterfaceSpecification]
rslsNetworkInterfaces = lens _rslsNetworkInterfaces (\s a -> s {_rslsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
rslsRAMDiskId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsRAMDiskId = lens _rslsRAMDiskId (\s a -> s {_rslsRAMDiskId = a})

-- | The IDs of the subnets in which to launch the instance. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
rslsSubnetId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsSubnetId = lens _rslsSubnetId (\s a -> s {_rslsSubnetId = a})

-- | The ID of the kernel.
rslsKernelId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKernelId = lens _rslsKernelId (\s a -> s {_rslsKernelId = a})

-- | The instance type.
rslsInstanceType :: Lens' RequestSpotLaunchSpecification (Maybe InstanceType)
rslsInstanceType = lens _rslsInstanceType (\s a -> s {_rslsInstanceType = a})

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
rslsEBSOptimized :: Lens' RequestSpotLaunchSpecification (Maybe Bool)
rslsEBSOptimized = lens _rslsEBSOptimized (\s a -> s {_rslsEBSOptimized = a})

-- | The Base64-encoded user data for the instance. User data is limited to 16 KB.
rslsUserData :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsUserData = lens _rslsUserData (\s a -> s {_rslsUserData = a})

-- | Indicates whether basic or detailed monitoring is enabled for the instance. Default: Disabled
rslsMonitoring :: Lens' RequestSpotLaunchSpecification (Maybe RunInstancesMonitoringEnabled)
rslsMonitoring = lens _rslsMonitoring (\s a -> s {_rslsMonitoring = a})

-- | The IAM instance profile.
rslsIAMInstanceProfile :: Lens' RequestSpotLaunchSpecification (Maybe IAMInstanceProfileSpecification)
rslsIAMInstanceProfile = lens _rslsIAMInstanceProfile (\s a -> s {_rslsIAMInstanceProfile = a})

-- | The ID of the AMI.
rslsImageId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsImageId = lens _rslsImageId (\s a -> s {_rslsImageId = a})

-- | Deprecated.
rslsAddressingType :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsAddressingType = lens _rslsAddressingType (\s a -> s {_rslsAddressingType = a})

-- | One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
rslsBlockDeviceMappings :: Lens' RequestSpotLaunchSpecification [BlockDeviceMapping]
rslsBlockDeviceMappings = lens _rslsBlockDeviceMappings (\s a -> s {_rslsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information for the instance.
rslsPlacement :: Lens' RequestSpotLaunchSpecification (Maybe SpotPlacement)
rslsPlacement = lens _rslsPlacement (\s a -> s {_rslsPlacement = a})

instance Hashable RequestSpotLaunchSpecification

instance NFData RequestSpotLaunchSpecification

instance ToQuery RequestSpotLaunchSpecification where
  toQuery RequestSpotLaunchSpecification' {..} =
    mconcat
      [ toQuery (toQueryList "SecurityGroupId" <$> _rslsSecurityGroupIds),
        toQuery (toQueryList "SecurityGroup" <$> _rslsSecurityGroups),
        "KeyName" =: _rslsKeyName,
        toQuery
          (toQueryList "NetworkInterface" <$> _rslsNetworkInterfaces),
        "RamdiskId" =: _rslsRAMDiskId,
        "SubnetId" =: _rslsSubnetId,
        "KernelId" =: _rslsKernelId,
        "InstanceType" =: _rslsInstanceType,
        "EbsOptimized" =: _rslsEBSOptimized,
        "UserData" =: _rslsUserData,
        "Monitoring" =: _rslsMonitoring,
        "IamInstanceProfile" =: _rslsIAMInstanceProfile,
        "ImageId" =: _rslsImageId,
        "AddressingType" =: _rslsAddressingType,
        toQuery
          (toQueryList "BlockDeviceMapping" <$> _rslsBlockDeviceMappings),
        "Placement" =: _rslsPlacement
      ]
