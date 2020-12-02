{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchConfiguration where

import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.InstanceMetadataOptions
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch configuration.
--
--
--
-- /See:/ 'launchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { _lcAssociatePublicIPAddress ::
      !(Maybe Bool),
    _lcSecurityGroups :: !(Maybe [Text]),
    _lcSpotPrice :: !(Maybe Text),
    _lcInstanceMonitoring ::
      !(Maybe InstanceMonitoring),
    _lcKeyName :: !(Maybe Text),
    _lcClassicLinkVPCSecurityGroups :: !(Maybe [Text]),
    _lcRAMDiskId :: !(Maybe Text),
    _lcKernelId :: !(Maybe Text),
    _lcEBSOptimized :: !(Maybe Bool),
    _lcUserData :: !(Maybe Text),
    _lcClassicLinkVPCId :: !(Maybe Text),
    _lcIAMInstanceProfile :: !(Maybe Text),
    _lcMetadataOptions ::
      !(Maybe InstanceMetadataOptions),
    _lcLaunchConfigurationARN :: !(Maybe Text),
    _lcPlacementTenancy :: !(Maybe Text),
    _lcBlockDeviceMappings ::
      !(Maybe [BlockDeviceMapping]),
    _lcLaunchConfigurationName :: !Text,
    _lcImageId :: !Text,
    _lcInstanceType :: !Text,
    _lcCreatedTime :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcAssociatePublicIPAddress' - For Auto Scaling groups that are running in a VPC, specifies whether to assign a public IP address to the group's instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcSecurityGroups' - A list that contains the security groups to assign to the instances in the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- * 'lcSpotPrice' - The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcInstanceMonitoring' - Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcKeyName' - The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'lcClassicLinkVPCSecurityGroups' - The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcRAMDiskId' - The ID of the RAM disk associated with the AMI.
--
-- * 'lcKernelId' - The ID of the kernel associated with the AMI.
--
-- * 'lcEBSOptimized' - Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'lcUserData' - The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'lcClassicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcIAMInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcMetadataOptions' - The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcLaunchConfigurationARN' - The Amazon Resource Name (ARN) of the launch configuration.
--
-- * 'lcPlacementTenancy' - The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'lcBlockDeviceMappings' - A block device mapping, which specifies the block devices for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'lcLaunchConfigurationName' - The name of the launch configuration.
--
-- * 'lcImageId' - The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'lcInstanceType' - The instance type for the instances. For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
--
-- * 'lcCreatedTime' - The creation date and time for the launch configuration.
launchConfiguration ::
  -- | 'lcLaunchConfigurationName'
  Text ->
  -- | 'lcImageId'
  Text ->
  -- | 'lcInstanceType'
  Text ->
  -- | 'lcCreatedTime'
  UTCTime ->
  LaunchConfiguration
launchConfiguration
  pLaunchConfigurationName_
  pImageId_
  pInstanceType_
  pCreatedTime_ =
    LaunchConfiguration'
      { _lcAssociatePublicIPAddress = Nothing,
        _lcSecurityGroups = Nothing,
        _lcSpotPrice = Nothing,
        _lcInstanceMonitoring = Nothing,
        _lcKeyName = Nothing,
        _lcClassicLinkVPCSecurityGroups = Nothing,
        _lcRAMDiskId = Nothing,
        _lcKernelId = Nothing,
        _lcEBSOptimized = Nothing,
        _lcUserData = Nothing,
        _lcClassicLinkVPCId = Nothing,
        _lcIAMInstanceProfile = Nothing,
        _lcMetadataOptions = Nothing,
        _lcLaunchConfigurationARN = Nothing,
        _lcPlacementTenancy = Nothing,
        _lcBlockDeviceMappings = Nothing,
        _lcLaunchConfigurationName = pLaunchConfigurationName_,
        _lcImageId = pImageId_,
        _lcInstanceType = pInstanceType_,
        _lcCreatedTime = _Time # pCreatedTime_
      }

-- | For Auto Scaling groups that are running in a VPC, specifies whether to assign a public IP address to the group's instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
lcAssociatePublicIPAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIPAddress = lens _lcAssociatePublicIPAddress (\s a -> s {_lcAssociatePublicIPAddress = a})

-- | A list that contains the security groups to assign to the instances in the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups = lens _lcSecurityGroups (\s a -> s {_lcSecurityGroups = a}) . _Default . _Coerce

-- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\s a -> s {_lcSpotPrice = a})

-- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring = lens _lcInstanceMonitoring (\s a -> s {_lcInstanceMonitoring = a})

-- | The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\s a -> s {_lcKeyName = a})

-- | The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
lcClassicLinkVPCSecurityGroups :: Lens' LaunchConfiguration [Text]
lcClassicLinkVPCSecurityGroups = lens _lcClassicLinkVPCSecurityGroups (\s a -> s {_lcClassicLinkVPCSecurityGroups = a}) . _Default . _Coerce

-- | The ID of the RAM disk associated with the AMI.
lcRAMDiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRAMDiskId = lens _lcRAMDiskId (\s a -> s {_lcRAMDiskId = a})

-- | The ID of the kernel associated with the AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\s a -> s {_lcKernelId = a})

-- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
lcEBSOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEBSOptimized = lens _lcEBSOptimized (\s a -> s {_lcEBSOptimized = a})

-- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\s a -> s {_lcUserData = a})

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
lcClassicLinkVPCId :: Lens' LaunchConfiguration (Maybe Text)
lcClassicLinkVPCId = lens _lcClassicLinkVPCId (\s a -> s {_lcClassicLinkVPCId = a})

-- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
lcIAMInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIAMInstanceProfile = lens _lcIAMInstanceProfile (\s a -> s {_lcIAMInstanceProfile = a})

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
lcMetadataOptions :: Lens' LaunchConfiguration (Maybe InstanceMetadataOptions)
lcMetadataOptions = lens _lcMetadataOptions (\s a -> s {_lcMetadataOptions = a})

-- | The Amazon Resource Name (ARN) of the launch configuration.
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN = lens _lcLaunchConfigurationARN (\s a -> s {_lcLaunchConfigurationARN = a})

-- | The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy = lens _lcPlacementTenancy (\s a -> s {_lcPlacementTenancy = a})

-- | A block device mapping, which specifies the block devices for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings = lens _lcBlockDeviceMappings (\s a -> s {_lcBlockDeviceMappings = a}) . _Default . _Coerce

-- | The name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName = lens _lcLaunchConfigurationName (\s a -> s {_lcLaunchConfigurationName = a})

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\s a -> s {_lcImageId = a})

-- | The instance type for the instances. For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\s a -> s {_lcInstanceType = a})

-- | The creation date and time for the launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration UTCTime
lcCreatedTime = lens _lcCreatedTime (\s a -> s {_lcCreatedTime = a}) . _Time

instance FromXML LaunchConfiguration where
  parseXML x =
    LaunchConfiguration'
      <$> (x .@? "AssociatePublicIpAddress")
      <*> (x .@? "SecurityGroups" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "SpotPrice")
      <*> (x .@? "InstanceMonitoring")
      <*> (x .@? "KeyName")
      <*> ( x .@? "ClassicLinkVPCSecurityGroups" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "RamdiskId")
      <*> (x .@? "KernelId")
      <*> (x .@? "EbsOptimized")
      <*> (x .@? "UserData")
      <*> (x .@? "ClassicLinkVPCId")
      <*> (x .@? "IamInstanceProfile")
      <*> (x .@? "MetadataOptions")
      <*> (x .@? "LaunchConfigurationARN")
      <*> (x .@? "PlacementTenancy")
      <*> ( x .@? "BlockDeviceMappings" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@ "LaunchConfigurationName")
      <*> (x .@ "ImageId")
      <*> (x .@ "InstanceType")
      <*> (x .@ "CreatedTime")

instance Hashable LaunchConfiguration

instance NFData LaunchConfiguration
