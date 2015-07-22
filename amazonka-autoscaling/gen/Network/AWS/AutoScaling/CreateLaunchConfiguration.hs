{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch configuration.
--
-- If you exceed your maximum limit of launch configurations, which by
-- default is 100 per region, the call fails. For information about viewing
-- and updating this limit, see DescribeAccountLimits.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/LaunchConfiguration.html Launch Configurations>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateLaunchConfiguration.html>
module Network.AWS.AutoScaling.CreateLaunchConfiguration
    (
    -- * Request
      CreateLaunchConfiguration
    -- ** Request constructor
    , createLaunchConfiguration
    -- ** Request lenses
    , clcrqInstanceId
    , clcrqSecurityGroups
    , clcrqAssociatePublicIPAddress
    , clcrqInstanceMonitoring
    , clcrqSpotPrice
    , clcrqKeyName
    , clcrqClassicLinkVPCSecurityGroups
    , clcrqRAMDiskId
    , clcrqKernelId
    , clcrqInstanceType
    , clcrqEBSOptimized
    , clcrqUserData
    , clcrqClassicLinkVPCId
    , clcrqIAMInstanceProfile
    , clcrqImageId
    , clcrqPlacementTenancy
    , clcrqBlockDeviceMappings
    , clcrqLaunchConfigurationName

    -- * Response
    , CreateLaunchConfigurationResponse
    -- ** Response constructor
    , createLaunchConfigurationResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLaunchConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcrqInstanceId'
--
-- * 'clcrqSecurityGroups'
--
-- * 'clcrqAssociatePublicIPAddress'
--
-- * 'clcrqInstanceMonitoring'
--
-- * 'clcrqSpotPrice'
--
-- * 'clcrqKeyName'
--
-- * 'clcrqClassicLinkVPCSecurityGroups'
--
-- * 'clcrqRAMDiskId'
--
-- * 'clcrqKernelId'
--
-- * 'clcrqInstanceType'
--
-- * 'clcrqEBSOptimized'
--
-- * 'clcrqUserData'
--
-- * 'clcrqClassicLinkVPCId'
--
-- * 'clcrqIAMInstanceProfile'
--
-- * 'clcrqImageId'
--
-- * 'clcrqPlacementTenancy'
--
-- * 'clcrqBlockDeviceMappings'
--
-- * 'clcrqLaunchConfigurationName'
data CreateLaunchConfiguration = CreateLaunchConfiguration'
    { _clcrqInstanceId                   :: !(Maybe Text)
    , _clcrqSecurityGroups               :: !(Maybe [Text])
    , _clcrqAssociatePublicIPAddress     :: !(Maybe Bool)
    , _clcrqInstanceMonitoring           :: !(Maybe InstanceMonitoring)
    , _clcrqSpotPrice                    :: !(Maybe Text)
    , _clcrqKeyName                      :: !(Maybe Text)
    , _clcrqClassicLinkVPCSecurityGroups :: !(Maybe [Text])
    , _clcrqRAMDiskId                    :: !(Maybe Text)
    , _clcrqKernelId                     :: !(Maybe Text)
    , _clcrqInstanceType                 :: !(Maybe Text)
    , _clcrqEBSOptimized                 :: !(Maybe Bool)
    , _clcrqUserData                     :: !(Maybe Text)
    , _clcrqClassicLinkVPCId             :: !(Maybe Text)
    , _clcrqIAMInstanceProfile           :: !(Maybe Text)
    , _clcrqImageId                      :: !(Maybe Text)
    , _clcrqPlacementTenancy             :: !(Maybe Text)
    , _clcrqBlockDeviceMappings          :: !(Maybe [BlockDeviceMapping])
    , _clcrqLaunchConfigurationName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLaunchConfiguration' smart constructor.
createLaunchConfiguration :: Text -> CreateLaunchConfiguration
createLaunchConfiguration pLaunchConfigurationName =
    CreateLaunchConfiguration'
    { _clcrqInstanceId = Nothing
    , _clcrqSecurityGroups = Nothing
    , _clcrqAssociatePublicIPAddress = Nothing
    , _clcrqInstanceMonitoring = Nothing
    , _clcrqSpotPrice = Nothing
    , _clcrqKeyName = Nothing
    , _clcrqClassicLinkVPCSecurityGroups = Nothing
    , _clcrqRAMDiskId = Nothing
    , _clcrqKernelId = Nothing
    , _clcrqInstanceType = Nothing
    , _clcrqEBSOptimized = Nothing
    , _clcrqUserData = Nothing
    , _clcrqClassicLinkVPCId = Nothing
    , _clcrqIAMInstanceProfile = Nothing
    , _clcrqImageId = Nothing
    , _clcrqPlacementTenancy = Nothing
    , _clcrqBlockDeviceMappings = Nothing
    , _clcrqLaunchConfigurationName = pLaunchConfigurationName
    }

-- | The ID of the EC2 instance to use to create the launch configuration.
--
-- The new launch configuration derives attributes from the instance, with
-- the exception of the block device mapping.
--
-- To create a launch configuration with a block device mapping or override
-- any other instance attributes, specify them as part of the same request.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/create-lc-with-instanceID.html Create a Launch Configuration Using an EC2 Instance>
-- in the /Auto Scaling Developer Guide/.
clcrqInstanceId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqInstanceId = lens _clcrqInstanceId (\ s a -> s{_clcrqInstanceId = a});

-- | One or more security groups with which to associate the instances.
--
-- If your instances are launched in EC2-Classic, you can either specify
-- security group names or the security group IDs. For more information
-- about security groups for EC2-Classic, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If your instances are launched into a VPC, specify security group IDs.
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
clcrqSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcrqSecurityGroups = lens _clcrqSecurityGroups (\ s a -> s{_clcrqSecurityGroups = a}) . _Default;

-- | Used for groups that launch instances into a virtual private cloud
-- (VPC). Specifies whether to assign a public IP address to each instance.
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html Auto Scaling and Amazon Virtual Private Cloud>
-- in the /Auto Scaling Developer Guide/.
--
-- If you specify a value for this parameter, be sure to specify at least
-- one subnet using the /VPCZoneIdentifier/ parameter when you create your
-- group.
--
-- Default: If the instance is launched into a default subnet, the default
-- is @true@. If the instance is launched into a nondefault subnet, the
-- default is @false@. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqAssociatePublicIPAddress :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcrqAssociatePublicIPAddress = lens _clcrqAssociatePublicIPAddress (\ s a -> s{_clcrqAssociatePublicIPAddress = a});

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default.
--
-- When detailed monitoring is enabled, Amazon CloudWatch generates metrics
-- every minute and your account is charged a fee. When you disable
-- detailed monitoring, by specifying @False@, CloudWatch generates metrics
-- every 5 minutes. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-instance-monitoring.html Monitor Your Auto Scaling Instances>
-- in the /Auto Scaling Developer Guide/.
clcrqInstanceMonitoring :: Lens' CreateLaunchConfiguration (Maybe InstanceMonitoring)
clcrqInstanceMonitoring = lens _clcrqInstanceMonitoring (\ s a -> s{_clcrqInstanceMonitoring = a});

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot market price. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US-SpotInstances.html Launch Spot Instances in Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
clcrqSpotPrice :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqSpotPrice = lens _clcrqSpotPrice (\ s a -> s{_clcrqSpotPrice = a});

-- | The name of the key pair. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqKeyName :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqKeyName = lens _clcrqKeyName (\ s a -> s{_clcrqKeyName = a});

-- | The IDs of one or more security groups for the VPC specified in
-- @ClassicLinkVPCId@. This parameter is required if @ClassicLinkVPCId@ is
-- specified, and is not supported otherwise. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqClassicLinkVPCSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcrqClassicLinkVPCSecurityGroups = lens _clcrqClassicLinkVPCSecurityGroups (\ s a -> s{_clcrqClassicLinkVPCSecurityGroups = a}) . _Default;

-- | The ID of the RAM disk associated with the AMI.
clcrqRAMDiskId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqRAMDiskId = lens _clcrqRAMDiskId (\ s a -> s{_clcrqRAMDiskId = a});

-- | The ID of the kernel associated with the AMI.
clcrqKernelId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqKernelId = lens _clcrqKernelId (\ s a -> s{_clcrqKernelId = a});

-- | The instance type of the EC2 instance. For information about available
-- instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
-- in the /Amazon Elastic Cloud Compute User Guide./
clcrqInstanceType :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqInstanceType = lens _clcrqInstanceType (\ s a -> s{_clcrqInstanceType = a});

-- | Indicates whether the instance is optimized for Amazon EBS I\/O. By
-- default, the instance is not optimized for EBS I\/O. The optimization
-- provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal I\/O performance. This
-- optimization is not available with all instance types. Additional usage
-- charges apply. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqEBSOptimized :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcrqEBSOptimized = lens _clcrqEBSOptimized (\ s a -> s{_clcrqEBSOptimized = a});

-- | The user data to make available to the launched EC2 instances. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- At this time, launch configurations don\'t support compressed (zipped)
-- user data files.
clcrqUserData :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqUserData = lens _clcrqUserData (\ s a -> s{_clcrqUserData = a});

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. This parameter is supported only if you are launching EC2-Classic
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqClassicLinkVPCId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqClassicLinkVPCId = lens _clcrqClassicLinkVPCId (\ s a -> s{_clcrqClassicLinkVPCId = a});

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance.
--
-- EC2 instances launched with an IAM role will automatically have AWS
-- security credentials available. You can use IAM roles with Auto Scaling
-- to automatically enable applications running on your EC2 instances to
-- securely access other AWS resources. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-iam-role.html Launch Auto Scaling Instances with an IAM Role>
-- in the /Auto Scaling Developer Guide/.
clcrqIAMInstanceProfile :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqIAMInstanceProfile = lens _clcrqIAMInstanceProfile (\ s a -> s{_clcrqIAMInstanceProfile = a});

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqImageId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqImageId = lens _clcrqImageId (\ s a -> s{_clcrqImageId = a});

-- | The tenancy of the instance. An instance with a tenancy of @dedicated@
-- runs on single-tenant hardware and can only be launched into a VPC.
--
-- You must set the value of this parameter to @dedicated@ if want to
-- launch Dedicated Instances into a shared tenancy VPC (VPC with instance
-- placement tenancy attribute set to @default@).
--
-- If you specify a value for this parameter, be sure to specify at least
-- one subnet using the /VPCZoneIdentifier/ parameter when you create your
-- group.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html Auto Scaling and Amazon Virtual Private Cloud>
-- in the /Auto Scaling Developer Guide/.
--
-- Valid values: @default@ | @dedicated@
clcrqPlacementTenancy :: Lens' CreateLaunchConfiguration (Maybe Text)
clcrqPlacementTenancy = lens _clcrqPlacementTenancy (\ s a -> s{_clcrqPlacementTenancy = a});

-- | One or more mappings that specify how block devices are exposed to the
-- instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcrqBlockDeviceMappings :: Lens' CreateLaunchConfiguration [BlockDeviceMapping]
clcrqBlockDeviceMappings = lens _clcrqBlockDeviceMappings (\ s a -> s{_clcrqBlockDeviceMappings = a}) . _Default;

-- | The name of the launch configuration. This name must be unique within
-- the scope of your AWS account.
clcrqLaunchConfigurationName :: Lens' CreateLaunchConfiguration Text
clcrqLaunchConfigurationName = lens _clcrqLaunchConfigurationName (\ s a -> s{_clcrqLaunchConfigurationName = a});

instance AWSRequest CreateLaunchConfiguration where
        type Sv CreateLaunchConfiguration = AutoScaling
        type Rs CreateLaunchConfiguration =
             CreateLaunchConfigurationResponse
        request = post
        response
          = receiveNull CreateLaunchConfigurationResponse'

instance ToHeaders CreateLaunchConfiguration where
        toHeaders = const mempty

instance ToPath CreateLaunchConfiguration where
        toPath = const "/"

instance ToQuery CreateLaunchConfiguration where
        toQuery CreateLaunchConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("CreateLaunchConfiguration" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceId" =: _clcrqInstanceId,
               "SecurityGroups" =:
                 toQuery
                   (toQueryList "member" <$> _clcrqSecurityGroups),
               "AssociatePublicIpAddress" =:
                 _clcrqAssociatePublicIPAddress,
               "InstanceMonitoring" =: _clcrqInstanceMonitoring,
               "SpotPrice" =: _clcrqSpotPrice,
               "KeyName" =: _clcrqKeyName,
               "ClassicLinkVPCSecurityGroups" =:
                 toQuery
                   (toQueryList "member" <$>
                      _clcrqClassicLinkVPCSecurityGroups),
               "RamdiskId" =: _clcrqRAMDiskId,
               "KernelId" =: _clcrqKernelId,
               "InstanceType" =: _clcrqInstanceType,
               "EbsOptimized" =: _clcrqEBSOptimized,
               "UserData" =: _clcrqUserData,
               "ClassicLinkVPCId" =: _clcrqClassicLinkVPCId,
               "IamInstanceProfile" =: _clcrqIAMInstanceProfile,
               "ImageId" =: _clcrqImageId,
               "PlacementTenancy" =: _clcrqPlacementTenancy,
               "BlockDeviceMappings" =:
                 toQuery
                   (toQueryList "member" <$> _clcrqBlockDeviceMappings),
               "LaunchConfigurationName" =:
                 _clcrqLaunchConfigurationName]

-- | /See:/ 'createLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse =
    CreateLaunchConfigurationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLaunchConfigurationResponse' smart constructor.
createLaunchConfigurationResponse :: CreateLaunchConfigurationResponse
createLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
