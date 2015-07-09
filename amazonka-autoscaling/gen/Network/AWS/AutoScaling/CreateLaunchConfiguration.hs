{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a launch configuration.
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
    , clcInstanceId
    , clcSecurityGroups
    , clcAssociatePublicIPAddress
    , clcInstanceMonitoring
    , clcSpotPrice
    , clcKeyName
    , clcClassicLinkVPCSecurityGroups
    , clcRAMDiskId
    , clcKernelId
    , clcInstanceType
    , clcEBSOptimized
    , clcUserData
    , clcClassicLinkVPCId
    , clcIAMInstanceProfile
    , clcImageId
    , clcPlacementTenancy
    , clcBlockDeviceMappings
    , clcLaunchConfigurationName

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
-- * 'clcInstanceId'
--
-- * 'clcSecurityGroups'
--
-- * 'clcAssociatePublicIPAddress'
--
-- * 'clcInstanceMonitoring'
--
-- * 'clcSpotPrice'
--
-- * 'clcKeyName'
--
-- * 'clcClassicLinkVPCSecurityGroups'
--
-- * 'clcRAMDiskId'
--
-- * 'clcKernelId'
--
-- * 'clcInstanceType'
--
-- * 'clcEBSOptimized'
--
-- * 'clcUserData'
--
-- * 'clcClassicLinkVPCId'
--
-- * 'clcIAMInstanceProfile'
--
-- * 'clcImageId'
--
-- * 'clcPlacementTenancy'
--
-- * 'clcBlockDeviceMappings'
--
-- * 'clcLaunchConfigurationName'
data CreateLaunchConfiguration = CreateLaunchConfiguration'
    { _clcInstanceId                   :: !(Maybe Text)
    , _clcSecurityGroups               :: !(Maybe [Text])
    , _clcAssociatePublicIPAddress     :: !(Maybe Bool)
    , _clcInstanceMonitoring           :: !(Maybe InstanceMonitoring)
    , _clcSpotPrice                    :: !(Maybe Text)
    , _clcKeyName                      :: !(Maybe Text)
    , _clcClassicLinkVPCSecurityGroups :: !(Maybe [Text])
    , _clcRAMDiskId                    :: !(Maybe Text)
    , _clcKernelId                     :: !(Maybe Text)
    , _clcInstanceType                 :: !(Maybe Text)
    , _clcEBSOptimized                 :: !(Maybe Bool)
    , _clcUserData                     :: !(Maybe Text)
    , _clcClassicLinkVPCId             :: !(Maybe Text)
    , _clcIAMInstanceProfile           :: !(Maybe Text)
    , _clcImageId                      :: !(Maybe Text)
    , _clcPlacementTenancy             :: !(Maybe Text)
    , _clcBlockDeviceMappings          :: !(Maybe [BlockDeviceMapping])
    , _clcLaunchConfigurationName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLaunchConfiguration' smart constructor.
createLaunchConfiguration :: Text -> CreateLaunchConfiguration
createLaunchConfiguration pLaunchConfigurationName =
    CreateLaunchConfiguration'
    { _clcInstanceId = Nothing
    , _clcSecurityGroups = Nothing
    , _clcAssociatePublicIPAddress = Nothing
    , _clcInstanceMonitoring = Nothing
    , _clcSpotPrice = Nothing
    , _clcKeyName = Nothing
    , _clcClassicLinkVPCSecurityGroups = Nothing
    , _clcRAMDiskId = Nothing
    , _clcKernelId = Nothing
    , _clcInstanceType = Nothing
    , _clcEBSOptimized = Nothing
    , _clcUserData = Nothing
    , _clcClassicLinkVPCId = Nothing
    , _clcIAMInstanceProfile = Nothing
    , _clcImageId = Nothing
    , _clcPlacementTenancy = Nothing
    , _clcBlockDeviceMappings = Nothing
    , _clcLaunchConfigurationName = pLaunchConfigurationName
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
clcInstanceId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceId = lens _clcInstanceId (\ s a -> s{_clcInstanceId = a});

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
clcSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcSecurityGroups = lens _clcSecurityGroups (\ s a -> s{_clcSecurityGroups = a}) . _Default;

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
clcAssociatePublicIPAddress :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcAssociatePublicIPAddress = lens _clcAssociatePublicIPAddress (\ s a -> s{_clcAssociatePublicIPAddress = a});

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default.
--
-- When detailed monitoring is enabled, Amazon CloudWatch generates metrics
-- every minute and your account is charged a fee. When you disable
-- detailed monitoring, by specifying @False@, CloudWatch generates metrics
-- every 5 minutes. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-instance-monitoring.html Monitor Your Auto Scaling Instances>
-- in the /Auto Scaling Developer Guide/.
clcInstanceMonitoring :: Lens' CreateLaunchConfiguration (Maybe InstanceMonitoring)
clcInstanceMonitoring = lens _clcInstanceMonitoring (\ s a -> s{_clcInstanceMonitoring = a});

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot market price. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US-SpotInstances.html Launch Spot Instances in Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
clcSpotPrice :: Lens' CreateLaunchConfiguration (Maybe Text)
clcSpotPrice = lens _clcSpotPrice (\ s a -> s{_clcSpotPrice = a});

-- | The name of the key pair. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcKeyName :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKeyName = lens _clcKeyName (\ s a -> s{_clcKeyName = a});

-- | The IDs of one or more security groups for the VPC specified in
-- @ClassicLinkVPCId@. This parameter is required if @ClassicLinkVPCId@ is
-- specified, and is not supported otherwise. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcClassicLinkVPCSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcClassicLinkVPCSecurityGroups = lens _clcClassicLinkVPCSecurityGroups (\ s a -> s{_clcClassicLinkVPCSecurityGroups = a}) . _Default;

-- | The ID of the RAM disk associated with the AMI.
clcRAMDiskId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcRAMDiskId = lens _clcRAMDiskId (\ s a -> s{_clcRAMDiskId = a});

-- | The ID of the kernel associated with the AMI.
clcKernelId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKernelId = lens _clcKernelId (\ s a -> s{_clcKernelId = a});

-- | The instance type of the EC2 instance. For information about available
-- instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
-- in the /Amazon Elastic Cloud Compute User Guide./
clcInstanceType :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceType = lens _clcInstanceType (\ s a -> s{_clcInstanceType = a});

-- | Indicates whether the instance is optimized for Amazon EBS I\/O. By
-- default, the instance is not optimized for EBS I\/O. The optimization
-- provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal I\/O performance. This
-- optimization is not available with all instance types. Additional usage
-- charges apply. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcEBSOptimized :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcEBSOptimized = lens _clcEBSOptimized (\ s a -> s{_clcEBSOptimized = a});

-- | The user data to make available to the launched EC2 instances. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- At this time, launch configurations don\'t support compressed (zipped)
-- user data files.
clcUserData :: Lens' CreateLaunchConfiguration (Maybe Text)
clcUserData = lens _clcUserData (\ s a -> s{_clcUserData = a});

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. This parameter is supported only if you are launching EC2-Classic
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcClassicLinkVPCId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcClassicLinkVPCId = lens _clcClassicLinkVPCId (\ s a -> s{_clcClassicLinkVPCId = a});

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance.
--
-- EC2 instances launched with an IAM role will automatically have AWS
-- security credentials available. You can use IAM roles with Auto Scaling
-- to automatically enable applications running on your EC2 instances to
-- securely access other AWS resources. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-iam-role.html Launch Auto Scaling Instances with an IAM Role>
-- in the /Auto Scaling Developer Guide/.
clcIAMInstanceProfile :: Lens' CreateLaunchConfiguration (Maybe Text)
clcIAMInstanceProfile = lens _clcIAMInstanceProfile (\ s a -> s{_clcIAMInstanceProfile = a});

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcImageId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcImageId = lens _clcImageId (\ s a -> s{_clcImageId = a});

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
clcPlacementTenancy :: Lens' CreateLaunchConfiguration (Maybe Text)
clcPlacementTenancy = lens _clcPlacementTenancy (\ s a -> s{_clcPlacementTenancy = a});

-- | One or more mappings that specify how block devices are exposed to the
-- instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
-- in the /Amazon Elastic Compute Cloud User Guide/.
clcBlockDeviceMappings :: Lens' CreateLaunchConfiguration [BlockDeviceMapping]
clcBlockDeviceMappings = lens _clcBlockDeviceMappings (\ s a -> s{_clcBlockDeviceMappings = a}) . _Default;

-- | The name of the launch configuration. This name must be unique within
-- the scope of your AWS account.
clcLaunchConfigurationName :: Lens' CreateLaunchConfiguration Text
clcLaunchConfigurationName = lens _clcLaunchConfigurationName (\ s a -> s{_clcLaunchConfigurationName = a});

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
               "InstanceId" =: _clcInstanceId,
               "SecurityGroups" =:
                 toQuery
                   (toQueryList "member" <$> _clcSecurityGroups),
               "AssociatePublicIpAddress" =:
                 _clcAssociatePublicIPAddress,
               "InstanceMonitoring" =: _clcInstanceMonitoring,
               "SpotPrice" =: _clcSpotPrice,
               "KeyName" =: _clcKeyName,
               "ClassicLinkVPCSecurityGroups" =:
                 toQuery
                   (toQueryList "member" <$>
                      _clcClassicLinkVPCSecurityGroups),
               "RamdiskId" =: _clcRAMDiskId,
               "KernelId" =: _clcKernelId,
               "InstanceType" =: _clcInstanceType,
               "EbsOptimized" =: _clcEBSOptimized,
               "UserData" =: _clcUserData,
               "ClassicLinkVPCId" =: _clcClassicLinkVPCId,
               "IamInstanceProfile" =: _clcIAMInstanceProfile,
               "ImageId" =: _clcImageId,
               "PlacementTenancy" =: _clcPlacementTenancy,
               "BlockDeviceMappings" =:
                 toQuery
                   (toQueryList "member" <$> _clcBlockDeviceMappings),
               "LaunchConfigurationName" =:
                 _clcLaunchConfigurationName]

-- | /See:/ 'createLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse =
    CreateLaunchConfigurationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLaunchConfigurationResponse' smart constructor.
createLaunchConfigurationResponse :: CreateLaunchConfigurationResponse
createLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
