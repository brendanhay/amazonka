{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch configuration.
--
--
-- If you exceed your maximum limit of launch configurations, the call fails. For information about viewing this limit, see 'DescribeAccountLimits' . For information about updating this limit, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-account-limits.html Auto Scaling Limits> in the /Auto Scaling User Guide/ .
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/LaunchConfiguration.html Launch Configurations> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.CreateLaunchConfiguration
    (
    -- * Creating a Request
      createLaunchConfiguration
    , CreateLaunchConfiguration
    -- * Request Lenses
    , clcInstanceId
    , clcAssociatePublicIPAddress
    , clcSecurityGroups
    , clcSpotPrice
    , clcInstanceMonitoring
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

    -- * Destructuring the Response
    , createLaunchConfigurationResponse
    , CreateLaunchConfigurationResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLaunchConfiguration' smart constructor.
data CreateLaunchConfiguration = CreateLaunchConfiguration'
  { _clcInstanceId                   :: !(Maybe Text)
  , _clcAssociatePublicIPAddress     :: !(Maybe Bool)
  , _clcSecurityGroups               :: !(Maybe [Text])
  , _clcSpotPrice                    :: !(Maybe Text)
  , _clcInstanceMonitoring           :: !(Maybe InstanceMonitoring)
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clcInstanceId' - The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, with the exception of the block device mapping. If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ . To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/create-lc-with-instanceID.html Create a Launch Configuration Using an EC2 Instance> in the /Auto Scaling User Guide/ .
--
-- * 'clcAssociatePublicIPAddress' - Used for groups that launch instances into a virtual private cloud (VPC). Specifies whether to assign a public IP address to each instance. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ . If you specify this parameter, be sure to specify at least one subnet when you create your group. Default: If the instance is launched into a default subnet, the default is to assign a public IP address. If the instance is launched into a nondefault subnet, the default is not to assign a public IP address.
--
-- * 'clcSecurityGroups' - One or more security groups with which to associate the instances. If your instances are launched in EC2-Classic, you can either specify security group names or the security group IDs. For more information about security groups for EC2-Classic, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ . If your instances are launched into a VPC, specify security group IDs. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- * 'clcSpotPrice' - The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot market price. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/US-SpotInstances.html Launching Spot Instances in Your Auto Scaling Group> in the /Auto Scaling User Guide/ .
--
-- * 'clcInstanceMonitoring' - Enables detailed monitoring (@true@ ) or basic monitoring (@false@ ) for the Auto Scaling instances. The default is @true@ .
--
-- * 'clcKeyName' - The name of the key pair. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcClassicLinkVPCSecurityGroups' - The IDs of one or more security groups for the specified ClassicLink-enabled VPC. This parameter is required if you specify a ClassicLink-enabled VPC, and is not supported otherwise. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcRAMDiskId' - The ID of the RAM disk associated with the AMI.
--
-- * 'clcKernelId' - The ID of the kernel associated with the AMI.
--
-- * 'clcInstanceType' - The instance type of the EC2 instance. If you do not specify @InstanceId@ , you must specify @InstanceType@ . For information about available instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon Elastic Compute Cloud User Guide./
--
-- * 'clcEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. By default, the instance is not optimized for EBS I/O. The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional usage charges apply. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcUserData' - The user data to make available to the launched EC2 instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcClassicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. This parameter is supported only if you are launching EC2-Classic instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcIAMInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. EC2 instances launched with an IAM role will automatically have AWS security credentials available. You can use IAM roles with Auto Scaling to automatically enable applications running on your EC2 instances to securely access other AWS resources. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/us-iam-role.html Launch Auto Scaling Instances with an IAM Role> in the /Auto Scaling User Guide/ .
--
-- * 'clcImageId' - The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. If you do not specify @InstanceId@ , you must specify @ImageId@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcPlacementTenancy' - The tenancy of the instance. An instance with a tenancy of @dedicated@ runs on single-tenant hardware and can only be launched into a VPC. You must set the value of this parameter to @dedicated@ if want to launch Dedicated Instances into a shared tenancy VPC (VPC with instance placement tenancy attribute set to @default@ ). If you specify this parameter, be sure to specify at least one subnet when you create your group. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ . Valid values: @default@ | @dedicated@
--
-- * 'clcBlockDeviceMappings' - One or more mappings that specify how block devices are exposed to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'clcLaunchConfigurationName' - The name of the launch configuration. This name must be unique within the scope of your AWS account.
createLaunchConfiguration
    :: Text -- ^ 'clcLaunchConfigurationName'
    -> CreateLaunchConfiguration
createLaunchConfiguration pLaunchConfigurationName_ =
  CreateLaunchConfiguration'
    { _clcInstanceId = Nothing
    , _clcAssociatePublicIPAddress = Nothing
    , _clcSecurityGroups = Nothing
    , _clcSpotPrice = Nothing
    , _clcInstanceMonitoring = Nothing
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
    , _clcLaunchConfigurationName = pLaunchConfigurationName_
    }


-- | The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, with the exception of the block device mapping. If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ . To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/create-lc-with-instanceID.html Create a Launch Configuration Using an EC2 Instance> in the /Auto Scaling User Guide/ .
clcInstanceId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceId = lens _clcInstanceId (\ s a -> s{_clcInstanceId = a})

-- | Used for groups that launch instances into a virtual private cloud (VPC). Specifies whether to assign a public IP address to each instance. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ . If you specify this parameter, be sure to specify at least one subnet when you create your group. Default: If the instance is launched into a default subnet, the default is to assign a public IP address. If the instance is launched into a nondefault subnet, the default is not to assign a public IP address.
clcAssociatePublicIPAddress :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcAssociatePublicIPAddress = lens _clcAssociatePublicIPAddress (\ s a -> s{_clcAssociatePublicIPAddress = a})

-- | One or more security groups with which to associate the instances. If your instances are launched in EC2-Classic, you can either specify security group names or the security group IDs. For more information about security groups for EC2-Classic, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ . If your instances are launched into a VPC, specify security group IDs. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
clcSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcSecurityGroups = lens _clcSecurityGroups (\ s a -> s{_clcSecurityGroups = a}) . _Default . _Coerce

-- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot market price. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/US-SpotInstances.html Launching Spot Instances in Your Auto Scaling Group> in the /Auto Scaling User Guide/ .
clcSpotPrice :: Lens' CreateLaunchConfiguration (Maybe Text)
clcSpotPrice = lens _clcSpotPrice (\ s a -> s{_clcSpotPrice = a})

-- | Enables detailed monitoring (@true@ ) or basic monitoring (@false@ ) for the Auto Scaling instances. The default is @true@ .
clcInstanceMonitoring :: Lens' CreateLaunchConfiguration (Maybe InstanceMonitoring)
clcInstanceMonitoring = lens _clcInstanceMonitoring (\ s a -> s{_clcInstanceMonitoring = a})

-- | The name of the key pair. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
clcKeyName :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKeyName = lens _clcKeyName (\ s a -> s{_clcKeyName = a})

-- | The IDs of one or more security groups for the specified ClassicLink-enabled VPC. This parameter is required if you specify a ClassicLink-enabled VPC, and is not supported otherwise. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
clcClassicLinkVPCSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcClassicLinkVPCSecurityGroups = lens _clcClassicLinkVPCSecurityGroups (\ s a -> s{_clcClassicLinkVPCSecurityGroups = a}) . _Default . _Coerce

-- | The ID of the RAM disk associated with the AMI.
clcRAMDiskId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcRAMDiskId = lens _clcRAMDiskId (\ s a -> s{_clcRAMDiskId = a})

-- | The ID of the kernel associated with the AMI.
clcKernelId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKernelId = lens _clcKernelId (\ s a -> s{_clcKernelId = a})

-- | The instance type of the EC2 instance. If you do not specify @InstanceId@ , you must specify @InstanceType@ . For information about available instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon Elastic Compute Cloud User Guide./
clcInstanceType :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceType = lens _clcInstanceType (\ s a -> s{_clcInstanceType = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O. By default, the instance is not optimized for EBS I/O. The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional usage charges apply. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
clcEBSOptimized :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcEBSOptimized = lens _clcEBSOptimized (\ s a -> s{_clcEBSOptimized = a})

-- | The user data to make available to the launched EC2 instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
clcUserData :: Lens' CreateLaunchConfiguration (Maybe Text)
clcUserData = lens _clcUserData (\ s a -> s{_clcUserData = a})

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. This parameter is supported only if you are launching EC2-Classic instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
clcClassicLinkVPCId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcClassicLinkVPCId = lens _clcClassicLinkVPCId (\ s a -> s{_clcClassicLinkVPCId = a})

-- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. EC2 instances launched with an IAM role will automatically have AWS security credentials available. You can use IAM roles with Auto Scaling to automatically enable applications running on your EC2 instances to securely access other AWS resources. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/us-iam-role.html Launch Auto Scaling Instances with an IAM Role> in the /Auto Scaling User Guide/ .
clcIAMInstanceProfile :: Lens' CreateLaunchConfiguration (Maybe Text)
clcIAMInstanceProfile = lens _clcIAMInstanceProfile (\ s a -> s{_clcIAMInstanceProfile = a})

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. If you do not specify @InstanceId@ , you must specify @ImageId@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon Elastic Compute Cloud User Guide/ .
clcImageId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcImageId = lens _clcImageId (\ s a -> s{_clcImageId = a})

-- | The tenancy of the instance. An instance with a tenancy of @dedicated@ runs on single-tenant hardware and can only be launched into a VPC. You must set the value of this parameter to @dedicated@ if want to launch Dedicated Instances into a shared tenancy VPC (VPC with instance placement tenancy attribute set to @default@ ). If you specify this parameter, be sure to specify at least one subnet when you create your group. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ . Valid values: @default@ | @dedicated@
clcPlacementTenancy :: Lens' CreateLaunchConfiguration (Maybe Text)
clcPlacementTenancy = lens _clcPlacementTenancy (\ s a -> s{_clcPlacementTenancy = a})

-- | One or more mappings that specify how block devices are exposed to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon Elastic Compute Cloud User Guide/ .
clcBlockDeviceMappings :: Lens' CreateLaunchConfiguration [BlockDeviceMapping]
clcBlockDeviceMappings = lens _clcBlockDeviceMappings (\ s a -> s{_clcBlockDeviceMappings = a}) . _Default . _Coerce

-- | The name of the launch configuration. This name must be unique within the scope of your AWS account.
clcLaunchConfigurationName :: Lens' CreateLaunchConfiguration Text
clcLaunchConfigurationName = lens _clcLaunchConfigurationName (\ s a -> s{_clcLaunchConfigurationName = a})

instance AWSRequest CreateLaunchConfiguration where
        type Rs CreateLaunchConfiguration =
             CreateLaunchConfigurationResponse
        request = postQuery autoScaling
        response
          = receiveNull CreateLaunchConfigurationResponse'

instance Hashable CreateLaunchConfiguration where

instance NFData CreateLaunchConfiguration where

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
               "AssociatePublicIpAddress" =:
                 _clcAssociatePublicIPAddress,
               "SecurityGroups" =:
                 toQuery
                   (toQueryList "member" <$> _clcSecurityGroups),
               "SpotPrice" =: _clcSpotPrice,
               "InstanceMonitoring" =: _clcInstanceMonitoring,
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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLaunchConfigurationResponse' with the minimum fields required to make a request.
--
createLaunchConfigurationResponse
    :: CreateLaunchConfigurationResponse
createLaunchConfigurationResponse = CreateLaunchConfigurationResponse'


instance NFData CreateLaunchConfigurationResponse
         where
