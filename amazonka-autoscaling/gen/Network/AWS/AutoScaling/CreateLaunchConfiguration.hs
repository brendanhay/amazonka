{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a launch configuration. If you exceed your maximum limit of launch
-- configurations, which by default is 100 per region, the call fails. For
-- information about viewing and updating these limits, see
-- 'DescribeAccountLimits'.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateLaunchConfiguration.html>
module Network.AWS.AutoScaling.CreateLaunchConfiguration
    (
    -- * Request
      CreateLaunchConfiguration
    -- ** Request constructor
    , createLaunchConfiguration
    -- ** Request lenses
    , clcAssociatePublicIpAddress
    , clcBlockDeviceMappings
    , clcEbsOptimized
    , clcIamInstanceProfile
    , clcImageId
    , clcInstanceId
    , clcInstanceMonitoring
    , clcInstanceType
    , clcKernelId
    , clcKeyName
    , clcLaunchConfigurationName
    , clcPlacementTenancy
    , clcRamdiskId
    , clcSecurityGroups
    , clcSpotPrice
    , clcUserData

    -- * Response
    , CreateLaunchConfigurationResponse
    -- ** Response constructor
    , createLaunchConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data CreateLaunchConfiguration = CreateLaunchConfiguration
    { _clcAssociatePublicIpAddress :: Maybe Bool
    , _clcBlockDeviceMappings      :: List "BlockDeviceMappings" BlockDeviceMapping
    , _clcEbsOptimized             :: Maybe Bool
    , _clcIamInstanceProfile       :: Maybe Text
    , _clcImageId                  :: Maybe Text
    , _clcInstanceId               :: Maybe Text
    , _clcInstanceMonitoring       :: Maybe InstanceMonitoring
    , _clcInstanceType             :: Maybe Text
    , _clcKernelId                 :: Maybe Text
    , _clcKeyName                  :: Maybe Text
    , _clcLaunchConfigurationName  :: Text
    , _clcPlacementTenancy         :: Maybe Text
    , _clcRamdiskId                :: Maybe Text
    , _clcSecurityGroups           :: List "SecurityGroups" Text
    , _clcSpotPrice                :: Maybe Text
    , _clcUserData                 :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateLaunchConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcAssociatePublicIpAddress' @::@ 'Maybe' 'Bool'
--
-- * 'clcBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'clcEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'clcIamInstanceProfile' @::@ 'Maybe' 'Text'
--
-- * 'clcImageId' @::@ 'Maybe' 'Text'
--
-- * 'clcInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'clcInstanceMonitoring' @::@ 'Maybe' 'InstanceMonitoring'
--
-- * 'clcInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'clcKernelId' @::@ 'Maybe' 'Text'
--
-- * 'clcKeyName' @::@ 'Maybe' 'Text'
--
-- * 'clcLaunchConfigurationName' @::@ 'Text'
--
-- * 'clcPlacementTenancy' @::@ 'Maybe' 'Text'
--
-- * 'clcRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'clcSecurityGroups' @::@ ['Text']
--
-- * 'clcSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'clcUserData' @::@ 'Maybe' 'Text'
--
createLaunchConfiguration :: Text -- ^ 'clcLaunchConfigurationName'
                          -> CreateLaunchConfiguration
createLaunchConfiguration p1 = CreateLaunchConfiguration
    { _clcLaunchConfigurationName  = p1
    , _clcImageId                  = Nothing
    , _clcKeyName                  = Nothing
    , _clcSecurityGroups           = mempty
    , _clcUserData                 = Nothing
    , _clcInstanceId               = Nothing
    , _clcInstanceType             = Nothing
    , _clcKernelId                 = Nothing
    , _clcRamdiskId                = Nothing
    , _clcBlockDeviceMappings      = mempty
    , _clcInstanceMonitoring       = Nothing
    , _clcSpotPrice                = Nothing
    , _clcIamInstanceProfile       = Nothing
    , _clcEbsOptimized             = Nothing
    , _clcAssociatePublicIpAddress = Nothing
    , _clcPlacementTenancy         = Nothing
    }

-- | Used for groups that launch instances into a virtual private cloud (VPC).
-- Specifies whether to assign a public IP address to each instance. For
-- more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html
-- Auto Scaling and Amazon VPC> in the /Auto Scaling Developer Guide/. If
-- you specify a value for this parameter, be sure to specify at least one
-- subnet using the /VPCZoneIdentifier/ parameter when you create your
-- group. Default: If the instance is launched into a default subnet, the
-- default is @true@. If the instance is launched into a nondefault subnet,
-- the default is @false@. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide//as-supported-platforms.html
-- Supported Platforms> in the /Amazon Elastic Compute Cloud User Guide/.
clcAssociatePublicIpAddress :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcAssociatePublicIpAddress =
    lens _clcAssociatePublicIpAddress
        (\s a -> s { _clcAssociatePublicIpAddress = a })

-- | One or more mappings that specify how block devices are exposed to the
-- instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html
-- Block Device Mapping> in the /Amazon Elastic Compute Cloud User Guide/.
clcBlockDeviceMappings :: Lens' CreateLaunchConfiguration [BlockDeviceMapping]
clcBlockDeviceMappings =
    lens _clcBlockDeviceMappings (\s a -> s { _clcBlockDeviceMappings = a })
        . _List

-- | Indicates whether the instance is optimized for Amazon EBS I/O. By
-- default, the instance is not optimized for EBS I/O. The optimization
-- provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal I/O performance. This optimization
-- is not available with all instance types. Additional usage charges apply.
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html
-- Amazon EBS-Optimized Instances> in the /Amazon Elastic Compute Cloud User
-- Guide/.
clcEbsOptimized :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcEbsOptimized = lens _clcEbsOptimized (\s a -> s { _clcEbsOptimized = a })

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. Amazon EC2 instances
-- launched with an IAM role will automatically have AWS security
-- credentials available. You can use IAM roles with Auto Scaling to
-- automatically enable applications running on your Amazon EC2 instances to
-- securely access other AWS resources. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-iam-role.html
-- Launch Auto Scaling Instances with an IAM Role> in the /Auto Scaling
-- Developer Guide/.
clcIamInstanceProfile :: Lens' CreateLaunchConfiguration (Maybe Text)
clcIamInstanceProfile =
    lens _clcIamInstanceProfile (\s a -> s { _clcIamInstanceProfile = a })

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html
-- Finding an AMI> in the /Amazon Elastic Compute Cloud User Guide/.
clcImageId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcImageId = lens _clcImageId (\s a -> s { _clcImageId = a })

-- | The ID of the EC2 instance to use to create the launch configuration. The
-- new launch configuration derives attributes from the instance, with the
-- exception of the block device mapping. To create a launch configuration
-- with a block device mapping or override any other instance attributes,
-- specify them as part of the same request. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/create-lc-with-instanceID.html
-- Create a Launch Configuration Using an EC2 Instance> in the /Auto Scaling
-- Developer Guide/.
clcInstanceId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceId = lens _clcInstanceId (\s a -> s { _clcInstanceId = a })

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default. When detailed monitoring is enabled, Amazon
-- Cloudwatch generates metrics every minute and your account is charged a
-- fee. When you disable detailed monitoring, by specifying @False@,
-- Cloudwatch generates metrics every 5 minutes. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-instance-monitoring.html
-- Monitor Your Auto Scaling Instances> in the /Auto Scaling Developer
-- Guide/.
clcInstanceMonitoring :: Lens' CreateLaunchConfiguration (Maybe InstanceMonitoring)
clcInstanceMonitoring =
    lens _clcInstanceMonitoring (\s a -> s { _clcInstanceMonitoring = a })

-- | The instance type of the Amazon EC2 instance. For information about
-- available Amazon EC2 instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes
-- Available Instance Types> in the /Amazon Elastic Cloud Compute User
-- Guide./.
clcInstanceType :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceType = lens _clcInstanceType (\s a -> s { _clcInstanceType = a })

-- | The ID of the kernel associated with the Amazon EC2 AMI.
clcKernelId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKernelId = lens _clcKernelId (\s a -> s { _clcKernelId = a })

-- | The name of the key pair. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html
-- Amazon EC2 Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/.
clcKeyName :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKeyName = lens _clcKeyName (\s a -> s { _clcKeyName = a })

-- | The name of the launch configuration. This name must be unique within the
-- scope of your AWS account.
clcLaunchConfigurationName :: Lens' CreateLaunchConfiguration Text
clcLaunchConfigurationName =
    lens _clcLaunchConfigurationName
        (\s a -> s { _clcLaunchConfigurationName = a })

-- | The tenancy of the instance. An instance with a tenancy of @dedicated@
-- runs on single-tenant hardware and can only be launched in a VPC. You
-- must set the value of this parameter to @dedicated@ if want to launch
-- Dedicated Instances in a shared tenancy VPC (VPC with instance placement
-- tenancy attribute set to @default@). If you specify a value for this
-- parameter, be sure to specify at least one VPC subnet using the
-- /VPCZoneIdentifier/ parameter when you create your group. For more
-- information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html
-- Auto Scaling and Amazon VPC> in the /Auto Scaling Developer Guide/. Valid
-- values: @default@ | @dedicated@.
clcPlacementTenancy :: Lens' CreateLaunchConfiguration (Maybe Text)
clcPlacementTenancy =
    lens _clcPlacementTenancy (\s a -> s { _clcPlacementTenancy = a })

-- | The ID of the RAM disk associated with the Amazon EC2 AMI.
clcRamdiskId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcRamdiskId = lens _clcRamdiskId (\s a -> s { _clcRamdiskId = a })

-- | One or more security groups with which to associate the instances. If
-- your instances are launched in EC2-Classic, you can either specify
-- security group names or the security group IDs. For more information
-- about security groups for EC2-Classic, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html
-- Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User
-- Guide/. If your instances are launched in a VPC, specify security group
-- IDs. For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html
-- Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User
-- Guide/.
clcSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcSecurityGroups =
    lens _clcSecurityGroups (\s a -> s { _clcSecurityGroups = a })
        . _List

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot market price. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US-SpotInstances.html
-- Launch Spot Instances in Your Auto Scaling Group> in the /Auto Scaling
-- Developer Guide/.
clcSpotPrice :: Lens' CreateLaunchConfiguration (Maybe Text)
clcSpotPrice = lens _clcSpotPrice (\s a -> s { _clcSpotPrice = a })

-- | The user data to make available to the launched EC2 instances. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html
-- Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud
-- User Guide/. At this time, launch configurations don't support compressed
-- (zipped) user data files.
clcUserData :: Lens' CreateLaunchConfiguration (Maybe Text)
clcUserData = lens _clcUserData (\s a -> s { _clcUserData = a })

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLaunchConfigurationResponse' constructor.
createLaunchConfigurationResponse :: CreateLaunchConfigurationResponse
createLaunchConfigurationResponse = CreateLaunchConfigurationResponse

instance ToPath CreateLaunchConfiguration where
    toPath = const "/"

instance ToQuery CreateLaunchConfiguration where
    toQuery CreateLaunchConfiguration{..} = mconcat
        [ "AssociatePublicIpAddress" =? _clcAssociatePublicIpAddress
        , "BlockDeviceMappings"      =? _clcBlockDeviceMappings
        , "EbsOptimized"             =? _clcEbsOptimized
        , "IamInstanceProfile"       =? _clcIamInstanceProfile
        , "ImageId"                  =? _clcImageId
        , "InstanceId"               =? _clcInstanceId
        , "InstanceMonitoring"       =? _clcInstanceMonitoring
        , "InstanceType"             =? _clcInstanceType
        , "KernelId"                 =? _clcKernelId
        , "KeyName"                  =? _clcKeyName
        , "LaunchConfigurationName"  =? _clcLaunchConfigurationName
        , "PlacementTenancy"         =? _clcPlacementTenancy
        , "RamdiskId"                =? _clcRamdiskId
        , "SecurityGroups"           =? _clcSecurityGroups
        , "SpotPrice"                =? _clcSpotPrice
        , "UserData"                 =? _clcUserData
        ]

instance ToHeaders CreateLaunchConfiguration

instance AWSRequest CreateLaunchConfiguration where
    type Sv CreateLaunchConfiguration = AutoScaling
    type Rs CreateLaunchConfiguration = CreateLaunchConfigurationResponse

    request  = post "CreateLaunchConfiguration"
    response = nullResponse CreateLaunchConfigurationResponse
