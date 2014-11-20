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

-- | Creates a new launch configuration. The launch configuration name must be
-- unique within the scope of the client's AWS account. The maximum limit of
-- launch configurations, which by default is 100, must not yet have been met;
-- otherwise, the call will fail. When created, the new launch configuration
-- is available for immediate use.
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

-- | Used for Auto Scaling groups that launch instances into an Amazon Virtual
-- Private Cloud (Amazon VPC). Specifies whether to assign a public IP
-- address to each instance launched in a Amazon VPC. For more information,
-- see Auto Scaling in Amazon Virtual Private Cloud. Default: If the
-- instance is launched into a default subnet in a default VPC, the default
-- is true. If the instance is launched into a nondefault subnet in a VPC,
-- the default is false. For information about default VPC and VPC
-- platforms, see Supported Platforms.
clcAssociatePublicIpAddress :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcAssociatePublicIpAddress =
    lens _clcAssociatePublicIpAddress
        (\s a -> s { _clcAssociatePublicIpAddress = a })

-- | A list of mappings that specify how block devices are exposed to the
-- instance. Each mapping is made up of a VirtualName, a DeviceName, and an
-- ebs data structure that contains information about the associated Elastic
-- Block Storage volume. For more information about Amazon EC2
-- BlockDeviceMappings, go to Block Device Mapping in the Amazon EC2 product
-- documentation.
clcBlockDeviceMappings :: Lens' CreateLaunchConfiguration [BlockDeviceMapping]
clcBlockDeviceMappings =
    lens _clcBlockDeviceMappings (\s a -> s { _clcBlockDeviceMappings = a })
        . _List

-- | Whether the instance is optimized for EBS I/O. The optimization provides
-- dedicated throughput to Amazon EBS and an optimized configuration stack
-- to provide optimal EBS I/O performance. This optimization is not
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance. By default the instance is not optimized
-- for EBS I/O. For information about EBS-optimized instances, go to
-- EBS-Optimized Instances in the Amazon Elastic Compute Cloud User Guide.
clcEbsOptimized :: Lens' CreateLaunchConfiguration (Maybe Bool)
clcEbsOptimized = lens _clcEbsOptimized (\s a -> s { _clcEbsOptimized = a })

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. Amazon EC2 instances
-- launched with an IAM role will automatically have AWS security
-- credentials available. You can use IAM roles with Auto Scaling to
-- automatically enable applications running on your Amazon EC2 instances to
-- securely access other AWS resources. For information on launching EC2
-- instances with an IAM role, go to Launching Auto Scaling Instances With
-- an IAM Role in the Auto Scaling Developer Guide.
clcIamInstanceProfile :: Lens' CreateLaunchConfiguration (Maybe Text)
clcIamInstanceProfile =
    lens _clcIamInstanceProfile (\s a -> s { _clcIamInstanceProfile = a })

-- | Unique ID of the Amazon Machine Image (AMI) you want to use to launch
-- your EC2 instances. For information about finding Amazon EC2 AMIs, see
-- Finding a Suitable AMI in the Amazon Elastic Compute Cloud User Guide.
clcImageId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcImageId = lens _clcImageId (\s a -> s { _clcImageId = a })

-- | The ID of the Amazon EC2 instance you want to use to create the launch
-- configuration. Use this attribute if you want the launch configuration to
-- derive its attributes from an EC2 instance. When you use an instance to
-- create a launch configuration, all you need to specify is the InstanceId.
-- The new launch configuration, by default, derives all the attributes from
-- the specified instance with the exception of BlockDeviceMapping. If you
-- want to create a launch configuration with BlockDeviceMapping or override
-- any other instance attributes, specify them as part of the same request.
-- For more information on using an InstanceID to create a launch
-- configuration, see Create a Launch Configuration Using an Amazon EC2
-- Instance in the Auto Scaling Developer Guide.
clcInstanceId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceId = lens _clcInstanceId (\s a -> s { _clcInstanceId = a })

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default. When detailed monitoring is enabled, Amazon
-- Cloudwatch will generate metrics every minute and your account will be
-- charged a fee. When you disable detailed monitoring, by specifying False,
-- Cloudwatch will generate metrics every 5 minutes. For more information,
-- see Monitor Your Auto Scaling Instances. For information about Amazon
-- CloudWatch, see the Amazon CloudWatch Developer Guide.
clcInstanceMonitoring :: Lens' CreateLaunchConfiguration (Maybe InstanceMonitoring)
clcInstanceMonitoring =
    lens _clcInstanceMonitoring (\s a -> s { _clcInstanceMonitoring = a })

-- | The instance type of the Amazon EC2 instance. For information about
-- available Amazon EC2 instance types, see Available Instance Types in the
-- Amazon Elastic Cloud Compute User Guide.
clcInstanceType :: Lens' CreateLaunchConfiguration (Maybe Text)
clcInstanceType = lens _clcInstanceType (\s a -> s { _clcInstanceType = a })

-- | The ID of the kernel associated with the Amazon EC2 AMI.
clcKernelId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKernelId = lens _clcKernelId (\s a -> s { _clcKernelId = a })

-- | The name of the Amazon EC2 key pair. For more information, see Getting a
-- Key Pair in the Amazon Elastic Compute Cloud User Guide.
clcKeyName :: Lens' CreateLaunchConfiguration (Maybe Text)
clcKeyName = lens _clcKeyName (\s a -> s { _clcKeyName = a })

-- | The name of the launch configuration to create.
clcLaunchConfigurationName :: Lens' CreateLaunchConfiguration Text
clcLaunchConfigurationName =
    lens _clcLaunchConfigurationName
        (\s a -> s { _clcLaunchConfigurationName = a })

-- | The tenancy of the instance. An instance with a tenancy of dedicated runs
-- on single-tenant hardware and can only be launched in a VPC. You must set
-- the value of this parameter to dedicated if want to launch Dedicated
-- Instances in a shared tenancy VPC (VPC with instance placement tenancy
-- attribute set to default). If you specify a value for this parameter, be
-- sure to specify at least one VPC subnet using the VPCZoneIdentifier
-- parameter when you create your Auto Scaling group. For more information,
-- see Auto Scaling in Amazon Virtual Private Cloud in the Auto Scaling
-- Developer Guide. Valid values: default | dedicated.
clcPlacementTenancy :: Lens' CreateLaunchConfiguration (Maybe Text)
clcPlacementTenancy =
    lens _clcPlacementTenancy (\s a -> s { _clcPlacementTenancy = a })

-- | The ID of the RAM disk associated with the Amazon EC2 AMI.
clcRamdiskId :: Lens' CreateLaunchConfiguration (Maybe Text)
clcRamdiskId = lens _clcRamdiskId (\s a -> s { _clcRamdiskId = a })

-- | The security groups with which to associate Amazon EC2 or Amazon VPC
-- instances. If your instances are launched in EC2, you can either specify
-- Amazon EC2 security group names or the security group IDs. For more
-- information about Amazon EC2 security groups, see Using Security Groups
-- in the Amazon Elastic Compute Cloud User Guide. If your instances are
-- launched within VPC, specify Amazon VPC security group IDs. For more
-- information about Amazon VPC security groups, see Security Groups in the
-- Amazon Virtual Private Cloud User Guide.
clcSecurityGroups :: Lens' CreateLaunchConfiguration [Text]
clcSecurityGroups =
    lens _clcSecurityGroups (\s a -> s { _clcSecurityGroups = a })
        . _List

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot market price. For more information on
-- launching Spot Instances, see Using Auto Scaling to Launch Spot Instances
-- in the Auto Scaling Developer Guide.
clcSpotPrice :: Lens' CreateLaunchConfiguration (Maybe Text)
clcSpotPrice = lens _clcSpotPrice (\s a -> s { _clcSpotPrice = a })

-- | The user data to make available to the launched Amazon EC2 instances. For
-- more information about Amazon EC2 user data, see User Data Retrieval in
-- the Amazon Elastic Compute Cloud User Guide.
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


Some kind of operator / class to check the types whether to continue?
