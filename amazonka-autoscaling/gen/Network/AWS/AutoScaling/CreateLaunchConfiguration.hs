{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.AutoScaling.CreateLaunchConfiguration
    (
    -- * Request
      CreateLaunchConfigurationType
    -- ** Request constructor
    , createLaunchConfigurationType
    -- ** Request lenses
    , clctAssociatePublicIpAddress
    , clctBlockDeviceMappings
    , clctEbsOptimized
    , clctIamInstanceProfile
    , clctImageId
    , clctInstanceId
    , clctInstanceMonitoring
    , clctInstanceType
    , clctKernelId
    , clctKeyName
    , clctLaunchConfigurationName
    , clctPlacementTenancy
    , clctRamdiskId
    , clctSecurityGroups
    , clctSpotPrice
    , clctUserData

    -- * Response
    , CreateLaunchConfigurationResponse
    -- ** Response constructor
    , createLaunchConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data CreateLaunchConfigurationType = CreateLaunchConfigurationType
    { _clctAssociatePublicIpAddress :: Maybe Bool
    , _clctBlockDeviceMappings      :: [BlockDeviceMapping]
    , _clctEbsOptimized             :: Maybe Bool
    , _clctIamInstanceProfile       :: Maybe Text
    , _clctImageId                  :: Maybe Text
    , _clctInstanceId               :: Maybe Text
    , _clctInstanceMonitoring       :: Maybe InstanceMonitoring
    , _clctInstanceType             :: Maybe Text
    , _clctKernelId                 :: Maybe Text
    , _clctKeyName                  :: Maybe Text
    , _clctLaunchConfigurationName  :: Text
    , _clctPlacementTenancy         :: Maybe Text
    , _clctRamdiskId                :: Maybe Text
    , _clctSecurityGroups           :: [Text]
    , _clctSpotPrice                :: Maybe Text
    , _clctUserData                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateLaunchConfigurationType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clctAssociatePublicIpAddress' @::@ 'Maybe' 'Bool'
--
-- * 'clctBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'clctEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'clctIamInstanceProfile' @::@ 'Maybe' 'Text'
--
-- * 'clctImageId' @::@ 'Maybe' 'Text'
--
-- * 'clctInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'clctInstanceMonitoring' @::@ 'Maybe' 'InstanceMonitoring'
--
-- * 'clctInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'clctKernelId' @::@ 'Maybe' 'Text'
--
-- * 'clctKeyName' @::@ 'Maybe' 'Text'
--
-- * 'clctLaunchConfigurationName' @::@ 'Text'
--
-- * 'clctPlacementTenancy' @::@ 'Maybe' 'Text'
--
-- * 'clctRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'clctSecurityGroups' @::@ ['Text']
--
-- * 'clctSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'clctUserData' @::@ 'Maybe' 'Text'
--
createLaunchConfigurationType :: Text -- ^ 'clctLaunchConfigurationName'
                              -> CreateLaunchConfigurationType
createLaunchConfigurationType p1 = CreateLaunchConfigurationType
    { _clctLaunchConfigurationName  = p1
    , _clctImageId                  = Nothing
    , _clctKeyName                  = Nothing
    , _clctSecurityGroups           = mempty
    , _clctUserData                 = Nothing
    , _clctInstanceId               = Nothing
    , _clctInstanceType             = Nothing
    , _clctKernelId                 = Nothing
    , _clctRamdiskId                = Nothing
    , _clctBlockDeviceMappings      = mempty
    , _clctInstanceMonitoring       = Nothing
    , _clctSpotPrice                = Nothing
    , _clctIamInstanceProfile       = Nothing
    , _clctEbsOptimized             = Nothing
    , _clctAssociatePublicIpAddress = Nothing
    , _clctPlacementTenancy         = Nothing
    }

-- | Used for Auto Scaling groups that launch instances into an Amazon Virtual
-- Private Cloud (Amazon VPC). Specifies whether to assign a public IP
-- address to each instance launched in a Amazon VPC. For more information,
-- see Auto Scaling in Amazon Virtual Private Cloud. Default: If the
-- instance is launched into a default subnet in a default VPC, the default
-- is true. If the instance is launched into a nondefault subnet in a VPC,
-- the default is false. For information about default VPC and VPC
-- platforms, see Supported Platforms.
clctAssociatePublicIpAddress :: Lens' CreateLaunchConfigurationType (Maybe Bool)
clctAssociatePublicIpAddress =
    lens _clctAssociatePublicIpAddress
        (\s a -> s { _clctAssociatePublicIpAddress = a })

-- | A list of mappings that specify how block devices are exposed to the
-- instance. Each mapping is made up of a VirtualName, a DeviceName, and an
-- ebs data structure that contains information about the associated Elastic
-- Block Storage volume. For more information about Amazon EC2
-- BlockDeviceMappings, go to Block Device Mapping in the Amazon EC2 product
-- documentation.
clctBlockDeviceMappings :: Lens' CreateLaunchConfigurationType [BlockDeviceMapping]
clctBlockDeviceMappings =
    lens _clctBlockDeviceMappings (\s a -> s { _clctBlockDeviceMappings = a })

-- | Whether the instance is optimized for EBS I/O. The optimization provides
-- dedicated throughput to Amazon EBS and an optimized configuration stack
-- to provide optimal EBS I/O performance. This optimization is not
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance. By default the instance is not optimized
-- for EBS I/O. For information about EBS-optimized instances, go to
-- EBS-Optimized Instances in the Amazon Elastic Compute Cloud User Guide.
clctEbsOptimized :: Lens' CreateLaunchConfigurationType (Maybe Bool)
clctEbsOptimized = lens _clctEbsOptimized (\s a -> s { _clctEbsOptimized = a })

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. Amazon EC2 instances
-- launched with an IAM role will automatically have AWS security
-- credentials available. You can use IAM roles with Auto Scaling to
-- automatically enable applications running on your Amazon EC2 instances to
-- securely access other AWS resources. For information on launching EC2
-- instances with an IAM role, go to Launching Auto Scaling Instances With
-- an IAM Role in the Auto Scaling Developer Guide.
clctIamInstanceProfile :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctIamInstanceProfile =
    lens _clctIamInstanceProfile (\s a -> s { _clctIamInstanceProfile = a })

-- | Unique ID of the Amazon Machine Image (AMI) you want to use to launch
-- your EC2 instances. For information about finding Amazon EC2 AMIs, see
-- Finding a Suitable AMI in the Amazon Elastic Compute Cloud User Guide.
clctImageId :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctImageId = lens _clctImageId (\s a -> s { _clctImageId = a })

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
clctInstanceId :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctInstanceId = lens _clctInstanceId (\s a -> s { _clctInstanceId = a })

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default. When detailed monitoring is enabled, Amazon
-- Cloudwatch will generate metrics every minute and your account will be
-- charged a fee. When you disable detailed monitoring, by specifying False,
-- Cloudwatch will generate metrics every 5 minutes. For more information,
-- see Monitor Your Auto Scaling Instances. For information about Amazon
-- CloudWatch, see the Amazon CloudWatch Developer Guide.
clctInstanceMonitoring :: Lens' CreateLaunchConfigurationType (Maybe InstanceMonitoring)
clctInstanceMonitoring =
    lens _clctInstanceMonitoring (\s a -> s { _clctInstanceMonitoring = a })

-- | The instance type of the Amazon EC2 instance. For information about
-- available Amazon EC2 instance types, see Available Instance Types in the
-- Amazon Elastic Cloud Compute User Guide.
clctInstanceType :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctInstanceType = lens _clctInstanceType (\s a -> s { _clctInstanceType = a })

-- | The ID of the kernel associated with the Amazon EC2 AMI.
clctKernelId :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctKernelId = lens _clctKernelId (\s a -> s { _clctKernelId = a })

-- | The name of the Amazon EC2 key pair. For more information, see Getting a
-- Key Pair in the Amazon Elastic Compute Cloud User Guide.
clctKeyName :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctKeyName = lens _clctKeyName (\s a -> s { _clctKeyName = a })

-- | The name of the launch configuration to create.
clctLaunchConfigurationName :: Lens' CreateLaunchConfigurationType Text
clctLaunchConfigurationName =
    lens _clctLaunchConfigurationName
        (\s a -> s { _clctLaunchConfigurationName = a })

-- | The tenancy of the instance. An instance with a tenancy of dedicated runs
-- on single-tenant hardware and can only be launched in a VPC. You must set
-- the value of this parameter to dedicated if want to launch Dedicated
-- Instances in a shared tenancy VPC (VPC with instance placement tenancy
-- attribute set to default). If you specify a value for this parameter, be
-- sure to specify at least one VPC subnet using the VPCZoneIdentifier
-- parameter when you create your Auto Scaling group. For more information,
-- see Auto Scaling in Amazon Virtual Private Cloud in the Auto Scaling
-- Developer Guide. Valid values: default | dedicated.
clctPlacementTenancy :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctPlacementTenancy =
    lens _clctPlacementTenancy (\s a -> s { _clctPlacementTenancy = a })

-- | The ID of the RAM disk associated with the Amazon EC2 AMI.
clctRamdiskId :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctRamdiskId = lens _clctRamdiskId (\s a -> s { _clctRamdiskId = a })

-- | The security groups with which to associate Amazon EC2 or Amazon VPC
-- instances. If your instances are launched in EC2, you can either specify
-- Amazon EC2 security group names or the security group IDs. For more
-- information about Amazon EC2 security groups, see Using Security Groups
-- in the Amazon Elastic Compute Cloud User Guide. If your instances are
-- launched within VPC, specify Amazon VPC security group IDs. For more
-- information about Amazon VPC security groups, see Security Groups in the
-- Amazon Virtual Private Cloud User Guide.
clctSecurityGroups :: Lens' CreateLaunchConfigurationType [Text]
clctSecurityGroups =
    lens _clctSecurityGroups (\s a -> s { _clctSecurityGroups = a })

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot market price. For more information on
-- launching Spot Instances, see Using Auto Scaling to Launch Spot Instances
-- in the Auto Scaling Developer Guide.
clctSpotPrice :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctSpotPrice = lens _clctSpotPrice (\s a -> s { _clctSpotPrice = a })

-- | The user data to make available to the launched Amazon EC2 instances. For
-- more information about Amazon EC2 user data, see User Data Retrieval in
-- the Amazon Elastic Compute Cloud User Guide.
clctUserData :: Lens' CreateLaunchConfigurationType (Maybe Text)
clctUserData = lens _clctUserData (\s a -> s { _clctUserData = a })
instance ToQuery CreateLaunchConfigurationType

instance ToPath CreateLaunchConfigurationType where
    toPath = const "/"

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLaunchConfigurationResponse' constructor.
createLaunchConfigurationResponse :: CreateLaunchConfigurationResponse
createLaunchConfigurationResponse = CreateLaunchConfigurationResponse
instance FromXML CreateLaunchConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateLaunchConfigurationResponse"

instance AWSRequest CreateLaunchConfigurationType where
    type Sv CreateLaunchConfigurationType = AutoScaling
    type Rs CreateLaunchConfigurationType = CreateLaunchConfigurationResponse

    request  = post "CreateLaunchConfiguration"
    response = nullaryResponse CreateLaunchConfigurationResponse
