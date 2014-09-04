{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.CreateLaunchConfiguration
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
-- https://autoscaling.amazonaws.com/?LaunchConfigurationName=my-test-lc
-- &AssociatePublicIpAddress=true &PlacementTenancy=dedicated
-- &ImageId=ami-0078da69 &InstanceType=m1.small
-- &Action=CreateLaunchConfiguration &AUTHPARAMS
-- 7c6e177f-f082-11e1-ac58-3714bEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.CreateLaunchConfiguration
    (
    -- * Request
      CreateLaunchConfiguration
    -- ** Request constructor
    , mkCreateLaunchConfigurationType
    -- ** Request lenses
    , clctLaunchConfigurationName
    , clctImageId
    , clctKeyName
    , clctSecurityGroups
    , clctUserData
    , clctInstanceId
    , clctInstanceType
    , clctKernelId
    , clctRamdiskId
    , clctBlockDeviceMappings
    , clctInstanceMonitoring
    , clctSpotPrice
    , clctIamInstanceProfile
    , clctEbsOptimized
    , clctAssociatePublicIpAddress
    , clctPlacementTenancy

    -- * Response
    , CreateLaunchConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLaunchConfiguration' request.
mkCreateLaunchConfigurationType :: Text -- ^ 'clctLaunchConfigurationName'
                                -> CreateLaunchConfiguration
mkCreateLaunchConfigurationType p1 = CreateLaunchConfiguration
    { _clctLaunchConfigurationName = p1
    , _clctImageId = Nothing
    , _clctKeyName = Nothing
    , _clctSecurityGroups = mempty
    , _clctUserData = Nothing
    , _clctInstanceId = Nothing
    , _clctInstanceType = Nothing
    , _clctKernelId = Nothing
    , _clctRamdiskId = Nothing
    , _clctBlockDeviceMappings = mempty
    , _clctInstanceMonitoring = Nothing
    , _clctSpotPrice = Nothing
    , _clctIamInstanceProfile = Nothing
    , _clctEbsOptimized = Nothing
    , _clctAssociatePublicIpAddress = Nothing
    , _clctPlacementTenancy = Nothing
    }
{-# INLINE mkCreateLaunchConfigurationType #-}

data CreateLaunchConfiguration = CreateLaunchConfiguration
    { _clctLaunchConfigurationName :: Text
      -- ^ The name of the launch configuration to create.
    , _clctImageId :: Maybe Text
      -- ^ Unique ID of the Amazon Machine Image (AMI) you want to use to
      -- launch your EC2 instances. For information about finding Amazon
      -- EC2 AMIs, see Finding a Suitable AMI in the Amazon Elastic
      -- Compute Cloud User Guide.
    , _clctKeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair. For more information, see
      -- Getting a Key Pair in the Amazon Elastic Compute Cloud User
      -- Guide.
    , _clctSecurityGroups :: [Text]
      -- ^ The security groups with which to associate Amazon EC2 or Amazon
      -- VPC instances. If your instances are launched in EC2, you can
      -- either specify Amazon EC2 security group names or the security
      -- group IDs. For more information about Amazon EC2 security groups,
      -- see Using Security Groups in the Amazon Elastic Compute Cloud
      -- User Guide. If your instances are launched within VPC, specify
      -- Amazon VPC security group IDs. For more information about Amazon
      -- VPC security groups, see Security Groups in the Amazon Virtual
      -- Private Cloud User Guide.
    , _clctUserData :: Maybe ByteString
      -- ^ The user data to make available to the launched Amazon EC2
      -- instances. For more information about Amazon EC2 user data, see
      -- User Data Retrieval in the Amazon Elastic Compute Cloud User
      -- Guide. At this time, Auto Scaling launch configurations don't
      -- support compressed (e.g. zipped) user data files.
    , _clctInstanceId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance you want to use to create the
      -- launch configuration. Use this attribute if you want the launch
      -- configuration to derive its attributes from an EC2 instance. When
      -- you use an instance to create a launch configuration, all you
      -- need to specify is the InstanceId. The new launch configuration,
      -- by default, derives all the attributes from the specified
      -- instance with the exception of BlockDeviceMapping. If you want to
      -- create a launch configuration with BlockDeviceMapping or override
      -- any other instance attributes, specify them as part of the same
      -- request. For more information on using an InstanceID to create a
      -- launch configuration, see Create a Launch Configuration Using an
      -- Amazon EC2 Instance in the Auto Scaling Developer Guide.
    , _clctInstanceType :: Maybe Text
      -- ^ The instance type of the Amazon EC2 instance. For information
      -- about available Amazon EC2 instance types, see Available Instance
      -- Types in the Amazon Elastic Cloud Compute User Guide.
    , _clctKernelId :: Maybe Text
      -- ^ The ID of the kernel associated with the Amazon EC2 AMI.
    , _clctRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk associated with the Amazon EC2 AMI.
    , _clctBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ A list of mappings that specify how block devices are exposed to
      -- the instance. Each mapping is made up of a VirtualName, a
      -- DeviceName, and an ebs data structure that contains information
      -- about the associated Elastic Block Storage volume. For more
      -- information about Amazon EC2 BlockDeviceMappings, go to Block
      -- Device Mapping in the Amazon EC2 product documentation.
    , _clctInstanceMonitoring :: Maybe InstanceMonitoring
      -- ^ Enables detailed monitoring if it is disabled. Detailed
      -- monitoring is enabled by default. When detailed monitoring is
      -- enabled, Amazon Cloudwatch will generate metrics every minute and
      -- your account will be charged a fee. When you disable detailed
      -- monitoring, by specifying False, Cloudwatch will generate metrics
      -- every 5 minutes. For more information, see Monitor Your Auto
      -- Scaling Instances. For information about Amazon CloudWatch, see
      -- the Amazon CloudWatch Developer Guide.
    , _clctSpotPrice :: Maybe Text
      -- ^ The maximum hourly price to be paid for any Spot Instance
      -- launched to fulfill the request. Spot Instances are launched when
      -- the price you specify exceeds the current Spot market price. For
      -- more information on launching Spot Instances, see Using Auto
      -- Scaling to Launch Spot Instances in the Auto Scaling Developer
      -- Guide.
    , _clctIamInstanceProfile :: Maybe Text
      -- ^ The name or the Amazon Resource Name (ARN) of the instance
      -- profile associated with the IAM role for the instance. Amazon EC2
      -- instances launched with an IAM role will automatically have AWS
      -- security credentials available. You can use IAM roles with Auto
      -- Scaling to automatically enable applications running on your
      -- Amazon EC2 instances to securely access other AWS resources. For
      -- information on launching EC2 instances with an IAM role, go to
      -- Launching Auto Scaling Instances With an IAM Role in the Auto
      -- Scaling Developer Guide.
    , _clctEbsOptimized :: Maybe Bool
      -- ^ Whether the instance is optimized for EBS I/O. The optimization
      -- provides dedicated throughput to Amazon EBS and an optimized
      -- configuration stack to provide optimal EBS I/O performance. This
      -- optimization is not available with all instance types. Additional
      -- usage charges apply when using an EBS Optimized instance. By
      -- default the instance is not optimized for EBS I/O. For
      -- information about EBS-optimized instances, go to EBS-Optimized
      -- Instances in the Amazon Elastic Compute Cloud User Guide.
    , _clctAssociatePublicIpAddress :: Maybe Bool
      -- ^ Used for Auto Scaling groups that launch instances into an Amazon
      -- Virtual Private Cloud (Amazon VPC). Specifies whether to assign a
      -- public IP address to each instance launched in a Amazon VPC. For
      -- more information, see Auto Scaling in Amazon Virtual Private
      -- Cloud. If you specify a value for this parameter, be sure to
      -- specify at least one VPC subnet using the VPCZoneIdentifier
      -- parameter when you create your Auto Scaling group. Default: If
      -- the instance is launched into a default subnet in a default VPC,
      -- the default is true. If the instance is launched into a
      -- nondefault subnet in a VPC, the default is false. For information
      -- about default VPC and VPC platforms, see Supported Platforms.
    , _clctPlacementTenancy :: Maybe Text
      -- ^ The tenancy of the instance. An instance with a tenancy of
      -- dedicated runs on single-tenant hardware and can only be launched
      -- in a VPC. You must set the value of this parameter to dedicated
      -- if want to launch Dedicated Instances in a shared tenancy VPC
      -- (VPC with instance placement tenancy attribute set to default).
      -- If you specify a value for this parameter, be sure to specify at
      -- least one VPC subnet using the VPCZoneIdentifier parameter when
      -- you create your Auto Scaling group. For more information, see
      -- Auto Scaling in Amazon Virtual Private Cloud in the Auto Scaling
      -- Developer Guide. Valid values: default | dedicated.
    } deriving (Show, Generic)

-- | The name of the launch configuration to create.
clctLaunchConfigurationName :: Lens' CreateLaunchConfiguration (Text)
clctLaunchConfigurationName = lens _clctLaunchConfigurationName (\s a -> s { _clctLaunchConfigurationName = a })
{-# INLINE clctLaunchConfigurationName #-}

-- | Unique ID of the Amazon Machine Image (AMI) you want to use to launch your
-- EC2 instances. For information about finding Amazon EC2 AMIs, see Finding a
-- Suitable AMI in the Amazon Elastic Compute Cloud User Guide.
clctImageId :: Lens' CreateLaunchConfiguration (Maybe Text)
clctImageId = lens _clctImageId (\s a -> s { _clctImageId = a })
{-# INLINE clctImageId #-}

-- | The name of the Amazon EC2 key pair. For more information, see Getting a
-- Key Pair in the Amazon Elastic Compute Cloud User Guide.
clctKeyName :: Lens' CreateLaunchConfiguration (Maybe Text)
clctKeyName = lens _clctKeyName (\s a -> s { _clctKeyName = a })
{-# INLINE clctKeyName #-}

-- | The security groups with which to associate Amazon EC2 or Amazon VPC
-- instances. If your instances are launched in EC2, you can either specify
-- Amazon EC2 security group names or the security group IDs. For more
-- information about Amazon EC2 security groups, see Using Security Groups in
-- the Amazon Elastic Compute Cloud User Guide. If your instances are launched
-- within VPC, specify Amazon VPC security group IDs. For more information
-- about Amazon VPC security groups, see Security Groups in the Amazon Virtual
-- Private Cloud User Guide.
clctSecurityGroups :: Lens' CreateLaunchConfiguration ([Text])
clctSecurityGroups = lens _clctSecurityGroups (\s a -> s { _clctSecurityGroups = a })
{-# INLINE clctSecurityGroups #-}

-- | The user data to make available to the launched Amazon EC2 instances. For
-- more information about Amazon EC2 user data, see User Data Retrieval in the
-- Amazon Elastic Compute Cloud User Guide. At this time, Auto Scaling launch
-- configurations don't support compressed (e.g. zipped) user data files.
clctUserData :: Lens' CreateLaunchConfiguration (Maybe ByteString)
clctUserData = lens _clctUserData (\s a -> s { _clctUserData = a })
{-# INLINE clctUserData #-}

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
clctInstanceId :: Lens' CreateLaunchConfiguration (Maybe Text)
clctInstanceId = lens _clctInstanceId (\s a -> s { _clctInstanceId = a })
{-# INLINE clctInstanceId #-}

-- | The instance type of the Amazon EC2 instance. For information about
-- available Amazon EC2 instance types, see Available Instance Types in the
-- Amazon Elastic Cloud Compute User Guide.
clctInstanceType :: Lens' CreateLaunchConfiguration (Maybe Text)
clctInstanceType = lens _clctInstanceType (\s a -> s { _clctInstanceType = a })
{-# INLINE clctInstanceType #-}

-- | The ID of the kernel associated with the Amazon EC2 AMI.
clctKernelId :: Lens' CreateLaunchConfiguration (Maybe Text)
clctKernelId = lens _clctKernelId (\s a -> s { _clctKernelId = a })
{-# INLINE clctKernelId #-}

-- | The ID of the RAM disk associated with the Amazon EC2 AMI.
clctRamdiskId :: Lens' CreateLaunchConfiguration (Maybe Text)
clctRamdiskId = lens _clctRamdiskId (\s a -> s { _clctRamdiskId = a })
{-# INLINE clctRamdiskId #-}

-- | A list of mappings that specify how block devices are exposed to the
-- instance. Each mapping is made up of a VirtualName, a DeviceName, and an
-- ebs data structure that contains information about the associated Elastic
-- Block Storage volume. For more information about Amazon EC2
-- BlockDeviceMappings, go to Block Device Mapping in the Amazon EC2 product
-- documentation.
clctBlockDeviceMappings :: Lens' CreateLaunchConfiguration ([BlockDeviceMapping])
clctBlockDeviceMappings = lens _clctBlockDeviceMappings (\s a -> s { _clctBlockDeviceMappings = a })
{-# INLINE clctBlockDeviceMappings #-}

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default. When detailed monitoring is enabled, Amazon Cloudwatch
-- will generate metrics every minute and your account will be charged a fee.
-- When you disable detailed monitoring, by specifying False, Cloudwatch will
-- generate metrics every 5 minutes. For more information, see Monitor Your
-- Auto Scaling Instances. For information about Amazon CloudWatch, see the
-- Amazon CloudWatch Developer Guide.
clctInstanceMonitoring :: Lens' CreateLaunchConfiguration (Maybe InstanceMonitoring)
clctInstanceMonitoring = lens _clctInstanceMonitoring (\s a -> s { _clctInstanceMonitoring = a })
{-# INLINE clctInstanceMonitoring #-}

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you specify
-- exceeds the current Spot market price. For more information on launching
-- Spot Instances, see Using Auto Scaling to Launch Spot Instances in the Auto
-- Scaling Developer Guide.
clctSpotPrice :: Lens' CreateLaunchConfiguration (Maybe Text)
clctSpotPrice = lens _clctSpotPrice (\s a -> s { _clctSpotPrice = a })
{-# INLINE clctSpotPrice #-}

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. Amazon EC2 instances
-- launched with an IAM role will automatically have AWS security credentials
-- available. You can use IAM roles with Auto Scaling to automatically enable
-- applications running on your Amazon EC2 instances to securely access other
-- AWS resources. For information on launching EC2 instances with an IAM role,
-- go to Launching Auto Scaling Instances With an IAM Role in the Auto Scaling
-- Developer Guide.
clctIamInstanceProfile :: Lens' CreateLaunchConfiguration (Maybe Text)
clctIamInstanceProfile = lens _clctIamInstanceProfile (\s a -> s { _clctIamInstanceProfile = a })
{-# INLINE clctIamInstanceProfile #-}

-- | Whether the instance is optimized for EBS I/O. The optimization provides
-- dedicated throughput to Amazon EBS and an optimized configuration stack to
-- provide optimal EBS I/O performance. This optimization is not available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance. By default the instance is not optimized for EBS I/O.
-- For information about EBS-optimized instances, go to EBS-Optimized
-- Instances in the Amazon Elastic Compute Cloud User Guide.
clctEbsOptimized :: Lens' CreateLaunchConfiguration (Maybe Bool)
clctEbsOptimized = lens _clctEbsOptimized (\s a -> s { _clctEbsOptimized = a })
{-# INLINE clctEbsOptimized #-}

-- | Used for Auto Scaling groups that launch instances into an Amazon Virtual
-- Private Cloud (Amazon VPC). Specifies whether to assign a public IP address
-- to each instance launched in a Amazon VPC. For more information, see Auto
-- Scaling in Amazon Virtual Private Cloud. If you specify a value for this
-- parameter, be sure to specify at least one VPC subnet using the
-- VPCZoneIdentifier parameter when you create your Auto Scaling group.
-- Default: If the instance is launched into a default subnet in a default
-- VPC, the default is true. If the instance is launched into a nondefault
-- subnet in a VPC, the default is false. For information about default VPC
-- and VPC platforms, see Supported Platforms.
clctAssociatePublicIpAddress :: Lens' CreateLaunchConfiguration (Maybe Bool)
clctAssociatePublicIpAddress = lens _clctAssociatePublicIpAddress (\s a -> s { _clctAssociatePublicIpAddress = a })
{-# INLINE clctAssociatePublicIpAddress #-}

-- | The tenancy of the instance. An instance with a tenancy of dedicated runs
-- on single-tenant hardware and can only be launched in a VPC. You must set
-- the value of this parameter to dedicated if want to launch Dedicated
-- Instances in a shared tenancy VPC (VPC with instance placement tenancy
-- attribute set to default). If you specify a value for this parameter, be
-- sure to specify at least one VPC subnet using the VPCZoneIdentifier
-- parameter when you create your Auto Scaling group. For more information,
-- see Auto Scaling in Amazon Virtual Private Cloud in the Auto Scaling
-- Developer Guide. Valid values: default | dedicated.
clctPlacementTenancy :: Lens' CreateLaunchConfiguration (Maybe Text)
clctPlacementTenancy = lens _clctPlacementTenancy (\s a -> s { _clctPlacementTenancy = a })
{-# INLINE clctPlacementTenancy #-}

instance ToQuery CreateLaunchConfiguration where
    toQuery = genericQuery def

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateLaunchConfiguration where
    type Sv CreateLaunchConfiguration = AutoScaling
    type Rs CreateLaunchConfiguration = CreateLaunchConfigurationResponse

    request = post "CreateLaunchConfiguration"
    response _ = nullaryResponse CreateLaunchConfigurationResponse
