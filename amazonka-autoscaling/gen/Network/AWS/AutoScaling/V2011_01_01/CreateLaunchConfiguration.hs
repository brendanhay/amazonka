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
    , createLaunchConfiguration
    -- ** Request lenses
    , clctLaunchConfigurationName
    , clctAssociatePublicIpAddress
    , clctBlockDeviceMappings
    , clctEbsOptimized
    , clctInstanceMonitoring
    , clctSecurityGroups
    , clctSpotPrice
    , clctInstanceId
    , clctIamInstanceProfile
    , clctImageId
    , clctKeyName
    , clctInstanceType
    , clctKernelId
    , clctRamdiskId
    , clctPlacementTenancy
    , clctUserData

    -- * Response
    , CreateLaunchConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateLaunchConfiguration' request.
createLaunchConfiguration :: Text -- ^ 'clctLaunchConfigurationName'
                          -> CreateLaunchConfiguration
createLaunchConfiguration p1 = CreateLaunchConfiguration
    { _clctLaunchConfigurationName = p1
    , _clctAssociatePublicIpAddress = Nothing
    , _clctBlockDeviceMappings = mempty
    , _clctEbsOptimized = Nothing
    , _clctInstanceMonitoring = Nothing
    , _clctSecurityGroups = mempty
    , _clctSpotPrice = Nothing
    , _clctInstanceId = Nothing
    , _clctIamInstanceProfile = Nothing
    , _clctImageId = Nothing
    , _clctKeyName = Nothing
    , _clctInstanceType = Nothing
    , _clctKernelId = Nothing
    , _clctRamdiskId = Nothing
    , _clctPlacementTenancy = Nothing
    , _clctUserData = Nothing
    }

data CreateLaunchConfiguration = CreateLaunchConfiguration
    { _clctLaunchConfigurationName :: Text
      -- ^ The name of the launch configuration to create.
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
    , _clctBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ A list of mappings that specify how block devices are exposed to
      -- the instance. Each mapping is made up of a VirtualName, a
      -- DeviceName, and an ebs data structure that contains information
      -- about the associated Elastic Block Storage volume. For more
      -- information about Amazon EC2 BlockDeviceMappings, go to Block
      -- Device Mapping in the Amazon EC2 product documentation.
    , _clctEbsOptimized :: Maybe Bool
      -- ^ Whether the instance is optimized for EBS I/O. The optimization
      -- provides dedicated throughput to Amazon EBS and an optimized
      -- configuration stack to provide optimal EBS I/O performance. This
      -- optimization is not available with all instance types. Additional
      -- usage charges apply when using an EBS Optimized instance. By
      -- default the instance is not optimized for EBS I/O. For
      -- information about EBS-optimized instances, go to EBS-Optimized
      -- Instances in the Amazon Elastic Compute Cloud User Guide.
    , _clctInstanceMonitoring :: Maybe InstanceMonitoring
      -- ^ Enables detailed monitoring if it is disabled. Detailed
      -- monitoring is enabled by default. When detailed monitoring is
      -- enabled, Amazon Cloudwatch will generate metrics every minute and
      -- your account will be charged a fee. When you disable detailed
      -- monitoring, by specifying False, Cloudwatch will generate metrics
      -- every 5 minutes. For more information, see Monitor Your Auto
      -- Scaling Instances. For information about Amazon CloudWatch, see
      -- the Amazon CloudWatch Developer Guide.
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
    , _clctSpotPrice :: Maybe Text
      -- ^ The maximum hourly price to be paid for any Spot Instance
      -- launched to fulfill the request. Spot Instances are launched when
      -- the price you specify exceeds the current Spot market price. For
      -- more information on launching Spot Instances, see Using Auto
      -- Scaling to Launch Spot Instances in the Auto Scaling Developer
      -- Guide.
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
    , _clctImageId :: Maybe Text
      -- ^ Unique ID of the Amazon Machine Image (AMI) you want to use to
      -- launch your EC2 instances. For information about finding Amazon
      -- EC2 AMIs, see Finding a Suitable AMI in the Amazon Elastic
      -- Compute Cloud User Guide.
    , _clctKeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair. For more information, see
      -- Getting a Key Pair in the Amazon Elastic Compute Cloud User
      -- Guide.
    , _clctInstanceType :: Maybe Text
      -- ^ The instance type of the Amazon EC2 instance. For information
      -- about available Amazon EC2 instance types, see Available Instance
      -- Types in the Amazon Elastic Cloud Compute User Guide.
    , _clctKernelId :: Maybe Text
      -- ^ The ID of the kernel associated with the Amazon EC2 AMI.
    , _clctRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk associated with the Amazon EC2 AMI.
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
    , _clctUserData :: Maybe ByteString
      -- ^ The user data to make available to the launched Amazon EC2
      -- instances. For more information about Amazon EC2 user data, see
      -- User Data Retrieval in the Amazon Elastic Compute Cloud User
      -- Guide. At this time, Auto Scaling launch configurations don't
      -- support compressed (e.g. zipped) user data files.
    } deriving (Show, Generic)

-- | The name of the launch configuration to create.
clctLaunchConfigurationName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctLaunchConfigurationName f x =
    (\y -> x { _clctLaunchConfigurationName = y })
       <$> f (_clctLaunchConfigurationName x)
{-# INLINE clctLaunchConfigurationName #-}

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
clctAssociatePublicIpAddress
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctAssociatePublicIpAddress f x =
    (\y -> x { _clctAssociatePublicIpAddress = y })
       <$> f (_clctAssociatePublicIpAddress x)
{-# INLINE clctAssociatePublicIpAddress #-}

-- | A list of mappings that specify how block devices are exposed to the
-- instance. Each mapping is made up of a VirtualName, a DeviceName, and an
-- ebs data structure that contains information about the associated Elastic
-- Block Storage volume. For more information about Amazon EC2
-- BlockDeviceMappings, go to Block Device Mapping in the Amazon EC2 product
-- documentation.
clctBlockDeviceMappings
    :: Functor f
    => ([BlockDeviceMapping]
    -> f ([BlockDeviceMapping]))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctBlockDeviceMappings f x =
    (\y -> x { _clctBlockDeviceMappings = y })
       <$> f (_clctBlockDeviceMappings x)
{-# INLINE clctBlockDeviceMappings #-}

-- | Whether the instance is optimized for EBS I/O. The optimization provides
-- dedicated throughput to Amazon EBS and an optimized configuration stack to
-- provide optimal EBS I/O performance. This optimization is not available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance. By default the instance is not optimized for EBS I/O.
-- For information about EBS-optimized instances, go to EBS-Optimized
-- Instances in the Amazon Elastic Compute Cloud User Guide.
clctEbsOptimized
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctEbsOptimized f x =
    (\y -> x { _clctEbsOptimized = y })
       <$> f (_clctEbsOptimized x)
{-# INLINE clctEbsOptimized #-}

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default. When detailed monitoring is enabled, Amazon Cloudwatch
-- will generate metrics every minute and your account will be charged a fee.
-- When you disable detailed monitoring, by specifying False, Cloudwatch will
-- generate metrics every 5 minutes. For more information, see Monitor Your
-- Auto Scaling Instances. For information about Amazon CloudWatch, see the
-- Amazon CloudWatch Developer Guide.
clctInstanceMonitoring
    :: Functor f
    => (Maybe InstanceMonitoring
    -> f (Maybe InstanceMonitoring))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctInstanceMonitoring f x =
    (\y -> x { _clctInstanceMonitoring = y })
       <$> f (_clctInstanceMonitoring x)
{-# INLINE clctInstanceMonitoring #-}

-- | The security groups with which to associate Amazon EC2 or Amazon VPC
-- instances. If your instances are launched in EC2, you can either specify
-- Amazon EC2 security group names or the security group IDs. For more
-- information about Amazon EC2 security groups, see Using Security Groups in
-- the Amazon Elastic Compute Cloud User Guide. If your instances are launched
-- within VPC, specify Amazon VPC security group IDs. For more information
-- about Amazon VPC security groups, see Security Groups in the Amazon Virtual
-- Private Cloud User Guide.
clctSecurityGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctSecurityGroups f x =
    (\y -> x { _clctSecurityGroups = y })
       <$> f (_clctSecurityGroups x)
{-# INLINE clctSecurityGroups #-}

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you specify
-- exceeds the current Spot market price. For more information on launching
-- Spot Instances, see Using Auto Scaling to Launch Spot Instances in the Auto
-- Scaling Developer Guide.
clctSpotPrice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctSpotPrice f x =
    (\y -> x { _clctSpotPrice = y })
       <$> f (_clctSpotPrice x)
{-# INLINE clctSpotPrice #-}

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
clctInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctInstanceId f x =
    (\y -> x { _clctInstanceId = y })
       <$> f (_clctInstanceId x)
{-# INLINE clctInstanceId #-}

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. Amazon EC2 instances
-- launched with an IAM role will automatically have AWS security credentials
-- available. You can use IAM roles with Auto Scaling to automatically enable
-- applications running on your Amazon EC2 instances to securely access other
-- AWS resources. For information on launching EC2 instances with an IAM role,
-- go to Launching Auto Scaling Instances With an IAM Role in the Auto Scaling
-- Developer Guide.
clctIamInstanceProfile
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctIamInstanceProfile f x =
    (\y -> x { _clctIamInstanceProfile = y })
       <$> f (_clctIamInstanceProfile x)
{-# INLINE clctIamInstanceProfile #-}

-- | Unique ID of the Amazon Machine Image (AMI) you want to use to launch your
-- EC2 instances. For information about finding Amazon EC2 AMIs, see Finding a
-- Suitable AMI in the Amazon Elastic Compute Cloud User Guide.
clctImageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctImageId f x =
    (\y -> x { _clctImageId = y })
       <$> f (_clctImageId x)
{-# INLINE clctImageId #-}

-- | The name of the Amazon EC2 key pair. For more information, see Getting a
-- Key Pair in the Amazon Elastic Compute Cloud User Guide.
clctKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctKeyName f x =
    (\y -> x { _clctKeyName = y })
       <$> f (_clctKeyName x)
{-# INLINE clctKeyName #-}

-- | The instance type of the Amazon EC2 instance. For information about
-- available Amazon EC2 instance types, see Available Instance Types in the
-- Amazon Elastic Cloud Compute User Guide.
clctInstanceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctInstanceType f x =
    (\y -> x { _clctInstanceType = y })
       <$> f (_clctInstanceType x)
{-# INLINE clctInstanceType #-}

-- | The ID of the kernel associated with the Amazon EC2 AMI.
clctKernelId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctKernelId f x =
    (\y -> x { _clctKernelId = y })
       <$> f (_clctKernelId x)
{-# INLINE clctKernelId #-}

-- | The ID of the RAM disk associated with the Amazon EC2 AMI.
clctRamdiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctRamdiskId f x =
    (\y -> x { _clctRamdiskId = y })
       <$> f (_clctRamdiskId x)
{-# INLINE clctRamdiskId #-}

-- | The tenancy of the instance. An instance with a tenancy of dedicated runs
-- on single-tenant hardware and can only be launched in a VPC. You must set
-- the value of this parameter to dedicated if want to launch Dedicated
-- Instances in a shared tenancy VPC (VPC with instance placement tenancy
-- attribute set to default). If you specify a value for this parameter, be
-- sure to specify at least one VPC subnet using the VPCZoneIdentifier
-- parameter when you create your Auto Scaling group. For more information,
-- see Auto Scaling in Amazon Virtual Private Cloud in the Auto Scaling
-- Developer Guide. Valid values: default | dedicated.
clctPlacementTenancy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctPlacementTenancy f x =
    (\y -> x { _clctPlacementTenancy = y })
       <$> f (_clctPlacementTenancy x)
{-# INLINE clctPlacementTenancy #-}

-- | The user data to make available to the launched Amazon EC2 instances. For
-- more information about Amazon EC2 user data, see User Data Retrieval in the
-- Amazon Elastic Compute Cloud User Guide. At this time, Auto Scaling launch
-- configurations don't support compressed (e.g. zipped) user data files.
clctUserData
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> CreateLaunchConfiguration
    -> f CreateLaunchConfiguration
clctUserData f x =
    (\y -> x { _clctUserData = y })
       <$> f (_clctUserData x)
{-# INLINE clctUserData #-}

instance ToQuery CreateLaunchConfiguration where
    toQuery = genericQuery def

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateLaunchConfiguration where
    type Sv CreateLaunchConfiguration = AutoScaling
    type Rs CreateLaunchConfiguration = CreateLaunchConfigurationResponse

    request = post "CreateLaunchConfiguration"
    response _ = nullaryResponse CreateLaunchConfigurationResponse
