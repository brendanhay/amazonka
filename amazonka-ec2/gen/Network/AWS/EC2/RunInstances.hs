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

-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Launches the specified number of instances using an AMI for which you have
-- permissions. When you launch an instance, it enters the pending state.
-- After the instance is ready for you, it enters the running state. To check
-- the state of your instance, call DescribeInstances. If you don't specify a
-- security group when launching an instance, Amazon EC2 uses the default
-- security group. For more information, see Security Groups in the Amazon
-- Elastic Compute Cloud User Guide. Linux instances have access to the public
-- key of the key pair at boot. You can use this key to provide secure access
-- to the instance. Amazon EC2 public images use this feature to provide
-- secure access without passwords. For more information, see Key Pairs in the
-- Amazon Elastic Compute Cloud User Guide. You can provide optional user data
-- when launching an instance. For more information, see Instance Metadata in
-- the Amazon Elastic Compute Cloud User Guide. If any of the AMIs have a
-- product code attached for which the user has not subscribed, RunInstances
-- fails. T2 instance types can only be launched into a VPC. If you do not
-- have a default VPC, or if you do not specify a subnet ID in the request,
-- RunInstances fails. For more information about troubleshooting, see What To
-- Do If An Instance Immediately Terminates, and Troubleshooting Connecting to
-- Your Instance in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.RunInstances
    (
    -- * Request
      RunInstances
    -- ** Request constructor
    , runInstances
    -- ** Request lenses
    , ri1AdditionalInfo
    , ri1BlockDeviceMappings
    , ri1ClientToken
    , ri1DisableApiTermination
    , ri1DryRun
    , ri1EbsOptimized
    , ri1IamInstanceProfile
    , ri1ImageId
    , ri1InstanceInitiatedShutdownBehavior
    , ri1InstanceType
    , ri1KernelId
    , ri1KeyName
    , ri1MaxCount
    , ri1MinCount
    , ri1Monitoring
    , ri1NetworkInterfaces
    , ri1Placement
    , ri1PrivateIpAddress
    , ri1RamdiskId
    , ri1SecurityGroupIds
    , ri1SecurityGroups
    , ri1SubnetId
    , ri1UserData

    -- * Response
    , Reservation
    -- ** Response constructor
    , reservation
    -- ** Response lenses
    , rGroups
    , rInstances
    , rOwnerId
    , rRequesterId
    , rReservationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data RunInstances = RunInstances
    { _ri1AdditionalInfo                    :: Maybe Text
    , _ri1BlockDeviceMappings               :: [BlockDeviceMapping]
    , _ri1ClientToken                       :: Maybe Text
    , _ri1DisableApiTermination             :: Maybe Bool
    , _ri1DryRun                            :: Maybe Bool
    , _ri1EbsOptimized                      :: Maybe Bool
    , _ri1IamInstanceProfile                :: Maybe IamInstanceProfileSpecification
    , _ri1ImageId                           :: Text
    , _ri1InstanceInitiatedShutdownBehavior :: Maybe Text
    , _ri1InstanceType                      :: Maybe Text
    , _ri1KernelId                          :: Maybe Text
    , _ri1KeyName                           :: Maybe Text
    , _ri1MaxCount                          :: Int
    , _ri1MinCount                          :: Int
    , _ri1Monitoring                        :: Maybe RunInstancesMonitoringEnabled
    , _ri1NetworkInterfaces                 :: [InstanceNetworkInterfaceSpecification]
    , _ri1Placement                         :: Maybe Placement
    , _ri1PrivateIpAddress                  :: Maybe Text
    , _ri1RamdiskId                         :: Maybe Text
    , _ri1SecurityGroupIds                  :: [Text]
    , _ri1SecurityGroups                    :: [Text]
    , _ri1SubnetId                          :: Maybe Text
    , _ri1UserData                          :: Maybe Text
    } (Eq, Show, Generic)

-- | 'RunInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ri1AdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'ri1BlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'ri1ClientToken' @::@ 'Maybe' 'Text'
--
-- * 'ri1DisableApiTermination' @::@ 'Maybe' 'Bool'
--
-- * 'ri1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ri1EbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'ri1IamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfileSpecification'
--
-- * 'ri1ImageId' @::@ 'Text'
--
-- * 'ri1InstanceInitiatedShutdownBehavior' @::@ 'Maybe' 'Text'
--
-- * 'ri1InstanceType' @::@ 'Maybe' 'Text'
--
-- * 'ri1KernelId' @::@ 'Maybe' 'Text'
--
-- * 'ri1KeyName' @::@ 'Maybe' 'Text'
--
-- * 'ri1MaxCount' @::@ 'Int'
--
-- * 'ri1MinCount' @::@ 'Int'
--
-- * 'ri1Monitoring' @::@ 'Maybe' 'RunInstancesMonitoringEnabled'
--
-- * 'ri1NetworkInterfaces' @::@ ['InstanceNetworkInterfaceSpecification']
--
-- * 'ri1Placement' @::@ 'Maybe' 'Placement'
--
-- * 'ri1PrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'ri1RamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'ri1SecurityGroupIds' @::@ ['Text']
--
-- * 'ri1SecurityGroups' @::@ ['Text']
--
-- * 'ri1SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'ri1UserData' @::@ 'Maybe' 'Text'
--
runInstances :: Text -- ^ 'ri1ImageId'
             -> Int -- ^ 'ri1MinCount'
             -> Int -- ^ 'ri1MaxCount'
             -> RunInstances
runInstances p1 p2 p3 = RunInstances
    { _ri1ImageId                           = p1
    , _ri1MinCount                          = p2
    , _ri1MaxCount                          = p3
    , _ri1DryRun                            = Nothing
    , _ri1KeyName                           = Nothing
    , _ri1SecurityGroups                    = mempty
    , _ri1SecurityGroupIds                  = mempty
    , _ri1UserData                          = Nothing
    , _ri1InstanceType                      = Nothing
    , _ri1Placement                         = Nothing
    , _ri1KernelId                          = Nothing
    , _ri1RamdiskId                         = Nothing
    , _ri1BlockDeviceMappings               = mempty
    , _ri1Monitoring                        = Nothing
    , _ri1SubnetId                          = Nothing
    , _ri1DisableApiTermination             = Nothing
    , _ri1InstanceInitiatedShutdownBehavior = Nothing
    , _ri1PrivateIpAddress                  = Nothing
    , _ri1ClientToken                       = Nothing
    , _ri1AdditionalInfo                    = Nothing
    , _ri1NetworkInterfaces                 = mempty
    , _ri1IamInstanceProfile                = Nothing
    , _ri1EbsOptimized                      = Nothing
    }

-- | Reserved.
ri1AdditionalInfo :: Lens' RunInstances (Maybe Text)
ri1AdditionalInfo =
    lens _ri1AdditionalInfo (\s a -> s { _ri1AdditionalInfo = a })

-- | The block device mapping.
ri1BlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
ri1BlockDeviceMappings =
    lens _ri1BlockDeviceMappings (\s a -> s { _ri1BlockDeviceMappings = a })

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see How to Ensure Idempotency in
-- the Amazon Elastic Compute Cloud User Guide. Constraints: Maximum 64
-- ASCII characters.
ri1ClientToken :: Lens' RunInstances (Maybe Text)
ri1ClientToken = lens _ri1ClientToken (\s a -> s { _ri1ClientToken = a })

-- | If you set this parameter to true, you can't terminate the instance using
-- the Amazon EC2 console, CLI, or API; otherwise, you can. If you set this
-- parameter to true and then later want to be able to terminate the
-- instance, you must first change the value of the disableApiTermination
-- attribute to false using ModifyInstanceAttribute. Alternatively, if you
-- set InstanceInitiatedShutdownBehavior to terminate, you can terminate the
-- instance by running the shutdown command from the instance. Default:
-- false.
ri1DisableApiTermination :: Lens' RunInstances (Maybe Bool)
ri1DisableApiTermination =
    lens _ri1DisableApiTermination
        (\s a -> s { _ri1DisableApiTermination = a })

ri1DryRun :: Lens' RunInstances (Maybe Bool)
ri1DryRun = lens _ri1DryRun (\s a -> s { _ri1DryRun = a })

-- | Indicates whether the instance is optimized for EBS I/O. This
-- optimization provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal Amazon EBS I/O performance. This
-- optimization isn't available with all instance types. Additional usage
-- charges apply when using an EBS-optimized instance. Default: false.
ri1EbsOptimized :: Lens' RunInstances (Maybe Bool)
ri1EbsOptimized = lens _ri1EbsOptimized (\s a -> s { _ri1EbsOptimized = a })

-- | The IAM instance profile.
ri1IamInstanceProfile :: Lens' RunInstances (Maybe IamInstanceProfileSpecification)
ri1IamInstanceProfile =
    lens _ri1IamInstanceProfile (\s a -> s { _ri1IamInstanceProfile = a })

-- | The ID of the AMI, which you can get by calling DescribeImages.
ri1ImageId :: Lens' RunInstances Text
ri1ImageId = lens _ri1ImageId (\s a -> s { _ri1ImageId = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown). Default: stop.
ri1InstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe Text)
ri1InstanceInitiatedShutdownBehavior =
    lens _ri1InstanceInitiatedShutdownBehavior
        (\s a -> s { _ri1InstanceInitiatedShutdownBehavior = a })

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide. Default: m1.small.
ri1InstanceType :: Lens' RunInstances (Maybe Text)
ri1InstanceType = lens _ri1InstanceType (\s a -> s { _ri1InstanceType = a })

-- | The ID of the kernel. We recommend that you use PV-GRUB instead of
-- kernels and RAM disks. For more information, see PV-GRUB in the Amazon
-- Elastic Compute Cloud User Guide.
ri1KernelId :: Lens' RunInstances (Maybe Text)
ri1KernelId = lens _ri1KernelId (\s a -> s { _ri1KernelId = a })

-- | The name of the key pair. You can create a key pair using CreateKeyPair
-- or ImportKeyPair. If you launch an instance without specifying a key
-- pair, you can't connect to the instance.
ri1KeyName :: Lens' RunInstances (Maybe Text)
ri1KeyName = lens _ri1KeyName (\s a -> s { _ri1KeyName = a })

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above MinCount.
-- Constraints: Between 1 and the maximum number you're allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see How many instances can I run in
-- Amazon EC2 in the Amazon EC2 General FAQ.
ri1MaxCount :: Lens' RunInstances Int
ri1MaxCount = lens _ri1MaxCount (\s a -> s { _ri1MaxCount = a })

-- | The minimum number of instances to launch. If you specify a minimum that
-- is more instances than Amazon EC2 can launch in the target Availability
-- Zone, Amazon EC2 launches no instances. Constraints: Between 1 and the
-- maximum number you're allowed for the specified instance type. For more
-- information about the default limits, and how to request an increase, see
-- How many instances can I run in Amazon EC2 in the Amazon EC2 General FAQ.
ri1MinCount :: Lens' RunInstances Int
ri1MinCount = lens _ri1MinCount (\s a -> s { _ri1MinCount = a })

-- | The monitoring for the instance.
ri1Monitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
ri1Monitoring = lens _ri1Monitoring (\s a -> s { _ri1Monitoring = a })

-- | One or more network interfaces.
ri1NetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
ri1NetworkInterfaces =
    lens _ri1NetworkInterfaces (\s a -> s { _ri1NetworkInterfaces = a })

-- | The placement for the instance.
ri1Placement :: Lens' RunInstances (Maybe Placement)
ri1Placement = lens _ri1Placement (\s a -> s { _ri1Placement = a })

-- | [EC2-VPC] The primary IP address. You must specify a value from the IP
-- address range of the subnet. Only one private IP address can be
-- designated as primary. Therefore, you can't specify this parameter if
-- PrivateIpAddresses.n.Primary is set to true and
-- PrivateIpAddresses.n.PrivateIpAddress is set to an IP address. Default:
-- We select an IP address from the IP address range of the subnet.
ri1PrivateIpAddress :: Lens' RunInstances (Maybe Text)
ri1PrivateIpAddress =
    lens _ri1PrivateIpAddress (\s a -> s { _ri1PrivateIpAddress = a })

-- | The ID of the RAM disk. We recommend that you use PV-GRUB instead of
-- kernels and RAM disks. For more information, see PV-GRUB in the Amazon
-- Elastic Compute Cloud User Guide.
ri1RamdiskId :: Lens' RunInstances (Maybe Text)
ri1RamdiskId = lens _ri1RamdiskId (\s a -> s { _ri1RamdiskId = a })

-- | One or more security group IDs. You can create a security group using
-- CreateSecurityGroup. Default: Amazon EC2 uses the default security group.
ri1SecurityGroupIds :: Lens' RunInstances [Text]
ri1SecurityGroupIds =
    lens _ri1SecurityGroupIds (\s a -> s { _ri1SecurityGroupIds = a })

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. Default: Amazon
-- EC2 uses the default security group.
ri1SecurityGroups :: Lens' RunInstances [Text]
ri1SecurityGroups =
    lens _ri1SecurityGroups (\s a -> s { _ri1SecurityGroups = a })

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
ri1SubnetId :: Lens' RunInstances (Maybe Text)
ri1SubnetId = lens _ri1SubnetId (\s a -> s { _ri1SubnetId = a })

-- | The Base64-encoded MIME user data for the instances.
ri1UserData :: Lens' RunInstances (Maybe Text)
ri1UserData = lens _ri1UserData (\s a -> s { _ri1UserData = a })
instance ToQuery RunInstances

instance ToPath RunInstances where
    toPath = const "/"

instance AWSRequest RunInstances where
    type Sv RunInstances = EC2
    type Rs RunInstances = Reservation

    request  = post "RunInstances"
    response = xmlResponse $ const decodeCursor
