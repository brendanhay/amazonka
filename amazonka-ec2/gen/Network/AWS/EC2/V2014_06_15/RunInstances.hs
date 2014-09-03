{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RunInstances
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
-- Your Instance in the Amazon Elastic Compute Cloud User Guide. This example
-- launches three instances using the AMI with the ID ami-60a54009.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-60a54009
-- &amp;MaxCount=3 &amp;MinCount=1 &amp;KeyName=my-key-pair
-- &amp;Placement.AvailabilityZone=us-east-1d &amp;AUTHPARAMS This example
-- launches an m1.small instance into a subnet. Because no network interface
-- is specified, the default network interface is used.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-31814f58
-- &amp;InstanceType=m1.small &amp;MaxCount=1 &amp;MinCount=1
-- &amp;KeyName=my-key-pair &amp;SubnetId=subnet-b2a249da &amp;AUTHPARAMS This
-- example launches an m1.large instance into a subnet. The network interface
-- specifies a primary private IP address of 10.0.2.106 and two secondary
-- private IP addresses (10.0.2.107 and 10.0.2.108).
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-beb0caec
-- &amp;InstanceType=m1.large &amp;MaxCount=1 &amp;MinCount=1
-- &amp;KeyName=my-key-pair &amp;NetworkInterface.0.DeviceIndex=0
-- &amp;NetworkInterface.0.PrivateIpAddresses.0.Primary=true
-- &amp;NetworkInterface.0.PrivateIpAddresses.0.PrivateIpAddress=10.0.2.106
-- &amp;NetworkInterface.0.PrivateIpAddresses.1.Primary=false
-- &amp;NetworkInterface.0.PrivateIpAddresses.1.PrivateIpAddress=10.0.2.107
-- &amp;NetworkInterface.0.PrivateIpAddresses.2.Primary=false
-- &amp;NetworkInterface.0.PrivateIpAddresses.2.PrivateIpAddress=10.0.2.108
-- &amp;NetworkInterface.0.SubnetId=subnet-a61dafcf &amp;AUTHPARAMS This
-- example launches a Dedicated Instance into the specified subnet.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-2a1fec43
-- &amp;MaxCount=1 &amp;MinCount=1 &amp;KeyName=my-key-pair
-- &amp;SubnetId=subnet-dea63cb7 &amp;Placement.Tenancy=dedicated
-- &amp;AUTHPARAMS This request launches an instance into a nondefault subnet,
-- and requests a public IP address for a new network interface with the
-- device index of 0. https://ec2.amazonaws.com/?Action=RunInstances
-- &amp;ImageId=ami-1a2b3c4d &amp;MaxCount=1 &amp;MinCount=1
-- &amp;NetworkInterface.0.DeviceIndex=0
-- &amp;NetworkInterface.0.AssociatePublicIpAddress=true
-- &amp;NetworkInterface.0.SubnetId=subnet-1a2b3c4d &amp;AUTHPARAMS This
-- request launches an m1.large instance with a block device mapping. There
-- are two instance store volumes mapped to /dev/sdc and /dev/sdd, and a 100
-- GB Amazon EBS volume mapped to /dev/sdf.
-- https://ec2.amazonaws.com/?Action=RunInstances &amp;ImageId=ami-1a2b3c4d
-- &amp;InstanceType=m1.large
-- &amp;BlockDeviceMapping.1.DeviceName=%2Fdev%2Fsdc
-- &amp;BlockDeviceMapping.1.VirtualName=ephemeral0
-- &amp;BlockDeviceMapping.2.DeviceName=%2Fdev%2Fsdd
-- &amp;BlockDeviceMapping.2.VirtualName=ephemeral1
-- &amp;BlockDeviceMapping.3.DeviceName=%2Fdev%2Fsdf
-- &amp;BlockDeviceMapping.3.Ebs.DeleteOnTermination=false
-- &amp;BlockDeviceMapping.3.Ebs.VolumeSize=100 &amp;EbsOptimized=false
-- &amp;MinCount=1 &amp;MaxCount=1 &amp;DisableApiTermination=false
-- &amp;Monitoring.Enabled=false &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.RunInstances
    (
    -- * Request
      RunInstances
    -- ** Request constructor
    , runInstances
    -- ** Request lenses
    , rixMinCount
    , rixMaxCount
    , rixImageId
    , rixBlockDeviceMappings
    , rixDisableApiTermination
    , rixEbsOptimized
    , rixIamInstanceProfile
    , rixNetworkInterfaces
    , rixInstanceType
    , rixPlacement
    , rixMonitoring
    , rixSecurityGroupIds
    , rixSecurityGroups
    , rixInstanceInitiatedShutdownBehavior
    , rixKeyName
    , rixUserData
    , rixKernelId
    , rixRamdiskId
    , rixSubnetId
    , rixPrivateIpAddress
    , rixClientToken
    , rixAdditionalInfo

    -- * Response
    , RunInstancesResponse
    -- ** Response lenses
    , ryGroups
    , ryInstances
    , ryReservationId
    , ryOwnerId
    , ryRequesterId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RunInstances' request.
runInstances :: Integer -- ^ 'rixMinCount'
             -> Integer -- ^ 'rixMaxCount'
             -> Text -- ^ 'rixImageId'
             -> RunInstances
runInstances p1 p2 p3 = RunInstances
    { _rixMinCount = p1
    , _rixMaxCount = p2
    , _rixImageId = p3
    , _rixBlockDeviceMappings = mempty
    , _rixDisableApiTermination = Nothing
    , _rixEbsOptimized = Nothing
    , _rixIamInstanceProfile = Nothing
    , _rixNetworkInterfaces = mempty
    , _rixInstanceType = Nothing
    , _rixPlacement = Nothing
    , _rixMonitoring = Nothing
    , _rixSecurityGroupIds = mempty
    , _rixSecurityGroups = mempty
    , _rixInstanceInitiatedShutdownBehavior = Nothing
    , _rixKeyName = Nothing
    , _rixUserData = Nothing
    , _rixKernelId = Nothing
    , _rixRamdiskId = Nothing
    , _rixSubnetId = Nothing
    , _rixPrivateIpAddress = Nothing
    , _rixClientToken = Nothing
    , _rixAdditionalInfo = Nothing
    }

data RunInstances = RunInstances
    { _rixMinCount :: Integer
      -- ^ The minimum number of instances to launch. If you specify a
      -- minimum that is more instances than Amazon EC2 can launch in the
      -- target Availability Zone, Amazon EC2 launches no instances.
      -- Constraints: Between 1 and the maximum number you're allowed for
      -- the specified instance type. For more information about the
      -- default limits, and how to request an increase, see How many
      -- instances can I run in Amazon EC2 in the Amazon EC2 General FAQ.
    , _rixMaxCount :: Integer
      -- ^ The maximum number of instances to launch. If you specify more
      -- instances than Amazon EC2 can launch in the target Availability
      -- Zone, Amazon EC2 launches the largest possible number of
      -- instances above MinCount. Constraints: Between 1 and the maximum
      -- number you're allowed for the specified instance type. For more
      -- information about the default limits, and how to request an
      -- increase, see How many instances can I run in Amazon EC2 in the
      -- Amazon EC2 General FAQ.
    , _rixImageId :: Text
      -- ^ The ID of the AMI, which you can get by calling DescribeImages.
    , _rixBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ The block device mapping.
    , _rixDisableApiTermination :: Maybe Bool
      -- ^ If you set this parameter to true, you can't terminate the
      -- instance using the Amazon EC2 console, CLI, or API; otherwise,
      -- you can. If you set this parameter to true and then later want to
      -- be able to terminate the instance, you must first change the
      -- value of the disableApiTermination attribute to false using
      -- ModifyInstanceAttribute. Alternatively, if you set
      -- InstanceInitiatedShutdownBehavior to terminate, you can terminate
      -- the instance by running the shutdown command from the instance.
      -- Default: false.
    , _rixEbsOptimized :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal Amazon EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS-optimized
      -- instance. Default: false.
    , _rixIamInstanceProfile :: Maybe IamInstanceProfileSpecification
      -- ^ The IAM instance profile.
    , _rixNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ One or more network interfaces.
    , _rixInstanceType :: Maybe InstanceType
      -- ^ The instance type. For more information, see Instance Types in
      -- the Amazon Elastic Compute Cloud User Guide. Default: m1.small.
    , _rixPlacement :: Maybe Placement
      -- ^ The placement for the instance.
    , _rixMonitoring :: Maybe RunInstancesMonitoringEnabled
      -- ^ The monitoring for the instance.
    , _rixSecurityGroupIds :: [Text]
      -- ^ One or more security group IDs. You can create a security group
      -- using CreateSecurityGroup. Default: Amazon EC2 uses the default
      -- security group.
    , _rixSecurityGroups :: [Text]
      -- ^ [EC2-Classic, default VPC] One or more security group names. For
      -- a nondefault VPC, you must use security group IDs instead.
      -- Default: Amazon EC2 uses the default security group.
    , _rixInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown). Default: stop.
    , _rixKeyName :: Maybe Text
      -- ^ The name of the key pair. You can create a key pair using
      -- CreateKeyPair or ImportKeyPair. If you launch an instance without
      -- specifying a key pair, you can't connect to the instance.
    , _rixUserData :: Maybe ByteString
      -- ^ The user data for the instances. You can specify the user data as
      -- a string, or if the user data contents are in a file, you can use
      -- file://filename.
    , _rixKernelId :: Maybe Text
      -- ^ The ID of the kernel. We recommend that you use PV-GRUB instead
      -- of kernels and RAM disks. For more information, see PV-GRUB: A
      -- New Amazon Kernel Image in the Amazon Elastic Compute Cloud User
      -- Guide.
    , _rixRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk.
    , _rixSubnetId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the subnet to launch the instance into.
    , _rixPrivateIpAddress :: Maybe Text
      -- ^ [EC2-VPC] The primary IP address. You must specify a value from
      -- the IP address range of the subnet. Only one private IP address
      -- can be designated as primary. Therefore, you can't specify this
      -- parameter if PrivateIpAddresses.n.Primary is set to true and
      -- PrivateIpAddresses.n.PrivateIpAddress is set to an IP address.
      -- Default: We select an IP address from the IP address range of the
      -- subnet.
    , _rixClientToken :: Maybe Text
      -- ^ Unique, case-sensitive identifier you provide to ensure the
      -- idempotency of the request. For more information, see How to
      -- Ensure Idempotency in the Amazon Elastic Compute Cloud User
      -- Guide. Constraints: Maximum 64 ASCII characters.
    , _rixAdditionalInfo :: Maybe Text
      -- ^ Reserved.
    } deriving (Show, Generic)

-- | The minimum number of instances to launch. If you specify a minimum that is
-- more instances than Amazon EC2 can launch in the target Availability Zone,
-- Amazon EC2 launches no instances. Constraints: Between 1 and the maximum
-- number you're allowed for the specified instance type. For more information
-- about the default limits, and how to request an increase, see How many
-- instances can I run in Amazon EC2 in the Amazon EC2 General FAQ.
rixMinCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RunInstances
    -> f RunInstances
rixMinCount f x =
    (\y -> x { _rixMinCount = y })
       <$> f (_rixMinCount x)
{-# INLINE rixMinCount #-}

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above MinCount.
-- Constraints: Between 1 and the maximum number you're allowed for the
-- specified instance type. For more information about the default limits, and
-- how to request an increase, see How many instances can I run in Amazon EC2
-- in the Amazon EC2 General FAQ.
rixMaxCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RunInstances
    -> f RunInstances
rixMaxCount f x =
    (\y -> x { _rixMaxCount = y })
       <$> f (_rixMaxCount x)
{-# INLINE rixMaxCount #-}

-- | The ID of the AMI, which you can get by calling DescribeImages.
rixImageId
    :: Functor f
    => (Text
    -> f (Text))
    -> RunInstances
    -> f RunInstances
rixImageId f x =
    (\y -> x { _rixImageId = y })
       <$> f (_rixImageId x)
{-# INLINE rixImageId #-}

-- | The block device mapping.
rixBlockDeviceMappings
    :: Functor f
    => ([BlockDeviceMapping]
    -> f ([BlockDeviceMapping]))
    -> RunInstances
    -> f RunInstances
rixBlockDeviceMappings f x =
    (\y -> x { _rixBlockDeviceMappings = y })
       <$> f (_rixBlockDeviceMappings x)
{-# INLINE rixBlockDeviceMappings #-}

-- | If you set this parameter to true, you can't terminate the instance using
-- the Amazon EC2 console, CLI, or API; otherwise, you can. If you set this
-- parameter to true and then later want to be able to terminate the instance,
-- you must first change the value of the disableApiTermination attribute to
-- false using ModifyInstanceAttribute. Alternatively, if you set
-- InstanceInitiatedShutdownBehavior to terminate, you can terminate the
-- instance by running the shutdown command from the instance. Default: false.
rixDisableApiTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> RunInstances
    -> f RunInstances
rixDisableApiTermination f x =
    (\y -> x { _rixDisableApiTermination = y })
       <$> f (_rixDisableApiTermination x)
{-# INLINE rixDisableApiTermination #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal Amazon EBS I/O performance. This optimization
-- isn't available with all instance types. Additional usage charges apply
-- when using an EBS-optimized instance. Default: false.
rixEbsOptimized
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> RunInstances
    -> f RunInstances
rixEbsOptimized f x =
    (\y -> x { _rixEbsOptimized = y })
       <$> f (_rixEbsOptimized x)
{-# INLINE rixEbsOptimized #-}

-- | The IAM instance profile.
rixIamInstanceProfile
    :: Functor f
    => (Maybe IamInstanceProfileSpecification
    -> f (Maybe IamInstanceProfileSpecification))
    -> RunInstances
    -> f RunInstances
rixIamInstanceProfile f x =
    (\y -> x { _rixIamInstanceProfile = y })
       <$> f (_rixIamInstanceProfile x)
{-# INLINE rixIamInstanceProfile #-}

-- | One or more network interfaces.
rixNetworkInterfaces
    :: Functor f
    => ([InstanceNetworkInterfaceSpecification]
    -> f ([InstanceNetworkInterfaceSpecification]))
    -> RunInstances
    -> f RunInstances
rixNetworkInterfaces f x =
    (\y -> x { _rixNetworkInterfaces = y })
       <$> f (_rixNetworkInterfaces x)
{-# INLINE rixNetworkInterfaces #-}

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide. Default: m1.small.
rixInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> RunInstances
    -> f RunInstances
rixInstanceType f x =
    (\y -> x { _rixInstanceType = y })
       <$> f (_rixInstanceType x)
{-# INLINE rixInstanceType #-}

-- | The placement for the instance.
rixPlacement
    :: Functor f
    => (Maybe Placement
    -> f (Maybe Placement))
    -> RunInstances
    -> f RunInstances
rixPlacement f x =
    (\y -> x { _rixPlacement = y })
       <$> f (_rixPlacement x)
{-# INLINE rixPlacement #-}

-- | The monitoring for the instance.
rixMonitoring
    :: Functor f
    => (Maybe RunInstancesMonitoringEnabled
    -> f (Maybe RunInstancesMonitoringEnabled))
    -> RunInstances
    -> f RunInstances
rixMonitoring f x =
    (\y -> x { _rixMonitoring = y })
       <$> f (_rixMonitoring x)
{-# INLINE rixMonitoring #-}

-- | One or more security group IDs. You can create a security group using
-- CreateSecurityGroup. Default: Amazon EC2 uses the default security group.
rixSecurityGroupIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> RunInstances
    -> f RunInstances
rixSecurityGroupIds f x =
    (\y -> x { _rixSecurityGroupIds = y })
       <$> f (_rixSecurityGroupIds x)
{-# INLINE rixSecurityGroupIds #-}

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. Default: Amazon
-- EC2 uses the default security group.
rixSecurityGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> RunInstances
    -> f RunInstances
rixSecurityGroups f x =
    (\y -> x { _rixSecurityGroups = y })
       <$> f (_rixSecurityGroups x)
{-# INLINE rixSecurityGroups #-}

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown). Default: stop.
rixInstanceInitiatedShutdownBehavior
    :: Functor f
    => (Maybe ShutdownBehavior
    -> f (Maybe ShutdownBehavior))
    -> RunInstances
    -> f RunInstances
rixInstanceInitiatedShutdownBehavior f x =
    (\y -> x { _rixInstanceInitiatedShutdownBehavior = y })
       <$> f (_rixInstanceInitiatedShutdownBehavior x)
{-# INLINE rixInstanceInitiatedShutdownBehavior #-}

-- | The name of the key pair. You can create a key pair using CreateKeyPair or
-- ImportKeyPair. If you launch an instance without specifying a key pair, you
-- can't connect to the instance.
rixKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixKeyName f x =
    (\y -> x { _rixKeyName = y })
       <$> f (_rixKeyName x)
{-# INLINE rixKeyName #-}

-- | The user data for the instances. You can specify the user data as a string,
-- or if the user data contents are in a file, you can use file://filename.
rixUserData
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> RunInstances
    -> f RunInstances
rixUserData f x =
    (\y -> x { _rixUserData = y })
       <$> f (_rixUserData x)
{-# INLINE rixUserData #-}

-- | The ID of the kernel. We recommend that you use PV-GRUB instead of kernels
-- and RAM disks. For more information, see PV-GRUB: A New Amazon Kernel Image
-- in the Amazon Elastic Compute Cloud User Guide.
rixKernelId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixKernelId f x =
    (\y -> x { _rixKernelId = y })
       <$> f (_rixKernelId x)
{-# INLINE rixKernelId #-}

-- | The ID of the RAM disk.
rixRamdiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixRamdiskId f x =
    (\y -> x { _rixRamdiskId = y })
       <$> f (_rixRamdiskId x)
{-# INLINE rixRamdiskId #-}

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
rixSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixSubnetId f x =
    (\y -> x { _rixSubnetId = y })
       <$> f (_rixSubnetId x)
{-# INLINE rixSubnetId #-}

-- | [EC2-VPC] The primary IP address. You must specify a value from the IP
-- address range of the subnet. Only one private IP address can be designated
-- as primary. Therefore, you can't specify this parameter if
-- PrivateIpAddresses.n.Primary is set to true and
-- PrivateIpAddresses.n.PrivateIpAddress is set to an IP address. Default: We
-- select an IP address from the IP address range of the subnet.
rixPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixPrivateIpAddress f x =
    (\y -> x { _rixPrivateIpAddress = y })
       <$> f (_rixPrivateIpAddress x)
{-# INLINE rixPrivateIpAddress #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request. For more information, see How to Ensure Idempotency in the
-- Amazon Elastic Compute Cloud User Guide. Constraints: Maximum 64 ASCII
-- characters.
rixClientToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixClientToken f x =
    (\y -> x { _rixClientToken = y })
       <$> f (_rixClientToken x)
{-# INLINE rixClientToken #-}

-- | Reserved.
rixAdditionalInfo
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstances
    -> f RunInstances
rixAdditionalInfo f x =
    (\y -> x { _rixAdditionalInfo = y })
       <$> f (_rixAdditionalInfo x)
{-# INLINE rixAdditionalInfo #-}

instance ToQuery RunInstances where
    toQuery = genericQuery def

data RunInstancesResponse = RunInstancesResponse
    { _ryGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _ryInstances :: [Instance]
      -- ^ One or more instances.
    , _ryReservationId :: Maybe Text
      -- ^ The ID of the reservation.
    , _ryOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that owns the reservation.
    , _ryRequesterId :: Maybe Text
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console or Auto Scaling).
    } deriving (Show, Generic)

-- | One or more security groups.
ryGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> RunInstancesResponse
    -> f RunInstancesResponse
ryGroups f x =
    (\y -> x { _ryGroups = y })
       <$> f (_ryGroups x)
{-# INLINE ryGroups #-}

-- | One or more instances.
ryInstances
    :: Functor f
    => ([Instance]
    -> f ([Instance]))
    -> RunInstancesResponse
    -> f RunInstancesResponse
ryInstances f x =
    (\y -> x { _ryInstances = y })
       <$> f (_ryInstances x)
{-# INLINE ryInstances #-}

-- | The ID of the reservation.
ryReservationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstancesResponse
    -> f RunInstancesResponse
ryReservationId f x =
    (\y -> x { _ryReservationId = y })
       <$> f (_ryReservationId x)
{-# INLINE ryReservationId #-}

-- | The ID of the AWS account that owns the reservation.
ryOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstancesResponse
    -> f RunInstancesResponse
ryOwnerId f x =
    (\y -> x { _ryOwnerId = y })
       <$> f (_ryOwnerId x)
{-# INLINE ryOwnerId #-}

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
ryRequesterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RunInstancesResponse
    -> f RunInstancesResponse
ryRequesterId f x =
    (\y -> x { _ryRequesterId = y })
       <$> f (_ryRequesterId x)
{-# INLINE ryRequesterId #-}

instance FromXML RunInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RunInstances where
    type Sv RunInstances = EC2
    type Rs RunInstances = RunInstancesResponse

    request = post "RunInstances"
    response _ = xmlResponse
