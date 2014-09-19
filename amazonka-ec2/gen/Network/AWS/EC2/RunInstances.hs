{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.RunInstances
    (
    -- * Request
      RunInstances
    -- ** Request constructor
    , runInstances
    -- ** Request lenses
    , ri3ImageId
    , ri3MinCount
    , ri3MaxCount
    , ri3KeyName
    , ri3SecurityGroups
    , ri3SecurityGroupIds
    , ri3UserData
    , ri3InstanceType
    , ri3Placement
    , ri3KernelId
    , ri3RamdiskId
    , ri3BlockDeviceMappings
    , ri3Monitoring
    , ri3SubnetId
    , ri3DisableApiTermination
    , ri3InstanceInitiatedShutdownBehavior
    , ri3PrivateIpAddress
    , ri3ClientToken
    , ri3AdditionalInfo
    , ri3NetworkInterfaces
    , ri3IamInstanceProfile
    , ri3EbsOptimized

    -- * Response
    , RunInstancesResponse
    -- ** Response constructor
    , runInstancesResponse
    -- ** Response lenses
    , rirrReservationId
    , rirrOwnerId
    , rirrRequesterId
    , rirrGroups
    , rirrInstances
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data RunInstances = RunInstances
    { _ri3ImageId :: Text
    , _ri3MinCount :: !Integer
    , _ri3MaxCount :: !Integer
    , _ri3KeyName :: Maybe Text
    , _ri3SecurityGroups :: [Text]
    , _ri3SecurityGroupIds :: [Text]
    , _ri3UserData :: Maybe ByteString
    , _ri3InstanceType :: Maybe InstanceType
    , _ri3Placement :: Maybe Placement
    , _ri3KernelId :: Maybe Text
    , _ri3RamdiskId :: Maybe Text
    , _ri3BlockDeviceMappings :: [BlockDeviceMapping]
    , _ri3Monitoring :: Maybe RunInstancesMonitoringEnabled
    , _ri3SubnetId :: Maybe Text
    , _ri3DisableApiTermination :: Maybe Bool
    , _ri3InstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
    , _ri3PrivateIpAddress :: Maybe Text
    , _ri3ClientToken :: Maybe Text
    , _ri3AdditionalInfo :: Maybe Text
    , _ri3NetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
    , _ri3IamInstanceProfile :: Maybe IamInstanceProfileSpecification
    , _ri3EbsOptimized :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RunInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Text@
--
-- * @MinCount ::@ @Integer@
--
-- * @MaxCount ::@ @Integer@
--
-- * @KeyName ::@ @Maybe Text@
--
-- * @SecurityGroups ::@ @[Text]@
--
-- * @SecurityGroupIds ::@ @[Text]@
--
-- * @UserData ::@ @Maybe ByteString@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @Placement ::@ @Maybe Placement@
--
-- * @KernelId ::@ @Maybe Text@
--
-- * @RamdiskId ::@ @Maybe Text@
--
-- * @BlockDeviceMappings ::@ @[BlockDeviceMapping]@
--
-- * @Monitoring ::@ @Maybe RunInstancesMonitoringEnabled@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @DisableApiTermination ::@ @Maybe Bool@
--
-- * @InstanceInitiatedShutdownBehavior ::@ @Maybe ShutdownBehavior@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @ClientToken ::@ @Maybe Text@
--
-- * @AdditionalInfo ::@ @Maybe Text@
--
-- * @NetworkInterfaces ::@ @[InstanceNetworkInterfaceSpecification]@
--
-- * @IamInstanceProfile ::@ @Maybe IamInstanceProfileSpecification@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
runInstances :: Text -- ^ 'ri3ImageId'
             -> Integer -- ^ 'ri3MinCount'
             -> Integer -- ^ 'ri3MaxCount'
             -> RunInstances
runInstances p1 p2 p3 = RunInstances
    { _ri3ImageId = p1
    , _ri3MinCount = p2
    , _ri3MaxCount = p3
    , _ri3KeyName = Nothing
    , _ri3SecurityGroups = mempty
    , _ri3SecurityGroupIds = mempty
    , _ri3UserData = Nothing
    , _ri3InstanceType = Nothing
    , _ri3Placement = Nothing
    , _ri3KernelId = Nothing
    , _ri3RamdiskId = Nothing
    , _ri3BlockDeviceMappings = mempty
    , _ri3Monitoring = Nothing
    , _ri3SubnetId = Nothing
    , _ri3DisableApiTermination = Nothing
    , _ri3InstanceInitiatedShutdownBehavior = Nothing
    , _ri3PrivateIpAddress = Nothing
    , _ri3ClientToken = Nothing
    , _ri3AdditionalInfo = Nothing
    , _ri3NetworkInterfaces = mempty
    , _ri3IamInstanceProfile = Nothing
    , _ri3EbsOptimized = Nothing
    }

-- | The ID of the AMI, which you can get by calling DescribeImages.
ri3ImageId :: Lens' RunInstances Text
ri3ImageId = lens _ri3ImageId (\s a -> s { _ri3ImageId = a })

-- | The minimum number of instances to launch. If you specify a minimum that is
-- more instances than Amazon EC2 can launch in the target Availability Zone,
-- Amazon EC2 launches no instances. Constraints: Between 1 and the maximum
-- number you're allowed for the specified instance type. For more information
-- about the default limits, and how to request an increase, see How many
-- instances can I run in Amazon EC2 in the Amazon EC2 General FAQ.
ri3MinCount :: Lens' RunInstances Integer
ri3MinCount = lens _ri3MinCount (\s a -> s { _ri3MinCount = a })

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above MinCount.
-- Constraints: Between 1 and the maximum number you're allowed for the
-- specified instance type. For more information about the default limits, and
-- how to request an increase, see How many instances can I run in Amazon EC2
-- in the Amazon EC2 General FAQ.
ri3MaxCount :: Lens' RunInstances Integer
ri3MaxCount = lens _ri3MaxCount (\s a -> s { _ri3MaxCount = a })

-- | The name of the key pair. You can create a key pair using CreateKeyPair or
-- ImportKeyPair. If you launch an instance without specifying a key pair, you
-- can't connect to the instance.
ri3KeyName :: Lens' RunInstances (Maybe Text)
ri3KeyName = lens _ri3KeyName (\s a -> s { _ri3KeyName = a })

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. Default: Amazon
-- EC2 uses the default security group.
ri3SecurityGroups :: Lens' RunInstances [Text]
ri3SecurityGroups =
    lens _ri3SecurityGroups (\s a -> s { _ri3SecurityGroups = a })

-- | One or more security group IDs. You can create a security group using
-- CreateSecurityGroup. Default: Amazon EC2 uses the default security group.
ri3SecurityGroupIds :: Lens' RunInstances [Text]
ri3SecurityGroupIds =
    lens _ri3SecurityGroupIds (\s a -> s { _ri3SecurityGroupIds = a })

-- | The user data for the instances. You can specify the user data as a string,
-- or if the user data contents are in a file, you can use file://filename.
ri3UserData :: Lens' RunInstances (Maybe ByteString)
ri3UserData = lens _ri3UserData (\s a -> s { _ri3UserData = a })

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide. Default: m1.small.
ri3InstanceType :: Lens' RunInstances (Maybe InstanceType)
ri3InstanceType = lens _ri3InstanceType (\s a -> s { _ri3InstanceType = a })

-- | The placement for the instance.
ri3Placement :: Lens' RunInstances (Maybe Placement)
ri3Placement = lens _ri3Placement (\s a -> s { _ri3Placement = a })

-- | The ID of the kernel. We recommend that you use PV-GRUB instead of kernels
-- and RAM disks. For more information, see PV-GRUB: A New Amazon Kernel Image
-- in the Amazon Elastic Compute Cloud User Guide.
ri3KernelId :: Lens' RunInstances (Maybe Text)
ri3KernelId = lens _ri3KernelId (\s a -> s { _ri3KernelId = a })

-- | The ID of the RAM disk.
ri3RamdiskId :: Lens' RunInstances (Maybe Text)
ri3RamdiskId = lens _ri3RamdiskId (\s a -> s { _ri3RamdiskId = a })

-- | The block device mapping.
ri3BlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
ri3BlockDeviceMappings =
    lens _ri3BlockDeviceMappings (\s a -> s { _ri3BlockDeviceMappings = a })

-- | The monitoring for the instance.
ri3Monitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
ri3Monitoring = lens _ri3Monitoring (\s a -> s { _ri3Monitoring = a })

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
ri3SubnetId :: Lens' RunInstances (Maybe Text)
ri3SubnetId = lens _ri3SubnetId (\s a -> s { _ri3SubnetId = a })

-- | If you set this parameter to true, you can't terminate the instance using
-- the Amazon EC2 console, CLI, or API; otherwise, you can. If you set this
-- parameter to true and then later want to be able to terminate the instance,
-- you must first change the value of the disableApiTermination attribute to
-- false using ModifyInstanceAttribute. Alternatively, if you set
-- InstanceInitiatedShutdownBehavior to terminate, you can terminate the
-- instance by running the shutdown command from the instance. Default: false.
ri3DisableApiTermination :: Lens' RunInstances (Maybe Bool)
ri3DisableApiTermination =
    lens _ri3DisableApiTermination
         (\s a -> s { _ri3DisableApiTermination = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown). Default: stop.
ri3InstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe ShutdownBehavior)
ri3InstanceInitiatedShutdownBehavior =
    lens _ri3InstanceInitiatedShutdownBehavior
         (\s a -> s { _ri3InstanceInitiatedShutdownBehavior = a })

-- | [EC2-VPC] The primary IP address. You must specify a value from the IP
-- address range of the subnet. Only one private IP address can be designated
-- as primary. Therefore, you can't specify this parameter if
-- PrivateIpAddresses.n.Primary is set to true and
-- PrivateIpAddresses.n.PrivateIpAddress is set to an IP address. Default: We
-- select an IP address from the IP address range of the subnet.
ri3PrivateIpAddress :: Lens' RunInstances (Maybe Text)
ri3PrivateIpAddress =
    lens _ri3PrivateIpAddress (\s a -> s { _ri3PrivateIpAddress = a })

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request. For more information, see How to Ensure Idempotency in the
-- Amazon Elastic Compute Cloud User Guide. Constraints: Maximum 64 ASCII
-- characters.
ri3ClientToken :: Lens' RunInstances (Maybe Text)
ri3ClientToken = lens _ri3ClientToken (\s a -> s { _ri3ClientToken = a })

-- | Reserved.
ri3AdditionalInfo :: Lens' RunInstances (Maybe Text)
ri3AdditionalInfo =
    lens _ri3AdditionalInfo (\s a -> s { _ri3AdditionalInfo = a })

-- | One or more network interfaces.
ri3NetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
ri3NetworkInterfaces =
    lens _ri3NetworkInterfaces (\s a -> s { _ri3NetworkInterfaces = a })

-- | The IAM instance profile.
ri3IamInstanceProfile :: Lens' RunInstances (Maybe IamInstanceProfileSpecification)
ri3IamInstanceProfile =
    lens _ri3IamInstanceProfile (\s a -> s { _ri3IamInstanceProfile = a })

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal Amazon EBS I/O performance. This optimization
-- isn't available with all instance types. Additional usage charges apply
-- when using an EBS-optimized instance. Default: false.
ri3EbsOptimized :: Lens' RunInstances (Maybe Bool)
ri3EbsOptimized = lens _ri3EbsOptimized (\s a -> s { _ri3EbsOptimized = a })

instance ToQuery RunInstances where
    toQuery = genericQuery def

-- | One or more reservations.
data RunInstancesResponse = RunInstancesResponse
    { _rirrReservationId :: Maybe Text
    , _rirrOwnerId :: Maybe Text
    , _rirrRequesterId :: Maybe Text
    , _rirrGroups :: [GroupIdentifier]
    , _rirrInstances :: [Instance]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RunInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservationId ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @RequesterId ::@ @Maybe Text@
--
-- * @Groups ::@ @[GroupIdentifier]@
--
-- * @Instances ::@ @[Instance]@
--
runInstancesResponse :: RunInstancesResponse
runInstancesResponse = RunInstancesResponse
    { _rirrReservationId = Nothing
    , _rirrOwnerId = Nothing
    , _rirrRequesterId = Nothing
    , _rirrGroups = mempty
    , _rirrInstances = mempty
    }

-- | The ID of the reservation.
rirrReservationId :: Lens' RunInstancesResponse (Maybe Text)
rirrReservationId =
    lens _rirrReservationId (\s a -> s { _rirrReservationId = a })

-- | The ID of the AWS account that owns the reservation.
rirrOwnerId :: Lens' RunInstancesResponse (Maybe Text)
rirrOwnerId = lens _rirrOwnerId (\s a -> s { _rirrOwnerId = a })

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rirrRequesterId :: Lens' RunInstancesResponse (Maybe Text)
rirrRequesterId = lens _rirrRequesterId (\s a -> s { _rirrRequesterId = a })

-- | One or more security groups.
rirrGroups :: Lens' RunInstancesResponse [GroupIdentifier]
rirrGroups = lens _rirrGroups (\s a -> s { _rirrGroups = a })

-- | One or more instances.
rirrInstances :: Lens' RunInstancesResponse [Instance]
rirrInstances = lens _rirrInstances (\s a -> s { _rirrInstances = a })

instance FromXML RunInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RunInstances where
    type Sv RunInstances = EC2
    type Rs RunInstances = RunInstancesResponse

    request = post "RunInstances"
    response _ = xmlResponse
