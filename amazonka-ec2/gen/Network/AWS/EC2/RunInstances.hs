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
-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified number of instances using an AMI for which you have permissions.
--
--
-- You can specify a number of options, or leave the default options. The following rules apply:
--
--     * [EC2-VPC] If you don't specify a subnet ID, we choose a default subnet from your default VPC for you. If you don't have a default VPC, you must specify a subnet ID in the request.
--
--     * [EC2-Classic] If don't specify an Availability Zone, we choose one for you.
--
--     * Some instance types must be launched into a VPC. If you do not have a default VPC, or if you do not specify a subnet ID, the request fails. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-vpc.html#vpc-only-instance-types Instance Types Available Only in a VPC> .
--
--     * [EC2-VPC] All instances have a network interface with a primary private IPv4 address. If you don't specify this address, we choose one from the IPv4 range of your subnet.
--
--     * Not all instance types support IPv6 addresses. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> .
--
--     * If you don't specify a security group ID, we use the default security group. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Security Groups> .
--
--     * If any of the AMIs have a product code attached for which the user has not subscribed, the request fails.
--
--
--
-- To ensure faster instance launches, break up large requests into smaller batches. For example, create 5 separate launch requests for 100 instances each instead of 1 launch request for 500 instances.
--
-- An instance is ready for you to use when it's in the @running@ state. You can check the state of your instance using 'DescribeInstances' . You can tag instances and EBS volumes during launch, after launch, or both. For more information, see 'CreateTags' and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Amazon EC2 Resources> .
--
-- Linux instances have access to the public key of the key pair at boot. You can use this key to provide secure access to the instance. Amazon EC2 public images use this feature to provide secure access without passwords. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For troubleshooting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html What To Do If An Instance Immediately Terminates> , and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html Troubleshooting Connecting to Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.RunInstances
    (
    -- * Creating a Request
      runInstances
    , RunInstances
    -- * Request Lenses
    , rAdditionalInfo
    , rSecurityGroupIds
    , rSecurityGroups
    , rClientToken
    , rDisableAPITermination
    , rKeyName
    , rNetworkInterfaces
    , rRAMDiskId
    , rSubnetId
    , rKernelId
    , rInstanceType
    , rEBSOptimized
    , rUserData
    , rMonitoring
    , rTagSpecifications
    , rIPv6AddressCount
    , rIAMInstanceProfile
    , rElasticGpuSpecification
    , rPrivateIPAddress
    , rInstanceInitiatedShutdownBehavior
    , rBlockDeviceMappings
    , rDryRun
    , rPlacement
    , rIPv6Addresses
    , rImageId
    , rMaxCount
    , rMinCount

    -- * Destructuring the Response
    , reservation
    , Reservation
    -- * Response Lenses
    , rGroups
    , rInstances
    , rRequesterId
    , rReservationId
    , rOwnerId
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RunInstances.
--
--
--
-- /See:/ 'runInstances' smart constructor.
data RunInstances = RunInstances'
    { _rAdditionalInfo                    :: !(Maybe Text)
    , _rSecurityGroupIds                  :: !(Maybe [Text])
    , _rSecurityGroups                    :: !(Maybe [Text])
    , _rClientToken                       :: !(Maybe Text)
    , _rDisableAPITermination             :: !(Maybe Bool)
    , _rKeyName                           :: !(Maybe Text)
    , _rNetworkInterfaces                 :: !(Maybe [InstanceNetworkInterfaceSpecification])
    , _rRAMDiskId                         :: !(Maybe Text)
    , _rSubnetId                          :: !(Maybe Text)
    , _rKernelId                          :: !(Maybe Text)
    , _rInstanceType                      :: !(Maybe InstanceType)
    , _rEBSOptimized                      :: !(Maybe Bool)
    , _rUserData                          :: !(Maybe Text)
    , _rMonitoring                        :: !(Maybe RunInstancesMonitoringEnabled)
    , _rTagSpecifications                 :: !(Maybe [TagSpecification])
    , _rIPv6AddressCount                  :: !(Maybe Int)
    , _rIAMInstanceProfile                :: !(Maybe IAMInstanceProfileSpecification)
    , _rElasticGpuSpecification           :: !(Maybe [ElasticGpuSpecification])
    , _rPrivateIPAddress                  :: !(Maybe Text)
    , _rInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
    , _rBlockDeviceMappings               :: !(Maybe [BlockDeviceMapping])
    , _rDryRun                            :: !(Maybe Bool)
    , _rPlacement                         :: !(Maybe Placement)
    , _rIPv6Addresses                     :: !(Maybe [InstanceIPv6Address])
    , _rImageId                           :: !Text
    , _rMaxCount                          :: !Int
    , _rMinCount                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAdditionalInfo' - Reserved.
--
-- * 'rSecurityGroupIds' - One or more security group IDs. You can create a security group using 'CreateSecurityGroup' . Default: Amazon EC2 uses the default security group.
--
-- * 'rSecurityGroups' - [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. Default: Amazon EC2 uses the default security group.
--
-- * 'rClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
--
-- * 'rDisableAPITermination' - If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute to @false@ after launch, use 'ModifyInstanceAttribute' . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance. Default: @false@
--
-- * 'rKeyName' - The name of the key pair. You can create a key pair using 'CreateKeyPair' or 'ImportKeyPair' . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- * 'rNetworkInterfaces' - One or more network interfaces.
--
-- * 'rRAMDiskId' - The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rSubnetId' - [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- * 'rKernelId' - The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rInstanceType' - The instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @m1.small@
--
-- * 'rEBSOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
--
-- * 'rUserData' - The user data to make available to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
--
-- * 'rMonitoring' - The monitoring for the instance.
--
-- * 'rTagSpecifications' - The tags to apply to the resources during launch. You can tag instances and volumes. The specified tags are applied to all instances or volumes that are created during launch.
--
-- * 'rIPv6AddressCount' - [EC2-VPC] A number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- * 'rIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rElasticGpuSpecification' - An Elastic GPU to associate with the instance.
--
-- * 'rPrivateIPAddress' - [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet. Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
--
-- * 'rInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
--
-- * 'rBlockDeviceMappings' - The block device mapping. /Important:/ Supplying both a snapshot ID and an encryption value as arguments for block-device mapping results in an error. This is because only blank volumes can be encrypted on start, and these are not created from a snapshot. If a snapshot is the basis for the volume, it contains data by definition and its encryption status cannot be changed using this action.
--
-- * 'rDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rPlacement' - The placement for the instance.
--
-- * 'rIPv6Addresses' - [EC2-VPC] Specify one or more IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- * 'rImageId' - The ID of the AMI, which you can get by calling 'DescribeImages' .
--
-- * 'rMaxCount' - The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ . Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
--
-- * 'rMinCount' - The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances. Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
runInstances
    :: Text -- ^ 'rImageId'
    -> Int -- ^ 'rMaxCount'
    -> Int -- ^ 'rMinCount'
    -> RunInstances
runInstances pImageId_ pMaxCount_ pMinCount_ =
    RunInstances'
    { _rAdditionalInfo = Nothing
    , _rSecurityGroupIds = Nothing
    , _rSecurityGroups = Nothing
    , _rClientToken = Nothing
    , _rDisableAPITermination = Nothing
    , _rKeyName = Nothing
    , _rNetworkInterfaces = Nothing
    , _rRAMDiskId = Nothing
    , _rSubnetId = Nothing
    , _rKernelId = Nothing
    , _rInstanceType = Nothing
    , _rEBSOptimized = Nothing
    , _rUserData = Nothing
    , _rMonitoring = Nothing
    , _rTagSpecifications = Nothing
    , _rIPv6AddressCount = Nothing
    , _rIAMInstanceProfile = Nothing
    , _rElasticGpuSpecification = Nothing
    , _rPrivateIPAddress = Nothing
    , _rInstanceInitiatedShutdownBehavior = Nothing
    , _rBlockDeviceMappings = Nothing
    , _rDryRun = Nothing
    , _rPlacement = Nothing
    , _rIPv6Addresses = Nothing
    , _rImageId = pImageId_
    , _rMaxCount = pMaxCount_
    , _rMinCount = pMinCount_
    }

-- | Reserved.
rAdditionalInfo :: Lens' RunInstances (Maybe Text)
rAdditionalInfo = lens _rAdditionalInfo (\ s a -> s{_rAdditionalInfo = a});

-- | One or more security group IDs. You can create a security group using 'CreateSecurityGroup' . Default: Amazon EC2 uses the default security group.
rSecurityGroupIds :: Lens' RunInstances [Text]
rSecurityGroupIds = lens _rSecurityGroupIds (\ s a -> s{_rSecurityGroupIds = a}) . _Default . _Coerce;

-- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. Default: Amazon EC2 uses the default security group.
rSecurityGroups :: Lens' RunInstances [Text]
rSecurityGroups = lens _rSecurityGroups (\ s a -> s{_rSecurityGroups = a}) . _Default . _Coerce;

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
rClientToken :: Lens' RunInstances (Maybe Text)
rClientToken = lens _rClientToken (\ s a -> s{_rClientToken = a});

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute to @false@ after launch, use 'ModifyInstanceAttribute' . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance. Default: @false@
rDisableAPITermination :: Lens' RunInstances (Maybe Bool)
rDisableAPITermination = lens _rDisableAPITermination (\ s a -> s{_rDisableAPITermination = a});

-- | The name of the key pair. You can create a key pair using 'CreateKeyPair' or 'ImportKeyPair' . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
rKeyName :: Lens' RunInstances (Maybe Text)
rKeyName = lens _rKeyName (\ s a -> s{_rKeyName = a});

-- | One or more network interfaces.
rNetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
rNetworkInterfaces = lens _rNetworkInterfaces (\ s a -> s{_rNetworkInterfaces = a}) . _Default . _Coerce;

-- | The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
rRAMDiskId :: Lens' RunInstances (Maybe Text)
rRAMDiskId = lens _rRAMDiskId (\ s a -> s{_rRAMDiskId = a});

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
rSubnetId :: Lens' RunInstances (Maybe Text)
rSubnetId = lens _rSubnetId (\ s a -> s{_rSubnetId = a});

-- | The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
rKernelId :: Lens' RunInstances (Maybe Text)
rKernelId = lens _rKernelId (\ s a -> s{_rKernelId = a});

-- | The instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @m1.small@
rInstanceType :: Lens' RunInstances (Maybe InstanceType)
rInstanceType = lens _rInstanceType (\ s a -> s{_rInstanceType = a});

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
rEBSOptimized :: Lens' RunInstances (Maybe Bool)
rEBSOptimized = lens _rEBSOptimized (\ s a -> s{_rEBSOptimized = a});

-- | The user data to make available to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
rUserData :: Lens' RunInstances (Maybe Text)
rUserData = lens _rUserData (\ s a -> s{_rUserData = a});

-- | The monitoring for the instance.
rMonitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
rMonitoring = lens _rMonitoring (\ s a -> s{_rMonitoring = a});

-- | The tags to apply to the resources during launch. You can tag instances and volumes. The specified tags are applied to all instances or volumes that are created during launch.
rTagSpecifications :: Lens' RunInstances [TagSpecification]
rTagSpecifications = lens _rTagSpecifications (\ s a -> s{_rTagSpecifications = a}) . _Default . _Coerce;

-- | [EC2-VPC] A number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
rIPv6AddressCount :: Lens' RunInstances (Maybe Int)
rIPv6AddressCount = lens _rIPv6AddressCount (\ s a -> s{_rIPv6AddressCount = a});

-- | The IAM instance profile.
rIAMInstanceProfile :: Lens' RunInstances (Maybe IAMInstanceProfileSpecification)
rIAMInstanceProfile = lens _rIAMInstanceProfile (\ s a -> s{_rIAMInstanceProfile = a});

-- | An Elastic GPU to associate with the instance.
rElasticGpuSpecification :: Lens' RunInstances [ElasticGpuSpecification]
rElasticGpuSpecification = lens _rElasticGpuSpecification (\ s a -> s{_rElasticGpuSpecification = a}) . _Default . _Coerce;

-- | [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet. Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
rPrivateIPAddress :: Lens' RunInstances (Maybe Text)
rPrivateIPAddress = lens _rPrivateIPAddress (\ s a -> s{_rPrivateIPAddress = a});

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
rInstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = lens _rInstanceInitiatedShutdownBehavior (\ s a -> s{_rInstanceInitiatedShutdownBehavior = a});

-- | The block device mapping. /Important:/ Supplying both a snapshot ID and an encryption value as arguments for block-device mapping results in an error. This is because only blank volumes can be encrypted on start, and these are not created from a snapshot. If a snapshot is the basis for the volume, it contains data by definition and its encryption status cannot be changed using this action.
rBlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
rBlockDeviceMappings = lens _rBlockDeviceMappings (\ s a -> s{_rBlockDeviceMappings = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rDryRun :: Lens' RunInstances (Maybe Bool)
rDryRun = lens _rDryRun (\ s a -> s{_rDryRun = a});

-- | The placement for the instance.
rPlacement :: Lens' RunInstances (Maybe Placement)
rPlacement = lens _rPlacement (\ s a -> s{_rPlacement = a});

-- | [EC2-VPC] Specify one or more IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
rIPv6Addresses :: Lens' RunInstances [InstanceIPv6Address]
rIPv6Addresses = lens _rIPv6Addresses (\ s a -> s{_rIPv6Addresses = a}) . _Default . _Coerce;

-- | The ID of the AMI, which you can get by calling 'DescribeImages' .
rImageId :: Lens' RunInstances Text
rImageId = lens _rImageId (\ s a -> s{_rImageId = a});

-- | The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ . Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
rMaxCount :: Lens' RunInstances Int
rMaxCount = lens _rMaxCount (\ s a -> s{_rMaxCount = a});

-- | The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances. Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
rMinCount :: Lens' RunInstances Int
rMinCount = lens _rMinCount (\ s a -> s{_rMinCount = a});

instance AWSRequest RunInstances where
        type Rs RunInstances = Reservation
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable RunInstances

instance NFData RunInstances

instance ToHeaders RunInstances where
        toHeaders = const mempty

instance ToPath RunInstances where
        toPath = const "/"

instance ToQuery RunInstances where
        toQuery RunInstances'{..}
          = mconcat
              ["Action" =: ("RunInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AdditionalInfo" =: _rAdditionalInfo,
               toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _rSecurityGroupIds),
               toQuery
                 (toQueryList "SecurityGroup" <$> _rSecurityGroups),
               "ClientToken" =: _rClientToken,
               "DisableApiTermination" =: _rDisableAPITermination,
               "KeyName" =: _rKeyName,
               toQuery
                 (toQueryList "NetworkInterface" <$>
                    _rNetworkInterfaces),
               "RamdiskId" =: _rRAMDiskId, "SubnetId" =: _rSubnetId,
               "KernelId" =: _rKernelId,
               "InstanceType" =: _rInstanceType,
               "EbsOptimized" =: _rEBSOptimized,
               "UserData" =: _rUserData,
               "Monitoring" =: _rMonitoring,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _rTagSpecifications),
               "Ipv6AddressCount" =: _rIPv6AddressCount,
               "IamInstanceProfile" =: _rIAMInstanceProfile,
               toQuery
                 (toQueryList "ElasticGpuSpecification" <$>
                    _rElasticGpuSpecification),
               "PrivateIpAddress" =: _rPrivateIPAddress,
               "InstanceInitiatedShutdownBehavior" =:
                 _rInstanceInitiatedShutdownBehavior,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _rBlockDeviceMappings),
               "DryRun" =: _rDryRun, "Placement" =: _rPlacement,
               toQuery
                 (toQueryList "Ipv6Address" <$> _rIPv6Addresses),
               "ImageId" =: _rImageId, "MaxCount" =: _rMaxCount,
               "MinCount" =: _rMinCount]
