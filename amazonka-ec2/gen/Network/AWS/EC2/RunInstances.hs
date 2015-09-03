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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified number of instances using an AMI for which you
-- have permissions.
--
-- When you launch an instance, it enters the 'pending' state. After the
-- instance is ready for you, it enters the 'running' state. To check the
-- state of your instance, call DescribeInstances.
--
-- If you don\'t specify a security group when launching an instance,
-- Amazon EC2 uses the default security group. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Security Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- [EC2-VPC only accounts] If you don\'t specify a subnet in the request,
-- we choose a default subnet from your default VPC for you.
--
-- [EC2-Classic accounts] If you\'re launching into EC2-Classic and you
-- don\'t specify an Availability Zone, we choose one for you.
--
-- Linux instances have access to the public key of the key pair at boot.
-- You can use this key to provide secure access to the instance. Amazon
-- EC2 public images use this feature to provide secure access without
-- passwords. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can provide optional user data when launching an instance. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If any of the AMIs have a product code attached for which the user has
-- not subscribed, 'RunInstances' fails.
--
-- T2 instance types can only be launched into a VPC. If you do not have a
-- default VPC, or if you do not specify a subnet ID in the request,
-- 'RunInstances' fails.
--
-- For more information about troubleshooting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html What To Do If An Instance Immediately Terminates>,
-- and
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html Troubleshooting Connecting to Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html AWS API Reference> for RunInstances.
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
    , rIAMInstanceProfile
    , rPrivateIPAddress
    , rInstanceInitiatedShutdownBehavior
    , rBlockDeviceMappings
    , rDryRun
    , rPlacement
    , rImageId
    , rMinCount
    , rMaxCount

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
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runInstances' smart constructor.
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
    , _rIAMInstanceProfile                :: !(Maybe IAMInstanceProfileSpecification)
    , _rPrivateIPAddress                  :: !(Maybe Text)
    , _rInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
    , _rBlockDeviceMappings               :: !(Maybe [BlockDeviceMapping])
    , _rDryRun                            :: !(Maybe Bool)
    , _rPlacement                         :: !(Maybe Placement)
    , _rImageId                           :: !Text
    , _rMinCount                          :: !Int
    , _rMaxCount                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAdditionalInfo'
--
-- * 'rSecurityGroupIds'
--
-- * 'rSecurityGroups'
--
-- * 'rClientToken'
--
-- * 'rDisableAPITermination'
--
-- * 'rKeyName'
--
-- * 'rNetworkInterfaces'
--
-- * 'rRAMDiskId'
--
-- * 'rSubnetId'
--
-- * 'rKernelId'
--
-- * 'rInstanceType'
--
-- * 'rEBSOptimized'
--
-- * 'rUserData'
--
-- * 'rMonitoring'
--
-- * 'rIAMInstanceProfile'
--
-- * 'rPrivateIPAddress'
--
-- * 'rInstanceInitiatedShutdownBehavior'
--
-- * 'rBlockDeviceMappings'
--
-- * 'rDryRun'
--
-- * 'rPlacement'
--
-- * 'rImageId'
--
-- * 'rMinCount'
--
-- * 'rMaxCount'
runInstances
    :: Text -- ^ 'rImageId'
    -> Int -- ^ 'rMinCount'
    -> Int -- ^ 'rMaxCount'
    -> RunInstances
runInstances pImageId_ pMinCount_ pMaxCount_ =
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
    , _rIAMInstanceProfile = Nothing
    , _rPrivateIPAddress = Nothing
    , _rInstanceInitiatedShutdownBehavior = Nothing
    , _rBlockDeviceMappings = Nothing
    , _rDryRun = Nothing
    , _rPlacement = Nothing
    , _rImageId = pImageId_
    , _rMinCount = pMinCount_
    , _rMaxCount = pMaxCount_
    }

-- | Reserved.
rAdditionalInfo :: Lens' RunInstances (Maybe Text)
rAdditionalInfo = lens _rAdditionalInfo (\ s a -> s{_rAdditionalInfo = a});

-- | One or more security group IDs. You can create a security group using
-- CreateSecurityGroup.
--
-- Default: Amazon EC2 uses the default security group.
rSecurityGroupIds :: Lens' RunInstances [Text]
rSecurityGroupIds = lens _rSecurityGroupIds (\ s a -> s{_rSecurityGroupIds = a}) . _Default . _Coerce;

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead.
--
-- Default: Amazon EC2 uses the default security group.
rSecurityGroups :: Lens' RunInstances [Text]
rSecurityGroups = lens _rSecurityGroups (\ s a -> s{_rSecurityGroups = a}) . _Default . _Coerce;

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
rClientToken :: Lens' RunInstances (Maybe Text)
rClientToken = lens _rClientToken (\ s a -> s{_rClientToken = a});

-- | If you set this parameter to 'true', you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. If you
-- set this parameter to 'true' and then later want to be able to terminate
-- the instance, you must first change the value of the
-- 'disableApiTermination' attribute to 'false' using
-- ModifyInstanceAttribute. Alternatively, if you set
-- 'InstanceInitiatedShutdownBehavior' to 'terminate', you can terminate
-- the instance by running the shutdown command from the instance.
--
-- Default: 'false'
rDisableAPITermination :: Lens' RunInstances (Maybe Bool)
rDisableAPITermination = lens _rDisableAPITermination (\ s a -> s{_rDisableAPITermination = a});

-- | The name of the key pair. You can create a key pair using CreateKeyPair
-- or ImportKeyPair.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
rKeyName :: Lens' RunInstances (Maybe Text)
rKeyName = lens _rKeyName (\ s a -> s{_rKeyName = a});

-- | One or more network interfaces.
rNetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
rNetworkInterfaces = lens _rNetworkInterfaces (\ s a -> s{_rNetworkInterfaces = a}) . _Default . _Coerce;

-- | The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rRAMDiskId :: Lens' RunInstances (Maybe Text)
rRAMDiskId = lens _rRAMDiskId (\ s a -> s{_rRAMDiskId = a});

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
rSubnetId :: Lens' RunInstances (Maybe Text)
rSubnetId = lens _rSubnetId (\ s a -> s{_rSubnetId = a});

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rKernelId :: Lens' RunInstances (Maybe Text)
rKernelId = lens _rKernelId (\ s a -> s{_rKernelId = a});

-- | The instance type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: 'm1.small'
rInstanceType :: Lens' RunInstances (Maybe InstanceType)
rInstanceType = lens _rInstanceType (\ s a -> s{_rInstanceType = a});

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS-optimized instance.
--
-- Default: 'false'
rEBSOptimized :: Lens' RunInstances (Maybe Bool)
rEBSOptimized = lens _rEBSOptimized (\ s a -> s{_rEBSOptimized = a});

-- | The Base64-encoded MIME user data for the instances.
rUserData :: Lens' RunInstances (Maybe Text)
rUserData = lens _rUserData (\ s a -> s{_rUserData = a});

-- | The monitoring for the instance.
rMonitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
rMonitoring = lens _rMonitoring (\ s a -> s{_rMonitoring = a});

-- | The IAM instance profile.
rIAMInstanceProfile :: Lens' RunInstances (Maybe IAMInstanceProfileSpecification)
rIAMInstanceProfile = lens _rIAMInstanceProfile (\ s a -> s{_rIAMInstanceProfile = a});

-- | [EC2-VPC] The primary IP address. You must specify a value from the IP
-- address range of the subnet.
--
-- Only one private IP address can be designated as primary. Therefore, you
-- can\'t specify this parameter if 'PrivateIpAddresses.n.Primary' is set
-- to 'true' and 'PrivateIpAddresses.n.PrivateIpAddress' is set to an IP
-- address.
--
-- Default: We select an IP address from the IP address range of the
-- subnet.
rPrivateIPAddress :: Lens' RunInstances (Maybe Text)
rPrivateIPAddress = lens _rPrivateIPAddress (\ s a -> s{_rPrivateIPAddress = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: 'stop'
rInstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = lens _rInstanceInitiatedShutdownBehavior (\ s a -> s{_rInstanceInitiatedShutdownBehavior = a});

-- | The block device mapping.
rBlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
rBlockDeviceMappings = lens _rBlockDeviceMappings (\ s a -> s{_rBlockDeviceMappings = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
rDryRun :: Lens' RunInstances (Maybe Bool)
rDryRun = lens _rDryRun (\ s a -> s{_rDryRun = a});

-- | The placement for the instance.
rPlacement :: Lens' RunInstances (Maybe Placement)
rPlacement = lens _rPlacement (\ s a -> s{_rPlacement = a});

-- | The ID of the AMI, which you can get by calling DescribeImages.
rImageId :: Lens' RunInstances Text
rImageId = lens _rImageId (\ s a -> s{_rImageId = a});

-- | The minimum number of instances to launch. If you specify a minimum that
-- is more instances than Amazon EC2 can launch in the target Availability
-- Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
rMinCount :: Lens' RunInstances Int
rMinCount = lens _rMinCount (\ s a -> s{_rMinCount = a});

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above 'MinCount'.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
rMaxCount :: Lens' RunInstances Int
rMaxCount = lens _rMaxCount (\ s a -> s{_rMaxCount = a});

instance AWSRequest RunInstances where
        type Rs RunInstances = Reservation
        request = postQuery eC2
        response = receiveXML (\ s h x -> parseXML x)

instance ToHeaders RunInstances where
        toHeaders = const mempty

instance ToPath RunInstances where
        toPath = const "/"

instance ToQuery RunInstances where
        toQuery RunInstances'{..}
          = mconcat
              ["Action" =: ("RunInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
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
               "IamInstanceProfile" =: _rIAMInstanceProfile,
               "PrivateIpAddress" =: _rPrivateIPAddress,
               "InstanceInitiatedShutdownBehavior" =:
                 _rInstanceInitiatedShutdownBehavior,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _rBlockDeviceMappings),
               "DryRun" =: _rDryRun, "Placement" =: _rPlacement,
               "ImageId" =: _rImageId, "MinCount" =: _rMinCount,
               "MaxCount" =: _rMaxCount]
