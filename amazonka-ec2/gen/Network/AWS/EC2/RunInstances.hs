{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified number of instances using an AMI for which you
-- have permissions.
--
-- When you launch an instance, it enters the @pending@ state. After the
-- instance is ready for you, it enters the @running@ state. To check the
-- state of your instance, call DescribeInstances.
--
-- If you don\'t specify a security group when launching an instance,
-- Amazon EC2 uses the default security group. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Security Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/.
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
-- not subscribed, @RunInstances@ fails.
--
-- T2 instance types can only be launched into a VPC. If you do not have a
-- default VPC, or if you do not specify a subnet ID in the request,
-- @RunInstances@ fails.
--
-- For more information about troubleshooting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html What To Do If An Instance Immediately Terminates>,
-- and
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html Troubleshooting Connecting to Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html>
module Network.AWS.EC2.RunInstances
    (
    -- * Request
      RunInstances
    -- ** Request constructor
    , runInstances
    -- ** Request lenses
    , rirqSecurityGroupIds
    , rirqAdditionalInfo
    , rirqSecurityGroups
    , rirqClientToken
    , rirqDisableAPITermination
    , rirqNetworkInterfaces
    , rirqKeyName
    , rirqRAMDiskId
    , rirqKernelId
    , rirqSubnetId
    , rirqInstanceType
    , rirqEBSOptimized
    , rirqUserData
    , rirqMonitoring
    , rirqIAMInstanceProfile
    , rirqInstanceInitiatedShutdownBehavior
    , rirqPrivateIPAddress
    , rirqBlockDeviceMappings
    , rirqDryRun
    , rirqPlacement
    , rirqImageId
    , rirqMinCount
    , rirqMaxCount

    -- * Response
    , Reservation
    -- ** Response constructor
    , reservation
    -- ** Response lenses
    , rGroups
    , rInstances
    , rRequesterId
    , rReservationId
    , rOwnerId
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirqSecurityGroupIds'
--
-- * 'rirqAdditionalInfo'
--
-- * 'rirqSecurityGroups'
--
-- * 'rirqClientToken'
--
-- * 'rirqDisableAPITermination'
--
-- * 'rirqNetworkInterfaces'
--
-- * 'rirqKeyName'
--
-- * 'rirqRAMDiskId'
--
-- * 'rirqKernelId'
--
-- * 'rirqSubnetId'
--
-- * 'rirqInstanceType'
--
-- * 'rirqEBSOptimized'
--
-- * 'rirqUserData'
--
-- * 'rirqMonitoring'
--
-- * 'rirqIAMInstanceProfile'
--
-- * 'rirqInstanceInitiatedShutdownBehavior'
--
-- * 'rirqPrivateIPAddress'
--
-- * 'rirqBlockDeviceMappings'
--
-- * 'rirqDryRun'
--
-- * 'rirqPlacement'
--
-- * 'rirqImageId'
--
-- * 'rirqMinCount'
--
-- * 'rirqMaxCount'
data RunInstances = RunInstances'
    { _rirqSecurityGroupIds                  :: !(Maybe [Text])
    , _rirqAdditionalInfo                    :: !(Maybe Text)
    , _rirqSecurityGroups                    :: !(Maybe [Text])
    , _rirqClientToken                       :: !(Maybe Text)
    , _rirqDisableAPITermination             :: !(Maybe Bool)
    , _rirqNetworkInterfaces                 :: !(Maybe [InstanceNetworkInterfaceSpecification])
    , _rirqKeyName                           :: !(Maybe Text)
    , _rirqRAMDiskId                         :: !(Maybe Text)
    , _rirqKernelId                          :: !(Maybe Text)
    , _rirqSubnetId                          :: !(Maybe Text)
    , _rirqInstanceType                      :: !(Maybe InstanceType)
    , _rirqEBSOptimized                      :: !(Maybe Bool)
    , _rirqUserData                          :: !(Maybe Text)
    , _rirqMonitoring                        :: !(Maybe RunInstancesMonitoringEnabled)
    , _rirqIAMInstanceProfile                :: !(Maybe IAMInstanceProfileSpecification)
    , _rirqInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
    , _rirqPrivateIPAddress                  :: !(Maybe Text)
    , _rirqBlockDeviceMappings               :: !(Maybe [BlockDeviceMapping])
    , _rirqDryRun                            :: !(Maybe Bool)
    , _rirqPlacement                         :: !(Maybe Placement)
    , _rirqImageId                           :: !Text
    , _rirqMinCount                          :: !Int
    , _rirqMaxCount                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RunInstances' smart constructor.
runInstances :: Text -> Int -> Int -> RunInstances
runInstances pImageId pMinCount pMaxCount =
    RunInstances'
    { _rirqSecurityGroupIds = Nothing
    , _rirqAdditionalInfo = Nothing
    , _rirqSecurityGroups = Nothing
    , _rirqClientToken = Nothing
    , _rirqDisableAPITermination = Nothing
    , _rirqNetworkInterfaces = Nothing
    , _rirqKeyName = Nothing
    , _rirqRAMDiskId = Nothing
    , _rirqKernelId = Nothing
    , _rirqSubnetId = Nothing
    , _rirqInstanceType = Nothing
    , _rirqEBSOptimized = Nothing
    , _rirqUserData = Nothing
    , _rirqMonitoring = Nothing
    , _rirqIAMInstanceProfile = Nothing
    , _rirqInstanceInitiatedShutdownBehavior = Nothing
    , _rirqPrivateIPAddress = Nothing
    , _rirqBlockDeviceMappings = Nothing
    , _rirqDryRun = Nothing
    , _rirqPlacement = Nothing
    , _rirqImageId = pImageId
    , _rirqMinCount = pMinCount
    , _rirqMaxCount = pMaxCount
    }

-- | One or more security group IDs. You can create a security group using
-- CreateSecurityGroup.
--
-- Default: Amazon EC2 uses the default security group.
rirqSecurityGroupIds :: Lens' RunInstances [Text]
rirqSecurityGroupIds = lens _rirqSecurityGroupIds (\ s a -> s{_rirqSecurityGroupIds = a}) . _Default;

-- | Reserved.
rirqAdditionalInfo :: Lens' RunInstances (Maybe Text)
rirqAdditionalInfo = lens _rirqAdditionalInfo (\ s a -> s{_rirqAdditionalInfo = a});

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead.
--
-- Default: Amazon EC2 uses the default security group.
rirqSecurityGroups :: Lens' RunInstances [Text]
rirqSecurityGroups = lens _rirqSecurityGroups (\ s a -> s{_rirqSecurityGroups = a}) . _Default;

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
rirqClientToken :: Lens' RunInstances (Maybe Text)
rirqClientToken = lens _rirqClientToken (\ s a -> s{_rirqClientToken = a});

-- | If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. If you
-- set this parameter to @true@ and then later want to be able to terminate
-- the instance, you must first change the value of the
-- @disableApiTermination@ attribute to @false@ using
-- ModifyInstanceAttribute. Alternatively, if you set
-- @InstanceInitiatedShutdownBehavior@ to @terminate@, you can terminate
-- the instance by running the shutdown command from the instance.
--
-- Default: @false@
rirqDisableAPITermination :: Lens' RunInstances (Maybe Bool)
rirqDisableAPITermination = lens _rirqDisableAPITermination (\ s a -> s{_rirqDisableAPITermination = a});

-- | One or more network interfaces.
rirqNetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
rirqNetworkInterfaces = lens _rirqNetworkInterfaces (\ s a -> s{_rirqNetworkInterfaces = a}) . _Default;

-- | The name of the key pair. You can create a key pair using CreateKeyPair
-- or ImportKeyPair.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
rirqKeyName :: Lens' RunInstances (Maybe Text)
rirqKeyName = lens _rirqKeyName (\ s a -> s{_rirqKeyName = a});

-- | The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rirqRAMDiskId :: Lens' RunInstances (Maybe Text)
rirqRAMDiskId = lens _rirqRAMDiskId (\ s a -> s{_rirqRAMDiskId = a});

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rirqKernelId :: Lens' RunInstances (Maybe Text)
rirqKernelId = lens _rirqKernelId (\ s a -> s{_rirqKernelId = a});

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
rirqSubnetId :: Lens' RunInstances (Maybe Text)
rirqSubnetId = lens _rirqSubnetId (\ s a -> s{_rirqSubnetId = a});

-- | The instance type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: @m1.small@
rirqInstanceType :: Lens' RunInstances (Maybe InstanceType)
rirqInstanceType = lens _rirqInstanceType (\ s a -> s{_rirqInstanceType = a});

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
rirqEBSOptimized :: Lens' RunInstances (Maybe Bool)
rirqEBSOptimized = lens _rirqEBSOptimized (\ s a -> s{_rirqEBSOptimized = a});

-- | The Base64-encoded MIME user data for the instances.
rirqUserData :: Lens' RunInstances (Maybe Text)
rirqUserData = lens _rirqUserData (\ s a -> s{_rirqUserData = a});

-- | The monitoring for the instance.
rirqMonitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
rirqMonitoring = lens _rirqMonitoring (\ s a -> s{_rirqMonitoring = a});

-- | The IAM instance profile.
rirqIAMInstanceProfile :: Lens' RunInstances (Maybe IAMInstanceProfileSpecification)
rirqIAMInstanceProfile = lens _rirqIAMInstanceProfile (\ s a -> s{_rirqIAMInstanceProfile = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
rirqInstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe ShutdownBehavior)
rirqInstanceInitiatedShutdownBehavior = lens _rirqInstanceInitiatedShutdownBehavior (\ s a -> s{_rirqInstanceInitiatedShutdownBehavior = a});

-- | [EC2-VPC] The primary IP address. You must specify a value from the IP
-- address range of the subnet.
--
-- Only one private IP address can be designated as primary. Therefore, you
-- can\'t specify this parameter if @PrivateIpAddresses.n.Primary@ is set
-- to @true@ and @PrivateIpAddresses.n.PrivateIpAddress@ is set to an IP
-- address.
--
-- Default: We select an IP address from the IP address range of the
-- subnet.
rirqPrivateIPAddress :: Lens' RunInstances (Maybe Text)
rirqPrivateIPAddress = lens _rirqPrivateIPAddress (\ s a -> s{_rirqPrivateIPAddress = a});

-- | The block device mapping.
rirqBlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
rirqBlockDeviceMappings = lens _rirqBlockDeviceMappings (\ s a -> s{_rirqBlockDeviceMappings = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rirqDryRun :: Lens' RunInstances (Maybe Bool)
rirqDryRun = lens _rirqDryRun (\ s a -> s{_rirqDryRun = a});

-- | The placement for the instance.
rirqPlacement :: Lens' RunInstances (Maybe Placement)
rirqPlacement = lens _rirqPlacement (\ s a -> s{_rirqPlacement = a});

-- | The ID of the AMI, which you can get by calling DescribeImages.
rirqImageId :: Lens' RunInstances Text
rirqImageId = lens _rirqImageId (\ s a -> s{_rirqImageId = a});

-- | The minimum number of instances to launch. If you specify a minimum that
-- is more instances than Amazon EC2 can launch in the target Availability
-- Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
rirqMinCount :: Lens' RunInstances Int
rirqMinCount = lens _rirqMinCount (\ s a -> s{_rirqMinCount = a});

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above @MinCount@.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
rirqMaxCount :: Lens' RunInstances Int
rirqMaxCount = lens _rirqMaxCount (\ s a -> s{_rirqMaxCount = a});

instance AWSRequest RunInstances where
        type Sv RunInstances = EC2
        type Rs RunInstances = Reservation
        request = post
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
               toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _rirqSecurityGroupIds),
               "AdditionalInfo" =: _rirqAdditionalInfo,
               toQuery
                 (toQueryList "SecurityGroup" <$>
                    _rirqSecurityGroups),
               "ClientToken" =: _rirqClientToken,
               "DisableApiTermination" =:
                 _rirqDisableAPITermination,
               toQuery
                 (toQueryList "item" <$> _rirqNetworkInterfaces),
               "KeyName" =: _rirqKeyName,
               "RamdiskId" =: _rirqRAMDiskId,
               "KernelId" =: _rirqKernelId,
               "SubnetId" =: _rirqSubnetId,
               "InstanceType" =: _rirqInstanceType,
               "EbsOptimized" =: _rirqEBSOptimized,
               "UserData" =: _rirqUserData,
               "Monitoring" =: _rirqMonitoring,
               "IamInstanceProfile" =: _rirqIAMInstanceProfile,
               "InstanceInitiatedShutdownBehavior" =:
                 _rirqInstanceInitiatedShutdownBehavior,
               "PrivateIpAddress" =: _rirqPrivateIPAddress,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _rirqBlockDeviceMappings),
               "DryRun" =: _rirqDryRun,
               "Placement" =: _rirqPlacement,
               "ImageId" =: _rirqImageId,
               "MinCount" =: _rirqMinCount,
               "MaxCount" =: _rirqMaxCount]
