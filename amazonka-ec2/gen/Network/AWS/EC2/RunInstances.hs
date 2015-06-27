{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Launches the specified number of instances using an AMI for which you
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
    , runSecurityGroupIds
    , runAdditionalInfo
    , runSecurityGroups
    , runClientToken
    , runDisableAPITermination
    , runNetworkInterfaces
    , runKeyName
    , runRAMDiskId
    , runKernelId
    , runSubnetId
    , runInstanceType
    , runEBSOptimized
    , runUserData
    , runMonitoring
    , runIAMInstanceProfile
    , runInstanceInitiatedShutdownBehavior
    , runPrivateIPAddress
    , runBlockDeviceMappings
    , runDryRun
    , runPlacement
    , runImageId
    , runMinCount
    , runMaxCount

    -- * Response
    , Reservation
    -- ** Response constructor
    , reservation
    -- ** Response lenses
    , resGroups
    , resInstances
    , resRequesterId
    , resReservationId
    , resOwnerId
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'runSecurityGroupIds'
--
-- * 'runAdditionalInfo'
--
-- * 'runSecurityGroups'
--
-- * 'runClientToken'
--
-- * 'runDisableAPITermination'
--
-- * 'runNetworkInterfaces'
--
-- * 'runKeyName'
--
-- * 'runRAMDiskId'
--
-- * 'runKernelId'
--
-- * 'runSubnetId'
--
-- * 'runInstanceType'
--
-- * 'runEBSOptimized'
--
-- * 'runUserData'
--
-- * 'runMonitoring'
--
-- * 'runIAMInstanceProfile'
--
-- * 'runInstanceInitiatedShutdownBehavior'
--
-- * 'runPrivateIPAddress'
--
-- * 'runBlockDeviceMappings'
--
-- * 'runDryRun'
--
-- * 'runPlacement'
--
-- * 'runImageId'
--
-- * 'runMinCount'
--
-- * 'runMaxCount'
data RunInstances = RunInstances'
    { _runSecurityGroupIds                  :: !(Maybe [Text])
    , _runAdditionalInfo                    :: !(Maybe Text)
    , _runSecurityGroups                    :: !(Maybe [Text])
    , _runClientToken                       :: !(Maybe Text)
    , _runDisableAPITermination             :: !(Maybe Bool)
    , _runNetworkInterfaces                 :: !(Maybe [InstanceNetworkInterfaceSpecification])
    , _runKeyName                           :: !(Maybe Text)
    , _runRAMDiskId                         :: !(Maybe Text)
    , _runKernelId                          :: !(Maybe Text)
    , _runSubnetId                          :: !(Maybe Text)
    , _runInstanceType                      :: !(Maybe InstanceType)
    , _runEBSOptimized                      :: !(Maybe Bool)
    , _runUserData                          :: !(Maybe Text)
    , _runMonitoring                        :: !(Maybe RunInstancesMonitoringEnabled)
    , _runIAMInstanceProfile                :: !(Maybe IAMInstanceProfileSpecification)
    , _runInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
    , _runPrivateIPAddress                  :: !(Maybe Text)
    , _runBlockDeviceMappings               :: !(Maybe [BlockDeviceMapping])
    , _runDryRun                            :: !(Maybe Bool)
    , _runPlacement                         :: !(Maybe Placement)
    , _runImageId                           :: !Text
    , _runMinCount                          :: !Int
    , _runMaxCount                          :: !Int
    } deriving (Eq,Read,Show)

-- | 'RunInstances' smart constructor.
runInstances :: Text -> Int -> Int -> RunInstances
runInstances pImageId pMinCount pMaxCount =
    RunInstances'
    { _runSecurityGroupIds = Nothing
    , _runAdditionalInfo = Nothing
    , _runSecurityGroups = Nothing
    , _runClientToken = Nothing
    , _runDisableAPITermination = Nothing
    , _runNetworkInterfaces = Nothing
    , _runKeyName = Nothing
    , _runRAMDiskId = Nothing
    , _runKernelId = Nothing
    , _runSubnetId = Nothing
    , _runInstanceType = Nothing
    , _runEBSOptimized = Nothing
    , _runUserData = Nothing
    , _runMonitoring = Nothing
    , _runIAMInstanceProfile = Nothing
    , _runInstanceInitiatedShutdownBehavior = Nothing
    , _runPrivateIPAddress = Nothing
    , _runBlockDeviceMappings = Nothing
    , _runDryRun = Nothing
    , _runPlacement = Nothing
    , _runImageId = pImageId
    , _runMinCount = pMinCount
    , _runMaxCount = pMaxCount
    }

-- | One or more security group IDs. You can create a security group using
-- CreateSecurityGroup.
--
-- Default: Amazon EC2 uses the default security group.
runSecurityGroupIds :: Lens' RunInstances [Text]
runSecurityGroupIds = lens _runSecurityGroupIds (\ s a -> s{_runSecurityGroupIds = a}) . _Default;

-- | Reserved.
runAdditionalInfo :: Lens' RunInstances (Maybe Text)
runAdditionalInfo = lens _runAdditionalInfo (\ s a -> s{_runAdditionalInfo = a});

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead.
--
-- Default: Amazon EC2 uses the default security group.
runSecurityGroups :: Lens' RunInstances [Text]
runSecurityGroups = lens _runSecurityGroups (\ s a -> s{_runSecurityGroups = a}) . _Default;

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
runClientToken :: Lens' RunInstances (Maybe Text)
runClientToken = lens _runClientToken (\ s a -> s{_runClientToken = a});

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
runDisableAPITermination :: Lens' RunInstances (Maybe Bool)
runDisableAPITermination = lens _runDisableAPITermination (\ s a -> s{_runDisableAPITermination = a});

-- | One or more network interfaces.
runNetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
runNetworkInterfaces = lens _runNetworkInterfaces (\ s a -> s{_runNetworkInterfaces = a}) . _Default;

-- | The name of the key pair. You can create a key pair using CreateKeyPair
-- or ImportKeyPair.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
runKeyName :: Lens' RunInstances (Maybe Text)
runKeyName = lens _runKeyName (\ s a -> s{_runKeyName = a});

-- | The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon Elastic Compute Cloud User Guide/.
runRAMDiskId :: Lens' RunInstances (Maybe Text)
runRAMDiskId = lens _runRAMDiskId (\ s a -> s{_runRAMDiskId = a});

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon Elastic Compute Cloud User Guide/.
runKernelId :: Lens' RunInstances (Maybe Text)
runKernelId = lens _runKernelId (\ s a -> s{_runKernelId = a});

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
runSubnetId :: Lens' RunInstances (Maybe Text)
runSubnetId = lens _runSubnetId (\ s a -> s{_runSubnetId = a});

-- | The instance type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: @m1.small@
runInstanceType :: Lens' RunInstances (Maybe InstanceType)
runInstanceType = lens _runInstanceType (\ s a -> s{_runInstanceType = a});

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
runEBSOptimized :: Lens' RunInstances (Maybe Bool)
runEBSOptimized = lens _runEBSOptimized (\ s a -> s{_runEBSOptimized = a});

-- | The Base64-encoded MIME user data for the instances.
runUserData :: Lens' RunInstances (Maybe Text)
runUserData = lens _runUserData (\ s a -> s{_runUserData = a});

-- | The monitoring for the instance.
runMonitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
runMonitoring = lens _runMonitoring (\ s a -> s{_runMonitoring = a});

-- | The IAM instance profile.
runIAMInstanceProfile :: Lens' RunInstances (Maybe IAMInstanceProfileSpecification)
runIAMInstanceProfile = lens _runIAMInstanceProfile (\ s a -> s{_runIAMInstanceProfile = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
runInstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe ShutdownBehavior)
runInstanceInitiatedShutdownBehavior = lens _runInstanceInitiatedShutdownBehavior (\ s a -> s{_runInstanceInitiatedShutdownBehavior = a});

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
runPrivateIPAddress :: Lens' RunInstances (Maybe Text)
runPrivateIPAddress = lens _runPrivateIPAddress (\ s a -> s{_runPrivateIPAddress = a});

-- | The block device mapping.
runBlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
runBlockDeviceMappings = lens _runBlockDeviceMappings (\ s a -> s{_runBlockDeviceMappings = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
runDryRun :: Lens' RunInstances (Maybe Bool)
runDryRun = lens _runDryRun (\ s a -> s{_runDryRun = a});

-- | The placement for the instance.
runPlacement :: Lens' RunInstances (Maybe Placement)
runPlacement = lens _runPlacement (\ s a -> s{_runPlacement = a});

-- | The ID of the AMI, which you can get by calling DescribeImages.
runImageId :: Lens' RunInstances Text
runImageId = lens _runImageId (\ s a -> s{_runImageId = a});

-- | The minimum number of instances to launch. If you specify a minimum that
-- is more instances than Amazon EC2 can launch in the target Availability
-- Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
runMinCount :: Lens' RunInstances Int
runMinCount = lens _runMinCount (\ s a -> s{_runMinCount = a});

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above @MinCount@.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
runMaxCount :: Lens' RunInstances Int
runMaxCount = lens _runMaxCount (\ s a -> s{_runMaxCount = a});

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
                    _runSecurityGroupIds),
               "AdditionalInfo" =: _runAdditionalInfo,
               toQuery
                 (toQueryList "SecurityGroup" <$> _runSecurityGroups),
               "ClientToken" =: _runClientToken,
               "DisableApiTermination" =: _runDisableAPITermination,
               toQuery
                 (toQueryList "item" <$> _runNetworkInterfaces),
               "KeyName" =: _runKeyName,
               "RamdiskId" =: _runRAMDiskId,
               "KernelId" =: _runKernelId,
               "SubnetId" =: _runSubnetId,
               "InstanceType" =: _runInstanceType,
               "EbsOptimized" =: _runEBSOptimized,
               "UserData" =: _runUserData,
               "Monitoring" =: _runMonitoring,
               "IamInstanceProfile" =: _runIAMInstanceProfile,
               "InstanceInitiatedShutdownBehavior" =:
                 _runInstanceInitiatedShutdownBehavior,
               "PrivateIpAddress" =: _runPrivateIPAddress,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _runBlockDeviceMappings),
               "DryRun" =: _runDryRun, "Placement" =: _runPlacement,
               "ImageId" =: _runImageId, "MinCount" =: _runMinCount,
               "MaxCount" =: _runMaxCount]
