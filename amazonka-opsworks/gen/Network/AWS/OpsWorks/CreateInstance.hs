{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an instance in a specified stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html Adding an Instance to a Layer>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateInstance.html>
module Network.AWS.OpsWorks.CreateInstance
    (
    -- * Request
      CreateInstance
    -- ** Request constructor
    , createInstance
    -- ** Request lenses
    , cirqInstallUpdatesOnBoot
    , cirqVirtualizationType
    , cirqHostname
    , cirqSSHKeyName
    , cirqAgentVersion
    , cirqSubnetId
    , cirqEBSOptimized
    , cirqOS
    , cirqAvailabilityZone
    , cirqAutoScalingType
    , cirqArchitecture
    , cirqAMIId
    , cirqBlockDeviceMappings
    , cirqRootDeviceType
    , cirqStackId
    , cirqLayerIds
    , cirqInstanceType

    -- * Response
    , CreateInstanceResponse
    -- ** Response constructor
    , createInstanceResponse
    -- ** Response lenses
    , cirsInstanceId
    , cirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirqInstallUpdatesOnBoot'
--
-- * 'cirqVirtualizationType'
--
-- * 'cirqHostname'
--
-- * 'cirqSSHKeyName'
--
-- * 'cirqAgentVersion'
--
-- * 'cirqSubnetId'
--
-- * 'cirqEBSOptimized'
--
-- * 'cirqOS'
--
-- * 'cirqAvailabilityZone'
--
-- * 'cirqAutoScalingType'
--
-- * 'cirqArchitecture'
--
-- * 'cirqAMIId'
--
-- * 'cirqBlockDeviceMappings'
--
-- * 'cirqRootDeviceType'
--
-- * 'cirqStackId'
--
-- * 'cirqLayerIds'
--
-- * 'cirqInstanceType'
data CreateInstance = CreateInstance'
    { _cirqInstallUpdatesOnBoot :: !(Maybe Bool)
    , _cirqVirtualizationType   :: !(Maybe Text)
    , _cirqHostname             :: !(Maybe Text)
    , _cirqSSHKeyName           :: !(Maybe Text)
    , _cirqAgentVersion         :: !(Maybe Text)
    , _cirqSubnetId             :: !(Maybe Text)
    , _cirqEBSOptimized         :: !(Maybe Bool)
    , _cirqOS                   :: !(Maybe Text)
    , _cirqAvailabilityZone     :: !(Maybe Text)
    , _cirqAutoScalingType      :: !(Maybe AutoScalingType)
    , _cirqArchitecture         :: !(Maybe Architecture)
    , _cirqAMIId                :: !(Maybe Text)
    , _cirqBlockDeviceMappings  :: !(Maybe [BlockDeviceMapping])
    , _cirqRootDeviceType       :: !(Maybe RootDeviceType)
    , _cirqStackId              :: !Text
    , _cirqLayerIds             :: ![Text]
    , _cirqInstanceType         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstance' smart constructor.
createInstance :: Text -> Text -> CreateInstance
createInstance pStackId pInstanceType =
    CreateInstance'
    { _cirqInstallUpdatesOnBoot = Nothing
    , _cirqVirtualizationType = Nothing
    , _cirqHostname = Nothing
    , _cirqSSHKeyName = Nothing
    , _cirqAgentVersion = Nothing
    , _cirqSubnetId = Nothing
    , _cirqEBSOptimized = Nothing
    , _cirqOS = Nothing
    , _cirqAvailabilityZone = Nothing
    , _cirqAutoScalingType = Nothing
    , _cirqArchitecture = Nothing
    , _cirqAMIId = Nothing
    , _cirqBlockDeviceMappings = Nothing
    , _cirqRootDeviceType = Nothing
    , _cirqStackId = pStackId
    , _cirqLayerIds = mempty
    , _cirqInstanceType = pInstanceType
    }

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@ to ensure that
-- your instances have the latest security updates.
cirqInstallUpdatesOnBoot :: Lens' CreateInstance (Maybe Bool)
cirqInstallUpdatesOnBoot = lens _cirqInstallUpdatesOnBoot (\ s a -> s{_cirqInstallUpdatesOnBoot = a});

-- | The instance\'s virtualization type, @paravirtual@ or @hvm@.
cirqVirtualizationType :: Lens' CreateInstance (Maybe Text)
cirqVirtualizationType = lens _cirqVirtualizationType (\ s a -> s{_cirqVirtualizationType = a});

-- | The instance host name.
cirqHostname :: Lens' CreateInstance (Maybe Text)
cirqHostname = lens _cirqHostname (\ s a -> s{_cirqHostname = a});

-- | The instance\'s Amazon EC2 key-pair name.
cirqSSHKeyName :: Lens' CreateInstance (Maybe Text)
cirqSSHKeyName = lens _cirqSSHKeyName (\ s a -> s{_cirqSSHKeyName = a});

-- | The default AWS OpsWorks agent version. You have the following options:
--
-- -   @INHERIT@ - Use the stack\'s default agent version setting.
-- -   /version_number/ - Use the specified agent version. This value
--     overrides the stack\'s default setting. To update the agent version,
--     edit the instance configuration and specify a new version. AWS
--     OpsWorks then automatically installs that version on the instance.
--
-- The default setting is @INHERIT@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions.
cirqAgentVersion :: Lens' CreateInstance (Maybe Text)
cirqAgentVersion = lens _cirqAgentVersion (\ s a -> s{_cirqAgentVersion = a});

-- | The ID of the instance\'s subnet. If the stack is running in a VPC, you
-- can use this parameter to override the stack\'s default subnet ID value
-- and direct AWS OpsWorks to launch the instance in a different subnet.
cirqSubnetId :: Lens' CreateInstance (Maybe Text)
cirqSubnetId = lens _cirqSubnetId (\ s a -> s{_cirqSubnetId = a});

-- | Whether to create an Amazon EBS-optimized instance.
cirqEBSOptimized :: Lens' CreateInstance (Maybe Bool)
cirqEBSOptimized = lens _cirqEBSOptimized (\ s a -> s{_cirqEBSOptimized = a});

-- | The instance\'s operating system, which must be set to one of the
-- following.
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2015.03@, @Ubuntu 12.04 LTS@, or @Ubuntu 14.04 LTS@.
-- -   @Microsoft Windows Server 2012 R2 Base@.
-- -   A custom AMI: @Custom@.
--
-- For more information on the supported operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Operating Systems>.
--
-- The default option is the current Amazon Linux version. If you set this
-- parameter to @Custom@, you must use the CreateInstance action\'s AmiId
-- parameter to specify the custom AMI that you want to use. For more
-- information on the supported operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>For
-- more information on how to use custom AMIs with AWS OpsWorks, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
cirqOS :: Lens' CreateInstance (Maybe Text)
cirqOS = lens _cirqOS (\ s a -> s{_cirqOS = a});

-- | The instance Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
cirqAvailabilityZone :: Lens' CreateInstance (Maybe Text)
cirqAvailabilityZone = lens _cirqAvailabilityZone (\ s a -> s{_cirqAvailabilityZone = a});

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
cirqAutoScalingType :: Lens' CreateInstance (Maybe AutoScalingType)
cirqAutoScalingType = lens _cirqAutoScalingType (\ s a -> s{_cirqAutoScalingType = a});

-- | The instance architecture. The default option is @x86_64@. Instance
-- types do not necessarily support both architectures. For a list of the
-- architectures that are supported by the different instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
cirqArchitecture :: Lens' CreateInstance (Maybe Architecture)
cirqArchitecture = lens _cirqArchitecture (\ s a -> s{_cirqArchitecture = a});

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the supported operating systems. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
cirqAMIId :: Lens' CreateInstance (Maybe Text)
cirqAMIId = lens _cirqAMIId (\ s a -> s{_cirqAMIId = a});

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block devices. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>.
cirqBlockDeviceMappings :: Lens' CreateInstance [BlockDeviceMapping]
cirqBlockDeviceMappings = lens _cirqBlockDeviceMappings (\ s a -> s{_cirqBlockDeviceMappings = a}) . _Default;

-- | The instance root device type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
cirqRootDeviceType :: Lens' CreateInstance (Maybe RootDeviceType)
cirqRootDeviceType = lens _cirqRootDeviceType (\ s a -> s{_cirqRootDeviceType = a});

-- | The stack ID.
cirqStackId :: Lens' CreateInstance Text
cirqStackId = lens _cirqStackId (\ s a -> s{_cirqStackId = a});

-- | An array that contains the instance\'s layer IDs.
cirqLayerIds :: Lens' CreateInstance [Text]
cirqLayerIds = lens _cirqLayerIds (\ s a -> s{_cirqLayerIds = a});

-- | The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
cirqInstanceType :: Lens' CreateInstance Text
cirqInstanceType = lens _cirqInstanceType (\ s a -> s{_cirqInstanceType = a});

instance AWSRequest CreateInstance where
        type Sv CreateInstance = OpsWorks
        type Rs CreateInstance = CreateInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateInstanceResponse' <$>
                   (x .?> "InstanceId") <*> (pure (fromEnum s)))

instance ToHeaders CreateInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateInstance where
        toJSON CreateInstance'{..}
          = object
              ["InstallUpdatesOnBoot" .= _cirqInstallUpdatesOnBoot,
               "VirtualizationType" .= _cirqVirtualizationType,
               "Hostname" .= _cirqHostname,
               "SshKeyName" .= _cirqSSHKeyName,
               "AgentVersion" .= _cirqAgentVersion,
               "SubnetId" .= _cirqSubnetId,
               "EbsOptimized" .= _cirqEBSOptimized, "Os" .= _cirqOS,
               "AvailabilityZone" .= _cirqAvailabilityZone,
               "AutoScalingType" .= _cirqAutoScalingType,
               "Architecture" .= _cirqArchitecture,
               "AmiId" .= _cirqAMIId,
               "BlockDeviceMappings" .= _cirqBlockDeviceMappings,
               "RootDeviceType" .= _cirqRootDeviceType,
               "StackId" .= _cirqStackId,
               "LayerIds" .= _cirqLayerIds,
               "InstanceType" .= _cirqInstanceType]

instance ToPath CreateInstance where
        toPath = const "/"

instance ToQuery CreateInstance where
        toQuery = const mempty

-- | Contains the response to a @CreateInstance@ request.
--
-- /See:/ 'createInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirsInstanceId'
--
-- * 'cirsStatus'
data CreateInstanceResponse = CreateInstanceResponse'
    { _cirsInstanceId :: !(Maybe Text)
    , _cirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstanceResponse' smart constructor.
createInstanceResponse :: Int -> CreateInstanceResponse
createInstanceResponse pStatus =
    CreateInstanceResponse'
    { _cirsInstanceId = Nothing
    , _cirsStatus = pStatus
    }

-- | The instance ID.
cirsInstanceId :: Lens' CreateInstanceResponse (Maybe Text)
cirsInstanceId = lens _cirsInstanceId (\ s a -> s{_cirsInstanceId = a});

-- | FIXME: Undocumented member.
cirsStatus :: Lens' CreateInstanceResponse Int
cirsStatus = lens _cirsStatus (\ s a -> s{_cirsStatus = a});
