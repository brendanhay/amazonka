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
    , ciInstallUpdatesOnBoot
    , ciVirtualizationType
    , ciHostname
    , ciSSHKeyName
    , ciAgentVersion
    , ciSubnetId
    , ciEBSOptimized
    , ciOS
    , ciAvailabilityZone
    , ciAutoScalingType
    , ciArchitecture
    , ciAMIId
    , ciBlockDeviceMappings
    , ciRootDeviceType
    , ciStackId
    , ciLayerIds
    , ciInstanceType

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
-- * 'ciInstallUpdatesOnBoot'
--
-- * 'ciVirtualizationType'
--
-- * 'ciHostname'
--
-- * 'ciSSHKeyName'
--
-- * 'ciAgentVersion'
--
-- * 'ciSubnetId'
--
-- * 'ciEBSOptimized'
--
-- * 'ciOS'
--
-- * 'ciAvailabilityZone'
--
-- * 'ciAutoScalingType'
--
-- * 'ciArchitecture'
--
-- * 'ciAMIId'
--
-- * 'ciBlockDeviceMappings'
--
-- * 'ciRootDeviceType'
--
-- * 'ciStackId'
--
-- * 'ciLayerIds'
--
-- * 'ciInstanceType'
data CreateInstance = CreateInstance'
    { _ciInstallUpdatesOnBoot :: !(Maybe Bool)
    , _ciVirtualizationType   :: !(Maybe Text)
    , _ciHostname             :: !(Maybe Text)
    , _ciSSHKeyName           :: !(Maybe Text)
    , _ciAgentVersion         :: !(Maybe Text)
    , _ciSubnetId             :: !(Maybe Text)
    , _ciEBSOptimized         :: !(Maybe Bool)
    , _ciOS                   :: !(Maybe Text)
    , _ciAvailabilityZone     :: !(Maybe Text)
    , _ciAutoScalingType      :: !(Maybe AutoScalingType)
    , _ciArchitecture         :: !(Maybe Architecture)
    , _ciAMIId                :: !(Maybe Text)
    , _ciBlockDeviceMappings  :: !(Maybe [BlockDeviceMapping])
    , _ciRootDeviceType       :: !(Maybe RootDeviceType)
    , _ciStackId              :: !Text
    , _ciLayerIds             :: ![Text]
    , _ciInstanceType         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstance' smart constructor.
createInstance :: Text -> Text -> CreateInstance
createInstance pStackId_ pInstanceType_ =
    CreateInstance'
    { _ciInstallUpdatesOnBoot = Nothing
    , _ciVirtualizationType = Nothing
    , _ciHostname = Nothing
    , _ciSSHKeyName = Nothing
    , _ciAgentVersion = Nothing
    , _ciSubnetId = Nothing
    , _ciEBSOptimized = Nothing
    , _ciOS = Nothing
    , _ciAvailabilityZone = Nothing
    , _ciAutoScalingType = Nothing
    , _ciArchitecture = Nothing
    , _ciAMIId = Nothing
    , _ciBlockDeviceMappings = Nothing
    , _ciRootDeviceType = Nothing
    , _ciStackId = pStackId_
    , _ciLayerIds = mempty
    , _ciInstanceType = pInstanceType_
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
ciInstallUpdatesOnBoot :: Lens' CreateInstance (Maybe Bool)
ciInstallUpdatesOnBoot = lens _ciInstallUpdatesOnBoot (\ s a -> s{_ciInstallUpdatesOnBoot = a});

-- | The instance\'s virtualization type, @paravirtual@ or @hvm@.
ciVirtualizationType :: Lens' CreateInstance (Maybe Text)
ciVirtualizationType = lens _ciVirtualizationType (\ s a -> s{_ciVirtualizationType = a});

-- | The instance host name.
ciHostname :: Lens' CreateInstance (Maybe Text)
ciHostname = lens _ciHostname (\ s a -> s{_ciHostname = a});

-- | The instance\'s Amazon EC2 key-pair name.
ciSSHKeyName :: Lens' CreateInstance (Maybe Text)
ciSSHKeyName = lens _ciSSHKeyName (\ s a -> s{_ciSSHKeyName = a});

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
ciAgentVersion :: Lens' CreateInstance (Maybe Text)
ciAgentVersion = lens _ciAgentVersion (\ s a -> s{_ciAgentVersion = a});

-- | The ID of the instance\'s subnet. If the stack is running in a VPC, you
-- can use this parameter to override the stack\'s default subnet ID value
-- and direct AWS OpsWorks to launch the instance in a different subnet.
ciSubnetId :: Lens' CreateInstance (Maybe Text)
ciSubnetId = lens _ciSubnetId (\ s a -> s{_ciSubnetId = a});

-- | Whether to create an Amazon EBS-optimized instance.
ciEBSOptimized :: Lens' CreateInstance (Maybe Bool)
ciEBSOptimized = lens _ciEBSOptimized (\ s a -> s{_ciEBSOptimized = a});

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
ciOS :: Lens' CreateInstance (Maybe Text)
ciOS = lens _ciOS (\ s a -> s{_ciOS = a});

-- | The instance Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
ciAvailabilityZone :: Lens' CreateInstance (Maybe Text)
ciAvailabilityZone = lens _ciAvailabilityZone (\ s a -> s{_ciAvailabilityZone = a});

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
ciAutoScalingType :: Lens' CreateInstance (Maybe AutoScalingType)
ciAutoScalingType = lens _ciAutoScalingType (\ s a -> s{_ciAutoScalingType = a});

-- | The instance architecture. The default option is @x86_64@. Instance
-- types do not necessarily support both architectures. For a list of the
-- architectures that are supported by the different instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
ciArchitecture :: Lens' CreateInstance (Maybe Architecture)
ciArchitecture = lens _ciArchitecture (\ s a -> s{_ciArchitecture = a});

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the supported operating systems. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
ciAMIId :: Lens' CreateInstance (Maybe Text)
ciAMIId = lens _ciAMIId (\ s a -> s{_ciAMIId = a});

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block devices. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>.
ciBlockDeviceMappings :: Lens' CreateInstance [BlockDeviceMapping]
ciBlockDeviceMappings = lens _ciBlockDeviceMappings (\ s a -> s{_ciBlockDeviceMappings = a}) . _Default . _Coerce;

-- | The instance root device type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
ciRootDeviceType :: Lens' CreateInstance (Maybe RootDeviceType)
ciRootDeviceType = lens _ciRootDeviceType (\ s a -> s{_ciRootDeviceType = a});

-- | The stack ID.
ciStackId :: Lens' CreateInstance Text
ciStackId = lens _ciStackId (\ s a -> s{_ciStackId = a});

-- | An array that contains the instance\'s layer IDs.
ciLayerIds :: Lens' CreateInstance [Text]
ciLayerIds = lens _ciLayerIds (\ s a -> s{_ciLayerIds = a}) . _Coerce;

-- | The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
ciInstanceType :: Lens' CreateInstance Text
ciInstanceType = lens _ciInstanceType (\ s a -> s{_ciInstanceType = a});

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
              ["InstallUpdatesOnBoot" .= _ciInstallUpdatesOnBoot,
               "VirtualizationType" .= _ciVirtualizationType,
               "Hostname" .= _ciHostname,
               "SshKeyName" .= _ciSSHKeyName,
               "AgentVersion" .= _ciAgentVersion,
               "SubnetId" .= _ciSubnetId,
               "EbsOptimized" .= _ciEBSOptimized, "Os" .= _ciOS,
               "AvailabilityZone" .= _ciAvailabilityZone,
               "AutoScalingType" .= _ciAutoScalingType,
               "Architecture" .= _ciArchitecture,
               "AmiId" .= _ciAMIId,
               "BlockDeviceMappings" .= _ciBlockDeviceMappings,
               "RootDeviceType" .= _ciRootDeviceType,
               "StackId" .= _ciStackId, "LayerIds" .= _ciLayerIds,
               "InstanceType" .= _ciInstanceType]

instance ToPath CreateInstance where
        toPath = const mempty

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
createInstanceResponse pStatus_ =
    CreateInstanceResponse'
    { _cirsInstanceId = Nothing
    , _cirsStatus = pStatus_
    }

-- | The instance ID.
cirsInstanceId :: Lens' CreateInstanceResponse (Maybe Text)
cirsInstanceId = lens _cirsInstanceId (\ s a -> s{_cirsInstanceId = a});

-- | FIXME: Undocumented member.
cirsStatus :: Lens' CreateInstanceResponse Int
cirsStatus = lens _cirsStatus (\ s a -> s{_cirsStatus = a});
