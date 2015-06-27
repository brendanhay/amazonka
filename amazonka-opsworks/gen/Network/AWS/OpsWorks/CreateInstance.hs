{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.CreateInstance
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

-- | Creates an instance in a specified stack. For more information, see
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
    , cirInstanceId
    , cirStatus
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
    { _ciInstallUpdatesOnBoot :: Maybe Bool
    , _ciVirtualizationType   :: Maybe Text
    , _ciHostname             :: Maybe Text
    , _ciSSHKeyName           :: Maybe Text
    , _ciSubnetId             :: Maybe Text
    , _ciEBSOptimized         :: Maybe Bool
    , _ciOS                   :: Maybe Text
    , _ciAvailabilityZone     :: Maybe Text
    , _ciAutoScalingType      :: Maybe AutoScalingType
    , _ciArchitecture         :: Maybe Architecture
    , _ciAMIId                :: Maybe Text
    , _ciBlockDeviceMappings  :: Maybe [BlockDeviceMapping]
    , _ciRootDeviceType       :: Maybe RootDeviceType
    , _ciStackId              :: Text
    , _ciLayerIds             :: [Text]
    , _ciInstanceType         :: Text
    } deriving (Eq,Read,Show)

-- | 'CreateInstance' smart constructor.
createInstance :: Text -> Text -> CreateInstance
createInstance pStackId pInstanceType =
    CreateInstance'
    { _ciInstallUpdatesOnBoot = Nothing
    , _ciVirtualizationType = Nothing
    , _ciHostname = Nothing
    , _ciSSHKeyName = Nothing
    , _ciSubnetId = Nothing
    , _ciEBSOptimized = Nothing
    , _ciOS = Nothing
    , _ciAvailabilityZone = Nothing
    , _ciAutoScalingType = Nothing
    , _ciArchitecture = Nothing
    , _ciAMIId = Nothing
    , _ciBlockDeviceMappings = Nothing
    , _ciRootDeviceType = Nothing
    , _ciStackId = pStackId
    , _ciLayerIds = mempty
    , _ciInstanceType = pInstanceType
    }

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
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

-- | The instance\'s Amazon EC2 key pair name.
ciSSHKeyName :: Lens' CreateInstance (Maybe Text)
ciSSHKeyName = lens _ciSSHKeyName (\ s a -> s{_ciSSHKeyName = a});

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
-- For Windows stacks: Microsoft Windows Server 2012 R2.
--
-- For Linux stacks:
--
-- -   Standard operating systems: an Amazon Linux version such as
--     @Amazon Linux 2014.09@, @Ubuntu 12.04 LTS@, or @Ubuntu 14.04 LTS@.
-- -   Custom AMIs: @Custom@
--
-- The default option is the current Amazon Linux version. If you set this
-- parameter to @Custom@, you must use the CreateInstance action\'s AmiId
-- parameter to specify the custom AMI that you want to use. For more
-- information on the standard operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>For
-- more information on how to use custom AMIs with OpsWorks, see
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
-- based on one of the standard AWS OpsWorks AMIs: Amazon Linux, Ubuntu
-- 12.04 LTS, or Ubuntu 14.04 LTS. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances.html Instances>.
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
ciAMIId :: Lens' CreateInstance (Maybe Text)
ciAMIId = lens _ciAMIId (\ s a -> s{_ciAMIId = a});

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block devices. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>.
ciBlockDeviceMappings :: Lens' CreateInstance [BlockDeviceMapping]
ciBlockDeviceMappings = lens _ciBlockDeviceMappings (\ s a -> s{_ciBlockDeviceMappings = a}) . _Default;

-- | The instance root device type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
ciRootDeviceType :: Lens' CreateInstance (Maybe RootDeviceType)
ciRootDeviceType = lens _ciRootDeviceType (\ s a -> s{_ciRootDeviceType = a});

-- | The stack ID.
ciStackId :: Lens' CreateInstance Text
ciStackId = lens _ciStackId (\ s a -> s{_ciStackId = a});

-- | An array that contains the instance layer IDs.
ciLayerIds :: Lens' CreateInstance [Text]
ciLayerIds = lens _ciLayerIds (\ s a -> s{_ciLayerIds = a});

-- | The instance type. AWS OpsWorks supports all instance types except
-- Cluster Compute, Cluster GPU, and High Memory Cluster. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the API Name column of the Available Instance Types table.
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
        toPath = const "/"

instance ToQuery CreateInstance where
        toQuery = const mempty

-- | Contains the response to a @CreateInstance@ request.
--
-- /See:/ 'createInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirInstanceId'
--
-- * 'cirStatus'
data CreateInstanceResponse = CreateInstanceResponse'
    { _cirInstanceId :: Maybe Text
    , _cirStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateInstanceResponse' smart constructor.
createInstanceResponse :: Int -> CreateInstanceResponse
createInstanceResponse pStatus =
    CreateInstanceResponse'
    { _cirInstanceId = Nothing
    , _cirStatus = pStatus
    }

-- | The instance ID.
cirInstanceId :: Lens' CreateInstanceResponse (Maybe Text)
cirInstanceId = lens _cirInstanceId (\ s a -> s{_cirInstanceId = a});

-- | FIXME: Undocumented member.
cirStatus :: Lens' CreateInstanceResponse Int
cirStatus = lens _cirStatus (\ s a -> s{_cirStatus = a});
