{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.UpdateInstance
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

-- | Updates a specified instance.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateInstance.html>
module Network.AWS.OpsWorks.UpdateInstance
    (
    -- * Request
      UpdateInstance
    -- ** Request constructor
    , updateInstance
    -- ** Request lenses
    , uiInstallUpdatesOnBoot
    , uiHostname
    , uiSSHKeyName
    , uiInstanceType
    , uiEBSOptimized
    , uiOS
    , uiAutoScalingType
    , uiLayerIds
    , uiArchitecture
    , uiAMIId
    , uiInstanceId

    -- * Response
    , UpdateInstanceResponse
    -- ** Response constructor
    , updateInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiInstallUpdatesOnBoot'
--
-- * 'uiHostname'
--
-- * 'uiSSHKeyName'
--
-- * 'uiInstanceType'
--
-- * 'uiEBSOptimized'
--
-- * 'uiOS'
--
-- * 'uiAutoScalingType'
--
-- * 'uiLayerIds'
--
-- * 'uiArchitecture'
--
-- * 'uiAMIId'
--
-- * 'uiInstanceId'
data UpdateInstance = UpdateInstance'{_uiInstallUpdatesOnBoot :: Maybe Bool, _uiHostname :: Maybe Text, _uiSSHKeyName :: Maybe Text, _uiInstanceType :: Maybe Text, _uiEBSOptimized :: Maybe Bool, _uiOS :: Maybe Text, _uiAutoScalingType :: Maybe AutoScalingType, _uiLayerIds :: Maybe [Text], _uiArchitecture :: Maybe Architecture, _uiAMIId :: Maybe Text, _uiInstanceId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateInstance' smart constructor.
updateInstance :: Text -> UpdateInstance
updateInstance pInstanceId = UpdateInstance'{_uiInstallUpdatesOnBoot = Nothing, _uiHostname = Nothing, _uiSSHKeyName = Nothing, _uiInstanceType = Nothing, _uiEBSOptimized = Nothing, _uiOS = Nothing, _uiAutoScalingType = Nothing, _uiLayerIds = Nothing, _uiArchitecture = Nothing, _uiAMIId = Nothing, _uiInstanceId = pInstanceId};

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
uiInstallUpdatesOnBoot :: Lens' UpdateInstance (Maybe Bool)
uiInstallUpdatesOnBoot = lens _uiInstallUpdatesOnBoot (\ s a -> s{_uiInstallUpdatesOnBoot = a});

-- | The instance host name.
uiHostname :: Lens' UpdateInstance (Maybe Text)
uiHostname = lens _uiHostname (\ s a -> s{_uiHostname = a});

-- | The instance\'s Amazon EC2 key name.
uiSSHKeyName :: Lens' UpdateInstance (Maybe Text)
uiSSHKeyName = lens _uiSSHKeyName (\ s a -> s{_uiSSHKeyName = a});

-- | The instance type. AWS OpsWorks supports all instance types except
-- Cluster Compute, Cluster GPU, and High Memory Cluster. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the API Name column of the Available Instance Types table.
uiInstanceType :: Lens' UpdateInstance (Maybe Text)
uiInstanceType = lens _uiInstanceType (\ s a -> s{_uiInstanceType = a});

-- | Whether this is an Amazon EBS-optimized instance.
uiEBSOptimized :: Lens' UpdateInstance (Maybe Bool)
uiEBSOptimized = lens _uiEBSOptimized (\ s a -> s{_uiEBSOptimized = a});

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
uiOS :: Lens' UpdateInstance (Maybe Text)
uiOS = lens _uiOS (\ s a -> s{_uiOS = a});

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
uiAutoScalingType :: Lens' UpdateInstance (Maybe AutoScalingType)
uiAutoScalingType = lens _uiAutoScalingType (\ s a -> s{_uiAutoScalingType = a});

-- | The instance\'s layer IDs.
uiLayerIds :: Lens' UpdateInstance [Text]
uiLayerIds = lens _uiLayerIds (\ s a -> s{_uiLayerIds = a}) . _Default;

-- | The instance architecture. Instance types do not necessarily support
-- both architectures. For a list of the architectures that are supported
-- by the different instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
uiArchitecture :: Lens' UpdateInstance (Maybe Architecture)
uiArchitecture = lens _uiArchitecture (\ s a -> s{_uiArchitecture = a});

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the standard AWS OpsWorks AMIs: Amazon Linux, Ubuntu
-- 12.04 LTS, or Ubuntu 14.04 LTS. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances.html Instances>
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
uiAMIId :: Lens' UpdateInstance (Maybe Text)
uiAMIId = lens _uiAMIId (\ s a -> s{_uiAMIId = a});

-- | The instance ID.
uiInstanceId :: Lens' UpdateInstance Text
uiInstanceId = lens _uiInstanceId (\ s a -> s{_uiInstanceId = a});

instance AWSRequest UpdateInstance where
        type Sv UpdateInstance = OpsWorks
        type Rs UpdateInstance = UpdateInstanceResponse
        request = postJSON
        response = receiveNull UpdateInstanceResponse'

instance ToHeaders UpdateInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateInstance where
        toJSON UpdateInstance'{..}
          = object
              ["InstallUpdatesOnBoot" .= _uiInstallUpdatesOnBoot,
               "Hostname" .= _uiHostname,
               "SshKeyName" .= _uiSSHKeyName,
               "InstanceType" .= _uiInstanceType,
               "EbsOptimized" .= _uiEBSOptimized, "Os" .= _uiOS,
               "AutoScalingType" .= _uiAutoScalingType,
               "LayerIds" .= _uiLayerIds,
               "Architecture" .= _uiArchitecture,
               "AmiId" .= _uiAMIId, "InstanceId" .= _uiInstanceId]

instance ToPath UpdateInstance where
        toPath = const "/"

instance ToQuery UpdateInstance where
        toQuery = const mempty

-- | /See:/ 'updateInstanceResponse' smart constructor.
data UpdateInstanceResponse = UpdateInstanceResponse' deriving (Eq, Read, Show)

-- | 'UpdateInstanceResponse' smart constructor.
updateInstanceResponse :: UpdateInstanceResponse
updateInstanceResponse = UpdateInstanceResponse';
