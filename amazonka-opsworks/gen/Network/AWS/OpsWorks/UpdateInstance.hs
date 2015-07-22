{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified instance.
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
    , uirqInstallUpdatesOnBoot
    , uirqHostname
    , uirqSSHKeyName
    , uirqAgentVersion
    , uirqInstanceType
    , uirqEBSOptimized
    , uirqOS
    , uirqAutoScalingType
    , uirqLayerIds
    , uirqArchitecture
    , uirqAMIId
    , uirqInstanceId

    -- * Response
    , UpdateInstanceResponse
    -- ** Response constructor
    , updateInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirqInstallUpdatesOnBoot'
--
-- * 'uirqHostname'
--
-- * 'uirqSSHKeyName'
--
-- * 'uirqAgentVersion'
--
-- * 'uirqInstanceType'
--
-- * 'uirqEBSOptimized'
--
-- * 'uirqOS'
--
-- * 'uirqAutoScalingType'
--
-- * 'uirqLayerIds'
--
-- * 'uirqArchitecture'
--
-- * 'uirqAMIId'
--
-- * 'uirqInstanceId'
data UpdateInstance = UpdateInstance'
    { _uirqInstallUpdatesOnBoot :: !(Maybe Bool)
    , _uirqHostname             :: !(Maybe Text)
    , _uirqSSHKeyName           :: !(Maybe Text)
    , _uirqAgentVersion         :: !(Maybe Text)
    , _uirqInstanceType         :: !(Maybe Text)
    , _uirqEBSOptimized         :: !(Maybe Bool)
    , _uirqOS                   :: !(Maybe Text)
    , _uirqAutoScalingType      :: !(Maybe AutoScalingType)
    , _uirqLayerIds             :: !(Maybe [Text])
    , _uirqArchitecture         :: !(Maybe Architecture)
    , _uirqAMIId                :: !(Maybe Text)
    , _uirqInstanceId           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateInstance' smart constructor.
updateInstance :: Text -> UpdateInstance
updateInstance pInstanceId_ =
    UpdateInstance'
    { _uirqInstallUpdatesOnBoot = Nothing
    , _uirqHostname = Nothing
    , _uirqSSHKeyName = Nothing
    , _uirqAgentVersion = Nothing
    , _uirqInstanceType = Nothing
    , _uirqEBSOptimized = Nothing
    , _uirqOS = Nothing
    , _uirqAutoScalingType = Nothing
    , _uirqLayerIds = Nothing
    , _uirqArchitecture = Nothing
    , _uirqAMIId = Nothing
    , _uirqInstanceId = pInstanceId_
    }

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
uirqInstallUpdatesOnBoot :: Lens' UpdateInstance (Maybe Bool)
uirqInstallUpdatesOnBoot = lens _uirqInstallUpdatesOnBoot (\ s a -> s{_uirqInstallUpdatesOnBoot = a});

-- | The instance host name.
uirqHostname :: Lens' UpdateInstance (Maybe Text)
uirqHostname = lens _uirqHostname (\ s a -> s{_uirqHostname = a});

-- | The instance\'s Amazon EC2 key name.
uirqSSHKeyName :: Lens' UpdateInstance (Maybe Text)
uirqSSHKeyName = lens _uirqSSHKeyName (\ s a -> s{_uirqSSHKeyName = a});

-- | The default AWS OpsWorks agent version. You have the following options:
--
-- -   @INHERIT@ - Use the stack\'s default agent version setting.
-- -   /version_number/ - Use the specified agent version. This value
--     overrides the stack\'s default setting. To update the agent version,
--     you must edit the instance configuration and specify a new version.
--     AWS OpsWorks then automatically installs that version on the
--     instance.
--
-- The default setting is @INHERIT@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions.
uirqAgentVersion :: Lens' UpdateInstance (Maybe Text)
uirqAgentVersion = lens _uirqAgentVersion (\ s a -> s{_uirqAgentVersion = a});

-- | The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
uirqInstanceType :: Lens' UpdateInstance (Maybe Text)
uirqInstanceType = lens _uirqInstanceType (\ s a -> s{_uirqInstanceType = a});

-- | This property cannot be updated.
uirqEBSOptimized :: Lens' UpdateInstance (Maybe Bool)
uirqEBSOptimized = lens _uirqEBSOptimized (\ s a -> s{_uirqEBSOptimized = a});

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
-- parameter to @Custom@, you must use the AmiId parameter to specify the
-- custom AMI that you want to use. For more information on the supported
-- operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>.
-- For more information on how to use custom AMIs with OpsWorks, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- You can specify a different Linux operating system for the updated
-- stack, but you cannot change from Linux to Windows or Windows to Linux.
uirqOS :: Lens' UpdateInstance (Maybe Text)
uirqOS = lens _uirqOS (\ s a -> s{_uirqOS = a});

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
uirqAutoScalingType :: Lens' UpdateInstance (Maybe AutoScalingType)
uirqAutoScalingType = lens _uirqAutoScalingType (\ s a -> s{_uirqAutoScalingType = a});

-- | The instance\'s layer IDs.
uirqLayerIds :: Lens' UpdateInstance [Text]
uirqLayerIds = lens _uirqLayerIds (\ s a -> s{_uirqLayerIds = a}) . _Default;

-- | The instance architecture. Instance types do not necessarily support
-- both architectures. For a list of the architectures that are supported
-- by the different instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
uirqArchitecture :: Lens' UpdateInstance (Maybe Architecture)
uirqArchitecture = lens _uirqArchitecture (\ s a -> s{_uirqArchitecture = a});

-- | A custom AMI ID to be used to create the instance. The AMI must be based
-- on one of the supported operating systems. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
uirqAMIId :: Lens' UpdateInstance (Maybe Text)
uirqAMIId = lens _uirqAMIId (\ s a -> s{_uirqAMIId = a});

-- | The instance ID.
uirqInstanceId :: Lens' UpdateInstance Text
uirqInstanceId = lens _uirqInstanceId (\ s a -> s{_uirqInstanceId = a});

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
              ["InstallUpdatesOnBoot" .= _uirqInstallUpdatesOnBoot,
               "Hostname" .= _uirqHostname,
               "SshKeyName" .= _uirqSSHKeyName,
               "AgentVersion" .= _uirqAgentVersion,
               "InstanceType" .= _uirqInstanceType,
               "EbsOptimized" .= _uirqEBSOptimized, "Os" .= _uirqOS,
               "AutoScalingType" .= _uirqAutoScalingType,
               "LayerIds" .= _uirqLayerIds,
               "Architecture" .= _uirqArchitecture,
               "AmiId" .= _uirqAMIId,
               "InstanceId" .= _uirqInstanceId]

instance ToPath UpdateInstance where
        toPath = const "/"

instance ToQuery UpdateInstance where
        toQuery = const mempty

-- | /See:/ 'updateInstanceResponse' smart constructor.
data UpdateInstanceResponse =
    UpdateInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateInstanceResponse' smart constructor.
updateInstanceResponse :: UpdateInstanceResponse
updateInstanceResponse = UpdateInstanceResponse'
