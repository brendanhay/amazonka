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
-- Module      : Network.AWS.OpsWorks.UpdateInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified instance.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateInstance
    (
    -- * Creating a Request
      updateInstance
    , UpdateInstance
    -- * Request Lenses
    , uiInstallUpdatesOnBoot
    , uiHostname
    , uiSSHKeyName
    , uiAgentVersion
    , uiInstanceType
    , uiEBSOptimized
    , uiOS
    , uiAutoScalingType
    , uiLayerIds
    , uiArchitecture
    , uiAMIId
    , uiInstanceId

    -- * Destructuring the Response
    , updateInstanceResponse
    , UpdateInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateInstance' smart constructor.
data UpdateInstance = UpdateInstance'
  { _uiInstallUpdatesOnBoot :: !(Maybe Bool)
  , _uiHostname             :: !(Maybe Text)
  , _uiSSHKeyName           :: !(Maybe Text)
  , _uiAgentVersion         :: !(Maybe Text)
  , _uiInstanceType         :: !(Maybe Text)
  , _uiEBSOptimized         :: !(Maybe Bool)
  , _uiOS                   :: !(Maybe Text)
  , _uiAutoScalingType      :: !(Maybe AutoScalingType)
  , _uiLayerIds             :: !(Maybe [Text])
  , _uiArchitecture         :: !(Maybe Architecture)
  , _uiAMIId                :: !(Maybe Text)
  , _uiInstanceId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiInstallUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- * 'uiHostname' - The instance host name.
--
-- * 'uiSSHKeyName' - The instance's Amazon EC2 key name.
--
-- * 'uiAgentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:     * @INHERIT@ - Use the stack's default agent version setting.     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, you must edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance. The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
--
-- * 'uiInstanceType' - The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
--
-- * 'uiEBSOptimized' - This property cannot be updated.
--
-- * 'uiOS' - The instance's operating system, which must be set to one of the following. You cannot update an instance that is using a custom AMI.     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .     * @CentOS Linux 7@      * @Red Hat Enterprise Linux 7@      * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ . For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> . The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the AmiId parameter to specify the custom AMI that you want to use. For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> . For more information on how to use custom AMIs with OpsWorks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- * 'uiAutoScalingType' - For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
--
-- * 'uiLayerIds' - The instance's layer IDs.
--
-- * 'uiArchitecture' - The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
--
-- * 'uiAMIId' - The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
--
-- * 'uiInstanceId' - The instance ID.
updateInstance
    :: Text -- ^ 'uiInstanceId'
    -> UpdateInstance
updateInstance pInstanceId_ =
  UpdateInstance'
    { _uiInstallUpdatesOnBoot = Nothing
    , _uiHostname = Nothing
    , _uiSSHKeyName = Nothing
    , _uiAgentVersion = Nothing
    , _uiInstanceType = Nothing
    , _uiEBSOptimized = Nothing
    , _uiOS = Nothing
    , _uiAutoScalingType = Nothing
    , _uiLayerIds = Nothing
    , _uiArchitecture = Nothing
    , _uiAMIId = Nothing
    , _uiInstanceId = pInstanceId_
    }


-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
uiInstallUpdatesOnBoot :: Lens' UpdateInstance (Maybe Bool)
uiInstallUpdatesOnBoot = lens _uiInstallUpdatesOnBoot (\ s a -> s{_uiInstallUpdatesOnBoot = a})

-- | The instance host name.
uiHostname :: Lens' UpdateInstance (Maybe Text)
uiHostname = lens _uiHostname (\ s a -> s{_uiHostname = a})

-- | The instance's Amazon EC2 key name.
uiSSHKeyName :: Lens' UpdateInstance (Maybe Text)
uiSSHKeyName = lens _uiSSHKeyName (\ s a -> s{_uiSSHKeyName = a})

-- | The default AWS OpsWorks Stacks agent version. You have the following options:     * @INHERIT@ - Use the stack's default agent version setting.     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, you must edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance. The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
uiAgentVersion :: Lens' UpdateInstance (Maybe Text)
uiAgentVersion = lens _uiAgentVersion (\ s a -> s{_uiAgentVersion = a})

-- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
uiInstanceType :: Lens' UpdateInstance (Maybe Text)
uiInstanceType = lens _uiInstanceType (\ s a -> s{_uiInstanceType = a})

-- | This property cannot be updated.
uiEBSOptimized :: Lens' UpdateInstance (Maybe Bool)
uiEBSOptimized = lens _uiEBSOptimized (\ s a -> s{_uiEBSOptimized = a})

-- | The instance's operating system, which must be set to one of the following. You cannot update an instance that is using a custom AMI.     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .     * @CentOS Linux 7@      * @Red Hat Enterprise Linux 7@      * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ . For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> . The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the AmiId parameter to specify the custom AMI that you want to use. For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> . For more information on how to use custom AMIs with OpsWorks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
uiOS :: Lens' UpdateInstance (Maybe Text)
uiOS = lens _uiOS (\ s a -> s{_uiOS = a})

-- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
uiAutoScalingType :: Lens' UpdateInstance (Maybe AutoScalingType)
uiAutoScalingType = lens _uiAutoScalingType (\ s a -> s{_uiAutoScalingType = a})

-- | The instance's layer IDs.
uiLayerIds :: Lens' UpdateInstance [Text]
uiLayerIds = lens _uiLayerIds (\ s a -> s{_uiLayerIds = a}) . _Default . _Coerce

-- | The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
uiArchitecture :: Lens' UpdateInstance (Maybe Architecture)
uiArchitecture = lens _uiArchitecture (\ s a -> s{_uiArchitecture = a})

-- | The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
uiAMIId :: Lens' UpdateInstance (Maybe Text)
uiAMIId = lens _uiAMIId (\ s a -> s{_uiAMIId = a})

-- | The instance ID.
uiInstanceId :: Lens' UpdateInstance Text
uiInstanceId = lens _uiInstanceId (\ s a -> s{_uiInstanceId = a})

instance AWSRequest UpdateInstance where
        type Rs UpdateInstance = UpdateInstanceResponse
        request = postJSON opsWorks
        response = receiveNull UpdateInstanceResponse'

instance Hashable UpdateInstance where

instance NFData UpdateInstance where

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
              (catMaybes
                 [("InstallUpdatesOnBoot" .=) <$>
                    _uiInstallUpdatesOnBoot,
                  ("Hostname" .=) <$> _uiHostname,
                  ("SshKeyName" .=) <$> _uiSSHKeyName,
                  ("AgentVersion" .=) <$> _uiAgentVersion,
                  ("InstanceType" .=) <$> _uiInstanceType,
                  ("EbsOptimized" .=) <$> _uiEBSOptimized,
                  ("Os" .=) <$> _uiOS,
                  ("AutoScalingType" .=) <$> _uiAutoScalingType,
                  ("LayerIds" .=) <$> _uiLayerIds,
                  ("Architecture" .=) <$> _uiArchitecture,
                  ("AmiId" .=) <$> _uiAMIId,
                  Just ("InstanceId" .= _uiInstanceId)])

instance ToPath UpdateInstance where
        toPath = const "/"

instance ToQuery UpdateInstance where
        toQuery = const mempty

-- | /See:/ 'updateInstanceResponse' smart constructor.
data UpdateInstanceResponse =
  UpdateInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInstanceResponse' with the minimum fields required to make a request.
--
updateInstanceResponse
    :: UpdateInstanceResponse
updateInstanceResponse = UpdateInstanceResponse'


instance NFData UpdateInstanceResponse where
