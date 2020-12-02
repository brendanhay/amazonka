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
-- Module      : Network.AWS.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an instance in a specified stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html Adding an Instance to a Layer> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.CreateInstance
    (
    -- * Creating a Request
      createInstance
    , CreateInstance
    -- * Request Lenses
    , ciInstallUpdatesOnBoot
    , ciVirtualizationType
    , ciHostname
    , ciSSHKeyName
    , ciAgentVersion
    , ciSubnetId
    , ciEBSOptimized
    , ciOS
    , ciAvailabilityZone
    , ciTenancy
    , ciAutoScalingType
    , ciArchitecture
    , ciAMIId
    , ciRootDeviceType
    , ciBlockDeviceMappings
    , ciStackId
    , ciLayerIds
    , ciInstanceType

    -- * Destructuring the Response
    , createInstanceResponse
    , CreateInstanceResponse
    -- * Response Lenses
    , cirsInstanceId
    , cirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstance' smart constructor.
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
  , _ciTenancy              :: !(Maybe Text)
  , _ciAutoScalingType      :: !(Maybe AutoScalingType)
  , _ciArchitecture         :: !(Maybe Architecture)
  , _ciAMIId                :: !(Maybe Text)
  , _ciRootDeviceType       :: !(Maybe RootDeviceType)
  , _ciBlockDeviceMappings  :: !(Maybe [BlockDeviceMapping])
  , _ciStackId              :: !Text
  , _ciLayerIds             :: ![Text]
  , _ciInstanceType         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciInstallUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- * 'ciVirtualizationType' - The instance's virtualization type, @paravirtual@ or @hvm@ .
--
-- * 'ciHostname' - The instance host name.
--
-- * 'ciSSHKeyName' - The instance's Amazon EC2 key-pair name.
--
-- * 'ciAgentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:     * @INHERIT@ - Use the stack's default agent version setting.     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance. The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
--
-- * 'ciSubnetId' - The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
--
-- * 'ciEBSOptimized' - Whether to create an Amazon EBS-optimized instance.
--
-- * 'ciOS' - The instance's operating system, which must be set to one of the following.     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .     * @CentOS Linux 7@      * @Red Hat Enterprise Linux 7@      * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .     * A custom AMI: @Custom@ . For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> . The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the 'CreateInstance' action's AmiId parameter to specify the custom AMI that you want to use. Block device mappings are not supported if the value is @Custom@ . For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> For more information on how to use custom AMIs with AWS OpsWorks Stacks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- * 'ciAvailabilityZone' - The instance Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'ciTenancy' - The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
--
-- * 'ciAutoScalingType' - For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
--
-- * 'ciArchitecture' - The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
--
-- * 'ciAMIId' - A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- * 'ciRootDeviceType' - The instance root device type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- * 'ciBlockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
--
-- * 'ciStackId' - The stack ID.
--
-- * 'ciLayerIds' - An array that contains the instance's layer IDs.
--
-- * 'ciInstanceType' - The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
createInstance
    :: Text -- ^ 'ciStackId'
    -> Text -- ^ 'ciInstanceType'
    -> CreateInstance
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
    , _ciTenancy = Nothing
    , _ciAutoScalingType = Nothing
    , _ciArchitecture = Nothing
    , _ciAMIId = Nothing
    , _ciRootDeviceType = Nothing
    , _ciBlockDeviceMappings = Nothing
    , _ciStackId = pStackId_
    , _ciLayerIds = mempty
    , _ciInstanceType = pInstanceType_
    }


-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
ciInstallUpdatesOnBoot :: Lens' CreateInstance (Maybe Bool)
ciInstallUpdatesOnBoot = lens _ciInstallUpdatesOnBoot (\ s a -> s{_ciInstallUpdatesOnBoot = a})

-- | The instance's virtualization type, @paravirtual@ or @hvm@ .
ciVirtualizationType :: Lens' CreateInstance (Maybe Text)
ciVirtualizationType = lens _ciVirtualizationType (\ s a -> s{_ciVirtualizationType = a})

-- | The instance host name.
ciHostname :: Lens' CreateInstance (Maybe Text)
ciHostname = lens _ciHostname (\ s a -> s{_ciHostname = a})

-- | The instance's Amazon EC2 key-pair name.
ciSSHKeyName :: Lens' CreateInstance (Maybe Text)
ciSSHKeyName = lens _ciSSHKeyName (\ s a -> s{_ciSSHKeyName = a})

-- | The default AWS OpsWorks Stacks agent version. You have the following options:     * @INHERIT@ - Use the stack's default agent version setting.     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance. The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
ciAgentVersion :: Lens' CreateInstance (Maybe Text)
ciAgentVersion = lens _ciAgentVersion (\ s a -> s{_ciAgentVersion = a})

-- | The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
ciSubnetId :: Lens' CreateInstance (Maybe Text)
ciSubnetId = lens _ciSubnetId (\ s a -> s{_ciSubnetId = a})

-- | Whether to create an Amazon EBS-optimized instance.
ciEBSOptimized :: Lens' CreateInstance (Maybe Bool)
ciEBSOptimized = lens _ciEBSOptimized (\ s a -> s{_ciEBSOptimized = a})

-- | The instance's operating system, which must be set to one of the following.     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .     * @CentOS Linux 7@      * @Red Hat Enterprise Linux 7@      * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .     * A custom AMI: @Custom@ . For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> . The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the 'CreateInstance' action's AmiId parameter to specify the custom AMI that you want to use. Block device mappings are not supported if the value is @Custom@ . For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> For more information on how to use custom AMIs with AWS OpsWorks Stacks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
ciOS :: Lens' CreateInstance (Maybe Text)
ciOS = lens _ciOS (\ s a -> s{_ciOS = a})

-- | The instance Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
ciAvailabilityZone :: Lens' CreateInstance (Maybe Text)
ciAvailabilityZone = lens _ciAvailabilityZone (\ s a -> s{_ciAvailabilityZone = a})

-- | The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
ciTenancy :: Lens' CreateInstance (Maybe Text)
ciTenancy = lens _ciTenancy (\ s a -> s{_ciTenancy = a})

-- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
ciAutoScalingType :: Lens' CreateInstance (Maybe AutoScalingType)
ciAutoScalingType = lens _ciAutoScalingType (\ s a -> s{_ciAutoScalingType = a})

-- | The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
ciArchitecture :: Lens' CreateInstance (Maybe Architecture)
ciArchitecture = lens _ciArchitecture (\ s a -> s{_ciArchitecture = a})

-- | A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
ciAMIId :: Lens' CreateInstance (Maybe Text)
ciAMIId = lens _ciAMIId (\ s a -> s{_ciAMIId = a})

-- | The instance root device type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
ciRootDeviceType :: Lens' CreateInstance (Maybe RootDeviceType)
ciRootDeviceType = lens _ciRootDeviceType (\ s a -> s{_ciRootDeviceType = a})

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
ciBlockDeviceMappings :: Lens' CreateInstance [BlockDeviceMapping]
ciBlockDeviceMappings = lens _ciBlockDeviceMappings (\ s a -> s{_ciBlockDeviceMappings = a}) . _Default . _Coerce

-- | The stack ID.
ciStackId :: Lens' CreateInstance Text
ciStackId = lens _ciStackId (\ s a -> s{_ciStackId = a})

-- | An array that contains the instance's layer IDs.
ciLayerIds :: Lens' CreateInstance [Text]
ciLayerIds = lens _ciLayerIds (\ s a -> s{_ciLayerIds = a}) . _Coerce

-- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
ciInstanceType :: Lens' CreateInstance Text
ciInstanceType = lens _ciInstanceType (\ s a -> s{_ciInstanceType = a})

instance AWSRequest CreateInstance where
        type Rs CreateInstance = CreateInstanceResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 CreateInstanceResponse' <$>
                   (x .?> "InstanceId") <*> (pure (fromEnum s)))

instance Hashable CreateInstance where

instance NFData CreateInstance where

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
              (catMaybes
                 [("InstallUpdatesOnBoot" .=) <$>
                    _ciInstallUpdatesOnBoot,
                  ("VirtualizationType" .=) <$> _ciVirtualizationType,
                  ("Hostname" .=) <$> _ciHostname,
                  ("SshKeyName" .=) <$> _ciSSHKeyName,
                  ("AgentVersion" .=) <$> _ciAgentVersion,
                  ("SubnetId" .=) <$> _ciSubnetId,
                  ("EbsOptimized" .=) <$> _ciEBSOptimized,
                  ("Os" .=) <$> _ciOS,
                  ("AvailabilityZone" .=) <$> _ciAvailabilityZone,
                  ("Tenancy" .=) <$> _ciTenancy,
                  ("AutoScalingType" .=) <$> _ciAutoScalingType,
                  ("Architecture" .=) <$> _ciArchitecture,
                  ("AmiId" .=) <$> _ciAMIId,
                  ("RootDeviceType" .=) <$> _ciRootDeviceType,
                  ("BlockDeviceMappings" .=) <$>
                    _ciBlockDeviceMappings,
                  Just ("StackId" .= _ciStackId),
                  Just ("LayerIds" .= _ciLayerIds),
                  Just ("InstanceType" .= _ciInstanceType)])

instance ToPath CreateInstance where
        toPath = const "/"

instance ToQuery CreateInstance where
        toQuery = const mempty

-- | Contains the response to a @CreateInstance@ request.
--
--
--
-- /See:/ 'createInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { _cirsInstanceId     :: !(Maybe Text)
  , _cirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsInstanceId' - The instance ID.
--
-- * 'cirsResponseStatus' - -- | The response status code.
createInstanceResponse
    :: Int -- ^ 'cirsResponseStatus'
    -> CreateInstanceResponse
createInstanceResponse pResponseStatus_ =
  CreateInstanceResponse'
    {_cirsInstanceId = Nothing, _cirsResponseStatus = pResponseStatus_}


-- | The instance ID.
cirsInstanceId :: Lens' CreateInstanceResponse (Maybe Text)
cirsInstanceId = lens _cirsInstanceId (\ s a -> s{_cirsInstanceId = a})

-- | -- | The response status code.
cirsResponseStatus :: Lens' CreateInstanceResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\ s a -> s{_cirsResponseStatus = a})

instance NFData CreateInstanceResponse where
