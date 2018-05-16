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
-- Module      : Network.AWS.OpsWorks.UpdateStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified stack.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateStack
    (
    -- * Creating a Request
      updateStack
    , UpdateStack
    -- * Request Lenses
    , usDefaultInstanceProfileARN
    , usServiceRoleARN
    , usDefaultRootDeviceType
    , usChefConfiguration
    , usAgentVersion
    , usDefaultSSHKeyName
    , usCustomJSON
    , usCustomCookbooksSource
    , usDefaultAvailabilityZone
    , usAttributes
    , usName
    , usDefaultOS
    , usUseOpsworksSecurityGroups
    , usUseCustomCookbooks
    , usDefaultSubnetId
    , usConfigurationManager
    , usHostnameTheme
    , usStackId

    -- * Destructuring the Response
    , updateStackResponse
    , UpdateStackResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStack' smart constructor.
data UpdateStack = UpdateStack'
  { _usDefaultInstanceProfileARN :: !(Maybe Text)
  , _usServiceRoleARN :: !(Maybe Text)
  , _usDefaultRootDeviceType :: !(Maybe RootDeviceType)
  , _usChefConfiguration :: !(Maybe ChefConfiguration)
  , _usAgentVersion :: !(Maybe Text)
  , _usDefaultSSHKeyName :: !(Maybe Text)
  , _usCustomJSON :: !(Maybe Text)
  , _usCustomCookbooksSource :: !(Maybe Source)
  , _usDefaultAvailabilityZone :: !(Maybe Text)
  , _usAttributes :: !(Maybe (Map StackAttributesKeys (Maybe Text)))
  , _usName :: !(Maybe Text)
  , _usDefaultOS :: !(Maybe Text)
  , _usUseOpsworksSecurityGroups :: !(Maybe Bool)
  , _usUseCustomCookbooks :: !(Maybe Bool)
  , _usDefaultSubnetId :: !(Maybe Text)
  , _usConfigurationManager :: !(Maybe StackConfigurationManager)
  , _usHostnameTheme :: !(Maybe Text)
  , _usStackId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usDefaultInstanceProfileARN' - The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'usServiceRoleARN' - Do not use this parameter. You cannot update a stack's service role.
--
-- * 'usDefaultRootDeviceType' - The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- * 'usChefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- * 'usAgentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances. The default setting is @LATEST@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
--
-- * 'usDefaultSSHKeyName' - A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <http://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
--
-- * 'usCustomJSON' - A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- * 'usCustomCookbooksSource' - Undocumented member.
--
-- * 'usDefaultAvailabilityZone' - The stack's default Availability Zone, which must be in the stack's region. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' .
--
-- * 'usAttributes' - One or more user-defined key-value pairs to be added to the stack attributes.
--
-- * 'usName' - The stack's new name.
--
-- * 'usDefaultOS' - The stack's operating system, which must be set to one of the following:     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .     * @CentOS Linux 7@      * @Red Hat Enterprise Linux 7@      * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information on how to use custom AMIs with OpsWorks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> . The default option is the stack's current operating system. For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
--
-- * 'usUseOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers. AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. @UseOpsworksSecurityGroups@ allows you to provide your own custom security groups instead of using the built-in groups. @UseOpsworksSecurityGroups@ has the following settings:      * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it, but you cannot delete the built-in security group.     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate EC2 security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on. Custom security groups are required only for those layers that need custom settings. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- * 'usUseCustomCookbooks' - Whether the stack uses custom cookbooks.
--
-- * 'usDefaultSubnetId' - The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
--
-- * 'usConfigurationManager' - The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 11.4.
--
-- * 'usHostnameTheme' - The stack's new host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:     * @Baked_Goods@      * @Clouds@      * @Europe_Cities@      * @Fruits@      * @Greek_Deities@      * @Legendary_creatures_from_Japan@      * @Planets_and_Moons@      * @Roman_Deities@      * @Scottish_Islands@      * @US_Cities@      * @Wild_Cats@  To obtain a generated host name, call @GetHostNameSuggestion@ , which returns a host name based on the current theme.
--
-- * 'usStackId' - The stack ID.
updateStack
    :: Text -- ^ 'usStackId'
    -> UpdateStack
updateStack pStackId_ =
  UpdateStack'
    { _usDefaultInstanceProfileARN = Nothing
    , _usServiceRoleARN = Nothing
    , _usDefaultRootDeviceType = Nothing
    , _usChefConfiguration = Nothing
    , _usAgentVersion = Nothing
    , _usDefaultSSHKeyName = Nothing
    , _usCustomJSON = Nothing
    , _usCustomCookbooksSource = Nothing
    , _usDefaultAvailabilityZone = Nothing
    , _usAttributes = Nothing
    , _usName = Nothing
    , _usDefaultOS = Nothing
    , _usUseOpsworksSecurityGroups = Nothing
    , _usUseCustomCookbooks = Nothing
    , _usDefaultSubnetId = Nothing
    , _usConfigurationManager = Nothing
    , _usHostnameTheme = Nothing
    , _usStackId = pStackId_
    }


-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
usDefaultInstanceProfileARN :: Lens' UpdateStack (Maybe Text)
usDefaultInstanceProfileARN = lens _usDefaultInstanceProfileARN (\ s a -> s{_usDefaultInstanceProfileARN = a})

-- | Do not use this parameter. You cannot update a stack's service role.
usServiceRoleARN :: Lens' UpdateStack (Maybe Text)
usServiceRoleARN = lens _usServiceRoleARN (\ s a -> s{_usServiceRoleARN = a})

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
usDefaultRootDeviceType :: Lens' UpdateStack (Maybe RootDeviceType)
usDefaultRootDeviceType = lens _usDefaultRootDeviceType (\ s a -> s{_usDefaultRootDeviceType = a})

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
usChefConfiguration :: Lens' UpdateStack (Maybe ChefConfiguration)
usChefConfiguration = lens _usChefConfiguration (\ s a -> s{_usChefConfiguration = a})

-- | The default AWS OpsWorks Stacks agent version. You have the following options:     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances. The default setting is @LATEST@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
usAgentVersion :: Lens' UpdateStack (Maybe Text)
usAgentVersion = lens _usAgentVersion (\ s a -> s{_usAgentVersion = a})

-- | A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <http://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
usDefaultSSHKeyName :: Lens' UpdateStack (Maybe Text)
usDefaultSSHKeyName = lens _usDefaultSSHKeyName (\ s a -> s{_usDefaultSSHKeyName = a})

-- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
usCustomJSON :: Lens' UpdateStack (Maybe Text)
usCustomJSON = lens _usCustomJSON (\ s a -> s{_usCustomJSON = a})

-- | Undocumented member.
usCustomCookbooksSource :: Lens' UpdateStack (Maybe Source)
usCustomCookbooksSource = lens _usCustomCookbooksSource (\ s a -> s{_usCustomCookbooksSource = a})

-- | The stack's default Availability Zone, which must be in the stack's region. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' .
usDefaultAvailabilityZone :: Lens' UpdateStack (Maybe Text)
usDefaultAvailabilityZone = lens _usDefaultAvailabilityZone (\ s a -> s{_usDefaultAvailabilityZone = a})

-- | One or more user-defined key-value pairs to be added to the stack attributes.
usAttributes :: Lens' UpdateStack (HashMap StackAttributesKeys (Maybe Text))
usAttributes = lens _usAttributes (\ s a -> s{_usAttributes = a}) . _Default . _Map

-- | The stack's new name.
usName :: Lens' UpdateStack (Maybe Text)
usName = lens _usName (\ s a -> s{_usName = a})

-- | The stack's operating system, which must be set to one of the following:     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .     * @CentOS Linux 7@      * @Red Hat Enterprise Linux 7@      * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information on how to use custom AMIs with OpsWorks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> . The default option is the stack's current operating system. For more information on the supported operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
usDefaultOS :: Lens' UpdateStack (Maybe Text)
usDefaultOS = lens _usDefaultOS (\ s a -> s{_usDefaultOS = a})

-- | Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers. AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. @UseOpsworksSecurityGroups@ allows you to provide your own custom security groups instead of using the built-in groups. @UseOpsworksSecurityGroups@ has the following settings:      * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it, but you cannot delete the built-in security group.     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate EC2 security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on. Custom security groups are required only for those layers that need custom settings. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
usUseOpsworksSecurityGroups :: Lens' UpdateStack (Maybe Bool)
usUseOpsworksSecurityGroups = lens _usUseOpsworksSecurityGroups (\ s a -> s{_usUseOpsworksSecurityGroups = a})

-- | Whether the stack uses custom cookbooks.
usUseCustomCookbooks :: Lens' UpdateStack (Maybe Bool)
usUseCustomCookbooks = lens _usUseCustomCookbooks (\ s a -> s{_usUseCustomCookbooks = a})

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
usDefaultSubnetId :: Lens' UpdateStack (Maybe Text)
usDefaultSubnetId = lens _usDefaultSubnetId (\ s a -> s{_usDefaultSubnetId = a})

-- | The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 11.4.
usConfigurationManager :: Lens' UpdateStack (Maybe StackConfigurationManager)
usConfigurationManager = lens _usConfigurationManager (\ s a -> s{_usConfigurationManager = a})

-- | The stack's new host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:     * @Baked_Goods@      * @Clouds@      * @Europe_Cities@      * @Fruits@      * @Greek_Deities@      * @Legendary_creatures_from_Japan@      * @Planets_and_Moons@      * @Roman_Deities@      * @Scottish_Islands@      * @US_Cities@      * @Wild_Cats@  To obtain a generated host name, call @GetHostNameSuggestion@ , which returns a host name based on the current theme.
usHostnameTheme :: Lens' UpdateStack (Maybe Text)
usHostnameTheme = lens _usHostnameTheme (\ s a -> s{_usHostnameTheme = a})

-- | The stack ID.
usStackId :: Lens' UpdateStack Text
usStackId = lens _usStackId (\ s a -> s{_usStackId = a})

instance AWSRequest UpdateStack where
        type Rs UpdateStack = UpdateStackResponse
        request = postJSON opsWorks
        response = receiveNull UpdateStackResponse'

instance Hashable UpdateStack where

instance NFData UpdateStack where

instance ToHeaders UpdateStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateStack where
        toJSON UpdateStack'{..}
          = object
              (catMaybes
                 [("DefaultInstanceProfileArn" .=) <$>
                    _usDefaultInstanceProfileARN,
                  ("ServiceRoleArn" .=) <$> _usServiceRoleARN,
                  ("DefaultRootDeviceType" .=) <$>
                    _usDefaultRootDeviceType,
                  ("ChefConfiguration" .=) <$> _usChefConfiguration,
                  ("AgentVersion" .=) <$> _usAgentVersion,
                  ("DefaultSshKeyName" .=) <$> _usDefaultSSHKeyName,
                  ("CustomJson" .=) <$> _usCustomJSON,
                  ("CustomCookbooksSource" .=) <$>
                    _usCustomCookbooksSource,
                  ("DefaultAvailabilityZone" .=) <$>
                    _usDefaultAvailabilityZone,
                  ("Attributes" .=) <$> _usAttributes,
                  ("Name" .=) <$> _usName,
                  ("DefaultOs" .=) <$> _usDefaultOS,
                  ("UseOpsworksSecurityGroups" .=) <$>
                    _usUseOpsworksSecurityGroups,
                  ("UseCustomCookbooks" .=) <$> _usUseCustomCookbooks,
                  ("DefaultSubnetId" .=) <$> _usDefaultSubnetId,
                  ("ConfigurationManager" .=) <$>
                    _usConfigurationManager,
                  ("HostnameTheme" .=) <$> _usHostnameTheme,
                  Just ("StackId" .= _usStackId)])

instance ToPath UpdateStack where
        toPath = const "/"

instance ToQuery UpdateStack where
        toQuery = const mempty

-- | /See:/ 'updateStackResponse' smart constructor.
data UpdateStackResponse =
  UpdateStackResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
--
updateStackResponse
    :: UpdateStackResponse
updateStackResponse = UpdateStackResponse'


instance NFData UpdateStackResponse where
