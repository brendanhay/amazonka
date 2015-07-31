{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CloneStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a clone of a specified stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-cloning.html Clone a Stack>.
-- By default, all parameters are set to the values used by the parent
-- stack.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CloneStack.html>
module Network.AWS.OpsWorks.CloneStack
    (
    -- * Request
      CloneStack
    -- ** Request constructor
    , cloneStack
    -- ** Request lenses
    , cCloneAppIds
    , cDefaultInstanceProfileARN
    , cDefaultRootDeviceType
    , cChefConfiguration
    , cVPCId
    , cAgentVersion
    , cDefaultSSHKeyName
    , cCustomJSON
    , cClonePermissions
    , cCustomCookbooksSource
    , cDefaultAvailabilityZone
    , cName
    , cUseOpsworksSecurityGroups
    , cDefaultOS
    , cAttributes
    , cUseCustomCookbooks
    , cDefaultSubnetId
    , cRegion
    , cConfigurationManager
    , cHostnameTheme
    , cSourceStackId
    , cServiceRoleARN

    -- * Response
    , CloneStackResponse
    -- ** Response constructor
    , cloneStackResponse
    -- ** Response lenses
    , csrsStackId
    , csrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cloneStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cCloneAppIds'
--
-- * 'cDefaultInstanceProfileARN'
--
-- * 'cDefaultRootDeviceType'
--
-- * 'cChefConfiguration'
--
-- * 'cVPCId'
--
-- * 'cAgentVersion'
--
-- * 'cDefaultSSHKeyName'
--
-- * 'cCustomJSON'
--
-- * 'cClonePermissions'
--
-- * 'cCustomCookbooksSource'
--
-- * 'cDefaultAvailabilityZone'
--
-- * 'cName'
--
-- * 'cUseOpsworksSecurityGroups'
--
-- * 'cDefaultOS'
--
-- * 'cAttributes'
--
-- * 'cUseCustomCookbooks'
--
-- * 'cDefaultSubnetId'
--
-- * 'cRegion'
--
-- * 'cConfigurationManager'
--
-- * 'cHostnameTheme'
--
-- * 'cSourceStackId'
--
-- * 'cServiceRoleARN'
data CloneStack = CloneStack'
    { _cCloneAppIds               :: !(Maybe [Text])
    , _cDefaultInstanceProfileARN :: !(Maybe Text)
    , _cDefaultRootDeviceType     :: !(Maybe RootDeviceType)
    , _cChefConfiguration         :: !(Maybe ChefConfiguration)
    , _cVPCId                     :: !(Maybe Text)
    , _cAgentVersion              :: !(Maybe Text)
    , _cDefaultSSHKeyName         :: !(Maybe Text)
    , _cCustomJSON                :: !(Maybe Text)
    , _cClonePermissions          :: !(Maybe Bool)
    , _cCustomCookbooksSource     :: !(Maybe Source)
    , _cDefaultAvailabilityZone   :: !(Maybe Text)
    , _cName                      :: !(Maybe Text)
    , _cUseOpsworksSecurityGroups :: !(Maybe Bool)
    , _cDefaultOS                 :: !(Maybe Text)
    , _cAttributes                :: !(Maybe (Map StackAttributesKeys Text))
    , _cUseCustomCookbooks        :: !(Maybe Bool)
    , _cDefaultSubnetId           :: !(Maybe Text)
    , _cRegion                    :: !(Maybe Text)
    , _cConfigurationManager      :: !(Maybe StackConfigurationManager)
    , _cHostnameTheme             :: !(Maybe Text)
    , _cSourceStackId             :: !Text
    , _cServiceRoleARN            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CloneStack' smart constructor.
cloneStack :: Text -> Text -> CloneStack
cloneStack pSourceStackId_ pServiceRoleARN_ =
    CloneStack'
    { _cCloneAppIds = Nothing
    , _cDefaultInstanceProfileARN = Nothing
    , _cDefaultRootDeviceType = Nothing
    , _cChefConfiguration = Nothing
    , _cVPCId = Nothing
    , _cAgentVersion = Nothing
    , _cDefaultSSHKeyName = Nothing
    , _cCustomJSON = Nothing
    , _cClonePermissions = Nothing
    , _cCustomCookbooksSource = Nothing
    , _cDefaultAvailabilityZone = Nothing
    , _cName = Nothing
    , _cUseOpsworksSecurityGroups = Nothing
    , _cDefaultOS = Nothing
    , _cAttributes = Nothing
    , _cUseCustomCookbooks = Nothing
    , _cDefaultSubnetId = Nothing
    , _cRegion = Nothing
    , _cConfigurationManager = Nothing
    , _cHostnameTheme = Nothing
    , _cSourceStackId = pSourceStackId_
    , _cServiceRoleARN = pServiceRoleARN_
    }

-- | A list of source stack app IDs to be included in the cloned stack.
cCloneAppIds :: Lens' CloneStack [Text]
cCloneAppIds = lens _cCloneAppIds (\ s a -> s{_cCloneAppIds = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default
-- profile for all of the stack\'s EC2 instances. For more information
-- about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
cDefaultInstanceProfileARN :: Lens' CloneStack (Maybe Text)
cDefaultInstanceProfileARN = lens _cDefaultInstanceProfileARN (\ s a -> s{_cDefaultInstanceProfileARN = a});

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create
-- an instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
cDefaultRootDeviceType :: Lens' CloneStack (Maybe RootDeviceType)
cDefaultRootDeviceType = lens _cDefaultRootDeviceType (\ s a -> s{_cDefaultRootDeviceType = a});

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
cChefConfiguration :: Lens' CloneStack (Maybe ChefConfiguration)
cChefConfiguration = lens _cChefConfiguration (\ s a -> s{_cChefConfiguration = a});

-- | The ID of the VPC that the cloned stack is to be launched into. It must
-- be in the specified region. All instances are launched into this VPC,
-- and you cannot change the ID later.
--
-- -   If your account supports EC2 Classic, the default value is no VPC.
-- -   If your account does not support EC2 Classic, the default value is
--     the default VPC for the specified region.
--
-- If the VPC ID corresponds to a default VPC and you have specified either
-- the @DefaultAvailabilityZone@ or the @DefaultSubnetId@ parameter only,
-- AWS OpsWorks infers the value of the other parameter. If you specify
-- neither parameter, AWS OpsWorks sets these parameters to the first valid
-- Availability Zone for the specified region and the corresponding default
-- VPC subnet ID, respectively.
--
-- If you specify a nondefault VPC ID, note the following:
--
-- -   It must belong to a VPC in your account that is in the specified
--     region.
-- -   You must specify a value for @DefaultSubnetId@.
--
-- For more information on how to use AWS OpsWorks with a VPC, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC>.
-- For more information on default VPC and EC2 Classic, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
cVPCId :: Lens' CloneStack (Maybe Text)
cVPCId = lens _cVPCId (\ s a -> s{_cVPCId = a});

-- | The default AWS OpsWorks agent version. You have the following options:
--
-- -   Auto-update - Set this parameter to @LATEST@. AWS OpsWorks
--     automatically installs new agent versions on the stack\'s instances
--     as soon as they are available.
-- -   Fixed version - Set this parameter to your preferred agent version.
--     To update the agent version, you must edit the stack configuration
--     and specify a new version. AWS OpsWorks then automatically installs
--     that version on the stack\'s instances.
--
-- The default setting is @LATEST@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions.
--
-- You can also specify an agent version when you create or update an
-- instance, which overrides the stack\'s default setting.
cAgentVersion :: Lens' CloneStack (Maybe Text)
cAgentVersion = lens _cAgentVersion (\ s a -> s{_cAgentVersion = a});

-- | A default Amazon EC2 key pair name. The default value is none. If you
-- specify a key pair name, AWS OpsWorks installs the public key on the
-- instance and you can use the private key with an SSH client to log in to
-- the instance. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
-- and
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
-- You can override this setting by specifying a different key pair, or no
-- key pair, when you
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
cDefaultSSHKeyName :: Lens' CloneStack (Maybe Text)
cDefaultSSHKeyName = lens _cDefaultSSHKeyName (\ s a -> s{_cDefaultSSHKeyName = a});

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as
-- \'\"\':
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
cCustomJSON :: Lens' CloneStack (Maybe Text)
cCustomJSON = lens _cCustomJSON (\ s a -> s{_cCustomJSON = a});

-- | Whether to clone the source stack\'s permissions.
cClonePermissions :: Lens' CloneStack (Maybe Bool)
cClonePermissions = lens _cClonePermissions (\ s a -> s{_cClonePermissions = a});

-- | FIXME: Undocumented member.
cCustomCookbooksSource :: Lens' CloneStack (Maybe Source)
cCustomCookbooksSource = lens _cCustomCookbooksSource (\ s a -> s{_cCustomCookbooksSource = a});

-- | The cloned stack\'s default Availability Zone, which must be in the
-- specified region. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
cDefaultAvailabilityZone :: Lens' CloneStack (Maybe Text)
cDefaultAvailabilityZone = lens _cDefaultAvailabilityZone (\ s a -> s{_cDefaultAvailabilityZone = a});

-- | The cloned stack name.
cName :: Lens' CloneStack (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a});

-- | Whether to associate the AWS OpsWorks built-in security groups with the
-- stack\'s layers.
--
-- AWS OpsWorks provides a standard set of built-in security groups, one
-- for each layer, which are associated with layers by default. With
-- @UseOpsworksSecurityGroups@ you can instead provide your own custom
-- security groups. @UseOpsworksSecurityGroups@ has the following settings:
--
-- -   True - AWS OpsWorks automatically associates the appropriate
--     built-in security group with each layer (default setting). You can
--     associate additional security groups with a layer after you create
--     it but you cannot delete the built-in security group.
-- -   False - AWS OpsWorks does not associate built-in security groups
--     with layers. You must create appropriate Amazon Elastic Compute
--     Cloud (Amazon EC2) security groups and associate a security group
--     with each layer that you create. However, you can still manually
--     associate a built-in security group with a layer on creation; custom
--     security groups are required only for those layers that need custom
--     settings.
--
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
cUseOpsworksSecurityGroups :: Lens' CloneStack (Maybe Bool)
cUseOpsworksSecurityGroups = lens _cUseOpsworksSecurityGroups (\ s a -> s{_cUseOpsworksSecurityGroups = a});

-- | The stack\'s operating system, which must be set to one of the
-- following.
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2015.03@, @Red Hat Enterprise Linux 7@,
--     @Ubuntu 12.04 LTS@, or @Ubuntu 14.04 LTS@.
-- -   @Microsoft Windows Server 2012 R2 Base@.
-- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
--     when you create instances. For more information on how to use custom
--     AMIs with OpsWorks, see
--     <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the parent stack\'s operating system. For more
-- information on the supported operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Operating Systems>.
--
-- You can specify a different Linux operating system for the cloned stack,
-- but you cannot change from Linux to Windows or Windows to Linux.
cDefaultOS :: Lens' CloneStack (Maybe Text)
cDefaultOS = lens _cDefaultOS (\ s a -> s{_cDefaultOS = a});

-- | A list of stack attributes and values as key\/value pairs to be added to
-- the cloned stack.
cAttributes :: Lens' CloneStack (HashMap StackAttributesKeys Text)
cAttributes = lens _cAttributes (\ s a -> s{_cAttributes = a}) . _Default . _Map;

-- | Whether to use custom cookbooks.
cUseCustomCookbooks :: Lens' CloneStack (Maybe Bool)
cUseCustomCookbooks = lens _cUseCustomCookbooks (\ s a -> s{_cUseCustomCookbooks = a});

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
cDefaultSubnetId :: Lens' CloneStack (Maybe Text)
cDefaultSubnetId = lens _cDefaultSubnetId (\ s a -> s{_cDefaultSubnetId = a});

-- | The cloned stack AWS region, such as \"us-east-1\". For more information
-- about AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
cRegion :: Lens' CloneStack (Maybe Text)
cRegion = lens _cRegion (\ s a -> s{_cRegion = a});

-- | The configuration manager. When you clone a Linux stack we recommend
-- that you use the configuration manager to specify the Chef version: 0.9,
-- 11.4, or 11.10. The default value is currently 11.10.
cConfigurationManager :: Lens' CloneStack (Maybe StackConfigurationManager)
cConfigurationManager = lens _cConfigurationManager (\ s a -> s{_cConfigurationManager = a});

-- | The stack\'s host name theme, with spaces are replaced by underscores.
-- The theme is used to generate host names for the stack\'s instances. By
-- default, @HostnameTheme@ is set to @Layer_Dependent@, which creates host
-- names by appending integers to the layer\'s short name. The other themes
-- are:
--
-- -   @Baked_Goods@
-- -   @Clouds@
-- -   @Europe_Cities@
-- -   @Fruits@
-- -   @Greek_Deities@
-- -   @Legendary_creatures_from_Japan@
-- -   @Planets_and_Moons@
-- -   @Roman_Deities@
-- -   @Scottish_Islands@
-- -   @US_Cities@
-- -   @Wild_Cats@
--
-- To obtain a generated host name, call @GetHostNameSuggestion@, which
-- returns a host name based on the current theme.
cHostnameTheme :: Lens' CloneStack (Maybe Text)
cHostnameTheme = lens _cHostnameTheme (\ s a -> s{_cHostnameTheme = a});

-- | The source stack ID.
cSourceStackId :: Lens' CloneStack Text
cSourceStackId = lens _cSourceStackId (\ s a -> s{_cSourceStackId = a});

-- | The stack AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks to work with AWS resources on your behalf. You must set
-- this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. If you create a stack by using the AWS OpsWorks console, it
-- creates the role for you. You can obtain an existing stack\'s IAM ARN
-- programmatically by calling DescribePermissions. For more information
-- about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- You must set this parameter to a valid service role ARN or the action
-- will fail; there is no default value. You can specify the source
-- stack\'s service role ARN, if you prefer, but you must do so explicitly.
cServiceRoleARN :: Lens' CloneStack Text
cServiceRoleARN = lens _cServiceRoleARN (\ s a -> s{_cServiceRoleARN = a});

instance AWSRequest CloneStack where
        type Sv CloneStack = OpsWorks
        type Rs CloneStack = CloneStackResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CloneStackResponse' <$>
                   (x .?> "StackId") <*> (pure (fromEnum s)))

instance ToHeaders CloneStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CloneStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CloneStack where
        toJSON CloneStack'{..}
          = object
              ["CloneAppIds" .= _cCloneAppIds,
               "DefaultInstanceProfileArn" .=
                 _cDefaultInstanceProfileARN,
               "DefaultRootDeviceType" .= _cDefaultRootDeviceType,
               "ChefConfiguration" .= _cChefConfiguration,
               "VpcId" .= _cVPCId, "AgentVersion" .= _cAgentVersion,
               "DefaultSshKeyName" .= _cDefaultSSHKeyName,
               "CustomJson" .= _cCustomJSON,
               "ClonePermissions" .= _cClonePermissions,
               "CustomCookbooksSource" .= _cCustomCookbooksSource,
               "DefaultAvailabilityZone" .=
                 _cDefaultAvailabilityZone,
               "Name" .= _cName,
               "UseOpsworksSecurityGroups" .=
                 _cUseOpsworksSecurityGroups,
               "DefaultOs" .= _cDefaultOS,
               "Attributes" .= _cAttributes,
               "UseCustomCookbooks" .= _cUseCustomCookbooks,
               "DefaultSubnetId" .= _cDefaultSubnetId,
               "Region" .= _cRegion,
               "ConfigurationManager" .= _cConfigurationManager,
               "HostnameTheme" .= _cHostnameTheme,
               "SourceStackId" .= _cSourceStackId,
               "ServiceRoleArn" .= _cServiceRoleARN]

instance ToPath CloneStack where
        toPath = const "/"

instance ToQuery CloneStack where
        toQuery = const mempty

-- | Contains the response to a @CloneStack@ request.
--
-- /See:/ 'cloneStackResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsStackId'
--
-- * 'csrsStatus'
data CloneStackResponse = CloneStackResponse'
    { _csrsStackId :: !(Maybe Text)
    , _csrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CloneStackResponse' smart constructor.
cloneStackResponse :: Int -> CloneStackResponse
cloneStackResponse pStatus_ =
    CloneStackResponse'
    { _csrsStackId = Nothing
    , _csrsStatus = pStatus_
    }

-- | The cloned stack ID.
csrsStackId :: Lens' CloneStackResponse (Maybe Text)
csrsStackId = lens _csrsStackId (\ s a -> s{_csrsStackId = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CloneStackResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
