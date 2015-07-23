{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-edit.html Create a New Stack>.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateStack.html>
module Network.AWS.OpsWorks.CreateStack
    (
    -- * Request
      CreateStack
    -- ** Request constructor
    , createStack
    -- ** Request lenses
    , csrqDefaultRootDeviceType
    , csrqChefConfiguration
    , csrqVPCId
    , csrqAgentVersion
    , csrqDefaultSSHKeyName
    , csrqCustomJSON
    , csrqCustomCookbooksSource
    , csrqDefaultAvailabilityZone
    , csrqUseOpsworksSecurityGroups
    , csrqDefaultOS
    , csrqAttributes
    , csrqUseCustomCookbooks
    , csrqDefaultSubnetId
    , csrqConfigurationManager
    , csrqHostnameTheme
    , csrqName
    , csrqRegion
    , csrqServiceRoleARN
    , csrqDefaultInstanceProfileARN

    -- * Response
    , CreateStackResponse
    -- ** Response constructor
    , createStackResponse
    -- ** Response lenses
    , crsStackId
    , crsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrqDefaultRootDeviceType'
--
-- * 'csrqChefConfiguration'
--
-- * 'csrqVPCId'
--
-- * 'csrqAgentVersion'
--
-- * 'csrqDefaultSSHKeyName'
--
-- * 'csrqCustomJSON'
--
-- * 'csrqCustomCookbooksSource'
--
-- * 'csrqDefaultAvailabilityZone'
--
-- * 'csrqUseOpsworksSecurityGroups'
--
-- * 'csrqDefaultOS'
--
-- * 'csrqAttributes'
--
-- * 'csrqUseCustomCookbooks'
--
-- * 'csrqDefaultSubnetId'
--
-- * 'csrqConfigurationManager'
--
-- * 'csrqHostnameTheme'
--
-- * 'csrqName'
--
-- * 'csrqRegion'
--
-- * 'csrqServiceRoleARN'
--
-- * 'csrqDefaultInstanceProfileARN'
data CreateStack = CreateStack'
    { _csrqDefaultRootDeviceType     :: !(Maybe RootDeviceType)
    , _csrqChefConfiguration         :: !(Maybe ChefConfiguration)
    , _csrqVPCId                     :: !(Maybe Text)
    , _csrqAgentVersion              :: !(Maybe Text)
    , _csrqDefaultSSHKeyName         :: !(Maybe Text)
    , _csrqCustomJSON                :: !(Maybe Text)
    , _csrqCustomCookbooksSource     :: !(Maybe Source)
    , _csrqDefaultAvailabilityZone   :: !(Maybe Text)
    , _csrqUseOpsworksSecurityGroups :: !(Maybe Bool)
    , _csrqDefaultOS                 :: !(Maybe Text)
    , _csrqAttributes                :: !(Maybe (Map StackAttributesKeys Text))
    , _csrqUseCustomCookbooks        :: !(Maybe Bool)
    , _csrqDefaultSubnetId           :: !(Maybe Text)
    , _csrqConfigurationManager      :: !(Maybe StackConfigurationManager)
    , _csrqHostnameTheme             :: !(Maybe Text)
    , _csrqName                      :: !Text
    , _csrqRegion                    :: !Text
    , _csrqServiceRoleARN            :: !Text
    , _csrqDefaultInstanceProfileARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStack' smart constructor.
createStack :: Text -> Text -> Text -> Text -> CreateStack
createStack pName_ pRegion_ pServiceRoleARN_ pDefaultInstanceProfileARN_ =
    CreateStack'
    { _csrqDefaultRootDeviceType = Nothing
    , _csrqChefConfiguration = Nothing
    , _csrqVPCId = Nothing
    , _csrqAgentVersion = Nothing
    , _csrqDefaultSSHKeyName = Nothing
    , _csrqCustomJSON = Nothing
    , _csrqCustomCookbooksSource = Nothing
    , _csrqDefaultAvailabilityZone = Nothing
    , _csrqUseOpsworksSecurityGroups = Nothing
    , _csrqDefaultOS = Nothing
    , _csrqAttributes = Nothing
    , _csrqUseCustomCookbooks = Nothing
    , _csrqDefaultSubnetId = Nothing
    , _csrqConfigurationManager = Nothing
    , _csrqHostnameTheme = Nothing
    , _csrqName = pName_
    , _csrqRegion = pRegion_
    , _csrqServiceRoleARN = pServiceRoleARN_
    , _csrqDefaultInstanceProfileARN = pDefaultInstanceProfileARN_
    }

-- | The default root device type. This value is the default for all
-- instances in the stack, but you can override it when you create an
-- instance. The default option is @instance-store@. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
csrqDefaultRootDeviceType :: Lens' CreateStack (Maybe RootDeviceType)
csrqDefaultRootDeviceType = lens _csrqDefaultRootDeviceType (\ s a -> s{_csrqDefaultRootDeviceType = a});

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
csrqChefConfiguration :: Lens' CreateStack (Maybe ChefConfiguration)
csrqChefConfiguration = lens _csrqChefConfiguration (\ s a -> s{_csrqChefConfiguration = a});

-- | The ID of the VPC that the stack is to be launched into. The VPC must be
-- in the stack\'s region. All instances are launched into this VPC. You
-- cannot change the ID later.
--
-- -   If your account supports EC2-Classic, the default value is @no VPC@.
-- -   If your account does not support EC2-Classic, the default value is
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
-- For more information on default VPC and EC2-Classic, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
csrqVPCId :: Lens' CreateStack (Maybe Text)
csrqVPCId = lens _csrqVPCId (\ s a -> s{_csrqVPCId = a});

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
csrqAgentVersion :: Lens' CreateStack (Maybe Text)
csrqAgentVersion = lens _csrqAgentVersion (\ s a -> s{_csrqAgentVersion = a});

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
csrqDefaultSSHKeyName :: Lens' CreateStack (Maybe Text)
csrqDefaultSSHKeyName = lens _csrqDefaultSSHKeyName (\ s a -> s{_csrqDefaultSSHKeyName = a});

-- | A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- or to pass data to recipes. The string should be in the following escape
-- characters such as \'\"\':
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
csrqCustomJSON :: Lens' CreateStack (Maybe Text)
csrqCustomJSON = lens _csrqCustomJSON (\ s a -> s{_csrqCustomJSON = a});

-- | FIXME: Undocumented member.
csrqCustomCookbooksSource :: Lens' CreateStack (Maybe Source)
csrqCustomCookbooksSource = lens _csrqCustomCookbooksSource (\ s a -> s{_csrqCustomCookbooksSource = a});

-- | The stack\'s default Availability Zone, which must be in the specified
-- region. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
csrqDefaultAvailabilityZone :: Lens' CreateStack (Maybe Text)
csrqDefaultAvailabilityZone = lens _csrqDefaultAvailabilityZone (\ s a -> s{_csrqDefaultAvailabilityZone = a});

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
--     it, but you cannot delete the built-in security group.
-- -   False - AWS OpsWorks does not associate built-in security groups
--     with layers. You must create appropriate EC2 security groups and
--     associate a security group with each layer that you create. However,
--     you can still manually associate a built-in security group with a
--     layer on creation; custom security groups are required only for
--     those layers that need custom settings.
--
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
csrqUseOpsworksSecurityGroups :: Lens' CreateStack (Maybe Bool)
csrqUseOpsworksSecurityGroups = lens _csrqUseOpsworksSecurityGroups (\ s a -> s{_csrqUseOpsworksSecurityGroups = a});

-- | The stack\'s default operating system, which is installed on every
-- instance unless you specify a different operating system when you create
-- the instance. You can specify one of the following.
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2015.03@, @Ubuntu 12.04 LTS@, or @Ubuntu 14.04 LTS@.
-- -   @Microsoft Windows Server 2012 R2 Base@.
-- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
--     when you create instances. For more information, see
--     <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the current Amazon Linux version. For more
-- information on the supported operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Operating Systems>.
csrqDefaultOS :: Lens' CreateStack (Maybe Text)
csrqDefaultOS = lens _csrqDefaultOS (\ s a -> s{_csrqDefaultOS = a});

-- | One or more user-defined key-value pairs to be added to the stack
-- attributes.
csrqAttributes :: Lens' CreateStack (HashMap StackAttributesKeys Text)
csrqAttributes = lens _csrqAttributes (\ s a -> s{_csrqAttributes = a}) . _Default . _Map;

-- | Whether the stack uses custom cookbooks.
csrqUseCustomCookbooks :: Lens' CreateStack (Maybe Bool)
csrqUseCustomCookbooks = lens _csrqUseCustomCookbooks (\ s a -> s{_csrqUseCustomCookbooks = a});

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
csrqDefaultSubnetId :: Lens' CreateStack (Maybe Text)
csrqDefaultSubnetId = lens _csrqDefaultSubnetId (\ s a -> s{_csrqDefaultSubnetId = a});

-- | The configuration manager. When you clone a stack we recommend that you
-- use the configuration manager to specify the Chef version: 0.9, 11.4, or
-- 11.10. The default value is currently 11.4.
csrqConfigurationManager :: Lens' CreateStack (Maybe StackConfigurationManager)
csrqConfigurationManager = lens _csrqConfigurationManager (\ s a -> s{_csrqConfigurationManager = a});

-- | The stack\'s host name theme, with spaces replaced by underscores. The
-- theme is used to generate host names for the stack\'s instances. By
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
csrqHostnameTheme :: Lens' CreateStack (Maybe Text)
csrqHostnameTheme = lens _csrqHostnameTheme (\ s a -> s{_csrqHostnameTheme = a});

-- | The stack name.
csrqName :: Lens' CreateStack Text
csrqName = lens _csrqName (\ s a -> s{_csrqName = a});

-- | The stack\'s AWS region, such as \"us-east-1\". For more information
-- about Amazon regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
csrqRegion :: Lens' CreateStack Text
csrqRegion = lens _csrqRegion (\ s a -> s{_csrqRegion = a});

-- | The stack\'s AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks to work with AWS resources on your behalf. You must set
-- this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
csrqServiceRoleARN :: Lens' CreateStack Text
csrqServiceRoleARN = lens _csrqServiceRoleARN (\ s a -> s{_csrqServiceRoleARN = a});

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default
-- profile for all of the stack\'s EC2 instances. For more information
-- about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
csrqDefaultInstanceProfileARN :: Lens' CreateStack Text
csrqDefaultInstanceProfileARN = lens _csrqDefaultInstanceProfileARN (\ s a -> s{_csrqDefaultInstanceProfileARN = a});

instance AWSRequest CreateStack where
        type Sv CreateStack = OpsWorks
        type Rs CreateStack = CreateStackResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateStackResponse' <$>
                   (x .?> "StackId") <*> (pure (fromEnum s)))

instance ToHeaders CreateStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStack where
        toJSON CreateStack'{..}
          = object
              ["DefaultRootDeviceType" .=
                 _csrqDefaultRootDeviceType,
               "ChefConfiguration" .= _csrqChefConfiguration,
               "VpcId" .= _csrqVPCId,
               "AgentVersion" .= _csrqAgentVersion,
               "DefaultSshKeyName" .= _csrqDefaultSSHKeyName,
               "CustomJson" .= _csrqCustomJSON,
               "CustomCookbooksSource" .=
                 _csrqCustomCookbooksSource,
               "DefaultAvailabilityZone" .=
                 _csrqDefaultAvailabilityZone,
               "UseOpsworksSecurityGroups" .=
                 _csrqUseOpsworksSecurityGroups,
               "DefaultOs" .= _csrqDefaultOS,
               "Attributes" .= _csrqAttributes,
               "UseCustomCookbooks" .= _csrqUseCustomCookbooks,
               "DefaultSubnetId" .= _csrqDefaultSubnetId,
               "ConfigurationManager" .= _csrqConfigurationManager,
               "HostnameTheme" .= _csrqHostnameTheme,
               "Name" .= _csrqName, "Region" .= _csrqRegion,
               "ServiceRoleArn" .= _csrqServiceRoleARN,
               "DefaultInstanceProfileArn" .=
                 _csrqDefaultInstanceProfileARN]

instance ToPath CreateStack where
        toPath = const "/"

instance ToQuery CreateStack where
        toQuery = const mempty

-- | Contains the response to a @CreateStack@ request.
--
-- /See:/ 'createStackResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsStackId'
--
-- * 'crsStatus'
data CreateStackResponse = CreateStackResponse'
    { _crsStackId :: !(Maybe Text)
    , _crsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStackResponse' smart constructor.
createStackResponse :: Int -> CreateStackResponse
createStackResponse pStatus_ =
    CreateStackResponse'
    { _crsStackId = Nothing
    , _crsStatus = pStatus_
    }

-- | The stack ID, which is an opaque string that you use to identify the
-- stack when performing actions such as @DescribeStacks@.
crsStackId :: Lens' CreateStackResponse (Maybe Text)
crsStackId = lens _crsStackId (\ s a -> s{_crsStackId = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CreateStackResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
