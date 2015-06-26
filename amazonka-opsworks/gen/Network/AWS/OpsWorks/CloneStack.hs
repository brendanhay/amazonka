{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.CloneStack
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

-- | Creates a clone of a specified stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-cloning.html Clone a Stack>.
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
    , cloCloneAppIds
    , cloDefaultInstanceProfileARN
    , cloDefaultRootDeviceType
    , cloChefConfiguration
    , cloVPCId
    , cloDefaultSSHKeyName
    , cloCustomJSON
    , cloClonePermissions
    , cloCustomCookbooksSource
    , cloDefaultAvailabilityZone
    , cloName
    , cloUseOpsworksSecurityGroups
    , cloDefaultOS
    , cloAttributes
    , cloUseCustomCookbooks
    , cloDefaultSubnetId
    , cloRegion
    , cloConfigurationManager
    , cloHostnameTheme
    , cloSourceStackId
    , cloServiceRoleARN

    -- * Response
    , CloneStackResponse
    -- ** Response constructor
    , cloneStackResponse
    -- ** Response lenses
    , csrStackId
    , csrStatusCode
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cloneStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cloCloneAppIds'
--
-- * 'cloDefaultInstanceProfileARN'
--
-- * 'cloDefaultRootDeviceType'
--
-- * 'cloChefConfiguration'
--
-- * 'cloVPCId'
--
-- * 'cloDefaultSSHKeyName'
--
-- * 'cloCustomJSON'
--
-- * 'cloClonePermissions'
--
-- * 'cloCustomCookbooksSource'
--
-- * 'cloDefaultAvailabilityZone'
--
-- * 'cloName'
--
-- * 'cloUseOpsworksSecurityGroups'
--
-- * 'cloDefaultOS'
--
-- * 'cloAttributes'
--
-- * 'cloUseCustomCookbooks'
--
-- * 'cloDefaultSubnetId'
--
-- * 'cloRegion'
--
-- * 'cloConfigurationManager'
--
-- * 'cloHostnameTheme'
--
-- * 'cloSourceStackId'
--
-- * 'cloServiceRoleARN'
data CloneStack = CloneStack'{_cloCloneAppIds :: Maybe [Text], _cloDefaultInstanceProfileARN :: Maybe Text, _cloDefaultRootDeviceType :: Maybe RootDeviceType, _cloChefConfiguration :: Maybe ChefConfiguration, _cloVPCId :: Maybe Text, _cloDefaultSSHKeyName :: Maybe Text, _cloCustomJSON :: Maybe Text, _cloClonePermissions :: Maybe Bool, _cloCustomCookbooksSource :: Maybe Source, _cloDefaultAvailabilityZone :: Maybe Text, _cloName :: Maybe Text, _cloUseOpsworksSecurityGroups :: Maybe Bool, _cloDefaultOS :: Maybe Text, _cloAttributes :: Maybe (Map StackAttributesKeys Text), _cloUseCustomCookbooks :: Maybe Bool, _cloDefaultSubnetId :: Maybe Text, _cloRegion :: Maybe Text, _cloConfigurationManager :: Maybe StackConfigurationManager, _cloHostnameTheme :: Maybe Text, _cloSourceStackId :: Text, _cloServiceRoleARN :: Text} deriving (Eq, Read, Show)

-- | 'CloneStack' smart constructor.
cloneStack :: Text -> Text -> CloneStack
cloneStack pSourceStackId pServiceRoleARN = CloneStack'{_cloCloneAppIds = Nothing, _cloDefaultInstanceProfileARN = Nothing, _cloDefaultRootDeviceType = Nothing, _cloChefConfiguration = Nothing, _cloVPCId = Nothing, _cloDefaultSSHKeyName = Nothing, _cloCustomJSON = Nothing, _cloClonePermissions = Nothing, _cloCustomCookbooksSource = Nothing, _cloDefaultAvailabilityZone = Nothing, _cloName = Nothing, _cloUseOpsworksSecurityGroups = Nothing, _cloDefaultOS = Nothing, _cloAttributes = Nothing, _cloUseCustomCookbooks = Nothing, _cloDefaultSubnetId = Nothing, _cloRegion = Nothing, _cloConfigurationManager = Nothing, _cloHostnameTheme = Nothing, _cloSourceStackId = pSourceStackId, _cloServiceRoleARN = pServiceRoleARN};

-- | A list of source stack app IDs to be included in the cloned stack.
cloCloneAppIds :: Lens' CloneStack [Text]
cloCloneAppIds = lens _cloCloneAppIds (\ s a -> s{_cloCloneAppIds = a}) . _Default;

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
cloDefaultInstanceProfileARN :: Lens' CloneStack (Maybe Text)
cloDefaultInstanceProfileARN = lens _cloDefaultInstanceProfileARN (\ s a -> s{_cloDefaultInstanceProfileARN = a});

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create
-- an instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
cloDefaultRootDeviceType :: Lens' CloneStack (Maybe RootDeviceType)
cloDefaultRootDeviceType = lens _cloDefaultRootDeviceType (\ s a -> s{_cloDefaultRootDeviceType = a});

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
cloChefConfiguration :: Lens' CloneStack (Maybe ChefConfiguration)
cloChefConfiguration = lens _cloChefConfiguration (\ s a -> s{_cloChefConfiguration = a});

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
cloVPCId :: Lens' CloneStack (Maybe Text)
cloVPCId = lens _cloVPCId (\ s a -> s{_cloVPCId = a});

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
cloDefaultSSHKeyName :: Lens' CloneStack (Maybe Text)
cloDefaultSSHKeyName = lens _cloDefaultSSHKeyName (\ s a -> s{_cloDefaultSSHKeyName = a});

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as
-- \'\"\'.:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
cloCustomJSON :: Lens' CloneStack (Maybe Text)
cloCustomJSON = lens _cloCustomJSON (\ s a -> s{_cloCustomJSON = a});

-- | Whether to clone the source stack\'s permissions.
cloClonePermissions :: Lens' CloneStack (Maybe Bool)
cloClonePermissions = lens _cloClonePermissions (\ s a -> s{_cloClonePermissions = a});

-- | FIXME: Undocumented member.
cloCustomCookbooksSource :: Lens' CloneStack (Maybe Source)
cloCustomCookbooksSource = lens _cloCustomCookbooksSource (\ s a -> s{_cloCustomCookbooksSource = a});

-- | The cloned stack\'s default Availability Zone, which must be in the
-- specified region. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
cloDefaultAvailabilityZone :: Lens' CloneStack (Maybe Text)
cloDefaultAvailabilityZone = lens _cloDefaultAvailabilityZone (\ s a -> s{_cloDefaultAvailabilityZone = a});

-- | The cloned stack name.
cloName :: Lens' CloneStack (Maybe Text)
cloName = lens _cloName (\ s a -> s{_cloName = a});

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
--     with layers. You must create appropriate EC2 security groups and
--     associate a security group with each layer that you create. However,
--     you can still manually associate a built-in security group with a
--     layer on creation; custom security groups are required only for
--     those layers that need custom settings.
--
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
cloUseOpsworksSecurityGroups :: Lens' CloneStack (Maybe Bool)
cloUseOpsworksSecurityGroups = lens _cloUseOpsworksSecurityGroups (\ s a -> s{_cloUseOpsworksSecurityGroups = a});

-- | The stack\'s operating system, which must be set to one of the
-- following.
--
-- -   Standard Linux operating systems: an Amazon Linux version such as
--     @Amazon Linux 2014.09@, @Ubuntu 12.04 LTS@, or @Ubuntu 14.04 LTS@.
-- -   Custom Linux AMIs: @Custom@. You specify the custom AMI you want to
--     use when you create instances.
-- -   Microsoft Windows Server 2012 R2.
--
-- The default option is the current Amazon Linux version.
cloDefaultOS :: Lens' CloneStack (Maybe Text)
cloDefaultOS = lens _cloDefaultOS (\ s a -> s{_cloDefaultOS = a});

-- | A list of stack attributes and values as key\/value pairs to be added to
-- the cloned stack.
cloAttributes :: Lens' CloneStack (HashMap StackAttributesKeys Text)
cloAttributes = lens _cloAttributes (\ s a -> s{_cloAttributes = a}) . _Default . _Map;

-- | Whether to use custom cookbooks.
cloUseCustomCookbooks :: Lens' CloneStack (Maybe Bool)
cloUseCustomCookbooks = lens _cloUseCustomCookbooks (\ s a -> s{_cloUseCustomCookbooks = a});

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
cloDefaultSubnetId :: Lens' CloneStack (Maybe Text)
cloDefaultSubnetId = lens _cloDefaultSubnetId (\ s a -> s{_cloDefaultSubnetId = a});

-- | The cloned stack AWS region, such as \"us-east-1\". For more information
-- about AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
cloRegion :: Lens' CloneStack (Maybe Text)
cloRegion = lens _cloRegion (\ s a -> s{_cloRegion = a});

-- | The configuration manager. When you clone a stack we recommend that you
-- use the configuration manager to specify the Chef version, 0.9, 11.4, or
-- 11.10. The default value is currently 11.4.
cloConfigurationManager :: Lens' CloneStack (Maybe StackConfigurationManager)
cloConfigurationManager = lens _cloConfigurationManager (\ s a -> s{_cloConfigurationManager = a});

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
cloHostnameTheme :: Lens' CloneStack (Maybe Text)
cloHostnameTheme = lens _cloHostnameTheme (\ s a -> s{_cloHostnameTheme = a});

-- | The source stack ID.
cloSourceStackId :: Lens' CloneStack Text
cloSourceStackId = lens _cloSourceStackId (\ s a -> s{_cloSourceStackId = a});

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
cloServiceRoleARN :: Lens' CloneStack Text
cloServiceRoleARN = lens _cloServiceRoleARN (\ s a -> s{_cloServiceRoleARN = a});

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
              ["CloneAppIds" .= _cloCloneAppIds,
               "DefaultInstanceProfileArn" .=
                 _cloDefaultInstanceProfileARN,
               "DefaultRootDeviceType" .= _cloDefaultRootDeviceType,
               "ChefConfiguration" .= _cloChefConfiguration,
               "VpcId" .= _cloVPCId,
               "DefaultSshKeyName" .= _cloDefaultSSHKeyName,
               "CustomJson" .= _cloCustomJSON,
               "ClonePermissions" .= _cloClonePermissions,
               "CustomCookbooksSource" .= _cloCustomCookbooksSource,
               "DefaultAvailabilityZone" .=
                 _cloDefaultAvailabilityZone,
               "Name" .= _cloName,
               "UseOpsworksSecurityGroups" .=
                 _cloUseOpsworksSecurityGroups,
               "DefaultOs" .= _cloDefaultOS,
               "Attributes" .= _cloAttributes,
               "UseCustomCookbooks" .= _cloUseCustomCookbooks,
               "DefaultSubnetId" .= _cloDefaultSubnetId,
               "Region" .= _cloRegion,
               "ConfigurationManager" .= _cloConfigurationManager,
               "HostnameTheme" .= _cloHostnameTheme,
               "SourceStackId" .= _cloSourceStackId,
               "ServiceRoleArn" .= _cloServiceRoleARN]

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
-- * 'csrStackId'
--
-- * 'csrStatusCode'
data CloneStackResponse = CloneStackResponse'{_csrStackId :: Maybe Text, _csrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CloneStackResponse' smart constructor.
cloneStackResponse :: Int -> CloneStackResponse
cloneStackResponse pStatusCode = CloneStackResponse'{_csrStackId = Nothing, _csrStatusCode = pStatusCode};

-- | The cloned stack ID.
csrStackId :: Lens' CloneStackResponse (Maybe Text)
csrStackId = lens _csrStackId (\ s a -> s{_csrStackId = a});

-- | FIXME: Undocumented member.
csrStatusCode :: Lens' CloneStackResponse Int
csrStatusCode = lens _csrStatusCode (\ s a -> s{_csrStatusCode = a});
