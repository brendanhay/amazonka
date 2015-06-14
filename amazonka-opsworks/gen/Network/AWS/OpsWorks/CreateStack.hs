{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.CreateStack
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

-- | Creates a new stack. For more information, see
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
    , csDefaultRootDeviceType
    , csChefConfiguration
    , csVPCId
    , csDefaultSSHKeyName
    , csCustomJSON
    , csCustomCookbooksSource
    , csDefaultAvailabilityZone
    , csUseOpsworksSecurityGroups
    , csDefaultOS
    , csAttributes
    , csUseCustomCookbooks
    , csDefaultSubnetId
    , csConfigurationManager
    , csHostnameTheme
    , csName
    , csRegion
    , csServiceRoleARN
    , csDefaultInstanceProfileARN

    -- * Response
    , CreateStackResponse
    -- ** Response constructor
    , createStackResponse
    -- ** Response lenses
    , creStackId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'createStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csDefaultRootDeviceType'
--
-- * 'csChefConfiguration'
--
-- * 'csVPCId'
--
-- * 'csDefaultSSHKeyName'
--
-- * 'csCustomJSON'
--
-- * 'csCustomCookbooksSource'
--
-- * 'csDefaultAvailabilityZone'
--
-- * 'csUseOpsworksSecurityGroups'
--
-- * 'csDefaultOS'
--
-- * 'csAttributes'
--
-- * 'csUseCustomCookbooks'
--
-- * 'csDefaultSubnetId'
--
-- * 'csConfigurationManager'
--
-- * 'csHostnameTheme'
--
-- * 'csName'
--
-- * 'csRegion'
--
-- * 'csServiceRoleARN'
--
-- * 'csDefaultInstanceProfileARN'
data CreateStack = CreateStack'{_csDefaultRootDeviceType :: Maybe RootDeviceType, _csChefConfiguration :: Maybe ChefConfiguration, _csVPCId :: Maybe Text, _csDefaultSSHKeyName :: Maybe Text, _csCustomJSON :: Maybe Text, _csCustomCookbooksSource :: Maybe Source, _csDefaultAvailabilityZone :: Maybe Text, _csUseOpsworksSecurityGroups :: Maybe Bool, _csDefaultOS :: Maybe Text, _csAttributes :: HashMap StackAttributesKeys Text, _csUseCustomCookbooks :: Maybe Bool, _csDefaultSubnetId :: Maybe Text, _csConfigurationManager :: Maybe StackConfigurationManager, _csHostnameTheme :: Maybe Text, _csName :: Text, _csRegion :: Text, _csServiceRoleARN :: Text, _csDefaultInstanceProfileARN :: Text} deriving (Eq, Read, Show)

-- | 'CreateStack' smart constructor.
createStack :: Text -> Text -> Text -> Text -> CreateStack
createStack pName pRegion pServiceRoleARN pDefaultInstanceProfileARN = CreateStack'{_csDefaultRootDeviceType = Nothing, _csChefConfiguration = Nothing, _csVPCId = Nothing, _csDefaultSSHKeyName = Nothing, _csCustomJSON = Nothing, _csCustomCookbooksSource = Nothing, _csDefaultAvailabilityZone = Nothing, _csUseOpsworksSecurityGroups = Nothing, _csDefaultOS = Nothing, _csAttributes = mempty, _csUseCustomCookbooks = Nothing, _csDefaultSubnetId = Nothing, _csConfigurationManager = Nothing, _csHostnameTheme = Nothing, _csName = pName, _csRegion = pRegion, _csServiceRoleARN = pServiceRoleARN, _csDefaultInstanceProfileARN = pDefaultInstanceProfileARN};

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. The default option is @instance-store@. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
csDefaultRootDeviceType :: Lens' CreateStack (Maybe RootDeviceType)
csDefaultRootDeviceType = lens _csDefaultRootDeviceType (\ s a -> s{_csDefaultRootDeviceType = a});

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
csChefConfiguration :: Lens' CreateStack (Maybe ChefConfiguration)
csChefConfiguration = lens _csChefConfiguration (\ s a -> s{_csChefConfiguration = a});

-- | The ID of the VPC that the stack is to be launched into. It must be in
-- the specified region. All instances are launched into this VPC, and you
-- cannot change the ID later.
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
csVPCId :: Lens' CreateStack (Maybe Text)
csVPCId = lens _csVPCId (\ s a -> s{_csVPCId = a});

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
csDefaultSSHKeyName :: Lens' CreateStack (Maybe Text)
csDefaultSSHKeyName = lens _csDefaultSSHKeyName (\ s a -> s{_csDefaultSSHKeyName = a});

-- | A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values,
-- or to pass data to recipes. The string should be in the following format
-- and must escape characters such as \'\"\'.:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
csCustomJSON :: Lens' CreateStack (Maybe Text)
csCustomJSON = lens _csCustomJSON (\ s a -> s{_csCustomJSON = a});

-- | FIXME: Undocumented member.
csCustomCookbooksSource :: Lens' CreateStack (Maybe Source)
csCustomCookbooksSource = lens _csCustomCookbooksSource (\ s a -> s{_csCustomCookbooksSource = a});

-- | The stack\'s default Availability Zone, which must be in the specified
-- region. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
csDefaultAvailabilityZone :: Lens' CreateStack (Maybe Text)
csDefaultAvailabilityZone = lens _csDefaultAvailabilityZone (\ s a -> s{_csDefaultAvailabilityZone = a});

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
csUseOpsworksSecurityGroups :: Lens' CreateStack (Maybe Bool)
csUseOpsworksSecurityGroups = lens _csUseOpsworksSecurityGroups (\ s a -> s{_csUseOpsworksSecurityGroups = a});

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
csDefaultOS :: Lens' CreateStack (Maybe Text)
csDefaultOS = lens _csDefaultOS (\ s a -> s{_csDefaultOS = a});

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
csAttributes :: Lens' CreateStack (HashMap StackAttributesKeys Text)
csAttributes = lens _csAttributes (\ s a -> s{_csAttributes = a}) . _Coerce;

-- | Whether the stack uses custom cookbooks.
csUseCustomCookbooks :: Lens' CreateStack (Maybe Bool)
csUseCustomCookbooks = lens _csUseCustomCookbooks (\ s a -> s{_csUseCustomCookbooks = a});

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
csDefaultSubnetId :: Lens' CreateStack (Maybe Text)
csDefaultSubnetId = lens _csDefaultSubnetId (\ s a -> s{_csDefaultSubnetId = a});

-- | The configuration manager. When you clone a stack we recommend that you
-- use the configuration manager to specify the Chef version, 0.9, 11.4, or
-- 11.10. The default value is currently 11.4.
csConfigurationManager :: Lens' CreateStack (Maybe StackConfigurationManager)
csConfigurationManager = lens _csConfigurationManager (\ s a -> s{_csConfigurationManager = a});

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
csHostnameTheme :: Lens' CreateStack (Maybe Text)
csHostnameTheme = lens _csHostnameTheme (\ s a -> s{_csHostnameTheme = a});

-- | The stack name.
csName :: Lens' CreateStack Text
csName = lens _csName (\ s a -> s{_csName = a});

-- | The stack AWS region, such as \"us-east-1\". For more information about
-- Amazon regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
csRegion :: Lens' CreateStack Text
csRegion = lens _csRegion (\ s a -> s{_csRegion = a});

-- | The stack AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks to work with AWS resources on your behalf. You must set
-- this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
csServiceRoleARN :: Lens' CreateStack Text
csServiceRoleARN = lens _csServiceRoleARN (\ s a -> s{_csServiceRoleARN = a});

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
csDefaultInstanceProfileARN :: Lens' CreateStack Text
csDefaultInstanceProfileARN = lens _csDefaultInstanceProfileARN (\ s a -> s{_csDefaultInstanceProfileARN = a});

instance AWSRequest CreateStack where
        type Sv CreateStack = OpsWorks
        type Rs CreateStack = CreateStackResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> CreateStackResponse' <$> x .?> "StackId")

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
              ["DefaultRootDeviceType" .= _csDefaultRootDeviceType,
               "ChefConfiguration" .= _csChefConfiguration,
               "VpcId" .= _csVPCId,
               "DefaultSshKeyName" .= _csDefaultSSHKeyName,
               "CustomJson" .= _csCustomJSON,
               "CustomCookbooksSource" .= _csCustomCookbooksSource,
               "DefaultAvailabilityZone" .=
                 _csDefaultAvailabilityZone,
               "UseOpsworksSecurityGroups" .=
                 _csUseOpsworksSecurityGroups,
               "DefaultOs" .= _csDefaultOS,
               "Attributes" .= _csAttributes,
               "UseCustomCookbooks" .= _csUseCustomCookbooks,
               "DefaultSubnetId" .= _csDefaultSubnetId,
               "ConfigurationManager" .= _csConfigurationManager,
               "HostnameTheme" .= _csHostnameTheme,
               "Name" .= _csName, "Region" .= _csRegion,
               "ServiceRoleArn" .= _csServiceRoleARN,
               "DefaultInstanceProfileArn" .=
                 _csDefaultInstanceProfileARN]

instance ToPath CreateStack where
        toPath = const "/"

instance ToQuery CreateStack where
        toQuery = const mempty

-- | /See:/ 'createStackResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creStackId'
newtype CreateStackResponse = CreateStackResponse'{_creStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateStackResponse' smart constructor.
createStackResponse :: CreateStackResponse
createStackResponse = CreateStackResponse'{_creStackId = Nothing};

-- | The stack ID, which is an opaque string that you use to identify the
-- stack when performing actions such as @DescribeStacks@.
creStackId :: Lens' CreateStackResponse (Maybe Text)
creStackId = lens _creStackId (\ s a -> s{_creStackId = a});
