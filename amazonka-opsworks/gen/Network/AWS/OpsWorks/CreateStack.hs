{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-edit.html Create a New Stack>.
--
-- Required Permissions: To use this action, an IAM user must have an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateStack.html>
module Network.AWS.OpsWorks.CreateStack
    (
    -- * Request
      CreateStack
    -- ** Request constructor
    , createStack
    -- ** Request lenses
    , csAttributes
    , csChefConfiguration
    , csConfigurationManager
    , csCustomCookbooksSource
    , csCustomJson
    , csDefaultAvailabilityZone
    , csDefaultInstanceProfileArn
    , csDefaultOs
    , csDefaultRootDeviceType
    , csDefaultSshKeyName
    , csDefaultSubnetId
    , csHostnameTheme
    , csName
    , csRegion
    , csServiceRoleArn
    , csUseCustomCookbooks
    , csUseOpsworksSecurityGroups
    , csVpcId

    -- * Response
    , CreateStackResponse
    -- ** Response constructor
    , createStackResponse
    -- ** Response lenses
    , csr1StackId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data CreateStack = CreateStack
    { _csAttributes                :: Map StackAttributesKeys Text
    , _csChefConfiguration         :: Maybe ChefConfiguration
    , _csConfigurationManager      :: Maybe StackConfigurationManager
    , _csCustomCookbooksSource     :: Maybe Source
    , _csCustomJson                :: Maybe Text
    , _csDefaultAvailabilityZone   :: Maybe Text
    , _csDefaultInstanceProfileArn :: Text
    , _csDefaultOs                 :: Maybe Text
    , _csDefaultRootDeviceType     :: Maybe RootDeviceType
    , _csDefaultSshKeyName         :: Maybe Text
    , _csDefaultSubnetId           :: Maybe Text
    , _csHostnameTheme             :: Maybe Text
    , _csName                      :: Text
    , _csRegion                    :: Text
    , _csServiceRoleArn            :: Text
    , _csUseCustomCookbooks        :: Maybe Bool
    , _csUseOpsworksSecurityGroups :: Maybe Bool
    , _csVpcId                     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'CreateStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csAttributes' @::@ 'HashMap' 'StackAttributesKeys' 'Text'
--
-- * 'csChefConfiguration' @::@ 'Maybe' 'ChefConfiguration'
--
-- * 'csConfigurationManager' @::@ 'Maybe' 'StackConfigurationManager'
--
-- * 'csCustomCookbooksSource' @::@ 'Maybe' 'Source'
--
-- * 'csCustomJson' @::@ 'Maybe' 'Text'
--
-- * 'csDefaultAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'csDefaultInstanceProfileArn' @::@ 'Text'
--
-- * 'csDefaultOs' @::@ 'Maybe' 'Text'
--
-- * 'csDefaultRootDeviceType' @::@ 'Maybe' 'RootDeviceType'
--
-- * 'csDefaultSshKeyName' @::@ 'Maybe' 'Text'
--
-- * 'csDefaultSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'csHostnameTheme' @::@ 'Maybe' 'Text'
--
-- * 'csName' @::@ 'Text'
--
-- * 'csRegion' @::@ 'Text'
--
-- * 'csServiceRoleArn' @::@ 'Text'
--
-- * 'csUseCustomCookbooks' @::@ 'Maybe' 'Bool'
--
-- * 'csUseOpsworksSecurityGroups' @::@ 'Maybe' 'Bool'
--
-- * 'csVpcId' @::@ 'Maybe' 'Text'
--
createStack :: Text -- ^ 'csName'
            -> Text -- ^ 'csRegion'
            -> Text -- ^ 'csServiceRoleArn'
            -> Text -- ^ 'csDefaultInstanceProfileArn'
            -> CreateStack
createStack p1 p2 p3 p4 = CreateStack
    { _csName                      = p1
    , _csRegion                    = p2
    , _csServiceRoleArn            = p3
    , _csDefaultInstanceProfileArn = p4
    , _csVpcId                     = Nothing
    , _csAttributes                = mempty
    , _csDefaultOs                 = Nothing
    , _csHostnameTheme             = Nothing
    , _csDefaultAvailabilityZone   = Nothing
    , _csDefaultSubnetId           = Nothing
    , _csCustomJson                = Nothing
    , _csConfigurationManager      = Nothing
    , _csChefConfiguration         = Nothing
    , _csUseCustomCookbooks        = Nothing
    , _csUseOpsworksSecurityGroups = Nothing
    , _csCustomCookbooksSource     = Nothing
    , _csDefaultSshKeyName         = Nothing
    , _csDefaultRootDeviceType     = Nothing
    }

-- | One or more user-defined key/value pairs to be added to the stack attributes.
csAttributes :: Lens' CreateStack (HashMap StackAttributesKeys Text)
csAttributes = lens _csAttributes (\s a -> s { _csAttributes = a }) . _Map

-- | A 'ChefConfiguration' object that specifies whether to enable Berkshelf and the
-- Berkshelf version on Chef 11.10 stacks. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create aNew Stack>.
csChefConfiguration :: Lens' CreateStack (Maybe ChefConfiguration)
csChefConfiguration =
    lens _csChefConfiguration (\s a -> s { _csChefConfiguration = a })

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
csConfigurationManager :: Lens' CreateStack (Maybe StackConfigurationManager)
csConfigurationManager =
    lens _csConfigurationManager (\s a -> s { _csConfigurationManager = a })

csCustomCookbooksSource :: Lens' CreateStack (Maybe Source)
csCustomCookbooksSource =
    lens _csCustomCookbooksSource (\s a -> s { _csCustomCookbooksSource = a })

-- | A string that contains user-defined, custom JSON. It is used to override the
-- corresponding default stack configuration JSON values. The string should be
-- in the following format and must escape characters such as '"'.:
--
-- '"{\"key1\": \"value1\", \"key2\": \"value2\",...}"'
--
-- For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the StackConfiguration JSON>.
csCustomJson :: Lens' CreateStack (Maybe Text)
csCustomJson = lens _csCustomJson (\s a -> s { _csCustomJson = a })

-- | The stack's default Availability Zone, which must be in the specified region.
-- For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>. If you also specify a value
-- for 'DefaultSubnetId', the subnet must be in the same zone. For more
-- information, see the 'VpcId' parameter description.
csDefaultAvailabilityZone :: Lens' CreateStack (Maybe Text)
csDefaultAvailabilityZone =
    lens _csDefaultAvailabilityZone
        (\s a -> s { _csDefaultAvailabilityZone = a })

-- | The ARN of an IAM profile that is the default profile for all of the stack's
-- EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
csDefaultInstanceProfileArn :: Lens' CreateStack Text
csDefaultInstanceProfileArn =
    lens _csDefaultInstanceProfileArn
        (\s a -> s { _csDefaultInstanceProfileArn = a })

-- | The stack's operating system, which must be set to one of the following.
--
-- Standard operating systems: an Amazon Linux version such as 'Amazon Linux2014.09', 'Ubuntu 12.04 LTS', or 'Ubuntu 14.04 LTS'. Custom AMIs: 'Custom'. You
-- specify the custom AMI you want to use when you create instances.   The
-- default option is the current Amazon Linux version.
csDefaultOs :: Lens' CreateStack (Maybe Text)
csDefaultOs = lens _csDefaultOs (\s a -> s { _csDefaultOs = a })

-- | The default root device type. This value is used by default for all instances
-- in the stack, but you can override it when you create an instance. The
-- default option is 'instance-store'. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for theRoot Device>.
csDefaultRootDeviceType :: Lens' CreateStack (Maybe RootDeviceType)
csDefaultRootDeviceType =
    lens _csDefaultRootDeviceType (\s a -> s { _csDefaultRootDeviceType = a })

-- | A default Amazon EC2 key pair name. The default value is none. If you specify
-- a key pair name, AWS OpsWorks installs the public key on the instance and you
-- can use the private key with an SSH client to log in to the instance. For
-- more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html  Using SSH to Communicate with an Instance> and <http://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>. You can override this setting by specifying a different
-- key pair, or no key pair, when you <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html  create an instance>.
csDefaultSshKeyName :: Lens' CreateStack (Maybe Text)
csDefaultSshKeyName =
    lens _csDefaultSshKeyName (\s a -> s { _csDefaultSshKeyName = a })

-- | The stack's default VPC subnet ID. This parameter is required if you specify
-- a value for the 'VpcId' parameter. All instances are launched into this subnet
-- unless you specify otherwise when you create the instance. If you also
-- specify a value for 'DefaultAvailabilityZone', the subnet must be in that zone.
-- For information on default values and when this parameter is required, see
-- the 'VpcId' parameter description.
csDefaultSubnetId :: Lens' CreateStack (Maybe Text)
csDefaultSubnetId =
    lens _csDefaultSubnetId (\s a -> s { _csDefaultSubnetId = a })

-- | The stack's host name theme, with spaces are replaced by underscores. The
-- theme is used to generate host names for the stack's instances. By default, 'HostnameTheme' is set to 'Layer_Dependent', which creates host names by appending integers to
-- the layer's short name. The other themes are:
--
-- 'Baked_Goods' 'Clouds' 'Europe_Cities' 'Fruits' 'Greek_Deities' 'Legendary_creatures_from_Japan' 'Planets_and_Moons' 'Roman_Deities' 'Scottish_Islands' 'US_Cities' 'Wild_Cats'  To
-- obtain a generated host name, call 'GetHostNameSuggestion', which returns a
-- host name based on the current theme.
csHostnameTheme :: Lens' CreateStack (Maybe Text)
csHostnameTheme = lens _csHostnameTheme (\s a -> s { _csHostnameTheme = a })

-- | The stack name.
csName :: Lens' CreateStack Text
csName = lens _csName (\s a -> s { _csName = a })

-- | The stack AWS region, such as "us-east-1". For more information about Amazon
-- regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
csRegion :: Lens' CreateStack Text
csRegion = lens _csRegion (\s a -> s { _csRegion = a })

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
csServiceRoleArn :: Lens' CreateStack Text
csServiceRoleArn = lens _csServiceRoleArn (\s a -> s { _csServiceRoleArn = a })

-- | Whether the stack uses custom cookbooks.
csUseCustomCookbooks :: Lens' CreateStack (Maybe Bool)
csUseCustomCookbooks =
    lens _csUseCustomCookbooks (\s a -> s { _csUseCustomCookbooks = a })

-- | Whether to associate the AWS OpsWorks built-in security groups with the
-- stack's layers.
--
-- AWS OpsWorks provides a standard set of built-in security groups, one for
-- each layer, which are associated with layers by default. With 'UseOpsworksSecurityGroups' you can instead provide your own custom security groups. 'UseOpsworksSecurityGroups' has the following settings:
--
-- True - AWS OpsWorks automatically associates the appropriate built-in
-- security group with each layer (default setting). You can associate
-- additional security groups with a layer after you create it but you cannot
-- delete the built-in security group.  False - AWS OpsWorks does not associate
-- built-in security groups with layers. You must create appropriate EC2
-- security groups and associate a security group with each layer that you
-- create. However, you can still manually associate a built-in security group
-- with a layer on creation; custom security groups are required only for those
-- layers that need custom settings.   For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a NewStack>.
csUseOpsworksSecurityGroups :: Lens' CreateStack (Maybe Bool)
csUseOpsworksSecurityGroups =
    lens _csUseOpsworksSecurityGroups
        (\s a -> s { _csUseOpsworksSecurityGroups = a })

-- | The ID of the VPC that the stack is to be launched into. It must be in the
-- specified region. All instances are launched into this VPC, and you cannot
-- change the ID later.
--
-- If your account supports EC2 Classic, the default value is no VPC. If your
-- account does not support EC2 Classic, the default value is the default VPC
-- for the specified region.  If the VPC ID corresponds to a default VPC and you
-- have specified either the 'DefaultAvailabilityZone' or the 'DefaultSubnetId'
-- parameter only, AWS OpsWorks infers the value of the other parameter. If you
-- specify neither parameter, AWS OpsWorks sets these parameters to the first
-- valid Availability Zone for the specified region and the corresponding
-- default VPC subnet ID, respectively.
--
-- If you specify a nondefault VPC ID, note the following:
--
-- It must belong to a VPC in your account that is in the specified region. You must specify a value for
-- 'DefaultSubnetId'.  For more information on how to use AWS OpsWorks with a VPC,
-- see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC>. For more information on default VPC and EC2
-- Classic, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
csVpcId :: Lens' CreateStack (Maybe Text)
csVpcId = lens _csVpcId (\s a -> s { _csVpcId = a })

newtype CreateStackResponse = CreateStackResponse
    { _csr1StackId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateStackResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csr1StackId' @::@ 'Maybe' 'Text'
--
createStackResponse :: CreateStackResponse
createStackResponse = CreateStackResponse
    { _csr1StackId = Nothing
    }

-- | The stack ID, which is an opaque string that you use to identify the stack
-- when performing actions such as 'DescribeStacks'.
csr1StackId :: Lens' CreateStackResponse (Maybe Text)
csr1StackId = lens _csr1StackId (\s a -> s { _csr1StackId = a })

instance ToPath CreateStack where
    toPath = const "/"

instance ToQuery CreateStack where
    toQuery = const mempty

instance ToHeaders CreateStack

instance ToJSON CreateStack where
    toJSON CreateStack{..} = object
        [ "Name"                      .= _csName
        , "Region"                    .= _csRegion
        , "VpcId"                     .= _csVpcId
        , "Attributes"                .= _csAttributes
        , "ServiceRoleArn"            .= _csServiceRoleArn
        , "DefaultInstanceProfileArn" .= _csDefaultInstanceProfileArn
        , "DefaultOs"                 .= _csDefaultOs
        , "HostnameTheme"             .= _csHostnameTheme
        , "DefaultAvailabilityZone"   .= _csDefaultAvailabilityZone
        , "DefaultSubnetId"           .= _csDefaultSubnetId
        , "CustomJson"                .= _csCustomJson
        , "ConfigurationManager"      .= _csConfigurationManager
        , "ChefConfiguration"         .= _csChefConfiguration
        , "UseCustomCookbooks"        .= _csUseCustomCookbooks
        , "UseOpsworksSecurityGroups" .= _csUseOpsworksSecurityGroups
        , "CustomCookbooksSource"     .= _csCustomCookbooksSource
        , "DefaultSshKeyName"         .= _csDefaultSshKeyName
        , "DefaultRootDeviceType"     .= _csDefaultRootDeviceType
        ]

instance AWSRequest CreateStack where
    type Sv CreateStack = OpsWorks
    type Rs CreateStack = CreateStackResponse

    request  = post "CreateStack"
    response = jsonResponse

instance FromJSON CreateStackResponse where
    parseJSON = withObject "CreateStackResponse" $ \o -> CreateStackResponse
        <$> o .:? "StackId"
