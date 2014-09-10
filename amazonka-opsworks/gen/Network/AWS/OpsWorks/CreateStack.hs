{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new stack. For more information, see Create a New Stack. Required
-- Permissions: To use this action, an IAM user must have an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks
    (
    -- * Request
      CreateStack
    -- ** Request constructor
    , mkCreateStack
    -- ** Request lenses
    , cs1Name
    , cs1Region
    , cs1VpcId
    , cs1Attributes
    , cs1ServiceRoleArn
    , cs1DefaultInstanceProfileArn
    , cs1DefaultOs
    , cs1HostnameTheme
    , cs1DefaultAvailabilityZone
    , cs1DefaultSubnetId
    , cs1CustomJson
    , cs1ConfigurationManager
    , cs1ChefConfiguration
    , cs1UseCustomCookbooks
    , cs1UseOpsworksSecurityGroups
    , cs1CustomCookbooksSource
    , cs1DefaultSshKeyName
    , cs1DefaultRootDeviceType

    -- * Response
    , CreateStackResponse
    -- ** Response constructor
    , mkCreateStackResponse
    -- ** Response lenses
    , csrrStackId
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateStack = CreateStack
    { _cs1Name :: Text
    , _cs1Region :: Text
    , _cs1VpcId :: Maybe Text
    , _cs1Attributes :: Map StackAttributesKeys Text
    , _cs1ServiceRoleArn :: Text
    , _cs1DefaultInstanceProfileArn :: Text
    , _cs1DefaultOs :: Maybe Text
    , _cs1HostnameTheme :: Maybe Text
    , _cs1DefaultAvailabilityZone :: Maybe Text
    , _cs1DefaultSubnetId :: Maybe Text
    , _cs1CustomJson :: Maybe Text
    , _cs1ConfigurationManager :: Maybe StackConfigurationManager
    , _cs1ChefConfiguration :: Maybe ChefConfiguration
    , _cs1UseCustomCookbooks :: Maybe Bool
    , _cs1UseOpsworksSecurityGroups :: Maybe Bool
    , _cs1CustomCookbooksSource :: Maybe Source
    , _cs1DefaultSshKeyName :: Maybe Text
    , _cs1DefaultRootDeviceType :: Maybe RootDeviceType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStack' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Region ::@ @Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map StackAttributesKeys Text@
--
-- * @ServiceRoleArn ::@ @Text@
--
-- * @DefaultInstanceProfileArn ::@ @Text@
--
-- * @DefaultOs ::@ @Maybe Text@
--
-- * @HostnameTheme ::@ @Maybe Text@
--
-- * @DefaultAvailabilityZone ::@ @Maybe Text@
--
-- * @DefaultSubnetId ::@ @Maybe Text@
--
-- * @CustomJson ::@ @Maybe Text@
--
-- * @ConfigurationManager ::@ @Maybe StackConfigurationManager@
--
-- * @ChefConfiguration ::@ @Maybe ChefConfiguration@
--
-- * @UseCustomCookbooks ::@ @Maybe Bool@
--
-- * @UseOpsworksSecurityGroups ::@ @Maybe Bool@
--
-- * @CustomCookbooksSource ::@ @Maybe Source@
--
-- * @DefaultSshKeyName ::@ @Maybe Text@
--
-- * @DefaultRootDeviceType ::@ @Maybe RootDeviceType@
--
mkCreateStack :: Text -- ^ 'cs1Name'
              -> Text -- ^ 'cs1Region'
              -> Text -- ^ 'cs1ServiceRoleArn'
              -> Text -- ^ 'cs1DefaultInstanceProfileArn'
              -> CreateStack
mkCreateStack p1 p2 p5 p6 = CreateStack
    { _cs1Name = p1
    , _cs1Region = p2
    , _cs1VpcId = Nothing
    , _cs1Attributes = mempty
    , _cs1ServiceRoleArn = p5
    , _cs1DefaultInstanceProfileArn = p6
    , _cs1DefaultOs = Nothing
    , _cs1HostnameTheme = Nothing
    , _cs1DefaultAvailabilityZone = Nothing
    , _cs1DefaultSubnetId = Nothing
    , _cs1CustomJson = Nothing
    , _cs1ConfigurationManager = Nothing
    , _cs1ChefConfiguration = Nothing
    , _cs1UseCustomCookbooks = Nothing
    , _cs1UseOpsworksSecurityGroups = Nothing
    , _cs1CustomCookbooksSource = Nothing
    , _cs1DefaultSshKeyName = Nothing
    , _cs1DefaultRootDeviceType = Nothing
    }

-- | The stack name.
cs1Name :: Lens' CreateStack Text
cs1Name = lens _cs1Name (\s a -> s { _cs1Name = a })

-- | The stack AWS region, such as "us-east-1". For more information about
-- Amazon regions, see Regions and Endpoints.
cs1Region :: Lens' CreateStack Text
cs1Region = lens _cs1Region (\s a -> s { _cs1Region = a })

-- | The ID of the VPC that the stack is to be launched into. It must be in the
-- specified region. All instances will be launched into this VPC, and you
-- cannot change the ID later. If your account supports EC2 Classic, the
-- default value is no VPC. If your account does not support EC2 Classic, the
-- default value is the default VPC for the specified region. If the VPC ID
-- corresponds to a default VPC and you have specified either the
-- DefaultAvailabilityZone or the DefaultSubnetId parameter only, AWS OpsWorks
-- infers the value of the other parameter. If you specify neither parameter,
-- AWS OpsWorks sets these parameters to the first valid Availability Zone for
-- the specified region and the corresponding default VPC subnet ID,
-- respectively. If you specify a nondefault VPC ID, note the following: It
-- must belong to a VPC in your account that is in the specified region. You
-- must specify a value for DefaultSubnetId. For more information on how to
-- use AWS OpsWorks with a VPC, see Running a Stack in a VPC. For more
-- information on default VPC and EC2 Classic, see Supported Platforms.
cs1VpcId :: Lens' CreateStack (Maybe Text)
cs1VpcId = lens _cs1VpcId (\s a -> s { _cs1VpcId = a })

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
cs1Attributes :: Lens' CreateStack (Map StackAttributesKeys Text)
cs1Attributes = lens _cs1Attributes (\s a -> s { _cs1Attributes = a })

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see Using Identifiers.
cs1ServiceRoleArn :: Lens' CreateStack Text
cs1ServiceRoleArn =
    lens _cs1ServiceRoleArn (\s a -> s { _cs1ServiceRoleArn = a })

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
cs1DefaultInstanceProfileArn :: Lens' CreateStack Text
cs1DefaultInstanceProfileArn =
    lens _cs1DefaultInstanceProfileArn
         (\s a -> s { _cs1DefaultInstanceProfileArn = a })

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
cs1DefaultOs :: Lens' CreateStack (Maybe Text)
cs1DefaultOs = lens _cs1DefaultOs (\s a -> s { _cs1DefaultOs = a })

-- | The stack's host name theme, with spaces are replaced by underscores. The
-- theme is used to generate host names for the stack's instances. By default,
-- HostnameTheme is set to Layer_Dependent, which creates host names by
-- appending integers to the layer's short name. The other themes are:
-- Baked_Goods Clouds European_Cities Fruits Greek_Deities
-- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
-- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
-- GetHostNameSuggestion, which returns a host name based on the current
-- theme.
cs1HostnameTheme :: Lens' CreateStack (Maybe Text)
cs1HostnameTheme =
    lens _cs1HostnameTheme (\s a -> s { _cs1HostnameTheme = a })

-- | The stack's default Availability Zone, which must be in the specified
-- region. For more information, see Regions and Endpoints. If you also
-- specify a value for DefaultSubnetId, the subnet must be in the same zone.
-- For more information, see the VpcId parameter description.
cs1DefaultAvailabilityZone :: Lens' CreateStack (Maybe Text)
cs1DefaultAvailabilityZone =
    lens _cs1DefaultAvailabilityZone
         (\s a -> s { _cs1DefaultAvailabilityZone = a })

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in
-- that zone. For information on default values and when this parameter is
-- required, see the VpcId parameter description.
cs1DefaultSubnetId :: Lens' CreateStack (Maybe Text)
cs1DefaultSubnetId =
    lens _cs1DefaultSubnetId (\s a -> s { _cs1DefaultSubnetId = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
cs1CustomJson :: Lens' CreateStack (Maybe Text)
cs1CustomJson = lens _cs1CustomJson (\s a -> s { _cs1CustomJson = a })

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
cs1ConfigurationManager :: Lens' CreateStack (Maybe StackConfigurationManager)
cs1ConfigurationManager =
    lens _cs1ConfigurationManager
         (\s a -> s { _cs1ConfigurationManager = a })

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
cs1ChefConfiguration :: Lens' CreateStack (Maybe ChefConfiguration)
cs1ChefConfiguration =
    lens _cs1ChefConfiguration (\s a -> s { _cs1ChefConfiguration = a })

-- | Whether the stack uses custom cookbooks.
cs1UseCustomCookbooks :: Lens' CreateStack (Maybe Bool)
cs1UseCustomCookbooks =
    lens _cs1UseCustomCookbooks (\s a -> s { _cs1UseCustomCookbooks = a })

-- | Whether to associate the AWS OpsWorks built-in security groups with the
-- stack's layers. AWS OpsWorks provides a standard set of built-in security
-- groups, one for each layer, which are associated with layers by default.
-- With UseOpsworksSecurityGroups you can instead provide your own custom
-- security groups. UseOpsworksSecurityGroups has the following settings: True
-- - AWS OpsWorks automatically associates the appropriate built-in security
-- group with each layer (default setting). You can associate additional
-- security groups with a layer after you create it but you cannot delete the
-- built-in security group. False - AWS OpsWorks does not associate built-in
-- security groups with layers. You must create appropriate EC2 security
-- groups and associate a security group with each layer that you create.
-- However, you can still manually associate a built-in security group with a
-- layer on creation; custom security groups are required only for those
-- layers that need custom settings. For more information, see Create a New
-- Stack.
cs1UseOpsworksSecurityGroups :: Lens' CreateStack (Maybe Bool)
cs1UseOpsworksSecurityGroups =
    lens _cs1UseOpsworksSecurityGroups
         (\s a -> s { _cs1UseOpsworksSecurityGroups = a })

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
cs1CustomCookbooksSource :: Lens' CreateStack (Maybe Source)
cs1CustomCookbooksSource =
    lens _cs1CustomCookbooksSource
         (\s a -> s { _cs1CustomCookbooksSource = a })

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
cs1DefaultSshKeyName :: Lens' CreateStack (Maybe Text)
cs1DefaultSshKeyName =
    lens _cs1DefaultSshKeyName (\s a -> s { _cs1DefaultSshKeyName = a })

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. The default option is instance-store. For more information, see
-- Storage for the Root Device.
cs1DefaultRootDeviceType :: Lens' CreateStack (Maybe RootDeviceType)
cs1DefaultRootDeviceType =
    lens _cs1DefaultRootDeviceType
         (\s a -> s { _cs1DefaultRootDeviceType = a })

instance ToPath CreateStack

instance ToQuery CreateStack

instance ToHeaders CreateStack

instance ToJSON CreateStack

-- | Contains the response to a CreateStack request.
newtype CreateStackResponse = CreateStackResponse
    { _csrrStackId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStackResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
mkCreateStackResponse :: CreateStackResponse
mkCreateStackResponse = CreateStackResponse
    { _csrrStackId = Nothing
    }

-- | The stack ID, which is an opaque string that you use to identify the stack
-- when performing actions such as DescribeStacks.
csrrStackId :: Lens' CreateStackResponse (Maybe Text)
csrrStackId = lens _csrrStackId (\s a -> s { _csrrStackId = a })

instance FromJSON CreateStackResponse

instance AWSRequest CreateStack where
    type Sv CreateStack = OpsWorks
    type Rs CreateStack = CreateStackResponse

    request = get
    response _ = jsonResponse
