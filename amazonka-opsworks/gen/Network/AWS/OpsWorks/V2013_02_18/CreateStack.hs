{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CreateStack
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
module Network.AWS.OpsWorks.V2013_02_18.CreateStack
    (
    -- * Request
      CreateStack
    -- ** Request constructor
    , createStack
    -- ** Request lenses
    , cstName
    , cstRegion
    , cstServiceRoleArn
    , cstDefaultInstanceProfileArn
    , cstUseCustomCookbooks
    , cstUseOpsworksSecurityGroups
    , cstChefConfiguration
    , cstDefaultRootDeviceType
    , cstCustomCookbooksSource
    , cstAttributes
    , cstConfigurationManager
    , cstVpcId
    , cstDefaultOs
    , cstHostnameTheme
    , cstDefaultAvailabilityZone
    , cstDefaultSubnetId
    , cstCustomJson
    , cstDefaultSshKeyName

    -- * Response
    , CreateStackResponse
    -- ** Response lenses
    , csuStackId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateStack' request.
createStack :: Text -- ^ 'cstName'
            -> Text -- ^ 'cstRegion'
            -> Text -- ^ 'cstServiceRoleArn'
            -> Text -- ^ 'cstDefaultInstanceProfileArn'
            -> CreateStack
createStack p1 p2 p3 p4 = CreateStack
    { _cstName = p1
    , _cstRegion = p2
    , _cstServiceRoleArn = p3
    , _cstDefaultInstanceProfileArn = p4
    , _cstUseCustomCookbooks = Nothing
    , _cstUseOpsworksSecurityGroups = Nothing
    , _cstChefConfiguration = Nothing
    , _cstDefaultRootDeviceType = Nothing
    , _cstCustomCookbooksSource = Nothing
    , _cstAttributes = mempty
    , _cstConfigurationManager = Nothing
    , _cstVpcId = Nothing
    , _cstDefaultOs = Nothing
    , _cstHostnameTheme = Nothing
    , _cstDefaultAvailabilityZone = Nothing
    , _cstDefaultSubnetId = Nothing
    , _cstCustomJson = Nothing
    , _cstDefaultSshKeyName = Nothing
    }
{-# INLINE createStack #-}

data CreateStack = CreateStack
    { _cstName :: Text
      -- ^ The stack name.
    , _cstRegion :: Text
      -- ^ The stack AWS region, such as "us-east-1". For more information
      -- about Amazon regions, see Regions and Endpoints.
    , _cstServiceRoleArn :: Text
      -- ^ The stack AWS Identity and Access Management (IAM) role, which
      -- allows AWS OpsWorks to work with AWS resources on your behalf.
      -- You must set this parameter to the Amazon Resource Name (ARN) for
      -- an existing IAM role. For more information about IAM ARNs, see
      -- Using Identifiers.
    , _cstDefaultInstanceProfileArn :: Text
      -- ^ The ARN of an IAM profile that is the default profile for all of
      -- the stack's EC2 instances. For more information about IAM ARNs,
      -- see Using Identifiers.
    , _cstUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , _cstUseOpsworksSecurityGroups :: Maybe Bool
      -- ^ Whether to associate the AWS OpsWorks built-in security groups
      -- with the stack's layers. AWS OpsWorks provides a standard set of
      -- built-in security groups, one for each layer, which are
      -- associated with layers by default. With UseOpsworksSecurityGroups
      -- you can instead provide your own custom security groups.
      -- UseOpsworksSecurityGroups has the following settings: True - AWS
      -- OpsWorks automatically associates the appropriate built-in
      -- security group with each layer (default setting). You can
      -- associate additional security groups with a layer after you
      -- create it but you cannot delete the built-in security group.
      -- False - AWS OpsWorks does not associate built-in security groups
      -- with layers. You must create appropriate EC2 security groups and
      -- associate a security group with each layer that you create.
      -- However, you can still manually associate a built-in security
      -- group with a layer on creation; custom security groups are
      -- required only for those layers that need custom settings. For
      -- more information, see Create a New Stack.
    , _cstChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version on Chef 11.10 stacks. For
      -- more information, see Create a New Stack.
    , _cstDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the stack, but you can override it when you
      -- create an instance. The default option is instance-store. For
      -- more information, see Storage for the Root Device.
    , _cstCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _cstAttributes :: Map StackAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _cstConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend
      -- that you use the configuration manager to specify the Chef
      -- version, 0.9, 11.4, or 11.10. The default value is currently
      -- 11.4.
    , _cstVpcId :: Maybe Text
      -- ^ The ID of the VPC that the stack is to be launched into. It must
      -- be in the specified region. All instances will be launched into
      -- this VPC, and you cannot change the ID later. If your account
      -- supports EC2 Classic, the default value is no VPC. If your
      -- account does not support EC2 Classic, the default value is the
      -- default VPC for the specified region. If the VPC ID corresponds
      -- to a default VPC and you have specified either the
      -- DefaultAvailabilityZone or the DefaultSubnetId parameter only,
      -- AWS OpsWorks infers the value of the other parameter. If you
      -- specify neither parameter, AWS OpsWorks sets these parameters to
      -- the first valid Availability Zone for the specified region and
      -- the corresponding default VPC subnet ID, respectively. If you
      -- specify a nondefault VPC ID, note the following: It must belong
      -- to a VPC in your account that is in the specified region. You
      -- must specify a value for DefaultSubnetId. For more information on
      -- how to use AWS OpsWorks with a VPC, see Running a Stack in a VPC.
      -- For more information on default VPC and EC2 Classic, see
      -- Supported Platforms.
    , _cstDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon
      -- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , _cstHostnameTheme :: Maybe Text
      -- ^ The stack's host name theme, with spaces are replaced by
      -- underscores. The theme is used to generate host names for the
      -- stack's instances. By default, HostnameTheme is set to
      -- Layer_Dependent, which creates host names by appending integers
      -- to the layer's short name. The other themes are: Baked_Goods
      -- Clouds European_Cities Fruits Greek_Deities
      -- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
      -- Scottish_Islands US_Cities Wild_Cats To obtain a generated host
      -- name, call GetHostNameSuggestion, which returns a host name based
      -- on the current theme.
    , _cstDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone, which must be in the
      -- specified region. For more information, see Regions and
      -- Endpoints. If you also specify a value for DefaultSubnetId, the
      -- subnet must be in the same zone. For more information, see the
      -- VpcId parameter description.
    , _cstDefaultSubnetId :: Maybe Text
      -- ^ The stack's default subnet ID. All instances will be launched
      -- into this subnet unless you specify otherwise when you create the
      -- instance. If you also specify a value for
      -- DefaultAvailabilityZone, the subnet must be in that zone. For
      -- information on default values and when this parameter is
      -- required, see the VpcId parameter description.
    , _cstCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _cstDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this
      -- value when you create or update an instance.
    } deriving (Show, Generic)

-- | The stack name.
cstName :: Lens' CreateStack (Text)
cstName f x =
    f (_cstName x)
        <&> \y -> x { _cstName = y }
{-# INLINE cstName #-}

-- | The stack AWS region, such as "us-east-1". For more information about
-- Amazon regions, see Regions and Endpoints.
cstRegion :: Lens' CreateStack (Text)
cstRegion f x =
    f (_cstRegion x)
        <&> \y -> x { _cstRegion = y }
{-# INLINE cstRegion #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see Using Identifiers.
cstServiceRoleArn :: Lens' CreateStack (Text)
cstServiceRoleArn f x =
    f (_cstServiceRoleArn x)
        <&> \y -> x { _cstServiceRoleArn = y }
{-# INLINE cstServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
cstDefaultInstanceProfileArn :: Lens' CreateStack (Text)
cstDefaultInstanceProfileArn f x =
    f (_cstDefaultInstanceProfileArn x)
        <&> \y -> x { _cstDefaultInstanceProfileArn = y }
{-# INLINE cstDefaultInstanceProfileArn #-}

-- | Whether the stack uses custom cookbooks.
cstUseCustomCookbooks :: Lens' CreateStack (Maybe Bool)
cstUseCustomCookbooks f x =
    f (_cstUseCustomCookbooks x)
        <&> \y -> x { _cstUseCustomCookbooks = y }
{-# INLINE cstUseCustomCookbooks #-}

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
cstUseOpsworksSecurityGroups :: Lens' CreateStack (Maybe Bool)
cstUseOpsworksSecurityGroups f x =
    f (_cstUseOpsworksSecurityGroups x)
        <&> \y -> x { _cstUseOpsworksSecurityGroups = y }
{-# INLINE cstUseOpsworksSecurityGroups #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
cstChefConfiguration :: Lens' CreateStack (Maybe ChefConfiguration)
cstChefConfiguration f x =
    f (_cstChefConfiguration x)
        <&> \y -> x { _cstChefConfiguration = y }
{-# INLINE cstChefConfiguration #-}

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. The default option is instance-store. For more information, see
-- Storage for the Root Device.
cstDefaultRootDeviceType :: Lens' CreateStack (Maybe RootDeviceType)
cstDefaultRootDeviceType f x =
    f (_cstDefaultRootDeviceType x)
        <&> \y -> x { _cstDefaultRootDeviceType = y }
{-# INLINE cstDefaultRootDeviceType #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
cstCustomCookbooksSource :: Lens' CreateStack (Maybe Source)
cstCustomCookbooksSource f x =
    f (_cstCustomCookbooksSource x)
        <&> \y -> x { _cstCustomCookbooksSource = y }
{-# INLINE cstCustomCookbooksSource #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
cstAttributes :: Lens' CreateStack (Map StackAttributesKeys Text)
cstAttributes f x =
    f (_cstAttributes x)
        <&> \y -> x { _cstAttributes = y }
{-# INLINE cstAttributes #-}

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
cstConfigurationManager :: Lens' CreateStack (Maybe StackConfigurationManager)
cstConfigurationManager f x =
    f (_cstConfigurationManager x)
        <&> \y -> x { _cstConfigurationManager = y }
{-# INLINE cstConfigurationManager #-}

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
cstVpcId :: Lens' CreateStack (Maybe Text)
cstVpcId f x =
    f (_cstVpcId x)
        <&> \y -> x { _cstVpcId = y }
{-# INLINE cstVpcId #-}

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
cstDefaultOs :: Lens' CreateStack (Maybe Text)
cstDefaultOs f x =
    f (_cstDefaultOs x)
        <&> \y -> x { _cstDefaultOs = y }
{-# INLINE cstDefaultOs #-}

-- | The stack's host name theme, with spaces are replaced by underscores. The
-- theme is used to generate host names for the stack's instances. By default,
-- HostnameTheme is set to Layer_Dependent, which creates host names by
-- appending integers to the layer's short name. The other themes are:
-- Baked_Goods Clouds European_Cities Fruits Greek_Deities
-- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
-- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
-- GetHostNameSuggestion, which returns a host name based on the current
-- theme.
cstHostnameTheme :: Lens' CreateStack (Maybe Text)
cstHostnameTheme f x =
    f (_cstHostnameTheme x)
        <&> \y -> x { _cstHostnameTheme = y }
{-# INLINE cstHostnameTheme #-}

-- | The stack's default Availability Zone, which must be in the specified
-- region. For more information, see Regions and Endpoints. If you also
-- specify a value for DefaultSubnetId, the subnet must be in the same zone.
-- For more information, see the VpcId parameter description.
cstDefaultAvailabilityZone :: Lens' CreateStack (Maybe Text)
cstDefaultAvailabilityZone f x =
    f (_cstDefaultAvailabilityZone x)
        <&> \y -> x { _cstDefaultAvailabilityZone = y }
{-# INLINE cstDefaultAvailabilityZone #-}

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in
-- that zone. For information on default values and when this parameter is
-- required, see the VpcId parameter description.
cstDefaultSubnetId :: Lens' CreateStack (Maybe Text)
cstDefaultSubnetId f x =
    f (_cstDefaultSubnetId x)
        <&> \y -> x { _cstDefaultSubnetId = y }
{-# INLINE cstDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
cstCustomJson :: Lens' CreateStack (Maybe Text)
cstCustomJson f x =
    f (_cstCustomJson x)
        <&> \y -> x { _cstCustomJson = y }
{-# INLINE cstCustomJson #-}

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
cstDefaultSshKeyName :: Lens' CreateStack (Maybe Text)
cstDefaultSshKeyName f x =
    f (_cstDefaultSshKeyName x)
        <&> \y -> x { _cstDefaultSshKeyName = y }
{-# INLINE cstDefaultSshKeyName #-}

instance ToPath CreateStack

instance ToQuery CreateStack

instance ToHeaders CreateStack

instance ToJSON CreateStack

data CreateStackResponse = CreateStackResponse
    { _csuStackId :: Maybe Text
      -- ^ The stack ID, which is an opaque string that you use to identify
      -- the stack when performing actions such as DescribeStacks.
    } deriving (Show, Generic)

-- | The stack ID, which is an opaque string that you use to identify the stack
-- when performing actions such as DescribeStacks.
csuStackId :: Lens' CreateStackResponse (Maybe Text)
csuStackId f x =
    f (_csuStackId x)
        <&> \y -> x { _csuStackId = y }
{-# INLINE csuStackId #-}

instance FromJSON CreateStackResponse

instance AWSRequest CreateStack where
    type Sv CreateStack = OpsWorks
    type Rs CreateStack = CreateStackResponse

    request = get
    response _ = jsonResponse
