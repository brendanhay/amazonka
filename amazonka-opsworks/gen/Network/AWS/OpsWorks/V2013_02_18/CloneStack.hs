{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CloneStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a clone of a specified stack. For more information, see Clone a
-- Stack. Required Permissions: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.CloneStack
    (
    -- * Request
      CloneStack
    -- ** Request constructor
    , cloneStack
    -- ** Request lenses
    , csrSourceStackId
    , csrServiceRoleArn
    , csrUseCustomCookbooks
    , csrUseOpsworksSecurityGroups
    , csrClonePermissions
    , csrChefConfiguration
    , csrDefaultRootDeviceType
    , csrCustomCookbooksSource
    , csrAttributes
    , csrConfigurationManager
    , csrName
    , csrRegion
    , csrVpcId
    , csrDefaultInstanceProfileArn
    , csrDefaultOs
    , csrHostnameTheme
    , csrDefaultAvailabilityZone
    , csrDefaultSubnetId
    , csrCustomJson
    , csrDefaultSshKeyName
    , csrCloneAppIds

    -- * Response
    , CloneStackResponse
    -- ** Response lenses
    , cssStackId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CloneStack' request.
cloneStack :: Text -- ^ 'csrSourceStackId'
           -> Text -- ^ 'csrServiceRoleArn'
           -> CloneStack
cloneStack p1 p2 = CloneStack
    { _csrSourceStackId = p1
    , _csrServiceRoleArn = p2
    , _csrUseCustomCookbooks = Nothing
    , _csrUseOpsworksSecurityGroups = Nothing
    , _csrClonePermissions = Nothing
    , _csrChefConfiguration = Nothing
    , _csrDefaultRootDeviceType = Nothing
    , _csrCustomCookbooksSource = Nothing
    , _csrAttributes = mempty
    , _csrConfigurationManager = Nothing
    , _csrName = Nothing
    , _csrRegion = Nothing
    , _csrVpcId = Nothing
    , _csrDefaultInstanceProfileArn = Nothing
    , _csrDefaultOs = Nothing
    , _csrHostnameTheme = Nothing
    , _csrDefaultAvailabilityZone = Nothing
    , _csrDefaultSubnetId = Nothing
    , _csrCustomJson = Nothing
    , _csrDefaultSshKeyName = Nothing
    , _csrCloneAppIds = mempty
    }

data CloneStack = CloneStack
    { _csrSourceStackId :: Text
      -- ^ The source stack ID.
    , _csrServiceRoleArn :: Text
      -- ^ The stack AWS Identity and Access Management (IAM) role, which
      -- allows AWS OpsWorks to work with AWS resources on your behalf.
      -- You must set this parameter to the Amazon Resource Name (ARN) for
      -- an existing IAM role. If you create a stack by using the AWS
      -- OpsWorks console, it creates the role for you. You can obtain an
      -- existing stack's IAM ARN programmatically by calling
      -- DescribePermissions. For more information about IAM ARNs, see
      -- Using Identifiers. You must set this parameter to a valid service
      -- role ARN or the action will fail; there is no default value. You
      -- can specify the source stack's service role ARN, if you prefer,
      -- but you must do so explicitly.
    , _csrUseCustomCookbooks :: Maybe Bool
      -- ^ Whether to use custom cookbooks.
    , _csrUseOpsworksSecurityGroups :: Maybe Bool
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
    , _csrClonePermissions :: Maybe Bool
      -- ^ Whether to clone the source stack's permissions.
    , _csrChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version on Chef 11.10 stacks. For
      -- more information, see Create a New Stack.
    , _csrDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the cloned stack, but you can override it when
      -- you create an instance. For more information, see Storage for the
      -- Root Device.
    , _csrCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _csrAttributes :: Map StackAttributesKeys Text
      -- ^ A list of stack attributes and values as key/value pairs to be
      -- added to the cloned stack.
    , _csrConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend
      -- that you use the configuration manager to specify the Chef
      -- version, 0.9, 11.4, or 11.10. The default value is currently
      -- 11.4.
    , _csrName :: Maybe Text
      -- ^ The cloned stack name.
    , _csrRegion :: Maybe Text
      -- ^ The cloned stack AWS region, such as "us-east-1". For more
      -- information about AWS regions, see Regions and Endpoints.
    , _csrVpcId :: Maybe Text
      -- ^ The ID of the VPC that the cloned stack is to be launched into.
      -- It must be in the specified region. All instances will be
      -- launched into this VPC, and you cannot change the ID later. If
      -- your account supports EC2 Classic, the default value is no VPC.
      -- If your account does not support EC2 Classic, the default value
      -- is the default VPC for the specified region. If the VPC ID
      -- corresponds to a default VPC and you have specified either the
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
    , _csrDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of
      -- the stack's EC2 instances. For more information about IAM ARNs,
      -- see Using Identifiers.
    , _csrDefaultOs :: Maybe Text
      -- ^ The cloned stack's default operating system, which must be set to
      -- Amazon Linux or Ubuntu 12.04 LTS. The default option is Amazon
      -- Linux.
    , _csrHostnameTheme :: Maybe Text
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
    , _csrDefaultAvailabilityZone :: Maybe Text
      -- ^ The cloned stack's default Availability Zone, which must be in
      -- the specified region. For more information, see Regions and
      -- Endpoints. If you also specify a value for DefaultSubnetId, the
      -- subnet must be in the same zone. For more information, see the
      -- VpcId parameter description.
    , _csrDefaultSubnetId :: Maybe Text
      -- ^ The stack's default subnet ID. All instances will be launched
      -- into this subnet unless you specify otherwise when you create the
      -- instance. If you also specify a value for
      -- DefaultAvailabilityZone, the subnet must be in the same zone. For
      -- information on default values and when this parameter is
      -- required, see the VpcId parameter description.
    , _csrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _csrDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this
      -- value when you create or update an instance.
    , _csrCloneAppIds :: [Text]
      -- ^ A list of source stack app IDs to be included in the cloned
      -- stack.
    } deriving (Show, Generic)

-- | The source stack ID.
csrSourceStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> CloneStack
    -> f CloneStack
csrSourceStackId f x =
    (\y -> x { _csrSourceStackId = y })
       <$> f (_csrSourceStackId x)
{-# INLINE csrSourceStackId #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. If
-- you create a stack by using the AWS OpsWorks console, it creates the role
-- for you. You can obtain an existing stack's IAM ARN programmatically by
-- calling DescribePermissions. For more information about IAM ARNs, see Using
-- Identifiers. You must set this parameter to a valid service role ARN or the
-- action will fail; there is no default value. You can specify the source
-- stack's service role ARN, if you prefer, but you must do so explicitly.
csrServiceRoleArn
    :: Functor f
    => (Text
    -> f (Text))
    -> CloneStack
    -> f CloneStack
csrServiceRoleArn f x =
    (\y -> x { _csrServiceRoleArn = y })
       <$> f (_csrServiceRoleArn x)
{-# INLINE csrServiceRoleArn #-}

-- | Whether to use custom cookbooks.
csrUseCustomCookbooks
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CloneStack
    -> f CloneStack
csrUseCustomCookbooks f x =
    (\y -> x { _csrUseCustomCookbooks = y })
       <$> f (_csrUseCustomCookbooks x)
{-# INLINE csrUseCustomCookbooks #-}

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
csrUseOpsworksSecurityGroups
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CloneStack
    -> f CloneStack
csrUseOpsworksSecurityGroups f x =
    (\y -> x { _csrUseOpsworksSecurityGroups = y })
       <$> f (_csrUseOpsworksSecurityGroups x)
{-# INLINE csrUseOpsworksSecurityGroups #-}

-- | Whether to clone the source stack's permissions.
csrClonePermissions
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CloneStack
    -> f CloneStack
csrClonePermissions f x =
    (\y -> x { _csrClonePermissions = y })
       <$> f (_csrClonePermissions x)
{-# INLINE csrClonePermissions #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
csrChefConfiguration
    :: Functor f
    => (Maybe ChefConfiguration
    -> f (Maybe ChefConfiguration))
    -> CloneStack
    -> f CloneStack
csrChefConfiguration f x =
    (\y -> x { _csrChefConfiguration = y })
       <$> f (_csrChefConfiguration x)
{-# INLINE csrChefConfiguration #-}

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
csrDefaultRootDeviceType
    :: Functor f
    => (Maybe RootDeviceType
    -> f (Maybe RootDeviceType))
    -> CloneStack
    -> f CloneStack
csrDefaultRootDeviceType f x =
    (\y -> x { _csrDefaultRootDeviceType = y })
       <$> f (_csrDefaultRootDeviceType x)
{-# INLINE csrDefaultRootDeviceType #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
csrCustomCookbooksSource
    :: Functor f
    => (Maybe Source
    -> f (Maybe Source))
    -> CloneStack
    -> f CloneStack
csrCustomCookbooksSource f x =
    (\y -> x { _csrCustomCookbooksSource = y })
       <$> f (_csrCustomCookbooksSource x)
{-# INLINE csrCustomCookbooksSource #-}

-- | A list of stack attributes and values as key/value pairs to be added to the
-- cloned stack.
csrAttributes
    :: Functor f
    => (Map StackAttributesKeys Text
    -> f (Map StackAttributesKeys Text))
    -> CloneStack
    -> f CloneStack
csrAttributes f x =
    (\y -> x { _csrAttributes = y })
       <$> f (_csrAttributes x)
{-# INLINE csrAttributes #-}

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
csrConfigurationManager
    :: Functor f
    => (Maybe StackConfigurationManager
    -> f (Maybe StackConfigurationManager))
    -> CloneStack
    -> f CloneStack
csrConfigurationManager f x =
    (\y -> x { _csrConfigurationManager = y })
       <$> f (_csrConfigurationManager x)
{-# INLINE csrConfigurationManager #-}

-- | The cloned stack name.
csrName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrName f x =
    (\y -> x { _csrName = y })
       <$> f (_csrName x)
{-# INLINE csrName #-}

-- | The cloned stack AWS region, such as "us-east-1". For more information
-- about AWS regions, see Regions and Endpoints.
csrRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrRegion f x =
    (\y -> x { _csrRegion = y })
       <$> f (_csrRegion x)
{-# INLINE csrRegion #-}

-- | The ID of the VPC that the cloned stack is to be launched into. It must be
-- in the specified region. All instances will be launched into this VPC, and
-- you cannot change the ID later. If your account supports EC2 Classic, the
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
csrVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrVpcId f x =
    (\y -> x { _csrVpcId = y })
       <$> f (_csrVpcId x)
{-# INLINE csrVpcId #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
csrDefaultInstanceProfileArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrDefaultInstanceProfileArn f x =
    (\y -> x { _csrDefaultInstanceProfileArn = y })
       <$> f (_csrDefaultInstanceProfileArn x)
{-# INLINE csrDefaultInstanceProfileArn #-}

-- | The cloned stack's default operating system, which must be set to Amazon
-- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
csrDefaultOs
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrDefaultOs f x =
    (\y -> x { _csrDefaultOs = y })
       <$> f (_csrDefaultOs x)
{-# INLINE csrDefaultOs #-}

-- | The stack's host name theme, with spaces are replaced by underscores. The
-- theme is used to generate host names for the stack's instances. By default,
-- HostnameTheme is set to Layer_Dependent, which creates host names by
-- appending integers to the layer's short name. The other themes are:
-- Baked_Goods Clouds European_Cities Fruits Greek_Deities
-- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
-- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
-- GetHostNameSuggestion, which returns a host name based on the current
-- theme.
csrHostnameTheme
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrHostnameTheme f x =
    (\y -> x { _csrHostnameTheme = y })
       <$> f (_csrHostnameTheme x)
{-# INLINE csrHostnameTheme #-}

-- | The cloned stack's default Availability Zone, which must be in the
-- specified region. For more information, see Regions and Endpoints. If you
-- also specify a value for DefaultSubnetId, the subnet must be in the same
-- zone. For more information, see the VpcId parameter description.
csrDefaultAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrDefaultAvailabilityZone f x =
    (\y -> x { _csrDefaultAvailabilityZone = y })
       <$> f (_csrDefaultAvailabilityZone x)
{-# INLINE csrDefaultAvailabilityZone #-}

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in the
-- same zone. For information on default values and when this parameter is
-- required, see the VpcId parameter description.
csrDefaultSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrDefaultSubnetId f x =
    (\y -> x { _csrDefaultSubnetId = y })
       <$> f (_csrDefaultSubnetId x)
{-# INLINE csrDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
csrCustomJson
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrCustomJson f x =
    (\y -> x { _csrCustomJson = y })
       <$> f (_csrCustomJson x)
{-# INLINE csrCustomJson #-}

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
csrDefaultSshKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStack
    -> f CloneStack
csrDefaultSshKeyName f x =
    (\y -> x { _csrDefaultSshKeyName = y })
       <$> f (_csrDefaultSshKeyName x)
{-# INLINE csrDefaultSshKeyName #-}

-- | A list of source stack app IDs to be included in the cloned stack.
csrCloneAppIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CloneStack
    -> f CloneStack
csrCloneAppIds f x =
    (\y -> x { _csrCloneAppIds = y })
       <$> f (_csrCloneAppIds x)
{-# INLINE csrCloneAppIds #-}

instance ToPath CloneStack

instance ToQuery CloneStack

instance ToHeaders CloneStack

instance ToJSON CloneStack

data CloneStackResponse = CloneStackResponse
    { _cssStackId :: Maybe Text
      -- ^ The cloned stack ID.
    } deriving (Show, Generic)

-- | The cloned stack ID.
cssStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CloneStackResponse
    -> f CloneStackResponse
cssStackId f x =
    (\y -> x { _cssStackId = y })
       <$> f (_cssStackId x)
{-# INLINE cssStackId #-}

instance FromJSON CloneStackResponse

instance AWSRequest CloneStack where
    type Sv CloneStack = OpsWorks
    type Rs CloneStack = CloneStackResponse

    request = get
    response _ = jsonResponse
