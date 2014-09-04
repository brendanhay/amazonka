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
    , mkCloneStackRequest
    -- ** Request lenses
    , csrSourceStackId
    , csrName
    , csrRegion
    , csrVpcId
    , csrAttributes
    , csrServiceRoleArn
    , csrDefaultInstanceProfileArn
    , csrDefaultOs
    , csrHostnameTheme
    , csrDefaultAvailabilityZone
    , csrDefaultSubnetId
    , csrCustomJson
    , csrConfigurationManager
    , csrChefConfiguration
    , csrUseCustomCookbooks
    , csrUseOpsworksSecurityGroups
    , csrCustomCookbooksSource
    , csrDefaultSshKeyName
    , csrClonePermissions
    , csrCloneAppIds
    , csrDefaultRootDeviceType

    -- * Response
    , CloneStackResponse
    -- ** Response lenses
    , cssStackId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CloneStack' request.
mkCloneStackRequest :: Text -- ^ 'csrSourceStackId'
                    -> Text -- ^ 'csrServiceRoleArn'
                    -> CloneStack
mkCloneStackRequest p1 p2 = CloneStack
    { _csrSourceStackId = p1
    , _csrName = Nothing
    , _csrRegion = Nothing
    , _csrVpcId = Nothing
    , _csrAttributes = mempty
    , _csrServiceRoleArn = p6
    , _csrDefaultInstanceProfileArn = Nothing
    , _csrDefaultOs = Nothing
    , _csrHostnameTheme = Nothing
    , _csrDefaultAvailabilityZone = Nothing
    , _csrDefaultSubnetId = Nothing
    , _csrCustomJson = Nothing
    , _csrConfigurationManager = Nothing
    , _csrChefConfiguration = Nothing
    , _csrUseCustomCookbooks = Nothing
    , _csrUseOpsworksSecurityGroups = Nothing
    , _csrCustomCookbooksSource = Nothing
    , _csrDefaultSshKeyName = Nothing
    , _csrClonePermissions = Nothing
    , _csrCloneAppIds = mempty
    , _csrDefaultRootDeviceType = Nothing
    }
{-# INLINE mkCloneStackRequest #-}

data CloneStack = CloneStack
    { _csrSourceStackId :: Text
      -- ^ The source stack ID.
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
    , _csrAttributes :: Map StackAttributesKeys Text
      -- ^ A list of stack attributes and values as key/value pairs to be
      -- added to the cloned stack.
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
    , _csrConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend
      -- that you use the configuration manager to specify the Chef
      -- version, 0.9, 11.4, or 11.10. The default value is currently
      -- 11.4.
    , _csrChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version on Chef 11.10 stacks. For
      -- more information, see Create a New Stack.
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
    , _csrCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _csrDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this
      -- value when you create or update an instance.
    , _csrClonePermissions :: Maybe Bool
      -- ^ Whether to clone the source stack's permissions.
    , _csrCloneAppIds :: [Text]
      -- ^ A list of source stack app IDs to be included in the cloned
      -- stack.
    , _csrDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the cloned stack, but you can override it when
      -- you create an instance. For more information, see Storage for the
      -- Root Device.
    } deriving (Show, Generic)

-- | The source stack ID.
csrSourceStackId :: Lens' CloneStack (Text)
csrSourceStackId = lens _csrSourceStackId (\s a -> s { _csrSourceStackId = a })
{-# INLINE csrSourceStackId #-}

-- | The cloned stack name.
csrName :: Lens' CloneStack (Maybe Text)
csrName = lens _csrName (\s a -> s { _csrName = a })
{-# INLINE csrName #-}

-- | The cloned stack AWS region, such as "us-east-1". For more information
-- about AWS regions, see Regions and Endpoints.
csrRegion :: Lens' CloneStack (Maybe Text)
csrRegion = lens _csrRegion (\s a -> s { _csrRegion = a })
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
csrVpcId :: Lens' CloneStack (Maybe Text)
csrVpcId = lens _csrVpcId (\s a -> s { _csrVpcId = a })
{-# INLINE csrVpcId #-}

-- | A list of stack attributes and values as key/value pairs to be added to the
-- cloned stack.
csrAttributes :: Lens' CloneStack (Map StackAttributesKeys Text)
csrAttributes = lens _csrAttributes (\s a -> s { _csrAttributes = a })
{-# INLINE csrAttributes #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. If
-- you create a stack by using the AWS OpsWorks console, it creates the role
-- for you. You can obtain an existing stack's IAM ARN programmatically by
-- calling DescribePermissions. For more information about IAM ARNs, see Using
-- Identifiers. You must set this parameter to a valid service role ARN or the
-- action will fail; there is no default value. You can specify the source
-- stack's service role ARN, if you prefer, but you must do so explicitly.
csrServiceRoleArn :: Lens' CloneStack (Text)
csrServiceRoleArn = lens _csrServiceRoleArn (\s a -> s { _csrServiceRoleArn = a })
{-# INLINE csrServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
csrDefaultInstanceProfileArn :: Lens' CloneStack (Maybe Text)
csrDefaultInstanceProfileArn = lens _csrDefaultInstanceProfileArn (\s a -> s { _csrDefaultInstanceProfileArn = a })
{-# INLINE csrDefaultInstanceProfileArn #-}

-- | The cloned stack's default operating system, which must be set to Amazon
-- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
csrDefaultOs :: Lens' CloneStack (Maybe Text)
csrDefaultOs = lens _csrDefaultOs (\s a -> s { _csrDefaultOs = a })
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
csrHostnameTheme :: Lens' CloneStack (Maybe Text)
csrHostnameTheme = lens _csrHostnameTheme (\s a -> s { _csrHostnameTheme = a })
{-# INLINE csrHostnameTheme #-}

-- | The cloned stack's default Availability Zone, which must be in the
-- specified region. For more information, see Regions and Endpoints. If you
-- also specify a value for DefaultSubnetId, the subnet must be in the same
-- zone. For more information, see the VpcId parameter description.
csrDefaultAvailabilityZone :: Lens' CloneStack (Maybe Text)
csrDefaultAvailabilityZone = lens _csrDefaultAvailabilityZone (\s a -> s { _csrDefaultAvailabilityZone = a })
{-# INLINE csrDefaultAvailabilityZone #-}

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in the
-- same zone. For information on default values and when this parameter is
-- required, see the VpcId parameter description.
csrDefaultSubnetId :: Lens' CloneStack (Maybe Text)
csrDefaultSubnetId = lens _csrDefaultSubnetId (\s a -> s { _csrDefaultSubnetId = a })
{-# INLINE csrDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
csrCustomJson :: Lens' CloneStack (Maybe Text)
csrCustomJson = lens _csrCustomJson (\s a -> s { _csrCustomJson = a })
{-# INLINE csrCustomJson #-}

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
csrConfigurationManager :: Lens' CloneStack (Maybe StackConfigurationManager)
csrConfigurationManager = lens _csrConfigurationManager (\s a -> s { _csrConfigurationManager = a })
{-# INLINE csrConfigurationManager #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
csrChefConfiguration :: Lens' CloneStack (Maybe ChefConfiguration)
csrChefConfiguration = lens _csrChefConfiguration (\s a -> s { _csrChefConfiguration = a })
{-# INLINE csrChefConfiguration #-}

-- | Whether to use custom cookbooks.
csrUseCustomCookbooks :: Lens' CloneStack (Maybe Bool)
csrUseCustomCookbooks = lens _csrUseCustomCookbooks (\s a -> s { _csrUseCustomCookbooks = a })
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
csrUseOpsworksSecurityGroups :: Lens' CloneStack (Maybe Bool)
csrUseOpsworksSecurityGroups = lens _csrUseOpsworksSecurityGroups (\s a -> s { _csrUseOpsworksSecurityGroups = a })
{-# INLINE csrUseOpsworksSecurityGroups #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
csrCustomCookbooksSource :: Lens' CloneStack (Maybe Source)
csrCustomCookbooksSource = lens _csrCustomCookbooksSource (\s a -> s { _csrCustomCookbooksSource = a })
{-# INLINE csrCustomCookbooksSource #-}

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
csrDefaultSshKeyName :: Lens' CloneStack (Maybe Text)
csrDefaultSshKeyName = lens _csrDefaultSshKeyName (\s a -> s { _csrDefaultSshKeyName = a })
{-# INLINE csrDefaultSshKeyName #-}

-- | Whether to clone the source stack's permissions.
csrClonePermissions :: Lens' CloneStack (Maybe Bool)
csrClonePermissions = lens _csrClonePermissions (\s a -> s { _csrClonePermissions = a })
{-# INLINE csrClonePermissions #-}

-- | A list of source stack app IDs to be included in the cloned stack.
csrCloneAppIds :: Lens' CloneStack ([Text])
csrCloneAppIds = lens _csrCloneAppIds (\s a -> s { _csrCloneAppIds = a })
{-# INLINE csrCloneAppIds #-}

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
csrDefaultRootDeviceType :: Lens' CloneStack (Maybe RootDeviceType)
csrDefaultRootDeviceType = lens _csrDefaultRootDeviceType (\s a -> s { _csrDefaultRootDeviceType = a })
{-# INLINE csrDefaultRootDeviceType #-}

instance ToPath CloneStack

instance ToQuery CloneStack

instance ToHeaders CloneStack

instance ToJSON CloneStack

newtype CloneStackResponse = CloneStackResponse
    { _cssStackId :: Maybe Text
      -- ^ The cloned stack ID.
    } deriving (Show, Generic)

-- | The cloned stack ID.
cssStackId :: Lens' CloneStackResponse (Maybe Text)
cssStackId = lens _cssStackId (\s a -> s { _cssStackId = a })
{-# INLINE cssStackId #-}

instance FromJSON CloneStackResponse

instance AWSRequest CloneStack where
    type Sv CloneStack = OpsWorks
    type Rs CloneStack = CloneStackResponse

    request = get
    response _ = jsonResponse
