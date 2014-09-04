{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified stack. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateStack
    (
    -- * Request
      UpdateStack
    -- ** Request constructor
    , updateStack
    -- ** Request lenses
    , usrStackId
    , usrUseCustomCookbooks
    , usrUseOpsworksSecurityGroups
    , usrChefConfiguration
    , usrDefaultRootDeviceType
    , usrCustomCookbooksSource
    , usrAttributes
    , usrConfigurationManager
    , usrName
    , usrServiceRoleArn
    , usrDefaultInstanceProfileArn
    , usrDefaultOs
    , usrHostnameTheme
    , usrDefaultAvailabilityZone
    , usrDefaultSubnetId
    , usrCustomJson
    , usrDefaultSshKeyName

    -- * Response
    , UpdateStackResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateStack' request.
updateStack :: Text -- ^ 'usrStackId'
            -> UpdateStack
updateStack p1 = UpdateStack
    { _usrStackId = p1
    , _usrUseCustomCookbooks = Nothing
    , _usrUseOpsworksSecurityGroups = Nothing
    , _usrChefConfiguration = Nothing
    , _usrDefaultRootDeviceType = Nothing
    , _usrCustomCookbooksSource = Nothing
    , _usrAttributes = mempty
    , _usrConfigurationManager = Nothing
    , _usrName = Nothing
    , _usrServiceRoleArn = Nothing
    , _usrDefaultInstanceProfileArn = Nothing
    , _usrDefaultOs = Nothing
    , _usrHostnameTheme = Nothing
    , _usrDefaultAvailabilityZone = Nothing
    , _usrDefaultSubnetId = Nothing
    , _usrCustomJson = Nothing
    , _usrDefaultSshKeyName = Nothing
    }
{-# INLINE updateStack #-}

data UpdateStack = UpdateStack
    { _usrStackId :: Text
      -- ^ The stack ID.
    , _usrUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , _usrUseOpsworksSecurityGroups :: Maybe Bool
      -- ^ Whether to associate the AWS OpsWorks built-in security groups
      -- with the stack's layers. AWS OpsWorks provides a standard set of
      -- built-in security groups, one for each layer, which are
      -- associated with layers by default. UseOpsworksSecurityGroups
      -- allows you to instead provide your own custom security groups.
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
    , _usrChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version on Chef 11.10 stacks. For
      -- more information, see Create a New Stack.
    , _usrDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the stack, but you can override it when you
      -- create an instance. For more information, see Storage for the
      -- Root Device.
    , _usrCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _usrAttributes :: Map StackAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _usrConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend
      -- that you use the configuration manager to specify the Chef
      -- version, 0.9, 11.4, or 11.10. The default value is currently
      -- 11.4.
    , _usrName :: Maybe Text
      -- ^ The stack's new name.
    , _usrServiceRoleArn :: Maybe Text
      -- ^ The stack AWS Identity and Access Management (IAM) role, which
      -- allows AWS OpsWorks to work with AWS resources on your behalf.
      -- You must set this parameter to the Amazon Resource Name (ARN) for
      -- an existing IAM role. For more information about IAM ARNs, see
      -- Using Identifiers. You must set this parameter to a valid service
      -- role ARN or the action will fail; there is no default value. You
      -- can specify the stack's current service role ARN, if you prefer,
      -- but you must do so explicitly.
    , _usrDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of
      -- the stack's EC2 instances. For more information about IAM ARNs,
      -- see Using Identifiers.
    , _usrDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon
      -- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , _usrHostnameTheme :: Maybe Text
      -- ^ The stack's new host name theme, with spaces are replaced by
      -- underscores. The theme is used to generate host names for the
      -- stack's instances. By default, HostnameTheme is set to
      -- Layer_Dependent, which creates host names by appending integers
      -- to the layer's short name. The other themes are: Baked_Goods
      -- Clouds European_Cities Fruits Greek_Deities
      -- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
      -- Scottish_Islands US_Cities Wild_Cats To obtain a generated host
      -- name, call GetHostNameSuggestion, which returns a host name based
      -- on the current theme.
    , _usrDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone, which must be in the
      -- specified region. For more information, see Regions and
      -- Endpoints. If you also specify a value for DefaultSubnetId, the
      -- subnet must be in the same zone. For more information, see
      -- CreateStack.
    , _usrDefaultSubnetId :: Maybe Text
      -- ^ The stack's default subnet ID. All instances will be launched
      -- into this subnet unless you specify otherwise when you create the
      -- instance. If you also specify a value for
      -- DefaultAvailabilityZone, the subnet must be in that zone. For
      -- more information, see CreateStack.
    , _usrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _usrDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this
      -- value when you create or update an instance.
    } deriving (Show, Generic)

-- | The stack ID.
usrStackId :: Lens' UpdateStack (Text)
usrStackId f x =
    f (_usrStackId x)
        <&> \y -> x { _usrStackId = y }
{-# INLINE usrStackId #-}

-- | Whether the stack uses custom cookbooks.
usrUseCustomCookbooks :: Lens' UpdateStack (Maybe Bool)
usrUseCustomCookbooks f x =
    f (_usrUseCustomCookbooks x)
        <&> \y -> x { _usrUseCustomCookbooks = y }
{-# INLINE usrUseCustomCookbooks #-}

-- | Whether to associate the AWS OpsWorks built-in security groups with the
-- stack's layers. AWS OpsWorks provides a standard set of built-in security
-- groups, one for each layer, which are associated with layers by default.
-- UseOpsworksSecurityGroups allows you to instead provide your own custom
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
usrUseOpsworksSecurityGroups :: Lens' UpdateStack (Maybe Bool)
usrUseOpsworksSecurityGroups f x =
    f (_usrUseOpsworksSecurityGroups x)
        <&> \y -> x { _usrUseOpsworksSecurityGroups = y }
{-# INLINE usrUseOpsworksSecurityGroups #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
usrChefConfiguration :: Lens' UpdateStack (Maybe ChefConfiguration)
usrChefConfiguration f x =
    f (_usrChefConfiguration x)
        <&> \y -> x { _usrChefConfiguration = y }
{-# INLINE usrChefConfiguration #-}

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
usrDefaultRootDeviceType :: Lens' UpdateStack (Maybe RootDeviceType)
usrDefaultRootDeviceType f x =
    f (_usrDefaultRootDeviceType x)
        <&> \y -> x { _usrDefaultRootDeviceType = y }
{-# INLINE usrDefaultRootDeviceType #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
usrCustomCookbooksSource :: Lens' UpdateStack (Maybe Source)
usrCustomCookbooksSource f x =
    f (_usrCustomCookbooksSource x)
        <&> \y -> x { _usrCustomCookbooksSource = y }
{-# INLINE usrCustomCookbooksSource #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
usrAttributes :: Lens' UpdateStack (Map StackAttributesKeys Text)
usrAttributes f x =
    f (_usrAttributes x)
        <&> \y -> x { _usrAttributes = y }
{-# INLINE usrAttributes #-}

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
usrConfigurationManager :: Lens' UpdateStack (Maybe StackConfigurationManager)
usrConfigurationManager f x =
    f (_usrConfigurationManager x)
        <&> \y -> x { _usrConfigurationManager = y }
{-# INLINE usrConfigurationManager #-}

-- | The stack's new name.
usrName :: Lens' UpdateStack (Maybe Text)
usrName f x =
    f (_usrName x)
        <&> \y -> x { _usrName = y }
{-# INLINE usrName #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see Using Identifiers. You must set this
-- parameter to a valid service role ARN or the action will fail; there is no
-- default value. You can specify the stack's current service role ARN, if you
-- prefer, but you must do so explicitly.
usrServiceRoleArn :: Lens' UpdateStack (Maybe Text)
usrServiceRoleArn f x =
    f (_usrServiceRoleArn x)
        <&> \y -> x { _usrServiceRoleArn = y }
{-# INLINE usrServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
usrDefaultInstanceProfileArn :: Lens' UpdateStack (Maybe Text)
usrDefaultInstanceProfileArn f x =
    f (_usrDefaultInstanceProfileArn x)
        <&> \y -> x { _usrDefaultInstanceProfileArn = y }
{-# INLINE usrDefaultInstanceProfileArn #-}

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
usrDefaultOs :: Lens' UpdateStack (Maybe Text)
usrDefaultOs f x =
    f (_usrDefaultOs x)
        <&> \y -> x { _usrDefaultOs = y }
{-# INLINE usrDefaultOs #-}

-- | The stack's new host name theme, with spaces are replaced by underscores.
-- The theme is used to generate host names for the stack's instances. By
-- default, HostnameTheme is set to Layer_Dependent, which creates host names
-- by appending integers to the layer's short name. The other themes are:
-- Baked_Goods Clouds European_Cities Fruits Greek_Deities
-- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
-- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
-- GetHostNameSuggestion, which returns a host name based on the current
-- theme.
usrHostnameTheme :: Lens' UpdateStack (Maybe Text)
usrHostnameTheme f x =
    f (_usrHostnameTheme x)
        <&> \y -> x { _usrHostnameTheme = y }
{-# INLINE usrHostnameTheme #-}

-- | The stack's default Availability Zone, which must be in the specified
-- region. For more information, see Regions and Endpoints. If you also
-- specify a value for DefaultSubnetId, the subnet must be in the same zone.
-- For more information, see CreateStack.
usrDefaultAvailabilityZone :: Lens' UpdateStack (Maybe Text)
usrDefaultAvailabilityZone f x =
    f (_usrDefaultAvailabilityZone x)
        <&> \y -> x { _usrDefaultAvailabilityZone = y }
{-# INLINE usrDefaultAvailabilityZone #-}

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in
-- that zone. For more information, see CreateStack.
usrDefaultSubnetId :: Lens' UpdateStack (Maybe Text)
usrDefaultSubnetId f x =
    f (_usrDefaultSubnetId x)
        <&> \y -> x { _usrDefaultSubnetId = y }
{-# INLINE usrDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
usrCustomJson :: Lens' UpdateStack (Maybe Text)
usrCustomJson f x =
    f (_usrCustomJson x)
        <&> \y -> x { _usrCustomJson = y }
{-# INLINE usrCustomJson #-}

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
usrDefaultSshKeyName :: Lens' UpdateStack (Maybe Text)
usrDefaultSshKeyName f x =
    f (_usrDefaultSshKeyName x)
        <&> \y -> x { _usrDefaultSshKeyName = y }
{-# INLINE usrDefaultSshKeyName #-}

instance ToPath UpdateStack

instance ToQuery UpdateStack

instance ToHeaders UpdateStack

instance ToJSON UpdateStack

data UpdateStackResponse = UpdateStackResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateStack where
    type Sv UpdateStack = OpsWorks
    type Rs UpdateStack = UpdateStackResponse

    request = get
    response _ = nullaryResponse UpdateStackResponse
