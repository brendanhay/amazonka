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
    , mkUpdateStackRequest
    -- ** Request lenses
    , usrStackId
    , usrName
    , usrAttributes
    , usrServiceRoleArn
    , usrDefaultInstanceProfileArn
    , usrDefaultOs
    , usrHostnameTheme
    , usrDefaultAvailabilityZone
    , usrDefaultSubnetId
    , usrCustomJson
    , usrConfigurationManager
    , usrChefConfiguration
    , usrUseCustomCookbooks
    , usrCustomCookbooksSource
    , usrDefaultSshKeyName
    , usrDefaultRootDeviceType
    , usrUseOpsworksSecurityGroups

    -- * Response
    , UpdateStackResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStack' request.
mkUpdateStackRequest :: Text -- ^ 'usrStackId'
                     -> UpdateStack
mkUpdateStackRequest p1 = UpdateStack
    { _usrStackId = p1
    , _usrName = Nothing
    , _usrAttributes = mempty
    , _usrServiceRoleArn = Nothing
    , _usrDefaultInstanceProfileArn = Nothing
    , _usrDefaultOs = Nothing
    , _usrHostnameTheme = Nothing
    , _usrDefaultAvailabilityZone = Nothing
    , _usrDefaultSubnetId = Nothing
    , _usrCustomJson = Nothing
    , _usrConfigurationManager = Nothing
    , _usrChefConfiguration = Nothing
    , _usrUseCustomCookbooks = Nothing
    , _usrCustomCookbooksSource = Nothing
    , _usrDefaultSshKeyName = Nothing
    , _usrDefaultRootDeviceType = Nothing
    , _usrUseOpsworksSecurityGroups = Nothing
    }
{-# INLINE mkUpdateStackRequest #-}

data UpdateStack = UpdateStack
    { _usrStackId :: Text
      -- ^ The stack ID.
    , _usrName :: Maybe Text
      -- ^ The stack's new name.
    , _usrAttributes :: Map StackAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
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
    , _usrConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend
      -- that you use the configuration manager to specify the Chef
      -- version, 0.9, 11.4, or 11.10. The default value is currently
      -- 11.4.
    , _usrChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version on Chef 11.10 stacks. For
      -- more information, see Create a New Stack.
    , _usrUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , _usrCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _usrDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this
      -- value when you create or update an instance.
    , _usrDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the stack, but you can override it when you
      -- create an instance. For more information, see Storage for the
      -- Root Device.
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
    } deriving (Show, Generic)

-- | The stack ID.
usrStackId :: Lens' UpdateStack (Text)
usrStackId = lens _usrStackId (\s a -> s { _usrStackId = a })
{-# INLINE usrStackId #-}

-- | The stack's new name.
usrName :: Lens' UpdateStack (Maybe Text)
usrName = lens _usrName (\s a -> s { _usrName = a })
{-# INLINE usrName #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
usrAttributes :: Lens' UpdateStack (Map StackAttributesKeys Text)
usrAttributes = lens _usrAttributes (\s a -> s { _usrAttributes = a })
{-# INLINE usrAttributes #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see Using Identifiers. You must set this
-- parameter to a valid service role ARN or the action will fail; there is no
-- default value. You can specify the stack's current service role ARN, if you
-- prefer, but you must do so explicitly.
usrServiceRoleArn :: Lens' UpdateStack (Maybe Text)
usrServiceRoleArn = lens _usrServiceRoleArn (\s a -> s { _usrServiceRoleArn = a })
{-# INLINE usrServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
usrDefaultInstanceProfileArn :: Lens' UpdateStack (Maybe Text)
usrDefaultInstanceProfileArn = lens _usrDefaultInstanceProfileArn (\s a -> s { _usrDefaultInstanceProfileArn = a })
{-# INLINE usrDefaultInstanceProfileArn #-}

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
usrDefaultOs :: Lens' UpdateStack (Maybe Text)
usrDefaultOs = lens _usrDefaultOs (\s a -> s { _usrDefaultOs = a })
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
usrHostnameTheme = lens _usrHostnameTheme (\s a -> s { _usrHostnameTheme = a })
{-# INLINE usrHostnameTheme #-}

-- | The stack's default Availability Zone, which must be in the specified
-- region. For more information, see Regions and Endpoints. If you also
-- specify a value for DefaultSubnetId, the subnet must be in the same zone.
-- For more information, see CreateStack.
usrDefaultAvailabilityZone :: Lens' UpdateStack (Maybe Text)
usrDefaultAvailabilityZone = lens _usrDefaultAvailabilityZone (\s a -> s { _usrDefaultAvailabilityZone = a })
{-# INLINE usrDefaultAvailabilityZone #-}

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in
-- that zone. For more information, see CreateStack.
usrDefaultSubnetId :: Lens' UpdateStack (Maybe Text)
usrDefaultSubnetId = lens _usrDefaultSubnetId (\s a -> s { _usrDefaultSubnetId = a })
{-# INLINE usrDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
usrCustomJson :: Lens' UpdateStack (Maybe Text)
usrCustomJson = lens _usrCustomJson (\s a -> s { _usrCustomJson = a })
{-# INLINE usrCustomJson #-}

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
usrConfigurationManager :: Lens' UpdateStack (Maybe StackConfigurationManager)
usrConfigurationManager = lens _usrConfigurationManager (\s a -> s { _usrConfigurationManager = a })
{-# INLINE usrConfigurationManager #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
usrChefConfiguration :: Lens' UpdateStack (Maybe ChefConfiguration)
usrChefConfiguration = lens _usrChefConfiguration (\s a -> s { _usrChefConfiguration = a })
{-# INLINE usrChefConfiguration #-}

-- | Whether the stack uses custom cookbooks.
usrUseCustomCookbooks :: Lens' UpdateStack (Maybe Bool)
usrUseCustomCookbooks = lens _usrUseCustomCookbooks (\s a -> s { _usrUseCustomCookbooks = a })
{-# INLINE usrUseCustomCookbooks #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
usrCustomCookbooksSource :: Lens' UpdateStack (Maybe Source)
usrCustomCookbooksSource = lens _usrCustomCookbooksSource (\s a -> s { _usrCustomCookbooksSource = a })
{-# INLINE usrCustomCookbooksSource #-}

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
usrDefaultSshKeyName :: Lens' UpdateStack (Maybe Text)
usrDefaultSshKeyName = lens _usrDefaultSshKeyName (\s a -> s { _usrDefaultSshKeyName = a })
{-# INLINE usrDefaultSshKeyName #-}

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
usrDefaultRootDeviceType :: Lens' UpdateStack (Maybe RootDeviceType)
usrDefaultRootDeviceType = lens _usrDefaultRootDeviceType (\s a -> s { _usrDefaultRootDeviceType = a })
{-# INLINE usrDefaultRootDeviceType #-}

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
usrUseOpsworksSecurityGroups = lens _usrUseOpsworksSecurityGroups (\s a -> s { _usrUseOpsworksSecurityGroups = a })
{-# INLINE usrUseOpsworksSecurityGroups #-}

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
