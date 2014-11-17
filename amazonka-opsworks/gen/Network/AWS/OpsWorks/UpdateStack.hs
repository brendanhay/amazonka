{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateStack
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
module Network.AWS.OpsWorks.UpdateStack
    (
    -- * Request
      UpdateStack
    -- ** Request constructor
    , updateStack
    -- ** Request lenses
    , usAttributes
    , usChefConfiguration
    , usConfigurationManager
    , usCustomCookbooksSource
    , usCustomJson
    , usDefaultAvailabilityZone
    , usDefaultInstanceProfileArn
    , usDefaultOs
    , usDefaultRootDeviceType
    , usDefaultSshKeyName
    , usDefaultSubnetId
    , usHostnameTheme
    , usName
    , usServiceRoleArn
    , usStackId
    , usUseCustomCookbooks
    , usUseOpsworksSecurityGroups

    -- * Response
    , UpdateStackResponse
    -- ** Response constructor
    , updateStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateStack = UpdateStack
    { _usAttributes                :: Map Text Text
    , _usChefConfiguration         :: Maybe ChefConfiguration
    , _usConfigurationManager      :: Maybe StackConfigurationManager
    , _usCustomCookbooksSource     :: Maybe Source
    , _usCustomJson                :: Maybe Text
    , _usDefaultAvailabilityZone   :: Maybe Text
    , _usDefaultInstanceProfileArn :: Maybe Text
    , _usDefaultOs                 :: Maybe Text
    , _usDefaultRootDeviceType     :: Maybe Text
    , _usDefaultSshKeyName         :: Maybe Text
    , _usDefaultSubnetId           :: Maybe Text
    , _usHostnameTheme             :: Maybe Text
    , _usName                      :: Maybe Text
    , _usServiceRoleArn            :: Maybe Text
    , _usStackId                   :: Text
    , _usUseCustomCookbooks        :: Maybe Bool
    , _usUseOpsworksSecurityGroups :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'UpdateStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'usChefConfiguration' @::@ 'Maybe' 'ChefConfiguration'
--
-- * 'usConfigurationManager' @::@ 'Maybe' 'StackConfigurationManager'
--
-- * 'usCustomCookbooksSource' @::@ 'Maybe' 'Source'
--
-- * 'usCustomJson' @::@ 'Maybe' 'Text'
--
-- * 'usDefaultAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'usDefaultInstanceProfileArn' @::@ 'Maybe' 'Text'
--
-- * 'usDefaultOs' @::@ 'Maybe' 'Text'
--
-- * 'usDefaultRootDeviceType' @::@ 'Maybe' 'Text'
--
-- * 'usDefaultSshKeyName' @::@ 'Maybe' 'Text'
--
-- * 'usDefaultSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'usHostnameTheme' @::@ 'Maybe' 'Text'
--
-- * 'usName' @::@ 'Maybe' 'Text'
--
-- * 'usServiceRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'usStackId' @::@ 'Text'
--
-- * 'usUseCustomCookbooks' @::@ 'Maybe' 'Bool'
--
-- * 'usUseOpsworksSecurityGroups' @::@ 'Maybe' 'Bool'
--
updateStack :: Text -- ^ 'usStackId'
            -> UpdateStack
updateStack p1 = UpdateStack
    { _usStackId                   = p1
    , _usName                      = Nothing
    , _usAttributes                = mempty
    , _usServiceRoleArn            = Nothing
    , _usDefaultInstanceProfileArn = Nothing
    , _usDefaultOs                 = Nothing
    , _usHostnameTheme             = Nothing
    , _usDefaultAvailabilityZone   = Nothing
    , _usDefaultSubnetId           = Nothing
    , _usCustomJson                = Nothing
    , _usConfigurationManager      = Nothing
    , _usChefConfiguration         = Nothing
    , _usUseCustomCookbooks        = Nothing
    , _usCustomCookbooksSource     = Nothing
    , _usDefaultSshKeyName         = Nothing
    , _usDefaultRootDeviceType     = Nothing
    , _usUseOpsworksSecurityGroups = Nothing
    }

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
usAttributes :: Lens' UpdateStack (HashMap Text Text)
usAttributes = lens _usAttributes (\s a -> s { _usAttributes = a })
    . _Map

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
usChefConfiguration :: Lens' UpdateStack (Maybe ChefConfiguration)
usChefConfiguration =
    lens _usChefConfiguration (\s a -> s { _usChefConfiguration = a })

-- | The configuration manager. When you clone a stack we recommend that you
-- use the configuration manager to specify the Chef version, 0.9, 11.4, or
-- 11.10. The default value is currently 11.4.
usConfigurationManager :: Lens' UpdateStack (Maybe StackConfigurationManager)
usConfigurationManager =
    lens _usConfigurationManager (\s a -> s { _usConfigurationManager = a })

usCustomCookbooksSource :: Lens' UpdateStack (Maybe Source)
usCustomCookbooksSource =
    lens _usCustomCookbooksSource (\s a -> s { _usCustomCookbooksSource = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as
-- '"'.: "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more
-- information on custom JSON, see Use Custom JSON to Modify the Stack
-- Configuration JSON.
usCustomJson :: Lens' UpdateStack (Maybe Text)
usCustomJson = lens _usCustomJson (\s a -> s { _usCustomJson = a })

-- | The stack's default Availability Zone, which must be in the specified
-- region. For more information, see Regions and Endpoints. If you also
-- specify a value for DefaultSubnetId, the subnet must be in the same zone.
-- For more information, see CreateStack.
usDefaultAvailabilityZone :: Lens' UpdateStack (Maybe Text)
usDefaultAvailabilityZone =
    lens _usDefaultAvailabilityZone
        (\s a -> s { _usDefaultAvailabilityZone = a })

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
usDefaultInstanceProfileArn :: Lens' UpdateStack (Maybe Text)
usDefaultInstanceProfileArn =
    lens _usDefaultInstanceProfileArn
        (\s a -> s { _usDefaultInstanceProfileArn = a })

-- | The stack's default operating system, which must be set to Amazon Linux
-- or Ubuntu 12.04 LTS. The default option is Amazon Linux.
usDefaultOs :: Lens' UpdateStack (Maybe Text)
usDefaultOs = lens _usDefaultOs (\s a -> s { _usDefaultOs = a })

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
usDefaultRootDeviceType :: Lens' UpdateStack (Maybe Text)
usDefaultRootDeviceType =
    lens _usDefaultRootDeviceType (\s a -> s { _usDefaultRootDeviceType = a })

-- | A default SSH key for the stack instances. You can override this value
-- when you create or update an instance.
usDefaultSshKeyName :: Lens' UpdateStack (Maybe Text)
usDefaultSshKeyName =
    lens _usDefaultSshKeyName (\s a -> s { _usDefaultSshKeyName = a })

-- | The stack's default subnet ID. All instances will be launched into this
-- subnet unless you specify otherwise when you create the instance. If you
-- also specify a value for DefaultAvailabilityZone, the subnet must be in
-- that zone. For more information, see CreateStack.
usDefaultSubnetId :: Lens' UpdateStack (Maybe Text)
usDefaultSubnetId =
    lens _usDefaultSubnetId (\s a -> s { _usDefaultSubnetId = a })

-- | The stack's new host name theme, with spaces are replaced by underscores.
-- The theme is used to generate host names for the stack's instances. By
-- default, HostnameTheme is set to Layer_Dependent, which creates host
-- names by appending integers to the layer's short name. The other themes
-- are: Baked_Goods Clouds European_Cities Fruits Greek_Deities
-- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
-- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name,
-- call GetHostNameSuggestion, which returns a host name based on the
-- current theme.
usHostnameTheme :: Lens' UpdateStack (Maybe Text)
usHostnameTheme = lens _usHostnameTheme (\s a -> s { _usHostnameTheme = a })

-- | The stack's new name.
usName :: Lens' UpdateStack (Maybe Text)
usName = lens _usName (\s a -> s { _usName = a })

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see Using Identifiers.
usServiceRoleArn :: Lens' UpdateStack (Maybe Text)
usServiceRoleArn = lens _usServiceRoleArn (\s a -> s { _usServiceRoleArn = a })

-- | The stack ID.
usStackId :: Lens' UpdateStack Text
usStackId = lens _usStackId (\s a -> s { _usStackId = a })

-- | Whether the stack uses custom cookbooks.
usUseCustomCookbooks :: Lens' UpdateStack (Maybe Bool)
usUseCustomCookbooks =
    lens _usUseCustomCookbooks (\s a -> s { _usUseCustomCookbooks = a })

-- | Whether to associate the AWS OpsWorks built-in security groups with the
-- stack's layers. AWS OpsWorks provides a standard set of built-in security
-- groups, one for each layer, which are associated with layers by default.
-- UseOpsworksSecurityGroups allows you to instead provide your own custom
-- security groups. UseOpsworksSecurityGroups has the following settings:
-- True - AWS OpsWorks automatically associates the appropriate built-in
-- security group with each layer (default setting). You can associate
-- additional security groups with a layer after you create it but you
-- cannot delete the built-in security group. False - AWS OpsWorks does not
-- associate built-in security groups with layers. You must create
-- appropriate EC2 security groups and associate a security group with each
-- layer that you create. However, you can still manually associate a
-- built-in security group with a layer on creation; custom security groups
-- are required only for those layers that need custom settings. For more
-- information, see Create a New Stack.
usUseOpsworksSecurityGroups :: Lens' UpdateStack (Maybe Bool)
usUseOpsworksSecurityGroups =
    lens _usUseOpsworksSecurityGroups
        (\s a -> s { _usUseOpsworksSecurityGroups = a })

data UpdateStackResponse = UpdateStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateStackResponse' constructor.
updateStackResponse :: UpdateStackResponse
updateStackResponse = UpdateStackResponse

instance AWSRequest UpdateStack where
    type Sv UpdateStack = OpsWorks
    type Rs UpdateStack = UpdateStackResponse

    request  = post
    response = nullResponse UpdateStackResponse

instance ToPath UpdateStack where
    toPath = const "/"

instance ToHeaders UpdateStack

instance ToQuery UpdateStack where
    toQuery = const mempty

instance ToJSON UpdateStack where
    toJSON = genericToJSON jsonOptions
