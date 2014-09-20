{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
    , usStackId
    , usName
    , usAttributes
    , usServiceRoleArn
    , usDefaultInstanceProfileArn
    , usDefaultOs
    , usHostnameTheme
    , usDefaultAvailabilityZone
    , usDefaultSubnetId
    , usCustomJson
    , usConfigurationManager
    , usChefConfiguration
    , usUseCustomCookbooks
    , usCustomCookbooksSource
    , usDefaultSshKeyName
    , usDefaultRootDeviceType
    , usUseOpsworksSecurityGroups

    -- * Response
    , UpdateStackResponse
    -- ** Response constructor
    , updateStackResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateStack = UpdateStack
    { _usStackId :: Text
    , _usName :: Maybe Text
    , _usAttributes :: Map StackAttributesKeys Text
    , _usServiceRoleArn :: Maybe Text
    , _usDefaultInstanceProfileArn :: Maybe Text
    , _usDefaultOs :: Maybe Text
    , _usHostnameTheme :: Maybe Text
    , _usDefaultAvailabilityZone :: Maybe Text
    , _usDefaultSubnetId :: Maybe Text
    , _usCustomJson :: Maybe Text
    , _usConfigurationManager :: Maybe StackConfigurationManager
    , _usChefConfiguration :: Maybe ChefConfiguration
    , _usUseCustomCookbooks :: Maybe Bool
    , _usCustomCookbooksSource :: Maybe Source'
    , _usDefaultSshKeyName :: Maybe Text
    , _usDefaultRootDeviceType :: Maybe RootDeviceType
    , _usUseOpsworksSecurityGroups :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStack' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map StackAttributesKeys Text@
--
-- * @ServiceRoleArn ::@ @Maybe Text@
--
-- * @DefaultInstanceProfileArn ::@ @Maybe Text@
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
-- * @CustomCookbooksSource ::@ @Maybe Source'@
--
-- * @DefaultSshKeyName ::@ @Maybe Text@
--
-- * @DefaultRootDeviceType ::@ @Maybe RootDeviceType@
--
-- * @UseOpsworksSecurityGroups ::@ @Maybe Bool@
--
updateStack :: Text -- ^ 'usStackId'
            -> UpdateStack
updateStack p1 = UpdateStack
    { _usStackId = p1
    , _usName = Nothing
    , _usAttributes = mempty
    , _usServiceRoleArn = Nothing
    , _usDefaultInstanceProfileArn = Nothing
    , _usDefaultOs = Nothing
    , _usHostnameTheme = Nothing
    , _usDefaultAvailabilityZone = Nothing
    , _usDefaultSubnetId = Nothing
    , _usCustomJson = Nothing
    , _usConfigurationManager = Nothing
    , _usChefConfiguration = Nothing
    , _usUseCustomCookbooks = Nothing
    , _usCustomCookbooksSource = Nothing
    , _usDefaultSshKeyName = Nothing
    , _usDefaultRootDeviceType = Nothing
    , _usUseOpsworksSecurityGroups = Nothing
    }

-- | The stack ID.
usStackId :: Lens' UpdateStack Text
usStackId = lens _usStackId (\s a -> s { _usStackId = a })

-- | The stack's new name.
usName :: Lens' UpdateStack (Maybe Text)
usName = lens _usName (\s a -> s { _usName = a })

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
usAttributes :: Lens' UpdateStack (Map StackAttributesKeys Text)
usAttributes = lens _usAttributes (\s a -> s { _usAttributes = a })

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS
-- OpsWorks to work with AWS resources on your behalf. You must set this
-- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
-- more information about IAM ARNs, see Using Identifiers. You must set this
-- parameter to a valid service role ARN or the action will fail; there is no
-- default value. You can specify the stack's current service role ARN, if you
-- prefer, but you must do so explicitly.
usServiceRoleArn :: Lens' UpdateStack (Maybe Text)
usServiceRoleArn =
    lens _usServiceRoleArn (\s a -> s { _usServiceRoleArn = a })

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
usDefaultInstanceProfileArn :: Lens' UpdateStack (Maybe Text)
usDefaultInstanceProfileArn =
    lens _usDefaultInstanceProfileArn
         (\s a -> s { _usDefaultInstanceProfileArn = a })

-- | The stack's default operating system, which must be set to Amazon Linux,
-- Ubuntu 12.04 LTS, or Ubuntu 14.04 LTS. The default option is Amazon Linux.
usDefaultOs :: Lens' UpdateStack (Maybe Text)
usDefaultOs = lens _usDefaultOs (\s a -> s { _usDefaultOs = a })

-- | The stack's new host name theme, with spaces are replaced by underscores.
-- The theme is used to generate host names for the stack's instances. By
-- default, HostnameTheme is set to Layer_Dependent, which creates host names
-- by appending integers to the layer's short name. The other themes are:
-- Baked_Goods Clouds European_Cities Fruits Greek_Deities
-- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
-- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
-- GetHostNameSuggestion, which returns a host name based on the current
-- theme.
usHostnameTheme :: Lens' UpdateStack (Maybe Text)
usHostnameTheme = lens _usHostnameTheme (\s a -> s { _usHostnameTheme = a })

-- | The stack's default Availability Zone, which must be in the specified
-- region. For more information, see Regions and Endpoints. If you also
-- specify a value for DefaultSubnetId, the subnet must be in the same zone.
-- For more information, see CreateStack.
usDefaultAvailabilityZone :: Lens' UpdateStack (Maybe Text)
usDefaultAvailabilityZone =
    lens _usDefaultAvailabilityZone
         (\s a -> s { _usDefaultAvailabilityZone = a })

-- | The stack's default VPC subnet ID. This parameter is required if you
-- specify a value for the VpcId parameter. All instances are launched into
-- this subnet unless you specify otherwise when you create the instance. If
-- you also specify a value for DefaultAvailabilityZone, the subnet must be in
-- that zone. For information on default values and when this parameter is
-- required, see the VpcId parameter description.
usDefaultSubnetId :: Lens' UpdateStack (Maybe Text)
usDefaultSubnetId =
    lens _usDefaultSubnetId (\s a -> s { _usDefaultSubnetId = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
usCustomJson :: Lens' UpdateStack (Maybe Text)
usCustomJson = lens _usCustomJson (\s a -> s { _usCustomJson = a })

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
usConfigurationManager :: Lens' UpdateStack (Maybe StackConfigurationManager)
usConfigurationManager =
    lens _usConfigurationManager (\s a -> s { _usConfigurationManager = a })

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
usChefConfiguration :: Lens' UpdateStack (Maybe ChefConfiguration)
usChefConfiguration =
    lens _usChefConfiguration (\s a -> s { _usChefConfiguration = a })

-- | Whether the stack uses custom cookbooks.
usUseCustomCookbooks :: Lens' UpdateStack (Maybe Bool)
usUseCustomCookbooks =
    lens _usUseCustomCookbooks (\s a -> s { _usUseCustomCookbooks = a })

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
usCustomCookbooksSource :: Lens' UpdateStack (Maybe Source')
usCustomCookbooksSource =
    lens _usCustomCookbooksSource
         (\s a -> s { _usCustomCookbooksSource = a })

-- | A default SSH key for the stack instances. You can override this value when
-- you create or update an instance.
usDefaultSshKeyName :: Lens' UpdateStack (Maybe Text)
usDefaultSshKeyName =
    lens _usDefaultSshKeyName (\s a -> s { _usDefaultSshKeyName = a })

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
usDefaultRootDeviceType :: Lens' UpdateStack (Maybe RootDeviceType)
usDefaultRootDeviceType =
    lens _usDefaultRootDeviceType
         (\s a -> s { _usDefaultRootDeviceType = a })

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
usUseOpsworksSecurityGroups :: Lens' UpdateStack (Maybe Bool)
usUseOpsworksSecurityGroups =
    lens _usUseOpsworksSecurityGroups
         (\s a -> s { _usUseOpsworksSecurityGroups = a })

instance ToPath UpdateStack

instance ToQuery UpdateStack

instance ToHeaders UpdateStack

instance ToJSON UpdateStack

data UpdateStackResponse = UpdateStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStackResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
updateStackResponse :: UpdateStackResponse
updateStackResponse = UpdateStackResponse

instance AWSRequest UpdateStack where
    type Sv UpdateStack = OpsWorks
    type Rs UpdateStack = UpdateStackResponse

    request = get
    response _ = nullaryResponse UpdateStackResponse
