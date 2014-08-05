{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.CreateStack where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateStack' request.
createStack :: Text -- ^ '_csrDefaultInstanceProfileArn'
            -> Text -- ^ '_csrServiceRoleArn'
            -> Text -- ^ '_csrName'
            -> Text -- ^ '_csrRegion'
            -> CreateStack
createStack p1 p2 p3 p4 = CreateStack
    { _csrDefaultInstanceProfileArn = p1
    , _csrServiceRoleArn = p2
    , _csrName = p3
    , _csrRegion = p4
    , _csrUseOpsworksSecurityGroups = Nothing
    , _csrUseCustomCookbooks = Nothing
    , _csrChefConfiguration = Nothing
    , _csrDefaultRootDeviceType = Nothing
    , _csrCustomCookbooksSource = Nothing
    , _csrAttributes = mempty
    , _csrConfigurationManager = Nothing
    , _csrVpcId = Nothing
    , _csrDefaultSshKeyName = Nothing
    , _csrCustomJson = Nothing
    , _csrDefaultAvailabilityZone = Nothing
    , _csrDefaultOs = Nothing
    , _csrDefaultSubnetId = Nothing
    , _csrHostnameTheme = Nothing
    }

data CreateStack = CreateStack
    { _csrDefaultInstanceProfileArn :: Text
      -- ^ The ARN of an IAM profile that is the default profile for all of
      -- the stack's EC2 instances. For more information about IAM ARNs,
      -- see Using Identifiers.
    , _csrServiceRoleArn :: Text
      -- ^ The stack AWS Identity and Access Management (IAM) role, which
      -- allows AWS OpsWorks to work with AWS resources on your behalf.
      -- You must set this parameter to the Amazon Resource Name (ARN) for
      -- an existing IAM role. For more information about IAM ARNs, see
      -- Using Identifiers.
    , _csrName :: Text
      -- ^ The stack name.
    , _csrRegion :: Text
      -- ^ The stack AWS region, such as "us-east-1". For more information
      -- about Amazon regions, see Regions and Endpoints.
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
    , _csrUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , _csrChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version on Chef 11.10 stacks. For
      -- more information, see Create a New Stack.
    , _csrDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the stack, but you can override it when you
      -- create an instance. The default option is instance-store. For
      -- more information, see Storage for the Root Device.
    , _csrCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _csrAttributes :: HashMap StackAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _csrConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend
      -- that you use the configuration manager to specify the Chef
      -- version, 0.9, 11.4, or 11.10. The default value is currently
      -- 11.4.
    , _csrVpcId :: Maybe Text
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
    , _csrDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this
      -- value when you create or update an instance.
    , _csrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _csrDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone, which must be in the
      -- specified region. For more information, see Regions and
      -- Endpoints. If you also specify a value for DefaultSubnetId, the
      -- subnet must be in the same zone. For more information, see the
      -- VpcId parameter description.
    , _csrDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon
      -- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , _csrDefaultSubnetId :: Maybe Text
      -- ^ The stack's default subnet ID. All instances will be launched
      -- into this subnet unless you specify otherwise when you create the
      -- instance. If you also specify a value for
      -- DefaultAvailabilityZone, the subnet must be in that zone. For
      -- information on default values and when this parameter is
      -- required, see the VpcId parameter description.
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
    } deriving (Show, Generic)

makeLenses ''CreateStack

instance ToPath CreateStack

instance ToQuery CreateStack

instance ToHeaders CreateStack

instance ToJSON CreateStack

data CreateStackResponse = CreateStackResponse
    { _cssStackId :: Maybe Text
      -- ^ The stack ID, which is an opaque string that you use to identify
      -- the stack when performing actions such as DescribeStacks.
    } deriving (Show, Generic)

makeLenses ''CreateStackResponse

instance FromJSON CreateStackResponse

instance AWSRequest CreateStack where
    type Sv CreateStack = OpsWorks
    type Rs CreateStack = CreateStackResponse

    request = get
    response _ = undefined
