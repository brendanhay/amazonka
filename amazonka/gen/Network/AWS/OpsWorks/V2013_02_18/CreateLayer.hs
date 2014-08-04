{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CreateLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a layer. For more information, see How to Create a Layer. You
-- should use CreateLayer for noncustom layer types such as PHP App Server
-- only if the stack does not have an existing layer of that type. A stack can
-- have at most one instance of each noncustom layer; if you attempt to create
-- a second instance, CreateLayer fails. A stack can have an arbitrary number
-- of custom layers, so you can call CreateLayer as many times as you like for
-- that layer type. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.CreateLayer where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateLayer' request.
createLayer :: LayerType -- ^ '_clrType'
            -> Text -- ^ '_clrShortname'
            -> Text -- ^ '_clrName'
            -> Text -- ^ '_clrStackId'
            -> CreateLayer
createLayer p1 p2 p3 p4 = CreateLayer
    { _clrType = p1
    , _clrShortname = p2
    , _clrName = p3
    , _clrStackId = p4
    , _clrInstallUpdatesOnBoot = Nothing
    , _clrEnableAutoHealing = Nothing
    , _clrAutoAssignPublicIps = Nothing
    , _clrUseEbsOptimizedInstances = Nothing
    , _clrAutoAssignElasticIps = Nothing
    , _clrAttributes = mempty
    , _clrCustomRecipes = Nothing
    , _clrCustomInstanceProfileArn = Nothing
    , _clrCustomSecurityGroupIds = mempty
    , _clrPackages = mempty
    , _clrVolumeConfigurations = mempty
    }

data CreateLayer = CreateLayer
    { _clrType :: LayerType
      -- ^ The layer type. A stack cannot have more than one built-in layer
      -- of the same type. It can have any number of custom layers. This
      -- parameter must be set to one of the following: custom: A custom
      -- layer db-master: A MySQL layer java-app: A Java App Server layer
      -- rails-app: A Rails App Server layer lb: An HAProxy layer
      -- memcached: A Memcached layer monitoring-master: A Ganglia layer
      -- nodejs-app: A Node.js App Server layer php-app: A PHP App Server
      -- layer web: A Static Web Server layer.
    , _clrShortname :: Text
      -- ^ The layer short name, which is used internally by AWS OpsWorks
      -- and by Chef recipes. The short name is also used as the name for
      -- the directory where your app files are installed. It can have a
      -- maximum of 200 characters, which are limited to the alphanumeric
      -- characters, '-', '_', and '.'.
    , _clrName :: Text
      -- ^ The layer name, which is used by the console.
    , _clrStackId :: Text
      -- ^ The layer stack ID.
    , _clrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. To control when
      -- updates are installed, set this value to false. You must then
      -- update your instances manually by using CreateDeployment to run
      -- the update_dependencies stack command or manually running yum
      -- (Amazon Linux) or apt-get (Ubuntu) on the instances. We strongly
      -- recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , _clrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , _clrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _clrUseEbsOptimizedInstances :: Maybe Bool
      -- ^ Whether to use Amazon EBS-optimized instances.
    , _clrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    , _clrAttributes :: HashMap LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _clrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer custom
      -- recipes.
    , _clrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that to be used for the layer's EC2
      -- instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , _clrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer custom security group IDs.
    , _clrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer packages.
    , _clrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon
      -- EBS volumes.
    } deriving (Generic)

makeLenses ''CreateLayer

instance ToPath CreateLayer

instance ToQuery CreateLayer

instance ToHeaders CreateLayer

instance ToJSON CreateLayer

data CreateLayerResponse = CreateLayerResponse
    { _clsLayerId :: Maybe Text
      -- ^ The layer ID.
    } deriving (Generic)

makeLenses ''CreateLayerResponse

instance FromJSON CreateLayerResponse

instance AWSRequest CreateLayer where
    type Sv CreateLayer = OpsWorks
    type Rs CreateLayer = CreateLayerResponse

    request = get
    response _ = jsonResponse
