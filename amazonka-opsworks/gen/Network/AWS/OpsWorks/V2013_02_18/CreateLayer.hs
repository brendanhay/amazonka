{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.CreateLayer
    (
    -- * Request
      CreateLayer
    -- ** Request constructor
    , mkCreateLayerRequest
    -- ** Request lenses
    , clrStackId
    , clrType
    , clrName
    , clrShortname
    , clrAttributes
    , clrCustomInstanceProfileArn
    , clrCustomSecurityGroupIds
    , clrPackages
    , clrVolumeConfigurations
    , clrEnableAutoHealing
    , clrAutoAssignElasticIps
    , clrAutoAssignPublicIps
    , clrCustomRecipes
    , clrInstallUpdatesOnBoot
    , clrUseEbsOptimizedInstances

    -- * Response
    , CreateLayerResponse
    -- ** Response lenses
    , clsLayerId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLayer' request.
mkCreateLayerRequest :: Text -- ^ 'clrStackId'
                     -> LayerType -- ^ 'clrType'
                     -> Text -- ^ 'clrName'
                     -> Text -- ^ 'clrShortname'
                     -> CreateLayer
mkCreateLayerRequest p1 p2 p3 p4 = CreateLayer
    { _clrStackId = p1
    , _clrType = p2
    , _clrName = p3
    , _clrShortname = p4
    , _clrAttributes = mempty
    , _clrCustomInstanceProfileArn = Nothing
    , _clrCustomSecurityGroupIds = mempty
    , _clrPackages = mempty
    , _clrVolumeConfigurations = mempty
    , _clrEnableAutoHealing = Nothing
    , _clrAutoAssignElasticIps = Nothing
    , _clrAutoAssignPublicIps = Nothing
    , _clrCustomRecipes = Nothing
    , _clrInstallUpdatesOnBoot = Nothing
    , _clrUseEbsOptimizedInstances = Nothing
    }
{-# INLINE mkCreateLayerRequest #-}

data CreateLayer = CreateLayer
    { _clrStackId :: Text
      -- ^ The layer stack ID.
    , _clrType :: LayerType
      -- ^ The layer type. A stack cannot have more than one built-in layer
      -- of the same type. It can have any number of custom layers. This
      -- parameter must be set to one of the following: custom: A custom
      -- layer db-master: A MySQL layer java-app: A Java App Server layer
      -- rails-app: A Rails App Server layer lb: An HAProxy layer
      -- memcached: A Memcached layer monitoring-master: A Ganglia layer
      -- nodejs-app: A Node.js App Server layer php-app: A PHP App Server
      -- layer web: A Static Web Server layer.
    , _clrName :: Text
      -- ^ The layer name, which is used by the console.
    , _clrShortname :: Text
      -- ^ The layer short name, which is used internally by AWS OpsWorks
      -- and by Chef recipes. The short name is also used as the name for
      -- the directory where your app files are installed. It can have a
      -- maximum of 200 characters, which are limited to the alphanumeric
      -- characters, '-', '_', and '.'.
    , _clrAttributes :: Map LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
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
    , _clrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , _clrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    , _clrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _clrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer custom
      -- recipes.
    , _clrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. To control when
      -- updates are installed, set this value to false. You must then
      -- update your instances manually by using CreateDeployment to run
      -- the update_dependencies stack command or manually running yum
      -- (Amazon Linux) or apt-get (Ubuntu) on the instances. We strongly
      -- recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , _clrUseEbsOptimizedInstances :: Maybe Bool
      -- ^ Whether to use Amazon EBS-optimized instances.
    } deriving (Show, Generic)

-- | The layer stack ID.
clrStackId :: Lens' CreateLayer (Text)
clrStackId = lens _clrStackId (\s a -> s { _clrStackId = a })
{-# INLINE clrStackId #-}

-- | The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers. This parameter must be
-- set to one of the following: custom: A custom layer db-master: A MySQL
-- layer java-app: A Java App Server layer rails-app: A Rails App Server layer
-- lb: An HAProxy layer memcached: A Memcached layer monitoring-master: A
-- Ganglia layer nodejs-app: A Node.js App Server layer php-app: A PHP App
-- Server layer web: A Static Web Server layer.
clrType :: Lens' CreateLayer (LayerType)
clrType = lens _clrType (\s a -> s { _clrType = a })
{-# INLINE clrType #-}

-- | The layer name, which is used by the console.
clrName :: Lens' CreateLayer (Text)
clrName = lens _clrName (\s a -> s { _clrName = a })
{-# INLINE clrName #-}

-- | The layer short name, which is used internally by AWS OpsWorks and by Chef
-- recipes. The short name is also used as the name for the directory where
-- your app files are installed. It can have a maximum of 200 characters,
-- which are limited to the alphanumeric characters, '-', '_', and '.'.
clrShortname :: Lens' CreateLayer (Text)
clrShortname = lens _clrShortname (\s a -> s { _clrShortname = a })
{-# INLINE clrShortname #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
clrAttributes :: Lens' CreateLayer (Map LayerAttributesKeys Text)
clrAttributes = lens _clrAttributes (\s a -> s { _clrAttributes = a })
{-# INLINE clrAttributes #-}

-- | The ARN of an IAM profile that to be used for the layer's EC2 instances.
-- For more information about IAM ARNs, see Using Identifiers.
clrCustomInstanceProfileArn :: Lens' CreateLayer (Maybe Text)
clrCustomInstanceProfileArn = lens _clrCustomInstanceProfileArn (\s a -> s { _clrCustomInstanceProfileArn = a })
{-# INLINE clrCustomInstanceProfileArn #-}

-- | An array containing the layer custom security group IDs.
clrCustomSecurityGroupIds :: Lens' CreateLayer ([Text])
clrCustomSecurityGroupIds = lens _clrCustomSecurityGroupIds (\s a -> s { _clrCustomSecurityGroupIds = a })
{-# INLINE clrCustomSecurityGroupIds #-}

-- | An array of Package objects that describe the layer packages.
clrPackages :: Lens' CreateLayer ([Text])
clrPackages = lens _clrPackages (\s a -> s { _clrPackages = a })
{-# INLINE clrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
clrVolumeConfigurations :: Lens' CreateLayer ([VolumeConfiguration])
clrVolumeConfigurations = lens _clrVolumeConfigurations (\s a -> s { _clrVolumeConfigurations = a })
{-# INLINE clrVolumeConfigurations #-}

-- | Whether to disable auto healing for the layer.
clrEnableAutoHealing :: Lens' CreateLayer (Maybe Bool)
clrEnableAutoHealing = lens _clrEnableAutoHealing (\s a -> s { _clrEnableAutoHealing = a })
{-# INLINE clrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
clrAutoAssignElasticIps :: Lens' CreateLayer (Maybe Bool)
clrAutoAssignElasticIps = lens _clrAutoAssignElasticIps (\s a -> s { _clrAutoAssignElasticIps = a })
{-# INLINE clrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
clrAutoAssignPublicIps :: Lens' CreateLayer (Maybe Bool)
clrAutoAssignPublicIps = lens _clrAutoAssignPublicIps (\s a -> s { _clrAutoAssignPublicIps = a })
{-# INLINE clrAutoAssignPublicIps #-}

-- | A LayerCustomRecipes object that specifies the layer custom recipes.
clrCustomRecipes :: Lens' CreateLayer (Maybe Recipes)
clrCustomRecipes = lens _clrCustomRecipes (\s a -> s { _clrCustomRecipes = a })
{-# INLINE clrCustomRecipes #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true, to ensure that your
-- instances have the latest security updates.
clrInstallUpdatesOnBoot :: Lens' CreateLayer (Maybe Bool)
clrInstallUpdatesOnBoot = lens _clrInstallUpdatesOnBoot (\s a -> s { _clrInstallUpdatesOnBoot = a })
{-# INLINE clrInstallUpdatesOnBoot #-}

-- | Whether to use Amazon EBS-optimized instances.
clrUseEbsOptimizedInstances :: Lens' CreateLayer (Maybe Bool)
clrUseEbsOptimizedInstances = lens _clrUseEbsOptimizedInstances (\s a -> s { _clrUseEbsOptimizedInstances = a })
{-# INLINE clrUseEbsOptimizedInstances #-}

instance ToPath CreateLayer

instance ToQuery CreateLayer

instance ToHeaders CreateLayer

instance ToJSON CreateLayer

newtype CreateLayerResponse = CreateLayerResponse
    { _clsLayerId :: Maybe Text
      -- ^ The layer ID.
    } deriving (Show, Generic)

-- | The layer ID.
clsLayerId :: Lens' CreateLayerResponse (Maybe Text)
clsLayerId = lens _clsLayerId (\s a -> s { _clsLayerId = a })
{-# INLINE clsLayerId #-}

instance FromJSON CreateLayerResponse

instance AWSRequest CreateLayer where
    type Sv CreateLayer = OpsWorks
    type Rs CreateLayer = CreateLayerResponse

    request = get
    response _ = jsonResponse
