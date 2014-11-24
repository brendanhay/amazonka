{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a layer. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html
-- How to Create a Layer>. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html
-- Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateLayer.html>
module Network.AWS.OpsWorks.CreateLayer
    (
    -- * Request
      CreateLayer
    -- ** Request constructor
    , createLayer
    -- ** Request lenses
    , clAttributes
    , clAutoAssignElasticIps
    , clAutoAssignPublicIps
    , clCustomInstanceProfileArn
    , clCustomRecipes
    , clCustomSecurityGroupIds
    , clEnableAutoHealing
    , clInstallUpdatesOnBoot
    , clName
    , clPackages
    , clShortname
    , clStackId
    , clType
    , clUseEbsOptimizedInstances
    , clVolumeConfigurations

    -- * Response
    , CreateLayerResponse
    -- ** Response constructor
    , createLayerResponse
    -- ** Response lenses
    , clrLayerId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data CreateLayer = CreateLayer
    { _clAttributes               :: Map LayerAttributesKeys Text
    , _clAutoAssignElasticIps     :: Maybe Bool
    , _clAutoAssignPublicIps      :: Maybe Bool
    , _clCustomInstanceProfileArn :: Maybe Text
    , _clCustomRecipes            :: Maybe Recipes
    , _clCustomSecurityGroupIds   :: List "InstanceIds" Text
    , _clEnableAutoHealing        :: Maybe Bool
    , _clInstallUpdatesOnBoot     :: Maybe Bool
    , _clName                     :: Text
    , _clPackages                 :: List "InstanceIds" Text
    , _clShortname                :: Text
    , _clStackId                  :: Text
    , _clType                     :: LayerType
    , _clUseEbsOptimizedInstances :: Maybe Bool
    , _clVolumeConfigurations     :: List "VolumeConfigurations" VolumeConfiguration
    } deriving (Eq, Show)

-- | 'CreateLayer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clAttributes' @::@ 'HashMap' 'LayerAttributesKeys' 'Text'
--
-- * 'clAutoAssignElasticIps' @::@ 'Maybe' 'Bool'
--
-- * 'clAutoAssignPublicIps' @::@ 'Maybe' 'Bool'
--
-- * 'clCustomInstanceProfileArn' @::@ 'Maybe' 'Text'
--
-- * 'clCustomRecipes' @::@ 'Maybe' 'Recipes'
--
-- * 'clCustomSecurityGroupIds' @::@ ['Text']
--
-- * 'clEnableAutoHealing' @::@ 'Maybe' 'Bool'
--
-- * 'clInstallUpdatesOnBoot' @::@ 'Maybe' 'Bool'
--
-- * 'clName' @::@ 'Text'
--
-- * 'clPackages' @::@ ['Text']
--
-- * 'clShortname' @::@ 'Text'
--
-- * 'clStackId' @::@ 'Text'
--
-- * 'clType' @::@ 'LayerType'
--
-- * 'clUseEbsOptimizedInstances' @::@ 'Maybe' 'Bool'
--
-- * 'clVolumeConfigurations' @::@ ['VolumeConfiguration']
--
createLayer :: Text -- ^ 'clStackId'
            -> LayerType -- ^ 'clType'
            -> Text -- ^ 'clName'
            -> Text -- ^ 'clShortname'
            -> CreateLayer
createLayer p1 p2 p3 p4 = CreateLayer
    { _clStackId                  = p1
    , _clType                     = p2
    , _clName                     = p3
    , _clShortname                = p4
    , _clAttributes               = mempty
    , _clCustomInstanceProfileArn = Nothing
    , _clCustomSecurityGroupIds   = mempty
    , _clPackages                 = mempty
    , _clVolumeConfigurations     = mempty
    , _clEnableAutoHealing        = Nothing
    , _clAutoAssignElasticIps     = Nothing
    , _clAutoAssignPublicIps      = Nothing
    , _clCustomRecipes            = Nothing
    , _clInstallUpdatesOnBoot     = Nothing
    , _clUseEbsOptimizedInstances = Nothing
    }

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
clAttributes :: Lens' CreateLayer (HashMap LayerAttributesKeys Text)
clAttributes = lens _clAttributes (\s a -> s { _clAttributes = a }) . _Map

-- | Whether to automatically assign an
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html
-- Elastic IP address> to the layer's instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html
-- How to Edit a Layer>.
clAutoAssignElasticIps :: Lens' CreateLayer (Maybe Bool)
clAutoAssignElasticIps =
    lens _clAutoAssignElasticIps (\s a -> s { _clAutoAssignElasticIps = a })

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html
-- How to Edit a Layer>.
clAutoAssignPublicIps :: Lens' CreateLayer (Maybe Bool)
clAutoAssignPublicIps =
    lens _clAutoAssignPublicIps (\s a -> s { _clAutoAssignPublicIps = a })

-- | The ARN of an IAM profile that to be used for the layer's EC2 instances.
-- For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html
-- Using Identifiers>.
clCustomInstanceProfileArn :: Lens' CreateLayer (Maybe Text)
clCustomInstanceProfileArn =
    lens _clCustomInstanceProfileArn
        (\s a -> s { _clCustomInstanceProfileArn = a })

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
clCustomRecipes :: Lens' CreateLayer (Maybe Recipes)
clCustomRecipes = lens _clCustomRecipes (\s a -> s { _clCustomRecipes = a })

-- | An array containing the layer custom security group IDs.
clCustomSecurityGroupIds :: Lens' CreateLayer [Text]
clCustomSecurityGroupIds =
    lens _clCustomSecurityGroupIds
        (\s a -> s { _clCustomSecurityGroupIds = a })
            . _List

-- | Whether to disable auto healing for the layer.
clEnableAutoHealing :: Lens' CreateLayer (Maybe Bool)
clEnableAutoHealing =
    lens _clEnableAutoHealing (\s a -> s { _clEnableAutoHealing = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your instances
-- manually by using 'CreateDeployment' to run the @update_dependencies@
-- stack command or manually running @yum@ (Amazon Linux) or @apt-get@
-- (Ubuntu) on the instances.
clInstallUpdatesOnBoot :: Lens' CreateLayer (Maybe Bool)
clInstallUpdatesOnBoot =
    lens _clInstallUpdatesOnBoot (\s a -> s { _clInstallUpdatesOnBoot = a })

-- | The layer name, which is used by the console.
clName :: Lens' CreateLayer Text
clName = lens _clName (\s a -> s { _clName = a })

-- | An array of @Package@ objects that describe the layer packages.
clPackages :: Lens' CreateLayer [Text]
clPackages = lens _clPackages (\s a -> s { _clPackages = a }) . _List

-- | The layer short name, which is used internally by AWS OpsWorks and by
-- Chef recipes. The short name is also used as the name for the directory
-- where your app files are installed. It can have a maximum of 200
-- characters, which are limited to the alphanumeric characters, '-', '_',
-- and '.'.
clShortname :: Lens' CreateLayer Text
clShortname = lens _clShortname (\s a -> s { _clShortname = a })

-- | The layer stack ID.
clStackId :: Lens' CreateLayer Text
clStackId = lens _clStackId (\s a -> s { _clStackId = a })

-- | The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers. This parameter must
-- be set to one of the following: custom: A custom layer db-master: A MySQL
-- layer java-app: A Java App Server layer rails-app: A Rails App Server
-- layer lb: An HAProxy layer memcached: A Memcached layer
-- monitoring-master: A Ganglia layer nodejs-app: A Node.js App Server layer
-- php-app: A PHP App Server layer web: A Static Web Server layer.
clType :: Lens' CreateLayer LayerType
clType = lens _clType (\s a -> s { _clType = a })

-- | Whether to use Amazon EBS-optimized instances.
clUseEbsOptimizedInstances :: Lens' CreateLayer (Maybe Bool)
clUseEbsOptimizedInstances =
    lens _clUseEbsOptimizedInstances
        (\s a -> s { _clUseEbsOptimizedInstances = a })

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS
-- volumes.
clVolumeConfigurations :: Lens' CreateLayer [VolumeConfiguration]
clVolumeConfigurations =
    lens _clVolumeConfigurations (\s a -> s { _clVolumeConfigurations = a })
        . _List

newtype CreateLayerResponse = CreateLayerResponse
    { _clrLayerId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CreateLayerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clrLayerId' @::@ 'Maybe' 'Text'
--
createLayerResponse :: CreateLayerResponse
createLayerResponse = CreateLayerResponse
    { _clrLayerId = Nothing
    }

-- | The layer ID.
clrLayerId :: Lens' CreateLayerResponse (Maybe Text)
clrLayerId = lens _clrLayerId (\s a -> s { _clrLayerId = a })

instance ToPath CreateLayer where
    toPath = const "/"

instance ToQuery CreateLayer where
    toQuery = const mempty

instance ToHeaders CreateLayer

instance ToJSON CreateLayer where
    toJSON CreateLayer{..} = object
        [ "StackId"                  .= _clStackId
        , "Type"                     .= _clType
        , "Name"                     .= _clName
        , "Shortname"                .= _clShortname
        , "Attributes"               .= _clAttributes
        , "CustomInstanceProfileArn" .= _clCustomInstanceProfileArn
        , "CustomSecurityGroupIds"   .= _clCustomSecurityGroupIds
        , "Packages"                 .= _clPackages
        , "VolumeConfigurations"     .= _clVolumeConfigurations
        , "EnableAutoHealing"        .= _clEnableAutoHealing
        , "AutoAssignElasticIps"     .= _clAutoAssignElasticIps
        , "AutoAssignPublicIps"      .= _clAutoAssignPublicIps
        , "CustomRecipes"            .= _clCustomRecipes
        , "InstallUpdatesOnBoot"     .= _clInstallUpdatesOnBoot
        , "UseEbsOptimizedInstances" .= _clUseEbsOptimizedInstances
        ]

instance AWSRequest CreateLayer where
    type Sv CreateLayer = OpsWorks
    type Rs CreateLayer = CreateLayerResponse

    request  = post "CreateLayer"
    response = jsonResponse

instance FromJSON CreateLayerResponse where
    parseJSON = withObject "CreateLayerResponse" $ \o -> CreateLayerResponse
        <$> o .:? "LayerId"
