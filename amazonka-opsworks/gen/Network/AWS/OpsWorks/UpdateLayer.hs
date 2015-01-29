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

-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates a specified layer.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateLayer.html>
module Network.AWS.OpsWorks.UpdateLayer
    (
    -- * Request
      UpdateLayer
    -- ** Request constructor
    , updateLayer
    -- ** Request lenses
    , ulAttributes
    , ulAutoAssignElasticIps
    , ulAutoAssignPublicIps
    , ulCustomInstanceProfileArn
    , ulCustomRecipes
    , ulCustomSecurityGroupIds
    , ulEnableAutoHealing
    , ulInstallUpdatesOnBoot
    , ulLayerId
    , ulLifecycleEventConfiguration
    , ulName
    , ulPackages
    , ulShortname
    , ulUseEbsOptimizedInstances
    , ulVolumeConfigurations

    -- * Response
    , UpdateLayerResponse
    -- ** Response constructor
    , updateLayerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateLayer = UpdateLayer
    { _ulAttributes                  :: Map LayerAttributesKeys Text
    , _ulAutoAssignElasticIps        :: Maybe Bool
    , _ulAutoAssignPublicIps         :: Maybe Bool
    , _ulCustomInstanceProfileArn    :: Maybe Text
    , _ulCustomRecipes               :: Maybe Recipes
    , _ulCustomSecurityGroupIds      :: List "CustomSecurityGroupIds" Text
    , _ulEnableAutoHealing           :: Maybe Bool
    , _ulInstallUpdatesOnBoot        :: Maybe Bool
    , _ulLayerId                     :: Text
    , _ulLifecycleEventConfiguration :: Maybe LifecycleEventConfiguration
    , _ulName                        :: Maybe Text
    , _ulPackages                    :: List "Packages" Text
    , _ulShortname                   :: Maybe Text
    , _ulUseEbsOptimizedInstances    :: Maybe Bool
    , _ulVolumeConfigurations        :: List "VolumeConfigurations" VolumeConfiguration
    } deriving (Eq, Read, Show)

-- | 'UpdateLayer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ulAttributes' @::@ 'HashMap' 'LayerAttributesKeys' 'Text'
--
-- * 'ulAutoAssignElasticIps' @::@ 'Maybe' 'Bool'
--
-- * 'ulAutoAssignPublicIps' @::@ 'Maybe' 'Bool'
--
-- * 'ulCustomInstanceProfileArn' @::@ 'Maybe' 'Text'
--
-- * 'ulCustomRecipes' @::@ 'Maybe' 'Recipes'
--
-- * 'ulCustomSecurityGroupIds' @::@ ['Text']
--
-- * 'ulEnableAutoHealing' @::@ 'Maybe' 'Bool'
--
-- * 'ulInstallUpdatesOnBoot' @::@ 'Maybe' 'Bool'
--
-- * 'ulLayerId' @::@ 'Text'
--
-- * 'ulLifecycleEventConfiguration' @::@ 'Maybe' 'LifecycleEventConfiguration'
--
-- * 'ulName' @::@ 'Maybe' 'Text'
--
-- * 'ulPackages' @::@ ['Text']
--
-- * 'ulShortname' @::@ 'Maybe' 'Text'
--
-- * 'ulUseEbsOptimizedInstances' @::@ 'Maybe' 'Bool'
--
-- * 'ulVolumeConfigurations' @::@ ['VolumeConfiguration']
--
updateLayer :: Text -- ^ 'ulLayerId'
            -> UpdateLayer
updateLayer p1 = UpdateLayer
    { _ulLayerId                     = p1
    , _ulName                        = Nothing
    , _ulShortname                   = Nothing
    , _ulAttributes                  = mempty
    , _ulCustomInstanceProfileArn    = Nothing
    , _ulCustomSecurityGroupIds      = mempty
    , _ulPackages                    = mempty
    , _ulVolumeConfigurations        = mempty
    , _ulEnableAutoHealing           = Nothing
    , _ulAutoAssignElasticIps        = Nothing
    , _ulAutoAssignPublicIps         = Nothing
    , _ulCustomRecipes               = Nothing
    , _ulInstallUpdatesOnBoot        = Nothing
    , _ulUseEbsOptimizedInstances    = Nothing
    , _ulLifecycleEventConfiguration = Nothing
    }

-- | One or more user-defined key/value pairs to be added to the stack attributes.
ulAttributes :: Lens' UpdateLayer (HashMap LayerAttributesKeys Text)
ulAttributes = lens _ulAttributes (\s a -> s { _ulAttributes = a }) . _Map

-- | Whether to automatically assign an <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's
-- instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
ulAutoAssignElasticIps :: Lens' UpdateLayer (Maybe Bool)
ulAutoAssignElasticIps =
    lens _ulAutoAssignElasticIps (\s a -> s { _ulAutoAssignElasticIps = a })

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How toEdit a Layer>.
ulAutoAssignPublicIps :: Lens' UpdateLayer (Maybe Bool)
ulAutoAssignPublicIps =
    lens _ulAutoAssignPublicIps (\s a -> s { _ulAutoAssignPublicIps = a })

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances.
-- For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
ulCustomInstanceProfileArn :: Lens' UpdateLayer (Maybe Text)
ulCustomInstanceProfileArn =
    lens _ulCustomInstanceProfileArn
        (\s a -> s { _ulCustomInstanceProfileArn = a })

-- | A 'LayerCustomRecipes' object that specifies the layer's custom recipes.
ulCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulCustomRecipes = lens _ulCustomRecipes (\s a -> s { _ulCustomRecipes = a })

-- | An array containing the layer's custom security group IDs.
ulCustomSecurityGroupIds :: Lens' UpdateLayer [Text]
ulCustomSecurityGroupIds =
    lens _ulCustomSecurityGroupIds
        (\s a -> s { _ulCustomSecurityGroupIds = a })
            . _List

-- | Whether to disable auto healing for the layer.
ulEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulEnableAutoHealing =
    lens _ulEnableAutoHealing (\s a -> s { _ulEnableAutoHealing = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is 'true'. To control when updates are installed, set
-- this value to 'false'. You must then update your instances manually by using 'CreateDeployment' to run the 'update_dependencies' stack command or manually running 'yum' (Amazon
-- Linux) or 'apt-get' (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of 'true', to ensure that your
-- instances have the latest security updates.
--
--
ulInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulInstallUpdatesOnBoot =
    lens _ulInstallUpdatesOnBoot (\s a -> s { _ulInstallUpdatesOnBoot = a })

-- | The layer ID.
ulLayerId :: Lens' UpdateLayer Text
ulLayerId = lens _ulLayerId (\s a -> s { _ulLayerId = a })


ulLifecycleEventConfiguration :: Lens' UpdateLayer (Maybe LifecycleEventConfiguration)
ulLifecycleEventConfiguration =
    lens _ulLifecycleEventConfiguration
        (\s a -> s { _ulLifecycleEventConfiguration = a })

-- | The layer name, which is used by the console.
ulName :: Lens' UpdateLayer (Maybe Text)
ulName = lens _ulName (\s a -> s { _ulName = a })

-- | An array of 'Package' objects that describe the layer's packages.
ulPackages :: Lens' UpdateLayer [Text]
ulPackages = lens _ulPackages (\s a -> s { _ulPackages = a }) . _List

-- | The layer short name, which is used internally by AWS OpsWorksand by Chef.
-- The short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be in
-- the following format: /\A[a-z0-9\-\_\.]+\Z/.
ulShortname :: Lens' UpdateLayer (Maybe Text)
ulShortname = lens _ulShortname (\s a -> s { _ulShortname = a })

-- | Whether to use Amazon EBS-optimized instances.
ulUseEbsOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulUseEbsOptimizedInstances =
    lens _ulUseEbsOptimizedInstances
        (\s a -> s { _ulUseEbsOptimizedInstances = a })

-- | A 'VolumeConfigurations' object that describes the layer's Amazon EBS volumes.
ulVolumeConfigurations :: Lens' UpdateLayer [VolumeConfiguration]
ulVolumeConfigurations =
    lens _ulVolumeConfigurations (\s a -> s { _ulVolumeConfigurations = a })
        . _List

data UpdateLayerResponse = UpdateLayerResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateLayerResponse' constructor.
updateLayerResponse :: UpdateLayerResponse
updateLayerResponse = UpdateLayerResponse

instance ToPath UpdateLayer where
    toPath = const "/"

instance ToQuery UpdateLayer where
    toQuery = const mempty

instance ToHeaders UpdateLayer

instance ToJSON UpdateLayer where
    toJSON UpdateLayer{..} = object
        [ "LayerId"                     .= _ulLayerId
        , "Name"                        .= _ulName
        , "Shortname"                   .= _ulShortname
        , "Attributes"                  .= _ulAttributes
        , "CustomInstanceProfileArn"    .= _ulCustomInstanceProfileArn
        , "CustomSecurityGroupIds"      .= _ulCustomSecurityGroupIds
        , "Packages"                    .= _ulPackages
        , "VolumeConfigurations"        .= _ulVolumeConfigurations
        , "EnableAutoHealing"           .= _ulEnableAutoHealing
        , "AutoAssignElasticIps"        .= _ulAutoAssignElasticIps
        , "AutoAssignPublicIps"         .= _ulAutoAssignPublicIps
        , "CustomRecipes"               .= _ulCustomRecipes
        , "InstallUpdatesOnBoot"        .= _ulInstallUpdatesOnBoot
        , "UseEbsOptimizedInstances"    .= _ulUseEbsOptimizedInstances
        , "LifecycleEventConfiguration" .= _ulLifecycleEventConfiguration
        ]

instance AWSRequest UpdateLayer where
    type Sv UpdateLayer = OpsWorks
    type Rs UpdateLayer = UpdateLayerResponse

    request  = post "UpdateLayer"
    response = nullResponse UpdateLayerResponse
