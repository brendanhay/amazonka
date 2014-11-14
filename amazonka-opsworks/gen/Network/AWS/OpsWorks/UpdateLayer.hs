{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified layer. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
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
import Network.AWS.Request
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateLayer = UpdateLayer
    { _ulAttributes               :: Map Text Text
    , _ulAutoAssignElasticIps     :: Maybe Bool
    , _ulAutoAssignPublicIps      :: Maybe Bool
    , _ulCustomInstanceProfileArn :: Maybe Text
    , _ulCustomRecipes            :: Maybe Recipes
    , _ulCustomSecurityGroupIds   :: [Text]
    , _ulEnableAutoHealing        :: Maybe Bool
    , _ulInstallUpdatesOnBoot     :: Maybe Bool
    , _ulLayerId                  :: Text
    , _ulName                     :: Maybe Text
    , _ulPackages                 :: [Text]
    , _ulShortname                :: Maybe Text
    , _ulUseEbsOptimizedInstances :: Maybe Bool
    , _ulVolumeConfigurations     :: [VolumeConfiguration]
    } deriving (Eq, Show, Generic)

-- | 'UpdateLayer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ulAttributes' @::@ 'HashMap' 'Text' 'Text'
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
    { _ulLayerId                  = p1
    , _ulName                     = Nothing
    , _ulShortname                = Nothing
    , _ulAttributes               = mempty
    , _ulCustomInstanceProfileArn = Nothing
    , _ulCustomSecurityGroupIds   = mempty
    , _ulPackages                 = mempty
    , _ulVolumeConfigurations     = mempty
    , _ulEnableAutoHealing        = Nothing
    , _ulAutoAssignElasticIps     = Nothing
    , _ulAutoAssignPublicIps      = Nothing
    , _ulCustomRecipes            = Nothing
    , _ulInstallUpdatesOnBoot     = Nothing
    , _ulUseEbsOptimizedInstances = Nothing
    }

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
ulAttributes :: Lens' UpdateLayer (HashMap Text Text)
ulAttributes = lens _ulAttributes (\s a -> s { _ulAttributes = a })
    . _Map

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
ulAutoAssignElasticIps :: Lens' UpdateLayer (Maybe Bool)
ulAutoAssignElasticIps =
    lens _ulAutoAssignElasticIps (\s a -> s { _ulAutoAssignElasticIps = a })

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
ulAutoAssignPublicIps :: Lens' UpdateLayer (Maybe Bool)
ulAutoAssignPublicIps =
    lens _ulAutoAssignPublicIps (\s a -> s { _ulAutoAssignPublicIps = a })

-- | The ARN of an IAM profile to be used for all of the layer's EC2
-- instances. For more information about IAM ARNs, see Using Identifiers.
ulCustomInstanceProfileArn :: Lens' UpdateLayer (Maybe Text)
ulCustomInstanceProfileArn =
    lens _ulCustomInstanceProfileArn
        (\s a -> s { _ulCustomInstanceProfileArn = a })

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
ulCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulCustomRecipes = lens _ulCustomRecipes (\s a -> s { _ulCustomRecipes = a })

-- | An array containing the layer's custom security group IDs.
ulCustomSecurityGroupIds :: Lens' UpdateLayer [Text]
ulCustomSecurityGroupIds =
    lens _ulCustomSecurityGroupIds
        (\s a -> s { _ulCustomSecurityGroupIds = a })

-- | Whether to disable auto healing for the layer.
ulEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulEnableAutoHealing =
    lens _ulEnableAutoHealing (\s a -> s { _ulEnableAutoHealing = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
ulInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulInstallUpdatesOnBoot =
    lens _ulInstallUpdatesOnBoot (\s a -> s { _ulInstallUpdatesOnBoot = a })

-- | The layer ID.
ulLayerId :: Lens' UpdateLayer Text
ulLayerId = lens _ulLayerId (\s a -> s { _ulLayerId = a })

-- | The layer name, which is used by the console.
ulName :: Lens' UpdateLayer (Maybe Text)
ulName = lens _ulName (\s a -> s { _ulName = a })

-- | An array of Package objects that describe the layer's packages.
ulPackages :: Lens' UpdateLayer [Text]
ulPackages = lens _ulPackages (\s a -> s { _ulPackages = a })

-- | The layer short name, which is used internally by AWS OpsWorksand by
-- Chef. The short name is also used as the name for the directory where
-- your app files are installed. It can have a maximum of 200 characters and
-- must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
ulShortname :: Lens' UpdateLayer (Maybe Text)
ulShortname = lens _ulShortname (\s a -> s { _ulShortname = a })

-- | Whether to use Amazon EBS-optimized instances.
ulUseEbsOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulUseEbsOptimizedInstances =
    lens _ulUseEbsOptimizedInstances
        (\s a -> s { _ulUseEbsOptimizedInstances = a })

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
ulVolumeConfigurations :: Lens' UpdateLayer [VolumeConfiguration]
ulVolumeConfigurations =
    lens _ulVolumeConfigurations (\s a -> s { _ulVolumeConfigurations = a })

instance ToPath UpdateLayer where
    toPath = const "/"

instance ToQuery UpdateLayer where
    toQuery = const mempty

instance ToHeaders UpdateLayer

instance ToBody UpdateLayer where
    toBody = toBody . encode . _ulLayerId

data UpdateLayerResponse = UpdateLayerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateLayerResponse' constructor.
updateLayerResponse :: UpdateLayerResponse
updateLayerResponse = UpdateLayerResponse

instance AWSRequest UpdateLayer where
    type Sv UpdateLayer = OpsWorks
    type Rs UpdateLayer = UpdateLayerResponse

    request  = post
    response = nullaryResponse UpdateLayerResponse
