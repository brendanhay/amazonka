{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks
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
module Network.AWS.OpsWorks
    (
    -- * Request
      UpdateLayer
    -- ** Request constructor
    , mkUpdateLayer
    -- ** Request lenses
    , ulLayerId
    , ulName
    , ulShortname
    , ulAttributes
    , ulCustomInstanceProfileArn
    , ulCustomSecurityGroupIds
    , ulPackages
    , ulVolumeConfigurations
    , ulEnableAutoHealing
    , ulAutoAssignElasticIps
    , ulAutoAssignPublicIps
    , ulCustomRecipes
    , ulInstallUpdatesOnBoot
    , ulUseEbsOptimizedInstances

    -- * Response
    , UpdateLayerResponse
    -- ** Response constructor
    , mkUpdateLayerResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateLayer = UpdateLayer
    { _ulLayerId :: Text
    , _ulName :: Maybe Text
    , _ulShortname :: Maybe Text
    , _ulAttributes :: Map LayerAttributesKeys Text
    , _ulCustomInstanceProfileArn :: Maybe Text
    , _ulCustomSecurityGroupIds :: [Text]
    , _ulPackages :: [Text]
    , _ulVolumeConfigurations :: [VolumeConfiguration]
    , _ulEnableAutoHealing :: Maybe Bool
    , _ulAutoAssignElasticIps :: Maybe Bool
    , _ulAutoAssignPublicIps :: Maybe Bool
    , _ulCustomRecipes :: Maybe Recipes
    , _ulInstallUpdatesOnBoot :: Maybe Bool
    , _ulUseEbsOptimizedInstances :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateLayer' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LayerId ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Shortname ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map LayerAttributesKeys Text@
--
-- * @CustomInstanceProfileArn ::@ @Maybe Text@
--
-- * @CustomSecurityGroupIds ::@ @[Text]@
--
-- * @Packages ::@ @[Text]@
--
-- * @VolumeConfigurations ::@ @[VolumeConfiguration]@
--
-- * @EnableAutoHealing ::@ @Maybe Bool@
--
-- * @AutoAssignElasticIps ::@ @Maybe Bool@
--
-- * @AutoAssignPublicIps ::@ @Maybe Bool@
--
-- * @CustomRecipes ::@ @Maybe Recipes@
--
-- * @InstallUpdatesOnBoot ::@ @Maybe Bool@
--
-- * @UseEbsOptimizedInstances ::@ @Maybe Bool@
--
mkUpdateLayer :: Text -- ^ 'ulLayerId'
              -> UpdateLayer
mkUpdateLayer p1 = UpdateLayer
    { _ulLayerId = p1
    , _ulName = Nothing
    , _ulShortname = Nothing
    , _ulAttributes = mempty
    , _ulCustomInstanceProfileArn = Nothing
    , _ulCustomSecurityGroupIds = mempty
    , _ulPackages = mempty
    , _ulVolumeConfigurations = mempty
    , _ulEnableAutoHealing = Nothing
    , _ulAutoAssignElasticIps = Nothing
    , _ulAutoAssignPublicIps = Nothing
    , _ulCustomRecipes = Nothing
    , _ulInstallUpdatesOnBoot = Nothing
    , _ulUseEbsOptimizedInstances = Nothing
    }

-- | The layer ID.
ulLayerId :: Lens' UpdateLayer Text
ulLayerId = lens _ulLayerId (\s a -> s { _ulLayerId = a })

-- | The layer name, which is used by the console.
ulName :: Lens' UpdateLayer (Maybe Text)
ulName = lens _ulName (\s a -> s { _ulName = a })

-- | The layer short name, which is used internally by AWS OpsWorksand by Chef.
-- The short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be in
-- the following format: /\A[a-z0-9\-\_\.]+\Z/.
ulShortname :: Lens' UpdateLayer (Maybe Text)
ulShortname = lens _ulShortname (\s a -> s { _ulShortname = a })

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
ulAttributes :: Lens' UpdateLayer (Map LayerAttributesKeys Text)
ulAttributes = lens _ulAttributes (\s a -> s { _ulAttributes = a })

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances.
-- For more information about IAM ARNs, see Using Identifiers.
ulCustomInstanceProfileArn :: Lens' UpdateLayer (Maybe Text)
ulCustomInstanceProfileArn =
    lens _ulCustomInstanceProfileArn
         (\s a -> s { _ulCustomInstanceProfileArn = a })

-- | An array containing the layer's custom security group IDs.
ulCustomSecurityGroupIds :: Lens' UpdateLayer [Text]
ulCustomSecurityGroupIds =
    lens _ulCustomSecurityGroupIds
         (\s a -> s { _ulCustomSecurityGroupIds = a })

-- | An array of Package objects that describe the layer's packages.
ulPackages :: Lens' UpdateLayer [Text]
ulPackages = lens _ulPackages (\s a -> s { _ulPackages = a })

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
ulVolumeConfigurations :: Lens' UpdateLayer [VolumeConfiguration]
ulVolumeConfigurations =
    lens _ulVolumeConfigurations (\s a -> s { _ulVolumeConfigurations = a })

-- | Whether to disable auto healing for the layer.
ulEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulEnableAutoHealing =
    lens _ulEnableAutoHealing (\s a -> s { _ulEnableAutoHealing = a })

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

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
ulCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulCustomRecipes = lens _ulCustomRecipes (\s a -> s { _ulCustomRecipes = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true, to ensure that your
-- instances have the latest security updates.
ulInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulInstallUpdatesOnBoot =
    lens _ulInstallUpdatesOnBoot (\s a -> s { _ulInstallUpdatesOnBoot = a })

-- | Whether to use Amazon EBS-optimized instances.
ulUseEbsOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulUseEbsOptimizedInstances =
    lens _ulUseEbsOptimizedInstances
         (\s a -> s { _ulUseEbsOptimizedInstances = a })

instance ToPath UpdateLayer

instance ToQuery UpdateLayer

instance ToHeaders UpdateLayer

instance ToJSON UpdateLayer

data UpdateLayerResponse = UpdateLayerResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateLayerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateLayerResponse :: UpdateLayerResponse
mkUpdateLayerResponse = UpdateLayerResponse

instance AWSRequest UpdateLayer where
    type Sv UpdateLayer = OpsWorks
    type Rs UpdateLayer = UpdateLayerResponse

    request = get
    response _ = nullaryResponse UpdateLayerResponse
