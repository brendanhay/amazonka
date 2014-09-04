{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateLayer
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
module Network.AWS.OpsWorks.V2013_02_18.UpdateLayer
    (
    -- * Request
      UpdateLayer
    -- ** Request constructor
    , updateLayer
    -- ** Request lenses
    , ulrLayerId
    , ulrEnableAutoHealing
    , ulrAutoAssignElasticIps
    , ulrAutoAssignPublicIps
    , ulrInstallUpdatesOnBoot
    , ulrUseEbsOptimizedInstances
    , ulrAttributes
    , ulrCustomRecipes
    , ulrName
    , ulrShortname
    , ulrCustomInstanceProfileArn
    , ulrCustomSecurityGroupIds
    , ulrPackages
    , ulrVolumeConfigurations

    -- * Response
    , UpdateLayerResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateLayer' request.
updateLayer :: Text -- ^ 'ulrLayerId'
            -> UpdateLayer
updateLayer p1 = UpdateLayer
    { _ulrLayerId = p1
    , _ulrEnableAutoHealing = Nothing
    , _ulrAutoAssignElasticIps = Nothing
    , _ulrAutoAssignPublicIps = Nothing
    , _ulrInstallUpdatesOnBoot = Nothing
    , _ulrUseEbsOptimizedInstances = Nothing
    , _ulrAttributes = mempty
    , _ulrCustomRecipes = Nothing
    , _ulrName = Nothing
    , _ulrShortname = Nothing
    , _ulrCustomInstanceProfileArn = Nothing
    , _ulrCustomSecurityGroupIds = mempty
    , _ulrPackages = mempty
    , _ulrVolumeConfigurations = mempty
    }
{-# INLINE updateLayer #-}

data UpdateLayer = UpdateLayer
    { _ulrLayerId :: Text
      -- ^ The layer ID.
    , _ulrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , _ulrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    , _ulrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _ulrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. To control when
      -- updates are installed, set this value to false. You must then
      -- update your instances manually by using CreateDeployment to run
      -- the update_dependencies stack command or manually running yum
      -- (Amazon Linux) or apt-get (Ubuntu) on the instances. We strongly
      -- recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , _ulrUseEbsOptimizedInstances :: Maybe Bool
      -- ^ Whether to use Amazon EBS-optimized instances.
    , _ulrAttributes :: Map LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _ulrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer's custom
      -- recipes.
    , _ulrName :: Maybe Text
      -- ^ The layer name, which is used by the console.
    , _ulrShortname :: Maybe Text
      -- ^ The layer short name, which is used internally by AWS OpsWorksand
      -- by Chef. The short name is also used as the name for the
      -- directory where your app files are installed. It can have a
      -- maximum of 200 characters and must be in the following format:
      -- /\A[a-z0-9\-\_\.]+\Z/.
    , _ulrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile to be used for all of the layer's EC2
      -- instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , _ulrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer's custom security group IDs.
    , _ulrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer's packages.
    , _ulrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon
      -- EBS volumes.
    } deriving (Show, Generic)

-- | The layer ID.
ulrLayerId :: Lens' UpdateLayer (Text)
ulrLayerId f x =
    f (_ulrLayerId x)
        <&> \y -> x { _ulrLayerId = y }
{-# INLINE ulrLayerId #-}

-- | Whether to disable auto healing for the layer.
ulrEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulrEnableAutoHealing f x =
    f (_ulrEnableAutoHealing x)
        <&> \y -> x { _ulrEnableAutoHealing = y }
{-# INLINE ulrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
ulrAutoAssignElasticIps :: Lens' UpdateLayer (Maybe Bool)
ulrAutoAssignElasticIps f x =
    f (_ulrAutoAssignElasticIps x)
        <&> \y -> x { _ulrAutoAssignElasticIps = y }
{-# INLINE ulrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
ulrAutoAssignPublicIps :: Lens' UpdateLayer (Maybe Bool)
ulrAutoAssignPublicIps f x =
    f (_ulrAutoAssignPublicIps x)
        <&> \y -> x { _ulrAutoAssignPublicIps = y }
{-# INLINE ulrAutoAssignPublicIps #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true, to ensure that your
-- instances have the latest security updates.
ulrInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulrInstallUpdatesOnBoot f x =
    f (_ulrInstallUpdatesOnBoot x)
        <&> \y -> x { _ulrInstallUpdatesOnBoot = y }
{-# INLINE ulrInstallUpdatesOnBoot #-}

-- | Whether to use Amazon EBS-optimized instances.
ulrUseEbsOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulrUseEbsOptimizedInstances f x =
    f (_ulrUseEbsOptimizedInstances x)
        <&> \y -> x { _ulrUseEbsOptimizedInstances = y }
{-# INLINE ulrUseEbsOptimizedInstances #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
ulrAttributes :: Lens' UpdateLayer (Map LayerAttributesKeys Text)
ulrAttributes f x =
    f (_ulrAttributes x)
        <&> \y -> x { _ulrAttributes = y }
{-# INLINE ulrAttributes #-}

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
ulrCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulrCustomRecipes f x =
    f (_ulrCustomRecipes x)
        <&> \y -> x { _ulrCustomRecipes = y }
{-# INLINE ulrCustomRecipes #-}

-- | The layer name, which is used by the console.
ulrName :: Lens' UpdateLayer (Maybe Text)
ulrName f x =
    f (_ulrName x)
        <&> \y -> x { _ulrName = y }
{-# INLINE ulrName #-}

-- | The layer short name, which is used internally by AWS OpsWorksand by Chef.
-- The short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be in
-- the following format: /\A[a-z0-9\-\_\.]+\Z/.
ulrShortname :: Lens' UpdateLayer (Maybe Text)
ulrShortname f x =
    f (_ulrShortname x)
        <&> \y -> x { _ulrShortname = y }
{-# INLINE ulrShortname #-}

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances.
-- For more information about IAM ARNs, see Using Identifiers.
ulrCustomInstanceProfileArn :: Lens' UpdateLayer (Maybe Text)
ulrCustomInstanceProfileArn f x =
    f (_ulrCustomInstanceProfileArn x)
        <&> \y -> x { _ulrCustomInstanceProfileArn = y }
{-# INLINE ulrCustomInstanceProfileArn #-}

-- | An array containing the layer's custom security group IDs.
ulrCustomSecurityGroupIds :: Lens' UpdateLayer ([Text])
ulrCustomSecurityGroupIds f x =
    f (_ulrCustomSecurityGroupIds x)
        <&> \y -> x { _ulrCustomSecurityGroupIds = y }
{-# INLINE ulrCustomSecurityGroupIds #-}

-- | An array of Package objects that describe the layer's packages.
ulrPackages :: Lens' UpdateLayer ([Text])
ulrPackages f x =
    f (_ulrPackages x)
        <&> \y -> x { _ulrPackages = y }
{-# INLINE ulrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
ulrVolumeConfigurations :: Lens' UpdateLayer ([VolumeConfiguration])
ulrVolumeConfigurations f x =
    f (_ulrVolumeConfigurations x)
        <&> \y -> x { _ulrVolumeConfigurations = y }
{-# INLINE ulrVolumeConfigurations #-}

instance ToPath UpdateLayer

instance ToQuery UpdateLayer

instance ToHeaders UpdateLayer

instance ToJSON UpdateLayer

data UpdateLayerResponse = UpdateLayerResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateLayer where
    type Sv UpdateLayer = OpsWorks
    type Rs UpdateLayer = UpdateLayerResponse

    request = get
    response _ = nullaryResponse UpdateLayerResponse
