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
ulrLayerId
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateLayer
    -> f UpdateLayer
ulrLayerId f x =
    (\y -> x { _ulrLayerId = y })
       <$> f (_ulrLayerId x)
{-# INLINE ulrLayerId #-}

-- | Whether to disable auto healing for the layer.
ulrEnableAutoHealing
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateLayer
    -> f UpdateLayer
ulrEnableAutoHealing f x =
    (\y -> x { _ulrEnableAutoHealing = y })
       <$> f (_ulrEnableAutoHealing x)
{-# INLINE ulrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
ulrAutoAssignElasticIps
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateLayer
    -> f UpdateLayer
ulrAutoAssignElasticIps f x =
    (\y -> x { _ulrAutoAssignElasticIps = y })
       <$> f (_ulrAutoAssignElasticIps x)
{-# INLINE ulrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
ulrAutoAssignPublicIps
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateLayer
    -> f UpdateLayer
ulrAutoAssignPublicIps f x =
    (\y -> x { _ulrAutoAssignPublicIps = y })
       <$> f (_ulrAutoAssignPublicIps x)
{-# INLINE ulrAutoAssignPublicIps #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true, to ensure that your
-- instances have the latest security updates.
ulrInstallUpdatesOnBoot
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateLayer
    -> f UpdateLayer
ulrInstallUpdatesOnBoot f x =
    (\y -> x { _ulrInstallUpdatesOnBoot = y })
       <$> f (_ulrInstallUpdatesOnBoot x)
{-# INLINE ulrInstallUpdatesOnBoot #-}

-- | Whether to use Amazon EBS-optimized instances.
ulrUseEbsOptimizedInstances
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateLayer
    -> f UpdateLayer
ulrUseEbsOptimizedInstances f x =
    (\y -> x { _ulrUseEbsOptimizedInstances = y })
       <$> f (_ulrUseEbsOptimizedInstances x)
{-# INLINE ulrUseEbsOptimizedInstances #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
ulrAttributes
    :: Functor f
    => (Map LayerAttributesKeys Text
    -> f (Map LayerAttributesKeys Text))
    -> UpdateLayer
    -> f UpdateLayer
ulrAttributes f x =
    (\y -> x { _ulrAttributes = y })
       <$> f (_ulrAttributes x)
{-# INLINE ulrAttributes #-}

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
ulrCustomRecipes
    :: Functor f
    => (Maybe Recipes
    -> f (Maybe Recipes))
    -> UpdateLayer
    -> f UpdateLayer
ulrCustomRecipes f x =
    (\y -> x { _ulrCustomRecipes = y })
       <$> f (_ulrCustomRecipes x)
{-# INLINE ulrCustomRecipes #-}

-- | The layer name, which is used by the console.
ulrName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateLayer
    -> f UpdateLayer
ulrName f x =
    (\y -> x { _ulrName = y })
       <$> f (_ulrName x)
{-# INLINE ulrName #-}

-- | The layer short name, which is used internally by AWS OpsWorksand by Chef.
-- The short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be in
-- the following format: /\A[a-z0-9\-\_\.]+\Z/.
ulrShortname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateLayer
    -> f UpdateLayer
ulrShortname f x =
    (\y -> x { _ulrShortname = y })
       <$> f (_ulrShortname x)
{-# INLINE ulrShortname #-}

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances.
-- For more information about IAM ARNs, see Using Identifiers.
ulrCustomInstanceProfileArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateLayer
    -> f UpdateLayer
ulrCustomInstanceProfileArn f x =
    (\y -> x { _ulrCustomInstanceProfileArn = y })
       <$> f (_ulrCustomInstanceProfileArn x)
{-# INLINE ulrCustomInstanceProfileArn #-}

-- | An array containing the layer's custom security group IDs.
ulrCustomSecurityGroupIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> UpdateLayer
    -> f UpdateLayer
ulrCustomSecurityGroupIds f x =
    (\y -> x { _ulrCustomSecurityGroupIds = y })
       <$> f (_ulrCustomSecurityGroupIds x)
{-# INLINE ulrCustomSecurityGroupIds #-}

-- | An array of Package objects that describe the layer's packages.
ulrPackages
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> UpdateLayer
    -> f UpdateLayer
ulrPackages f x =
    (\y -> x { _ulrPackages = y })
       <$> f (_ulrPackages x)
{-# INLINE ulrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
ulrVolumeConfigurations
    :: Functor f
    => ([VolumeConfiguration]
    -> f ([VolumeConfiguration]))
    -> UpdateLayer
    -> f UpdateLayer
ulrVolumeConfigurations f x =
    (\y -> x { _ulrVolumeConfigurations = y })
       <$> f (_ulrVolumeConfigurations x)
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
