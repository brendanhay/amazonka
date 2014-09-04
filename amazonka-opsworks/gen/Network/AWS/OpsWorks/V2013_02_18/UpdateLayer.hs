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
    , mkUpdateLayerRequest
    -- ** Request lenses
    , ulrLayerId
    , ulrName
    , ulrShortname
    , ulrAttributes
    , ulrCustomInstanceProfileArn
    , ulrCustomSecurityGroupIds
    , ulrPackages
    , ulrVolumeConfigurations
    , ulrEnableAutoHealing
    , ulrAutoAssignElasticIps
    , ulrAutoAssignPublicIps
    , ulrCustomRecipes
    , ulrInstallUpdatesOnBoot
    , ulrUseEbsOptimizedInstances

    -- * Response
    , UpdateLayerResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateLayer' request.
mkUpdateLayerRequest :: Text -- ^ 'ulrLayerId'
                     -> UpdateLayer
mkUpdateLayerRequest p1 = UpdateLayer
    { _ulrLayerId = p1
    , _ulrName = Nothing
    , _ulrShortname = Nothing
    , _ulrAttributes = mempty
    , _ulrCustomInstanceProfileArn = Nothing
    , _ulrCustomSecurityGroupIds = mempty
    , _ulrPackages = mempty
    , _ulrVolumeConfigurations = mempty
    , _ulrEnableAutoHealing = Nothing
    , _ulrAutoAssignElasticIps = Nothing
    , _ulrAutoAssignPublicIps = Nothing
    , _ulrCustomRecipes = Nothing
    , _ulrInstallUpdatesOnBoot = Nothing
    , _ulrUseEbsOptimizedInstances = Nothing
    }
{-# INLINE mkUpdateLayerRequest #-}

data UpdateLayer = UpdateLayer
    { _ulrLayerId :: Text
      -- ^ The layer ID.
    , _ulrName :: Maybe Text
      -- ^ The layer name, which is used by the console.
    , _ulrShortname :: Maybe Text
      -- ^ The layer short name, which is used internally by AWS OpsWorksand
      -- by Chef. The short name is also used as the name for the
      -- directory where your app files are installed. It can have a
      -- maximum of 200 characters and must be in the following format:
      -- /\A[a-z0-9\-\_\.]+\Z/.
    , _ulrAttributes :: Map LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
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
    , _ulrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , _ulrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    , _ulrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _ulrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer's custom
      -- recipes.
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
    } deriving (Show, Generic)

-- | The layer ID.
ulrLayerId :: Lens' UpdateLayer (Text)
ulrLayerId = lens _ulrLayerId (\s a -> s { _ulrLayerId = a })
{-# INLINE ulrLayerId #-}

-- | The layer name, which is used by the console.
ulrName :: Lens' UpdateLayer (Maybe Text)
ulrName = lens _ulrName (\s a -> s { _ulrName = a })
{-# INLINE ulrName #-}

-- | The layer short name, which is used internally by AWS OpsWorksand by Chef.
-- The short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be in
-- the following format: /\A[a-z0-9\-\_\.]+\Z/.
ulrShortname :: Lens' UpdateLayer (Maybe Text)
ulrShortname = lens _ulrShortname (\s a -> s { _ulrShortname = a })
{-# INLINE ulrShortname #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
ulrAttributes :: Lens' UpdateLayer (Map LayerAttributesKeys Text)
ulrAttributes = lens _ulrAttributes (\s a -> s { _ulrAttributes = a })
{-# INLINE ulrAttributes #-}

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances.
-- For more information about IAM ARNs, see Using Identifiers.
ulrCustomInstanceProfileArn :: Lens' UpdateLayer (Maybe Text)
ulrCustomInstanceProfileArn = lens _ulrCustomInstanceProfileArn (\s a -> s { _ulrCustomInstanceProfileArn = a })
{-# INLINE ulrCustomInstanceProfileArn #-}

-- | An array containing the layer's custom security group IDs.
ulrCustomSecurityGroupIds :: Lens' UpdateLayer ([Text])
ulrCustomSecurityGroupIds = lens _ulrCustomSecurityGroupIds (\s a -> s { _ulrCustomSecurityGroupIds = a })
{-# INLINE ulrCustomSecurityGroupIds #-}

-- | An array of Package objects that describe the layer's packages.
ulrPackages :: Lens' UpdateLayer ([Text])
ulrPackages = lens _ulrPackages (\s a -> s { _ulrPackages = a })
{-# INLINE ulrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
ulrVolumeConfigurations :: Lens' UpdateLayer ([VolumeConfiguration])
ulrVolumeConfigurations = lens _ulrVolumeConfigurations (\s a -> s { _ulrVolumeConfigurations = a })
{-# INLINE ulrVolumeConfigurations #-}

-- | Whether to disable auto healing for the layer.
ulrEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulrEnableAutoHealing = lens _ulrEnableAutoHealing (\s a -> s { _ulrEnableAutoHealing = a })
{-# INLINE ulrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
ulrAutoAssignElasticIps :: Lens' UpdateLayer (Maybe Bool)
ulrAutoAssignElasticIps = lens _ulrAutoAssignElasticIps (\s a -> s { _ulrAutoAssignElasticIps = a })
{-# INLINE ulrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
ulrAutoAssignPublicIps :: Lens' UpdateLayer (Maybe Bool)
ulrAutoAssignPublicIps = lens _ulrAutoAssignPublicIps (\s a -> s { _ulrAutoAssignPublicIps = a })
{-# INLINE ulrAutoAssignPublicIps #-}

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
ulrCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulrCustomRecipes = lens _ulrCustomRecipes (\s a -> s { _ulrCustomRecipes = a })
{-# INLINE ulrCustomRecipes #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true, to ensure that your
-- instances have the latest security updates.
ulrInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulrInstallUpdatesOnBoot = lens _ulrInstallUpdatesOnBoot (\s a -> s { _ulrInstallUpdatesOnBoot = a })
{-# INLINE ulrInstallUpdatesOnBoot #-}

-- | Whether to use Amazon EBS-optimized instances.
ulrUseEbsOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulrUseEbsOptimizedInstances = lens _ulrUseEbsOptimizedInstances (\s a -> s { _ulrUseEbsOptimizedInstances = a })
{-# INLINE ulrUseEbsOptimizedInstances #-}

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
