{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.UpdateLayer where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateLayer' request.
updateLayer :: Text -- ^ '_ulrLayerId'
            -> UpdateLayer
updateLayer p1 = UpdateLayer
    { _ulrLayerId = p1
    , _ulrInstallUpdatesOnBoot = Nothing
    , _ulrEnableAutoHealing = Nothing
    , _ulrAutoAssignPublicIps = Nothing
    , _ulrUseEbsOptimizedInstances = Nothing
    , _ulrAutoAssignElasticIps = Nothing
    , _ulrAttributes = mempty
    , _ulrCustomRecipes = Nothing
    , _ulrCustomInstanceProfileArn = Nothing
    , _ulrShortname = Nothing
    , _ulrName = Nothing
    , _ulrCustomSecurityGroupIds = mempty
    , _ulrPackages = mempty
    , _ulrVolumeConfigurations = mempty
    }

data UpdateLayer = UpdateLayer
    { _ulrLayerId :: Text
      -- ^ The layer ID.
    , _ulrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. To control when
      -- updates are installed, set this value to false. You must then
      -- update your instances manually by using CreateDeployment to run
      -- the update_dependencies stack command or manually running yum
      -- (Amazon Linux) or apt-get (Ubuntu) on the instances. We strongly
      -- recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , _ulrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , _ulrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _ulrUseEbsOptimizedInstances :: Maybe Bool
      -- ^ Whether to use Amazon EBS-optimized instances.
    , _ulrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    , _ulrAttributes :: Map LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _ulrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer's custom
      -- recipes.
    , _ulrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile to be used for all of the layer's EC2
      -- instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , _ulrShortname :: Maybe Text
      -- ^ The layer short name, which is used internally by AWS OpsWorksand
      -- by Chef. The short name is also used as the name for the
      -- directory where your app files are installed. It can have a
      -- maximum of 200 characters and must be in the following format:
      -- /\A[a-z0-9\-\_\.]+\Z/.
    , _ulrName :: Maybe Text
      -- ^ The layer name, which is used by the console.
    , _ulrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer's custom security group IDs.
    , _ulrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer's packages.
    , _ulrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon
      -- EBS volumes.
    } deriving (Show, Generic)

makeLenses ''UpdateLayer

instance ToPath UpdateLayer

instance ToQuery UpdateLayer

instance ToHeaders UpdateLayer

instance ToJSON UpdateLayer

data UpdateLayerResponse = UpdateLayerResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateLayerResponse

instance AWSRequest UpdateLayer where
    type Sv UpdateLayer = OpsWorks
    type Rs UpdateLayer = UpdateLayerResponse

    request = get
    response _ = nullaryResponse UpdateLayerResponse
