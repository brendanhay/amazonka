{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified instance. Required Permissions: To use this action, an
-- IAM user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateInstance where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateInstance' request.
updateInstance :: Text -- ^ '_uirInstanceId'
               -> UpdateInstance
updateInstance p1 = UpdateInstance
    { _uirInstanceId = p1
    , _uirArchitecture = Nothing
    , _uirAutoScalingType = Nothing
    , _uirInstallUpdatesOnBoot = Nothing
    , _uirEbsOptimized = Nothing
    , _uirHostname = Nothing
    , _uirSshKeyName = Nothing
    , _uirInstanceType = Nothing
    , _uirOs = Nothing
    , _uirAmiId = Nothing
    , _uirLayerIds = mempty
    }

data UpdateInstance = UpdateInstance
    { _uirInstanceId :: Text
      -- ^ The instance ID.
    , _uirArchitecture :: Maybe Architecture
      -- ^ The instance architecture. Instance types do not necessarily
      -- support both architectures. For a list of the architectures that
      -- are supported by the different instance types, see Instance
      -- Families and Types.
    , _uirAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance's auto scaling type, which has three possible
      -- values: AlwaysRunning: A 24/7 instance, which is not affected by
      -- auto scaling. TimeBasedAutoScaling: A time-based auto scaling
      -- instance, which is started and stopped based on a specified
      -- schedule. LoadBasedAutoScaling: A load-based auto scaling
      -- instance, which is started and stopped based on load metrics.
    , _uirInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. To control when
      -- updates are installed, set this value to false. You must then
      -- update your instances manually by using CreateDeployment to run
      -- the update_dependencies stack command or manually running yum
      -- (Amazon Linux) or apt-get (Ubuntu) on the instances. We strongly
      -- recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , _uirEbsOptimized :: Maybe Bool
      -- ^ Whether this is an Amazon EBS-optimized instance.
    , _uirHostname :: Maybe Text
      -- ^ The instance host name.
    , _uirSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    , _uirInstanceType :: Maybe Text
      -- ^ The instance type. AWS OpsWorks supports all instance types
      -- except Cluster Compute, Cluster GPU, and High Memory Cluster. For
      -- more information, see Instance Families and Types. The parameter
      -- values that you use to specify the various types are in the API
      -- Name column of the Available Instance Types table.
    , _uirOs :: Maybe Text
      -- ^ The instance operating system, which must be set to one of the
      -- following. Standard operating systems: Amazon Linux or Ubuntu
      -- 12.04 LTS Custom AMIs: Custom The default option is Amazon Linux.
      -- If you set this parameter to Custom, you must use the
      -- CreateInstance action's AmiId parameter to specify the custom AMI
      -- that you want to use. For more information on the standard
      -- operating systems, see Operating SystemsFor more information on
      -- how to use custom AMIs with OpsWorks, see Using Custom AMIs.
    , _uirAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should
      -- be based on one of the standard AWS OpsWorks APIs: Amazon Linux
      -- or Ubuntu 12.04 LTS. For more information, see Instances.
    , _uirLayerIds :: [Text]
      -- ^ The instance's layer IDs.
    } deriving (Show, Generic)

makeLenses ''UpdateInstance

instance ToPath UpdateInstance

instance ToQuery UpdateInstance

instance ToHeaders UpdateInstance

instance ToJSON UpdateInstance

data UpdateInstanceResponse = UpdateInstanceResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateInstanceResponse

instance AWSRequest UpdateInstance where
    type Sv UpdateInstance = OpsWorks
    type Rs UpdateInstance = UpdateInstanceResponse

    request = get
    response _ = nullaryResponse UpdateInstanceResponse
