{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateInstance
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
module Network.AWS.OpsWorks
    (
    -- * Request
      UpdateInstance
    -- ** Request constructor
    , mkUpdateInstance
    -- ** Request lenses
    , uiInstanceId
    , uiLayerIds
    , uiInstanceType
    , uiAutoScalingType
    , uiHostname
    , uiOs
    , uiAmiId
    , uiSshKeyName
    , uiArchitecture
    , uiInstallUpdatesOnBoot
    , uiEbsOptimized

    -- * Response
    , UpdateInstanceResponse
    -- ** Response constructor
    , mkUpdateInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateInstance = UpdateInstance
    { _uiInstanceId :: !Text
    , _uiLayerIds :: [Text]
    , _uiInstanceType :: !(Maybe Text)
    , _uiAutoScalingType :: Maybe AutoScalingType
    , _uiHostname :: !(Maybe Text)
    , _uiOs :: !(Maybe Text)
    , _uiAmiId :: !(Maybe Text)
    , _uiSshKeyName :: !(Maybe Text)
    , _uiArchitecture :: Maybe Architecture
    , _uiInstallUpdatesOnBoot :: !(Maybe Bool)
    , _uiEbsOptimized :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @LayerIds ::@ @[Text]@
--
-- * @InstanceType ::@ @Maybe Text@
--
-- * @AutoScalingType ::@ @Maybe AutoScalingType@
--
-- * @Hostname ::@ @Maybe Text@
--
-- * @Os ::@ @Maybe Text@
--
-- * @AmiId ::@ @Maybe Text@
--
-- * @SshKeyName ::@ @Maybe Text@
--
-- * @Architecture ::@ @Maybe Architecture@
--
-- * @InstallUpdatesOnBoot ::@ @Maybe Bool@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
mkUpdateInstance :: Text -- ^ 'uiInstanceId'
                 -> UpdateInstance
mkUpdateInstance p1 = UpdateInstance
    { _uiInstanceId = p1
    , _uiLayerIds = mempty
    , _uiInstanceType = Nothing
    , _uiAutoScalingType = Nothing
    , _uiHostname = Nothing
    , _uiOs = Nothing
    , _uiAmiId = Nothing
    , _uiSshKeyName = Nothing
    , _uiArchitecture = Nothing
    , _uiInstallUpdatesOnBoot = Nothing
    , _uiEbsOptimized = Nothing
    }

-- | The instance ID.
uiInstanceId :: Lens' UpdateInstance Text
uiInstanceId = lens _uiInstanceId (\s a -> s { _uiInstanceId = a })

-- | The instance's layer IDs.
uiLayerIds :: Lens' UpdateInstance [Text]
uiLayerIds = lens _uiLayerIds (\s a -> s { _uiLayerIds = a })

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that you use to specify
-- the various types are in the API Name column of the Available Instance
-- Types table.
uiInstanceType :: Lens' UpdateInstance (Maybe Text)
uiInstanceType = lens _uiInstanceType (\s a -> s { _uiInstanceType = a })

-- | The instance's auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. LoadBasedAutoScaling: A
-- load-based auto scaling instance, which is started and stopped based on
-- load metrics.
uiAutoScalingType :: Lens' UpdateInstance (Maybe AutoScalingType)
uiAutoScalingType =
    lens _uiAutoScalingType (\s a -> s { _uiAutoScalingType = a })

-- | The instance host name.
uiHostname :: Lens' UpdateInstance (Maybe Text)
uiHostname = lens _uiHostname (\s a -> s { _uiHostname = a })

-- | The instance operating system, which must be set to one of the following.
-- Standard operating systems: Amazon Linux or Ubuntu 12.04 LTS Custom AMIs:
-- Custom The default option is Amazon Linux. If you set this parameter to
-- Custom, you must use the CreateInstance action's AmiId parameter to specify
-- the custom AMI that you want to use. For more information on the standard
-- operating systems, see Operating SystemsFor more information on how to use
-- custom AMIs with OpsWorks, see Using Custom AMIs.
uiOs :: Lens' UpdateInstance (Maybe Text)
uiOs = lens _uiOs (\s a -> s { _uiOs = a })

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
-- For more information, see Instances.
uiAmiId :: Lens' UpdateInstance (Maybe Text)
uiAmiId = lens _uiAmiId (\s a -> s { _uiAmiId = a })

-- | The instance SSH key name.
uiSshKeyName :: Lens' UpdateInstance (Maybe Text)
uiSshKeyName = lens _uiSshKeyName (\s a -> s { _uiSshKeyName = a })

-- | The instance architecture. Instance types do not necessarily support both
-- architectures. For a list of the architectures that are supported by the
-- different instance types, see Instance Families and Types.
uiArchitecture :: Lens' UpdateInstance (Maybe Architecture)
uiArchitecture = lens _uiArchitecture (\s a -> s { _uiArchitecture = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true, to ensure that your
-- instances have the latest security updates.
uiInstallUpdatesOnBoot :: Lens' UpdateInstance (Maybe Bool)
uiInstallUpdatesOnBoot =
    lens _uiInstallUpdatesOnBoot (\s a -> s { _uiInstallUpdatesOnBoot = a })

-- | Whether this is an Amazon EBS-optimized instance.
uiEbsOptimized :: Lens' UpdateInstance (Maybe Bool)
uiEbsOptimized = lens _uiEbsOptimized (\s a -> s { _uiEbsOptimized = a })

instance ToPath UpdateInstance

instance ToQuery UpdateInstance

instance ToHeaders UpdateInstance

instance ToJSON UpdateInstance

data UpdateInstanceResponse = UpdateInstanceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateInstanceResponse :: UpdateInstanceResponse
mkUpdateInstanceResponse = UpdateInstanceResponse

instance AWSRequest UpdateInstance where
    type Sv UpdateInstance = OpsWorks
    type Rs UpdateInstance = UpdateInstanceResponse

    request = get
    response _ = nullaryResponse UpdateInstanceResponse
