{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an instance in a specified stack. For more information, see Adding
-- an Instance to a Layer. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.CreateInstance
    (
    -- * Request
      CreateInstance
    -- ** Request constructor
    , createInstance
    -- ** Request lenses
    , ciStackId
    , ciLayerIds
    , ciInstanceType
    , ciAutoScalingType
    , ciHostname
    , ciOs
    , ciAmiId
    , ciSshKeyName
    , ciAvailabilityZone
    , ciVirtualizationType
    , ciSubnetId
    , ciArchitecture
    , ciRootDeviceType
    , ciInstallUpdatesOnBoot
    , ciEbsOptimized

    -- * Response
    , CreateInstanceResponse
    -- ** Response constructor
    , createInstanceResponse
    -- ** Response lenses
    , cirInstanceId
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateInstance = CreateInstance
    { _ciStackId :: Text
    , _ciLayerIds :: [Text]
    , _ciInstanceType :: Text
    , _ciAutoScalingType :: Maybe AutoScalingType
    , _ciHostname :: Maybe Text
    , _ciOs :: Maybe Text
    , _ciAmiId :: Maybe Text
    , _ciSshKeyName :: Maybe Text
    , _ciAvailabilityZone :: Maybe Text
    , _ciVirtualizationType :: Maybe Text
    , _ciSubnetId :: Maybe Text
    , _ciArchitecture :: Maybe Architecture
    , _ciRootDeviceType :: Maybe RootDeviceType
    , _ciInstallUpdatesOnBoot :: Maybe Bool
    , _ciEbsOptimized :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
-- * @LayerIds ::@ @[Text]@
--
-- * @InstanceType ::@ @Text@
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
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @VirtualizationType ::@ @Maybe Text@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @Architecture ::@ @Maybe Architecture@
--
-- * @RootDeviceType ::@ @Maybe RootDeviceType@
--
-- * @InstallUpdatesOnBoot ::@ @Maybe Bool@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
createInstance :: Text -- ^ 'ciStackId'
               -> [Text] -- ^ 'ciLayerIds'
               -> Text -- ^ 'ciInstanceType'
               -> CreateInstance
createInstance p1 p2 p3 = CreateInstance
    { _ciStackId = p1
    , _ciLayerIds = p2
    , _ciInstanceType = p3
    , _ciAutoScalingType = Nothing
    , _ciHostname = Nothing
    , _ciOs = Nothing
    , _ciAmiId = Nothing
    , _ciSshKeyName = Nothing
    , _ciAvailabilityZone = Nothing
    , _ciVirtualizationType = Nothing
    , _ciSubnetId = Nothing
    , _ciArchitecture = Nothing
    , _ciRootDeviceType = Nothing
    , _ciInstallUpdatesOnBoot = Nothing
    , _ciEbsOptimized = Nothing
    }

-- | The stack ID.
ciStackId :: Lens' CreateInstance Text
ciStackId = lens _ciStackId (\s a -> s { _ciStackId = a })

-- | An array that contains the instance layer IDs.
ciLayerIds :: Lens' CreateInstance [Text]
ciLayerIds = lens _ciLayerIds (\s a -> s { _ciLayerIds = a })

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that you use to specify
-- the various types are in the API Name column of the Available Instance
-- Types table.
ciInstanceType :: Lens' CreateInstance Text
ciInstanceType = lens _ciInstanceType (\s a -> s { _ciInstanceType = a })

-- | For load-based or time-based instances, the type.
ciAutoScalingType :: Lens' CreateInstance (Maybe AutoScalingType)
ciAutoScalingType =
    lens _ciAutoScalingType (\s a -> s { _ciAutoScalingType = a })

-- | The instance host name.
ciHostname :: Lens' CreateInstance (Maybe Text)
ciHostname = lens _ciHostname (\s a -> s { _ciHostname = a })

-- | The instance operating system, which must be set to one of the following.
-- Standard operating systems: Amazon Linux, Ubuntu 12.04 LTS, or Ubuntu 14.04
-- LTS. Custom AMIs: Custom The default option is Amazon Linux. If you set
-- this parameter to Custom, you must use the CreateInstance action's AmiId
-- parameter to specify the custom AMI that you want to use. For more
-- information on the standard operating systems, see Operating SystemsFor
-- more information on how to use custom AMIs with OpsWorks, see Using Custom
-- AMIs.
ciOs :: Lens' CreateInstance (Maybe Text)
ciOs = lens _ciOs (\s a -> s { _ciOs = a })

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks AMIs: Amazon Linux, Ubuntu 12.04 LTS,
-- or Ubuntu 14.04 LTS. For more information, see Instances.
ciAmiId :: Lens' CreateInstance (Maybe Text)
ciAmiId = lens _ciAmiId (\s a -> s { _ciAmiId = a })

-- | The instance SSH key name.
ciSshKeyName :: Lens' CreateInstance (Maybe Text)
ciSshKeyName = lens _ciSshKeyName (\s a -> s { _ciSshKeyName = a })

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
ciAvailabilityZone :: Lens' CreateInstance (Maybe Text)
ciAvailabilityZone =
    lens _ciAvailabilityZone (\s a -> s { _ciAvailabilityZone = a })

-- | The instance's virtualization type, paravirtual or hvm.
ciVirtualizationType :: Lens' CreateInstance (Maybe Text)
ciVirtualizationType =
    lens _ciVirtualizationType (\s a -> s { _ciVirtualizationType = a })

-- | The ID of the instance's subnet. If the stack is running in a VPC, you can
-- use this parameter to override the stack's default subnet ID value and
-- direct AWS OpsWorks to launch the instance in a different subnet.
ciSubnetId :: Lens' CreateInstance (Maybe Text)
ciSubnetId = lens _ciSubnetId (\s a -> s { _ciSubnetId = a })

-- | The instance architecture. The default option is x86_64. Instance types do
-- not necessarily support both architectures. For a list of the architectures
-- that are supported by the different instance types, see Instance Families
-- and Types.
ciArchitecture :: Lens' CreateInstance (Maybe Architecture)
ciArchitecture = lens _ciArchitecture (\s a -> s { _ciArchitecture = a })

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
ciRootDeviceType :: Lens' CreateInstance (Maybe RootDeviceType)
ciRootDeviceType =
    lens _ciRootDeviceType (\s a -> s { _ciRootDeviceType = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true to ensure that your
-- instances have the latest security updates.
ciInstallUpdatesOnBoot :: Lens' CreateInstance (Maybe Bool)
ciInstallUpdatesOnBoot =
    lens _ciInstallUpdatesOnBoot (\s a -> s { _ciInstallUpdatesOnBoot = a })

-- | Whether to create an Amazon EBS-optimized instance.
ciEbsOptimized :: Lens' CreateInstance (Maybe Bool)
ciEbsOptimized = lens _ciEbsOptimized (\s a -> s { _ciEbsOptimized = a })

instance ToPath CreateInstance

instance ToQuery CreateInstance

instance ToHeaders CreateInstance

instance ToJSON CreateInstance

-- | Contains the response to a CreateInstance request.
newtype CreateInstanceResponse = CreateInstanceResponse
    { _cirInstanceId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
createInstanceResponse :: CreateInstanceResponse
createInstanceResponse = CreateInstanceResponse
    { _cirInstanceId = Nothing
    }

-- | The instance ID.
cirInstanceId :: Lens' CreateInstanceResponse (Maybe Text)
cirInstanceId = lens _cirInstanceId (\s a -> s { _cirInstanceId = a })

instance FromJSON CreateInstanceResponse

instance AWSRequest CreateInstance where
    type Sv CreateInstance = OpsWorks
    type Rs CreateInstance = CreateInstanceResponse

    request = get
    response _ = jsonResponse
