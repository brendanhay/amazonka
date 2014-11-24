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

-- Module      : Network.AWS.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an instance in a specified stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html
-- Adding an Instance to a Layer>. Required Permissions: To use this action,
-- an IAM user must have a Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html
-- Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateInstance.html>
module Network.AWS.OpsWorks.CreateInstance
    (
    -- * Request
      CreateInstance
    -- ** Request constructor
    , createInstance
    -- ** Request lenses
    , ciAmiId
    , ciArchitecture
    , ciAutoScalingType
    , ciAvailabilityZone
    , ciEbsOptimized
    , ciHostname
    , ciInstallUpdatesOnBoot
    , ciInstanceType
    , ciLayerIds
    , ciOs
    , ciRootDeviceType
    , ciSshKeyName
    , ciStackId
    , ciSubnetId
    , ciVirtualizationType

    -- * Response
    , CreateInstanceResponse
    -- ** Response constructor
    , createInstanceResponse
    -- ** Response lenses
    , cirInstanceId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data CreateInstance = CreateInstance
    { _ciAmiId                :: Maybe Text
    , _ciArchitecture         :: Maybe Architecture
    , _ciAutoScalingType      :: Maybe AutoScalingType
    , _ciAvailabilityZone     :: Maybe Text
    , _ciEbsOptimized         :: Maybe Bool
    , _ciHostname             :: Maybe Text
    , _ciInstallUpdatesOnBoot :: Maybe Bool
    , _ciInstanceType         :: Text
    , _ciLayerIds             :: List "InstanceIds" Text
    , _ciOs                   :: Maybe Text
    , _ciRootDeviceType       :: Maybe RootDeviceType
    , _ciSshKeyName           :: Maybe Text
    , _ciStackId              :: Text
    , _ciSubnetId             :: Maybe Text
    , _ciVirtualizationType   :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciAmiId' @::@ 'Maybe' 'Text'
--
-- * 'ciArchitecture' @::@ 'Maybe' 'Architecture'
--
-- * 'ciAutoScalingType' @::@ 'Maybe' 'AutoScalingType'
--
-- * 'ciAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ciEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'ciHostname' @::@ 'Maybe' 'Text'
--
-- * 'ciInstallUpdatesOnBoot' @::@ 'Maybe' 'Bool'
--
-- * 'ciInstanceType' @::@ 'Text'
--
-- * 'ciLayerIds' @::@ ['Text']
--
-- * 'ciOs' @::@ 'Maybe' 'Text'
--
-- * 'ciRootDeviceType' @::@ 'Maybe' 'RootDeviceType'
--
-- * 'ciSshKeyName' @::@ 'Maybe' 'Text'
--
-- * 'ciStackId' @::@ 'Text'
--
-- * 'ciSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'ciVirtualizationType' @::@ 'Maybe' 'Text'
--
createInstance :: Text -- ^ 'ciStackId'
               -> Text -- ^ 'ciInstanceType'
               -> CreateInstance
createInstance p1 p2 = CreateInstance
    { _ciStackId              = p1
    , _ciInstanceType         = p2
    , _ciLayerIds             = mempty
    , _ciAutoScalingType      = Nothing
    , _ciHostname             = Nothing
    , _ciOs                   = Nothing
    , _ciAmiId                = Nothing
    , _ciSshKeyName           = Nothing
    , _ciAvailabilityZone     = Nothing
    , _ciVirtualizationType   = Nothing
    , _ciSubnetId             = Nothing
    , _ciArchitecture         = Nothing
    , _ciRootDeviceType       = Nothing
    , _ciInstallUpdatesOnBoot = Nothing
    , _ciEbsOptimized         = Nothing
    }

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu
-- 12.04 LTS. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances.html
-- Instances>.
ciAmiId :: Lens' CreateInstance (Maybe Text)
ciAmiId = lens _ciAmiId (\s a -> s { _ciAmiId = a })

-- | The instance architecture. The default option is x86_64. Instance types
-- do not necessarily support both architectures. For a list of the
-- architectures that are supported by the different instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html
-- Instance Families and Types>.
ciArchitecture :: Lens' CreateInstance (Maybe Architecture)
ciArchitecture = lens _ciArchitecture (\s a -> s { _ciArchitecture = a })

-- | The instance auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is
-- started and stopped based on a specified schedule. To specify the
-- schedule, call SetTimeBasedAutoScaling>. LoadBasedAutoScaling: A
-- load-based auto scaling instance, which is started and stopped based on
-- load metrics. To use load-based auto scaling, you must enable it for the
-- instance layer and configure the thresholds by calling
-- SetLoadBasedAutoScaling>.
ciAutoScalingType :: Lens' CreateInstance (Maybe AutoScalingType)
ciAutoScalingType =
    lens _ciAutoScalingType (\s a -> s { _ciAutoScalingType = a })

-- | The instance Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and
-- Endpoints>.
ciAvailabilityZone :: Lens' CreateInstance (Maybe Text)
ciAvailabilityZone =
    lens _ciAvailabilityZone (\s a -> s { _ciAvailabilityZone = a })

-- | Whether to create an Amazon EBS-optimized instance.
ciEbsOptimized :: Lens' CreateInstance (Maybe Bool)
ciEbsOptimized = lens _ciEbsOptimized (\s a -> s { _ciEbsOptimized = a })

-- | The instance host name.
ciHostname :: Lens' CreateInstance (Maybe Text)
ciHostname = lens _ciHostname (\s a -> s { _ciHostname = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment> to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
ciInstallUpdatesOnBoot :: Lens' CreateInstance (Maybe Bool)
ciInstallUpdatesOnBoot =
    lens _ciInstallUpdatesOnBoot (\s a -> s { _ciInstallUpdatesOnBoot = a })

-- | The instance type. AWS OpsWorks supports all instance types except
-- Cluster Compute, Cluster GPU, and High Memory Cluster. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html
-- Instance Families and Types>. The parameter values that you use to
-- specify the various types are in the API Name column of the Available
-- Instance Types table.
ciInstanceType :: Lens' CreateInstance Text
ciInstanceType = lens _ciInstanceType (\s a -> s { _ciInstanceType = a })

-- | An array that contains the instance layer IDs.
ciLayerIds :: Lens' CreateInstance [Text]
ciLayerIds = lens _ciLayerIds (\s a -> s { _ciLayerIds = a }) . _List

-- | The instance operating system, which must be set to one of the following.
-- Standard operating systems: Amazon Linux or Ubuntu 12.04 LTS Custom AMIs:
-- Custom The default option is Amazon Linux. If you set this parameter to
-- Custom, you must use the CreateInstance> action's AmiId parameter to
-- specify the custom AMI that you want to use. For more information on the
-- standard operating systems, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html
-- Operating Systems>For more information on how to use custom AMIs with
-- OpsWorks, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html
-- Using Custom AMIs>.
ciOs :: Lens' CreateInstance (Maybe Text)
ciOs = lens _ciOs (\s a -> s { _ciOs = a })

-- | The instance root device type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device
-- Storage for the Root Device>.
ciRootDeviceType :: Lens' CreateInstance (Maybe RootDeviceType)
ciRootDeviceType = lens _ciRootDeviceType (\s a -> s { _ciRootDeviceType = a })

-- | The instance SSH key name.
ciSshKeyName :: Lens' CreateInstance (Maybe Text)
ciSshKeyName = lens _ciSshKeyName (\s a -> s { _ciSshKeyName = a })

-- | The stack ID.
ciStackId :: Lens' CreateInstance Text
ciStackId = lens _ciStackId (\s a -> s { _ciStackId = a })

-- | The ID of the instance's subnet. If the stack is running in a VPC, you
-- can use this parameter to override the stack's default subnet ID value
-- and direct AWS OpsWorks to launch the instance in a different subnet.
ciSubnetId :: Lens' CreateInstance (Maybe Text)
ciSubnetId = lens _ciSubnetId (\s a -> s { _ciSubnetId = a })

-- | The instance's virtualization type, paravirtual or hvm.
ciVirtualizationType :: Lens' CreateInstance (Maybe Text)
ciVirtualizationType =
    lens _ciVirtualizationType (\s a -> s { _ciVirtualizationType = a })

newtype CreateInstanceResponse = CreateInstanceResponse
    { _cirInstanceId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CreateInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirInstanceId' @::@ 'Maybe' 'Text'
--
createInstanceResponse :: CreateInstanceResponse
createInstanceResponse = CreateInstanceResponse
    { _cirInstanceId = Nothing
    }

-- | The instance ID.
cirInstanceId :: Lens' CreateInstanceResponse (Maybe Text)
cirInstanceId = lens _cirInstanceId (\s a -> s { _cirInstanceId = a })

instance ToPath CreateInstance where
    toPath = const "/"

instance ToQuery CreateInstance where
    toQuery = const mempty

instance ToHeaders CreateInstance

instance ToJSON CreateInstance where
    toJSON CreateInstance{..} = object
        [ "StackId"              .= _ciStackId
        , "LayerIds"             .= _ciLayerIds
        , "InstanceType"         .= _ciInstanceType
        , "AutoScalingType"      .= _ciAutoScalingType
        , "Hostname"             .= _ciHostname
        , "Os"                   .= _ciOs
        , "AmiId"                .= _ciAmiId
        , "SshKeyName"           .= _ciSshKeyName
        , "AvailabilityZone"     .= _ciAvailabilityZone
        , "VirtualizationType"   .= _ciVirtualizationType
        , "SubnetId"             .= _ciSubnetId
        , "Architecture"         .= _ciArchitecture
        , "RootDeviceType"       .= _ciRootDeviceType
        , "InstallUpdatesOnBoot" .= _ciInstallUpdatesOnBoot
        , "EbsOptimized"         .= _ciEbsOptimized
        ]

instance AWSRequest CreateInstance where
    type Sv CreateInstance = OpsWorks
    type Rs CreateInstance = CreateInstanceResponse

    request  = post "CreateInstance"
    response = jsonResponse

instance FromJSON CreateInstanceResponse where
    parseJSON = withObject "CreateInstanceResponse" $ \o -> CreateInstanceResponse
        <$> o .:? "InstanceId"
