{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.OpsWorks.Types
    (
    -- * Service
      OpsWorks
    -- ** Error
    , OpsWorksError

    -- * SslConfiguration
    , SslConfiguration
    , sslConfiguration
    , scCertificate
    , scChain
    , scPrivateKey

    -- * Command
    , Command
    , command
    , cAcknowledgedAt
    , cCommandId
    , cCompletedAt
    , cCreatedAt
    , cDeploymentId
    , cExitCode
    , cInstanceId
    , cLogUrl
    , cStatus
    , cType

    -- * RaidArray
    , RaidArray
    , raidArray
    , raAvailabilityZone
    , raCreatedAt
    , raDevice
    , raInstanceId
    , raIops
    , raMountPoint
    , raName
    , raNumberOfDisks
    , raRaidArrayId
    , raRaidLevel
    , raSize
    , raVolumeType

    -- * ElasticLoadBalancer
    , ElasticLoadBalancer
    , elasticLoadBalancer
    , elbAvailabilityZones
    , elbDnsName
    , elbEc2InstanceIds
    , elbElasticLoadBalancerName
    , elbLayerId
    , elbRegion
    , elbStackId
    , elbSubnetIds
    , elbVpcId

    -- * RdsDbInstance
    , RdsDbInstance
    , rdsDbInstance
    , rdiAddress
    , rdiDbInstanceIdentifier
    , rdiDbPassword
    , rdiDbUser
    , rdiEngine
    , rdiMissingOnRds
    , rdiRdsDbInstanceArn
    , rdiRegion
    , rdiStackId

    -- * AppAttributesKeys
    , AppAttributesKeys (..)

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssAppsCount
    , ssArn
    , ssInstancesCount
    , ssLayersCount
    , ssName
    , ssStackId

    -- * StackAttributesKeys
    , StackAttributesKeys (..)

    -- * LoadBasedAutoScalingConfiguration
    , LoadBasedAutoScalingConfiguration
    , loadBasedAutoScalingConfiguration
    , lbascDownScaling
    , lbascEnable
    , lbascLayerId
    , lbascUpScaling

    -- * SourceType
    , SourceType (..)

    -- * Volume
    , Volume
    , volume
    , vAvailabilityZone
    , vDevice
    , vEc2VolumeId
    , vInstanceId
    , vIops
    , vMountPoint
    , vName
    , vRaidArrayId
    , vRegion
    , vSize
    , vStatus
    , vVolumeId
    , vVolumeType

    -- * ChefConfiguration
    , ChefConfiguration
    , chefConfiguration
    , ccBerkshelfVersion
    , ccManageBerkshelf

    -- * LayerType
    , LayerType (..)

    -- * AutoScalingThresholds
    , AutoScalingThresholds
    , autoScalingThresholds
    , astCpuThreshold
    , astIgnoreMetricsTime
    , astInstanceCount
    , astLoadThreshold
    , astMemoryThreshold
    , astThresholdsWaitTime

    -- * App
    , App
    , app
    , appAppId
    , appAppSource
    , appAttributes
    , appCreatedAt
    , appDataSources
    , appDescription
    , appDomains
    , appEnableSsl
    , appName
    , appShortname
    , appSslConfiguration
    , appStackId
    , appType

    -- * ElasticIp
    , ElasticIp
    , elasticIp
    , eiDomain
    , eiInstanceId
    , eiIp
    , eiName
    , eiRegion

    -- * UserProfile
    , UserProfile
    , userProfile
    , upAllowSelfManagement
    , upIamUserArn
    , upName
    , upSshPublicKey
    , upSshUsername

    -- * AutoScalingType
    , AutoScalingType (..)

    -- * Source
    , Source
    , source
    , sPassword
    , sRevision
    , sSshKey
    , sType
    , sUrl
    , sUsername

    -- * DataSource
    , DataSource
    , dataSource
    , dsArn
    , dsDatabaseName
    , dsType

    -- * Architecture
    , Architecture (..)

    -- * StackConfigurationManager
    , StackConfigurationManager
    , stackConfigurationManager
    , scmName
    , scmVersion

    -- * ServiceError
    , ServiceError
    , serviceError
    , seCreatedAt
    , seInstanceId
    , seMessage
    , seServiceErrorId
    , seStackId
    , seType

    -- * LayerAttributesKeys
    , LayerAttributesKeys (..)

    -- * VolumeConfiguration
    , VolumeConfiguration
    , volumeConfiguration
    , vcIops
    , vcMountPoint
    , vcNumberOfDisks
    , vcRaidLevel
    , vcSize
    , vcVolumeType

    -- * Permission
    , Permission
    , permission
    , pAllowSsh
    , pAllowSudo
    , pIamUserArn
    , pLevel
    , pStackId

    -- * Layer
    , Layer
    , layer
    , lAttributes
    , lAutoAssignElasticIps
    , lAutoAssignPublicIps
    , lCreatedAt
    , lCustomInstanceProfileArn
    , lCustomRecipes
    , lCustomSecurityGroupIds
    , lDefaultRecipes
    , lDefaultSecurityGroupNames
    , lEnableAutoHealing
    , lInstallUpdatesOnBoot
    , lLayerId
    , lName
    , lPackages
    , lShortname
    , lStackId
    , lType
    , lUseEbsOptimizedInstances
    , lVolumeConfigurations

    -- * Recipes
    , Recipes
    , recipes
    , rConfigure
    , rDeploy
    , rSetup
    , rShutdown
    , rUndeploy

    -- * TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration
    , timeBasedAutoScalingConfiguration
    , tbascAutoScalingSchedule
    , tbascInstanceId

    -- * SelfUserProfile
    , SelfUserProfile
    , selfUserProfile
    , supIamUserArn
    , supName
    , supSshPublicKey
    , supSshUsername

    -- * RootDeviceType
    , RootDeviceType (..)

    -- * Stack
    , Stack
    , stack
    , sArn
    , sAttributes
    , sChefConfiguration
    , sConfigurationManager
    , sCreatedAt
    , sCustomCookbooksSource
    , sCustomJson
    , sDefaultAvailabilityZone
    , sDefaultInstanceProfileArn
    , sDefaultOs
    , sDefaultRootDeviceType
    , sDefaultSshKeyName
    , sDefaultSubnetId
    , sHostnameTheme
    , sName
    , sRegion
    , sServiceRoleArn
    , sStackId
    , sUseCustomCookbooks
    , sUseOpsworksSecurityGroups
    , sVpcId

    -- * DeploymentCommand
    , DeploymentCommand
    , deploymentCommand
    , dcArgs
    , dcName

    -- * WeeklyAutoScalingSchedule
    , WeeklyAutoScalingSchedule
    , weeklyAutoScalingSchedule
    , wassFriday
    , wassMonday
    , wassSaturday
    , wassSunday
    , wassThursday
    , wassTuesday
    , wassWednesday

    -- * DeploymentCommandName
    , DeploymentCommandName (..)

    -- * Instance
    , Instance
    , instance'
    , iAmiId
    , iArchitecture
    , iAutoScalingType
    , iAvailabilityZone
    , iCreatedAt
    , iEbsOptimized
    , iEc2InstanceId
    , iElasticIp
    , iHostname
    , iInstallUpdatesOnBoot
    , iInstanceId
    , iInstanceProfileArn
    , iInstanceType
    , iLastServiceErrorId
    , iLayerIds
    , iOs
    , iPrivateDns
    , iPrivateIp
    , iPublicDns
    , iPublicIp
    , iRootDeviceType
    , iRootDeviceVolumeId
    , iSecurityGroupIds
    , iSshHostDsaKeyFingerprint
    , iSshHostRsaKeyFingerprint
    , iSshKeyName
    , iStackId
    , iStatus
    , iSubnetId
    , iVirtualizationType

    -- * Deployment
    , Deployment
    , deployment
    , dAppId
    , dCommand
    , dComment
    , dCompletedAt
    , dCreatedAt
    , dCustomJson
    , dDeploymentId
    , dDuration
    , dIamUserArn
    , dInstanceIds
    , dStackId
    , dStatus

    -- * InstancesCount
    , InstancesCount
    , instancesCount
    , icBooting
    , icConnectionLost
    , icOnline
    , icPending
    , icRebooting
    , icRequested
    , icRunningSetup
    , icSetupFailed
    , icShuttingDown
    , icStartFailed
    , icStopped
    , icStopping
    , icTerminated
    , icTerminating

    -- * AppType
    , AppType (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-02-18@) of the Amazon OpsWorks.
data OpsWorks deriving (Typeable)

instance AWSService OpsWorks where
    type Sg OpsWorks = V4
    type Er OpsWorks = OpsWorksError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "OpsWorks"
        , _svcPrefix   = "opsworks"
        , _svcVersion  = "2013-02-18"
        , _svcTarget   = Nothing
        }

    handle = xmlError alwaysFail

data SslConfiguration = SslConfiguration
    { _scCertificate :: Text
    , _scChain       :: Maybe Text
    , _scPrivateKey  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SslConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scCertificate' @::@ 'Text'
--
-- * 'scChain' @::@ 'Maybe' 'Text'
--
-- * 'scPrivateKey' @::@ 'Text'
--
sslConfiguration :: Text -- ^ 'scCertificate'
                 -> Text -- ^ 'scPrivateKey'
                 -> SslConfiguration
sslConfiguration p1 p2 = SslConfiguration
    { _scCertificate = p1
    , _scPrivateKey  = p2
    , _scChain       = Nothing
    }

-- | The contents of the certificate's domain.crt file.
scCertificate :: Lens' SslConfiguration Text
scCertificate = lens _scCertificate (\s a -> s { _scCertificate = a })

-- | Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
scChain :: Lens' SslConfiguration (Maybe Text)
scChain = lens _scChain (\s a -> s { _scChain = a })

-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey :: Lens' SslConfiguration Text
scPrivateKey = lens _scPrivateKey (\s a -> s { _scPrivateKey = a })

data Command = Command
    { _cAcknowledgedAt :: Maybe Text
    , _cCommandId      :: Maybe Text
    , _cCompletedAt    :: Maybe Text
    , _cCreatedAt      :: Maybe Text
    , _cDeploymentId   :: Maybe Text
    , _cExitCode       :: Maybe Int
    , _cInstanceId     :: Maybe Text
    , _cLogUrl         :: Maybe Text
    , _cStatus         :: Maybe Text
    , _cType           :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Command' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cAcknowledgedAt' @::@ 'Maybe' 'Text'
--
-- * 'cCommandId' @::@ 'Maybe' 'Text'
--
-- * 'cCompletedAt' @::@ 'Maybe' 'Text'
--
-- * 'cCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'cDeploymentId' @::@ 'Maybe' 'Text'
--
-- * 'cExitCode' @::@ 'Maybe' 'Int'
--
-- * 'cInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'cLogUrl' @::@ 'Maybe' 'Text'
--
-- * 'cStatus' @::@ 'Maybe' 'Text'
--
-- * 'cType' @::@ 'Maybe' 'Text'
--
command :: Command
command = Command
    { _cCommandId      = Nothing
    , _cInstanceId     = Nothing
    , _cDeploymentId   = Nothing
    , _cCreatedAt      = Nothing
    , _cAcknowledgedAt = Nothing
    , _cCompletedAt    = Nothing
    , _cStatus         = Nothing
    , _cExitCode       = Nothing
    , _cLogUrl         = Nothing
    , _cType           = Nothing
    }

-- | Date and time when the command was acknowledged.
cAcknowledgedAt :: Lens' Command (Maybe Text)
cAcknowledgedAt = lens _cAcknowledgedAt (\s a -> s { _cAcknowledgedAt = a })

-- | The command ID.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\s a -> s { _cCommandId = a })

-- | Date when the command completed.
cCompletedAt :: Lens' Command (Maybe Text)
cCompletedAt = lens _cCompletedAt (\s a -> s { _cCompletedAt = a })

-- | Date and time when the command was run.
cCreatedAt :: Lens' Command (Maybe Text)
cCreatedAt = lens _cCreatedAt (\s a -> s { _cCreatedAt = a })

-- | The command deployment ID.
cDeploymentId :: Lens' Command (Maybe Text)
cDeploymentId = lens _cDeploymentId (\s a -> s { _cDeploymentId = a })

-- | The command exit code.
cExitCode :: Lens' Command (Maybe Int)
cExitCode = lens _cExitCode (\s a -> s { _cExitCode = a })

-- | The ID of the instance where the command was executed.
cInstanceId :: Lens' Command (Maybe Text)
cInstanceId = lens _cInstanceId (\s a -> s { _cInstanceId = a })

-- | The URL of the command log.
cLogUrl :: Lens' Command (Maybe Text)
cLogUrl = lens _cLogUrl (\s a -> s { _cLogUrl = a })

-- | The command status: failed successful skipped pending.
cStatus :: Lens' Command (Maybe Text)
cStatus = lens _cStatus (\s a -> s { _cStatus = a })

-- | The command type: deploy rollback start stop restart undeploy
-- update_dependencies install_dependencies update_custom_cookbooks
-- execute_recipes.
cType :: Lens' Command (Maybe Text)
cType = lens _cType (\s a -> s { _cType = a })

data RaidArray = RaidArray
    { _raAvailabilityZone :: Maybe Text
    , _raCreatedAt        :: Maybe Text
    , _raDevice           :: Maybe Text
    , _raInstanceId       :: Maybe Text
    , _raIops             :: Maybe Int
    , _raMountPoint       :: Maybe Text
    , _raName             :: Maybe Text
    , _raNumberOfDisks    :: Maybe Int
    , _raRaidArrayId      :: Maybe Text
    , _raRaidLevel        :: Maybe Int
    , _raSize             :: Maybe Int
    , _raVolumeType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RaidArray' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'raCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'raDevice' @::@ 'Maybe' 'Text'
--
-- * 'raInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'raIops' @::@ 'Maybe' 'Int'
--
-- * 'raMountPoint' @::@ 'Maybe' 'Text'
--
-- * 'raName' @::@ 'Maybe' 'Text'
--
-- * 'raNumberOfDisks' @::@ 'Maybe' 'Int'
--
-- * 'raRaidArrayId' @::@ 'Maybe' 'Text'
--
-- * 'raRaidLevel' @::@ 'Maybe' 'Int'
--
-- * 'raSize' @::@ 'Maybe' 'Int'
--
-- * 'raVolumeType' @::@ 'Maybe' 'Text'
--
raidArray :: RaidArray
raidArray = RaidArray
    { _raRaidArrayId      = Nothing
    , _raInstanceId       = Nothing
    , _raName             = Nothing
    , _raRaidLevel        = Nothing
    , _raNumberOfDisks    = Nothing
    , _raSize             = Nothing
    , _raDevice           = Nothing
    , _raMountPoint       = Nothing
    , _raAvailabilityZone = Nothing
    , _raCreatedAt        = Nothing
    , _raVolumeType       = Nothing
    , _raIops             = Nothing
    }

-- | The array's Availability Zone. For more information, see Regions and
-- Endpoints.
raAvailabilityZone :: Lens' RaidArray (Maybe Text)
raAvailabilityZone =
    lens _raAvailabilityZone (\s a -> s { _raAvailabilityZone = a })

-- | When the RAID array was created.
raCreatedAt :: Lens' RaidArray (Maybe Text)
raCreatedAt = lens _raCreatedAt (\s a -> s { _raCreatedAt = a })

-- | The array's Linux device. For example /dev/mdadm0.
raDevice :: Lens' RaidArray (Maybe Text)
raDevice = lens _raDevice (\s a -> s { _raDevice = a })

-- | The instance ID.
raInstanceId :: Lens' RaidArray (Maybe Text)
raInstanceId = lens _raInstanceId (\s a -> s { _raInstanceId = a })

-- | For PIOPS volumes, the IOPS per disk.
raIops :: Lens' RaidArray (Maybe Int)
raIops = lens _raIops (\s a -> s { _raIops = a })

-- | The array's mount point.
raMountPoint :: Lens' RaidArray (Maybe Text)
raMountPoint = lens _raMountPoint (\s a -> s { _raMountPoint = a })

-- | The array name.
raName :: Lens' RaidArray (Maybe Text)
raName = lens _raName (\s a -> s { _raName = a })

-- | The number of disks in the array.
raNumberOfDisks :: Lens' RaidArray (Maybe Int)
raNumberOfDisks = lens _raNumberOfDisks (\s a -> s { _raNumberOfDisks = a })

-- | The array ID.
raRaidArrayId :: Lens' RaidArray (Maybe Text)
raRaidArrayId = lens _raRaidArrayId (\s a -> s { _raRaidArrayId = a })

-- | The RAID level.
raRaidLevel :: Lens' RaidArray (Maybe Int)
raRaidLevel = lens _raRaidLevel (\s a -> s { _raRaidLevel = a })

-- | The array's size.
raSize :: Lens' RaidArray (Maybe Int)
raSize = lens _raSize (\s a -> s { _raSize = a })

-- | The volume type, standard or PIOPS.
raVolumeType :: Lens' RaidArray (Maybe Text)
raVolumeType = lens _raVolumeType (\s a -> s { _raVolumeType = a })

data ElasticLoadBalancer = ElasticLoadBalancer
    { _elbAvailabilityZones       :: [Text]
    , _elbDnsName                 :: Maybe Text
    , _elbEc2InstanceIds          :: [Text]
    , _elbElasticLoadBalancerName :: Maybe Text
    , _elbLayerId                 :: Maybe Text
    , _elbRegion                  :: Maybe Text
    , _elbStackId                 :: Maybe Text
    , _elbSubnetIds               :: [Text]
    , _elbVpcId                   :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ElasticLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elbAvailabilityZones' @::@ ['Text']
--
-- * 'elbDnsName' @::@ 'Maybe' 'Text'
--
-- * 'elbEc2InstanceIds' @::@ ['Text']
--
-- * 'elbElasticLoadBalancerName' @::@ 'Maybe' 'Text'
--
-- * 'elbLayerId' @::@ 'Maybe' 'Text'
--
-- * 'elbRegion' @::@ 'Maybe' 'Text'
--
-- * 'elbStackId' @::@ 'Maybe' 'Text'
--
-- * 'elbSubnetIds' @::@ ['Text']
--
-- * 'elbVpcId' @::@ 'Maybe' 'Text'
--
elasticLoadBalancer :: ElasticLoadBalancer
elasticLoadBalancer = ElasticLoadBalancer
    { _elbElasticLoadBalancerName = Nothing
    , _elbRegion                  = Nothing
    , _elbDnsName                 = Nothing
    , _elbStackId                 = Nothing
    , _elbLayerId                 = Nothing
    , _elbVpcId                   = Nothing
    , _elbAvailabilityZones       = mempty
    , _elbSubnetIds               = mempty
    , _elbEc2InstanceIds          = mempty
    }

-- | A list of Availability Zones.
elbAvailabilityZones :: Lens' ElasticLoadBalancer [Text]
elbAvailabilityZones =
    lens _elbAvailabilityZones (\s a -> s { _elbAvailabilityZones = a })

-- | The instance's public DNS name.
elbDnsName :: Lens' ElasticLoadBalancer (Maybe Text)
elbDnsName = lens _elbDnsName (\s a -> s { _elbDnsName = a })

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elbEc2InstanceIds :: Lens' ElasticLoadBalancer [Text]
elbEc2InstanceIds =
    lens _elbEc2InstanceIds (\s a -> s { _elbEc2InstanceIds = a })

-- | The Elastic Load Balancing instance's name.
elbElasticLoadBalancerName :: Lens' ElasticLoadBalancer (Maybe Text)
elbElasticLoadBalancerName =
    lens _elbElasticLoadBalancerName
        (\s a -> s { _elbElasticLoadBalancerName = a })

-- | The ID of the layer that the instance is attached to.
elbLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elbLayerId = lens _elbLayerId (\s a -> s { _elbLayerId = a })

-- | The instance's AWS region.
elbRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elbRegion = lens _elbRegion (\s a -> s { _elbRegion = a })

-- | The ID of the stack that the instance is associated with.
elbStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elbStackId = lens _elbStackId (\s a -> s { _elbStackId = a })

-- | A list of subnet IDs, if the stack is running in a VPC.
elbSubnetIds :: Lens' ElasticLoadBalancer [Text]
elbSubnetIds = lens _elbSubnetIds (\s a -> s { _elbSubnetIds = a })

-- | The VPC ID.
elbVpcId :: Lens' ElasticLoadBalancer (Maybe Text)
elbVpcId = lens _elbVpcId (\s a -> s { _elbVpcId = a })

data RdsDbInstance = RdsDbInstance
    { _rdiAddress              :: Maybe Text
    , _rdiDbInstanceIdentifier :: Maybe Text
    , _rdiDbPassword           :: Maybe Text
    , _rdiDbUser               :: Maybe Text
    , _rdiEngine               :: Maybe Text
    , _rdiMissingOnRds         :: Maybe Bool
    , _rdiRdsDbInstanceArn     :: Maybe Text
    , _rdiRegion               :: Maybe Text
    , _rdiStackId              :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RdsDbInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdiAddress' @::@ 'Maybe' 'Text'
--
-- * 'rdiDbInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rdiDbPassword' @::@ 'Maybe' 'Text'
--
-- * 'rdiDbUser' @::@ 'Maybe' 'Text'
--
-- * 'rdiEngine' @::@ 'Maybe' 'Text'
--
-- * 'rdiMissingOnRds' @::@ 'Maybe' 'Bool'
--
-- * 'rdiRdsDbInstanceArn' @::@ 'Maybe' 'Text'
--
-- * 'rdiRegion' @::@ 'Maybe' 'Text'
--
-- * 'rdiStackId' @::@ 'Maybe' 'Text'
--
rdsDbInstance :: RdsDbInstance
rdsDbInstance = RdsDbInstance
    { _rdiRdsDbInstanceArn     = Nothing
    , _rdiDbInstanceIdentifier = Nothing
    , _rdiDbUser               = Nothing
    , _rdiDbPassword           = Nothing
    , _rdiRegion               = Nothing
    , _rdiAddress              = Nothing
    , _rdiEngine               = Nothing
    , _rdiStackId              = Nothing
    , _rdiMissingOnRds         = Nothing
    }

-- | The instance's address.
rdiAddress :: Lens' RdsDbInstance (Maybe Text)
rdiAddress = lens _rdiAddress (\s a -> s { _rdiAddress = a })

-- | The DB instance identifier.
rdiDbInstanceIdentifier :: Lens' RdsDbInstance (Maybe Text)
rdiDbInstanceIdentifier =
    lens _rdiDbInstanceIdentifier (\s a -> s { _rdiDbInstanceIdentifier = a })

-- | The database password.
rdiDbPassword :: Lens' RdsDbInstance (Maybe Text)
rdiDbPassword = lens _rdiDbPassword (\s a -> s { _rdiDbPassword = a })

-- | The master user name.
rdiDbUser :: Lens' RdsDbInstance (Maybe Text)
rdiDbUser = lens _rdiDbUser (\s a -> s { _rdiDbUser = a })

-- | The instance's database engine.
rdiEngine :: Lens' RdsDbInstance (Maybe Text)
rdiEngine = lens _rdiEngine (\s a -> s { _rdiEngine = a })

-- | Set to true if AWS OpsWorks was unable to discover the Amazon RDS
-- instance. AWS OpsWorks attempts to discover the instance only once. If
-- this value is set to true, you must deregister the instance and then
-- register it again.
rdiMissingOnRds :: Lens' RdsDbInstance (Maybe Bool)
rdiMissingOnRds = lens _rdiMissingOnRds (\s a -> s { _rdiMissingOnRds = a })

-- | The instance's ARN.
rdiRdsDbInstanceArn :: Lens' RdsDbInstance (Maybe Text)
rdiRdsDbInstanceArn =
    lens _rdiRdsDbInstanceArn (\s a -> s { _rdiRdsDbInstanceArn = a })

-- | The instance's AWS region.
rdiRegion :: Lens' RdsDbInstance (Maybe Text)
rdiRegion = lens _rdiRegion (\s a -> s { _rdiRegion = a })

-- | The ID of the stack that the instance is registered with.
rdiStackId :: Lens' RdsDbInstance (Maybe Text)
rdiStackId = lens _rdiStackId (\s a -> s { _rdiStackId = a })

data AppAttributesKeys
    = AutoBundleOnDeploy -- ^ AutoBundleOnDeploy
    | DocumentRoot       -- ^ DocumentRoot
    | RailsEnv           -- ^ RailsEnv
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AppAttributesKeys

instance FromText AppAttributesKeys where
    parser = match "AutoBundleOnDeploy" AutoBundleOnDeploy
         <|> match "DocumentRoot"       DocumentRoot
         <|> match "RailsEnv"           RailsEnv

instance ToText AppAttributesKeys where
    toText = \case
        AutoBundleOnDeploy -> "AutoBundleOnDeploy"
        DocumentRoot       -> "DocumentRoot"
        RailsEnv           -> "RailsEnv"

data StackSummary = StackSummary
    { _ssAppsCount      :: Maybe Int
    , _ssArn            :: Maybe Text
    , _ssInstancesCount :: Maybe InstancesCount
    , _ssLayersCount    :: Maybe Int
    , _ssName           :: Maybe Text
    , _ssStackId        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'StackSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssAppsCount' @::@ 'Maybe' 'Int'
--
-- * 'ssArn' @::@ 'Maybe' 'Text'
--
-- * 'ssInstancesCount' @::@ 'Maybe' 'InstancesCount'
--
-- * 'ssLayersCount' @::@ 'Maybe' 'Int'
--
-- * 'ssName' @::@ 'Maybe' 'Text'
--
-- * 'ssStackId' @::@ 'Maybe' 'Text'
--
stackSummary :: StackSummary
stackSummary = StackSummary
    { _ssStackId        = Nothing
    , _ssName           = Nothing
    , _ssArn            = Nothing
    , _ssLayersCount    = Nothing
    , _ssAppsCount      = Nothing
    , _ssInstancesCount = Nothing
    }

-- | The number of apps.
ssAppsCount :: Lens' StackSummary (Maybe Int)
ssAppsCount = lens _ssAppsCount (\s a -> s { _ssAppsCount = a })

-- | The stack's ARN.
ssArn :: Lens' StackSummary (Maybe Text)
ssArn = lens _ssArn (\s a -> s { _ssArn = a })

-- | An InstancesCount object with the number of instances in each status.
ssInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssInstancesCount = lens _ssInstancesCount (\s a -> s { _ssInstancesCount = a })

-- | The number of layers.
ssLayersCount :: Lens' StackSummary (Maybe Int)
ssLayersCount = lens _ssLayersCount (\s a -> s { _ssLayersCount = a })

-- | The stack name.
ssName :: Lens' StackSummary (Maybe Text)
ssName = lens _ssName (\s a -> s { _ssName = a })

-- | The stack ID.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\s a -> s { _ssStackId = a })

data StackAttributesKeys
    = Color -- ^ Color
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StackAttributesKeys

instance FromText StackAttributesKeys where
    parser = match "Color" Color

instance ToText StackAttributesKeys where
    toText Color = "Color"

data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { _lbascDownScaling :: Maybe AutoScalingThresholds
    , _lbascEnable      :: Maybe Bool
    , _lbascLayerId     :: Maybe Text
    , _lbascUpScaling   :: Maybe AutoScalingThresholds
    } deriving (Eq, Show, Generic)

-- | 'LoadBasedAutoScalingConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbascDownScaling' @::@ 'Maybe' 'AutoScalingThresholds'
--
-- * 'lbascEnable' @::@ 'Maybe' 'Bool'
--
-- * 'lbascLayerId' @::@ 'Maybe' 'Text'
--
-- * 'lbascUpScaling' @::@ 'Maybe' 'AutoScalingThresholds'
--
loadBasedAutoScalingConfiguration :: LoadBasedAutoScalingConfiguration
loadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { _lbascLayerId     = Nothing
    , _lbascEnable      = Nothing
    , _lbascUpScaling   = Nothing
    , _lbascDownScaling = Nothing
    }

-- | A LoadBasedAutoscalingInstruction object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the number
-- of instances.
lbascDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascDownScaling = lens _lbascDownScaling (\s a -> s { _lbascDownScaling = a })

-- | Whether load-based auto scaling is enabled for the layer.
lbascEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbascEnable = lens _lbascEnable (\s a -> s { _lbascEnable = a })

-- | The layer ID.
lbascLayerId :: Lens' LoadBasedAutoScalingConfiguration (Maybe Text)
lbascLayerId = lens _lbascLayerId (\s a -> s { _lbascLayerId = a })

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the
-- number of instances.
lbascUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascUpScaling = lens _lbascUpScaling (\s a -> s { _lbascUpScaling = a })

data SourceType
    = Archive -- ^ archive
    | Git     -- ^ git
    | S3      -- ^ s3
    | Svn     -- ^ svn
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "archive" Archive
         <|> match "git"     Git
         <|> match "s3"      S3
         <|> match "svn"     Svn

instance ToText SourceType where
    toText = \case
        Archive -> "archive"
        Git     -> "git"
        S3      -> "s3"
        Svn     -> "svn"

data Volume = Volume
    { _vAvailabilityZone :: Maybe Text
    , _vDevice           :: Maybe Text
    , _vEc2VolumeId      :: Maybe Text
    , _vInstanceId       :: Maybe Text
    , _vIops             :: Maybe Int
    , _vMountPoint       :: Maybe Text
    , _vName             :: Maybe Text
    , _vRaidArrayId      :: Maybe Text
    , _vRegion           :: Maybe Text
    , _vSize             :: Maybe Int
    , _vStatus           :: Maybe Text
    , _vVolumeId         :: Maybe Text
    , _vVolumeType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Volume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'vDevice' @::@ 'Maybe' 'Text'
--
-- * 'vEc2VolumeId' @::@ 'Maybe' 'Text'
--
-- * 'vInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'vIops' @::@ 'Maybe' 'Int'
--
-- * 'vMountPoint' @::@ 'Maybe' 'Text'
--
-- * 'vName' @::@ 'Maybe' 'Text'
--
-- * 'vRaidArrayId' @::@ 'Maybe' 'Text'
--
-- * 'vRegion' @::@ 'Maybe' 'Text'
--
-- * 'vSize' @::@ 'Maybe' 'Int'
--
-- * 'vStatus' @::@ 'Maybe' 'Text'
--
-- * 'vVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'vVolumeType' @::@ 'Maybe' 'Text'
--
volume :: Volume
volume = Volume
    { _vVolumeId         = Nothing
    , _vEc2VolumeId      = Nothing
    , _vName             = Nothing
    , _vRaidArrayId      = Nothing
    , _vInstanceId       = Nothing
    , _vStatus           = Nothing
    , _vSize             = Nothing
    , _vDevice           = Nothing
    , _vMountPoint       = Nothing
    , _vRegion           = Nothing
    , _vAvailabilityZone = Nothing
    , _vVolumeType       = Nothing
    , _vIops             = Nothing
    }

-- | The volume Availability Zone. For more information, see Regions and
-- Endpoints.
vAvailabilityZone :: Lens' Volume (Maybe Text)
vAvailabilityZone =
    lens _vAvailabilityZone (\s a -> s { _vAvailabilityZone = a })

-- | The device name.
vDevice :: Lens' Volume (Maybe Text)
vDevice = lens _vDevice (\s a -> s { _vDevice = a })

-- | The Amazon EC2 volume ID.
vEc2VolumeId :: Lens' Volume (Maybe Text)
vEc2VolumeId = lens _vEc2VolumeId (\s a -> s { _vEc2VolumeId = a })

-- | The instance ID.
vInstanceId :: Lens' Volume (Maybe Text)
vInstanceId = lens _vInstanceId (\s a -> s { _vInstanceId = a })

-- | For PIOPS volumes, the IOPS per disk.
vIops :: Lens' Volume (Maybe Int)
vIops = lens _vIops (\s a -> s { _vIops = a })

-- | The volume mount point. For example "/dev/sdh".
vMountPoint :: Lens' Volume (Maybe Text)
vMountPoint = lens _vMountPoint (\s a -> s { _vMountPoint = a })

-- | The volume name.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\s a -> s { _vName = a })

-- | The RAID array ID.
vRaidArrayId :: Lens' Volume (Maybe Text)
vRaidArrayId = lens _vRaidArrayId (\s a -> s { _vRaidArrayId = a })

-- | The AWS region. For more information about AWS regions, see Regions and
-- Endpoints.
vRegion :: Lens' Volume (Maybe Text)
vRegion = lens _vRegion (\s a -> s { _vRegion = a })

-- | The volume size.
vSize :: Lens' Volume (Maybe Int)
vSize = lens _vSize (\s a -> s { _vSize = a })

-- | The value returned by DescribeVolumes.
vStatus :: Lens' Volume (Maybe Text)
vStatus = lens _vStatus (\s a -> s { _vStatus = a })

-- | The volume ID.
vVolumeId :: Lens' Volume (Maybe Text)
vVolumeId = lens _vVolumeId (\s a -> s { _vVolumeId = a })

-- | The volume type, standard or PIOPS.
vVolumeType :: Lens' Volume (Maybe Text)
vVolumeType = lens _vVolumeType (\s a -> s { _vVolumeType = a })

data ChefConfiguration = ChefConfiguration
    { _ccBerkshelfVersion :: Maybe Text
    , _ccManageBerkshelf  :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'ChefConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccBerkshelfVersion' @::@ 'Maybe' 'Text'
--
-- * 'ccManageBerkshelf' @::@ 'Maybe' 'Bool'
--
chefConfiguration :: ChefConfiguration
chefConfiguration = ChefConfiguration
    { _ccManageBerkshelf  = Nothing
    , _ccBerkshelfVersion = Nothing
    }

-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion =
    lens _ccBerkshelfVersion (\s a -> s { _ccBerkshelfVersion = a })

-- | Whether to enable Berkshelf.
ccManageBerkshelf :: Lens' ChefConfiguration (Maybe Bool)
ccManageBerkshelf =
    lens _ccManageBerkshelf (\s a -> s { _ccManageBerkshelf = a })

data LayerType
    = Custom           -- ^ custom
    | DbMaster         -- ^ db-master
    | Lb               -- ^ lb
    | Memcached        -- ^ memcached
    | MonitoringMaster -- ^ monitoring-master
    | NodejsApp        -- ^ nodejs-app
    | PhpApp           -- ^ php-app
    | RailsApp         -- ^ rails-app
    | Web              -- ^ web
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable LayerType

instance FromText LayerType where
    parser = match "custom"            Custom
         <|> match "db-master"         DbMaster
         <|> match "lb"                Lb
         <|> match "memcached"         Memcached
         <|> match "monitoring-master" MonitoringMaster
         <|> match "nodejs-app"        NodejsApp
         <|> match "php-app"           PhpApp
         <|> match "rails-app"         RailsApp
         <|> match "web"               Web

instance ToText LayerType where
    toText = \case
        Custom           -> "custom"
        DbMaster         -> "db-master"
        Lb               -> "lb"
        Memcached        -> "memcached"
        MonitoringMaster -> "monitoring-master"
        NodejsApp        -> "nodejs-app"
        PhpApp           -> "php-app"
        RailsApp         -> "rails-app"
        Web              -> "web"

data AutoScalingThresholds = AutoScalingThresholds
    { _astCpuThreshold       :: Maybe Double
    , _astIgnoreMetricsTime  :: Maybe Natural
    , _astInstanceCount      :: Maybe Int
    , _astLoadThreshold      :: Maybe Double
    , _astMemoryThreshold    :: Maybe Double
    , _astThresholdsWaitTime :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'AutoScalingThresholds' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'astCpuThreshold' @::@ 'Maybe' 'Double'
--
-- * 'astIgnoreMetricsTime' @::@ 'Maybe' 'Natural'
--
-- * 'astInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'astLoadThreshold' @::@ 'Maybe' 'Double'
--
-- * 'astMemoryThreshold' @::@ 'Maybe' 'Double'
--
-- * 'astThresholdsWaitTime' @::@ 'Maybe' 'Natural'
--
autoScalingThresholds :: AutoScalingThresholds
autoScalingThresholds = AutoScalingThresholds
    { _astInstanceCount      = Nothing
    , _astThresholdsWaitTime = Nothing
    , _astIgnoreMetricsTime  = Nothing
    , _astCpuThreshold       = Nothing
    , _astMemoryThreshold    = Nothing
    , _astLoadThreshold      = Nothing
    }

-- | The CPU utilization threshold, as a percent of the available CPU.
astCpuThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astCpuThreshold = lens _astCpuThreshold (\s a -> s { _astCpuThreshold = a })

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks should ignore metrics and not raise any additional scaling
-- events. For example, AWS OpsWorks adds new instances following an
-- upscaling event but the instances won't start reducing the load until
-- they have been booted and configured. There is no point in raising
-- additional scaling events during that operation, which typically takes
-- several minutes. IgnoreMetricsTime allows you to direct AWS OpsWorks to
-- not raise any scaling events long enough to get the new instances online.
astIgnoreMetricsTime :: Lens' AutoScalingThresholds (Maybe Natural)
astIgnoreMetricsTime =
    lens _astIgnoreMetricsTime (\s a -> s { _astIgnoreMetricsTime = a })

-- | The number of instances to add or remove when the load exceeds a
-- threshold.
astInstanceCount :: Lens' AutoScalingThresholds (Maybe Int)
astInstanceCount = lens _astInstanceCount (\s a -> s { _astInstanceCount = a })

-- | The load threshold. For more information about how load is computed, see
-- Load (computing).
astLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astLoadThreshold = lens _astLoadThreshold (\s a -> s { _astLoadThreshold = a })

-- | The memory utilization threshold, as a percent of the available memory.
astMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astMemoryThreshold =
    lens _astMemoryThreshold (\s a -> s { _astMemoryThreshold = a })

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
astThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Natural)
astThresholdsWaitTime =
    lens _astThresholdsWaitTime (\s a -> s { _astThresholdsWaitTime = a })

data App = App
    { _appAppId            :: Maybe Text
    , _appAppSource        :: Maybe Source
    , _appAttributes       :: Map Text Text
    , _appCreatedAt        :: Maybe Text
    , _appDataSources      :: [DataSource]
    , _appDescription      :: Maybe Text
    , _appDomains          :: [Text]
    , _appEnableSsl        :: Maybe Bool
    , _appName             :: Maybe Text
    , _appShortname        :: Maybe Text
    , _appSslConfiguration :: Maybe SslConfiguration
    , _appStackId          :: Maybe Text
    , _appType             :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'App' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'appAppId' @::@ 'Maybe' 'Text'
--
-- * 'appAppSource' @::@ 'Maybe' 'Source'
--
-- * 'appAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'appCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'appDataSources' @::@ ['DataSource']
--
-- * 'appDescription' @::@ 'Maybe' 'Text'
--
-- * 'appDomains' @::@ ['Text']
--
-- * 'appEnableSsl' @::@ 'Maybe' 'Bool'
--
-- * 'appName' @::@ 'Maybe' 'Text'
--
-- * 'appShortname' @::@ 'Maybe' 'Text'
--
-- * 'appSslConfiguration' @::@ 'Maybe' 'SslConfiguration'
--
-- * 'appStackId' @::@ 'Maybe' 'Text'
--
-- * 'appType' @::@ 'Maybe' 'Text'
--
app :: App
app = App
    { _appAppId            = Nothing
    , _appStackId          = Nothing
    , _appShortname        = Nothing
    , _appName             = Nothing
    , _appDescription      = Nothing
    , _appDataSources      = mempty
    , _appType             = Nothing
    , _appAppSource        = Nothing
    , _appDomains          = mempty
    , _appEnableSsl        = Nothing
    , _appSslConfiguration = Nothing
    , _appAttributes       = mempty
    , _appCreatedAt        = Nothing
    }

-- | The app ID.
appAppId :: Lens' App (Maybe Text)
appAppId = lens _appAppId (\s a -> s { _appAppId = a })

-- | A Source object that describes the app repository.
appAppSource :: Lens' App (Maybe Source)
appAppSource = lens _appAppSource (\s a -> s { _appAppSource = a })

-- | The stack attributes.
appAttributes :: Lens' App (HashMap Text Text)
appAttributes = lens _appAttributes (\s a -> s { _appAttributes = a })
    . _Map

-- | When the app was created.
appCreatedAt :: Lens' App (Maybe Text)
appCreatedAt = lens _appCreatedAt (\s a -> s { _appCreatedAt = a })

-- | The app's data sources.
appDataSources :: Lens' App [DataSource]
appDataSources = lens _appDataSources (\s a -> s { _appDataSources = a })

-- | A description of the app.
appDescription :: Lens' App (Maybe Text)
appDescription = lens _appDescription (\s a -> s { _appDescription = a })

-- | The app vhost settings with multiple domains separated by commas. For
-- example: 'www.example.com, example.com'.
appDomains :: Lens' App [Text]
appDomains = lens _appDomains (\s a -> s { _appDomains = a })

-- | Whether to enable SSL for the app.
appEnableSsl :: Lens' App (Maybe Bool)
appEnableSsl = lens _appEnableSsl (\s a -> s { _appEnableSsl = a })

-- | The app name.
appName :: Lens' App (Maybe Text)
appName = lens _appName (\s a -> s { _appName = a })

-- | The app's short name.
appShortname :: Lens' App (Maybe Text)
appShortname = lens _appShortname (\s a -> s { _appShortname = a })

-- | An SslConfiguration object with the SSL configuration.
appSslConfiguration :: Lens' App (Maybe SslConfiguration)
appSslConfiguration =
    lens _appSslConfiguration (\s a -> s { _appSslConfiguration = a })

-- | The app stack ID.
appStackId :: Lens' App (Maybe Text)
appStackId = lens _appStackId (\s a -> s { _appStackId = a })

-- | The app type.
appType :: Lens' App (Maybe Text)
appType = lens _appType (\s a -> s { _appType = a })

data ElasticIp = ElasticIp
    { _eiDomain     :: Maybe Text
    , _eiInstanceId :: Maybe Text
    , _eiIp         :: Maybe Text
    , _eiName       :: Maybe Text
    , _eiRegion     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiDomain' @::@ 'Maybe' 'Text'
--
-- * 'eiInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'eiIp' @::@ 'Maybe' 'Text'
--
-- * 'eiName' @::@ 'Maybe' 'Text'
--
-- * 'eiRegion' @::@ 'Maybe' 'Text'
--
elasticIp :: ElasticIp
elasticIp = ElasticIp
    { _eiIp         = Nothing
    , _eiName       = Nothing
    , _eiDomain     = Nothing
    , _eiRegion     = Nothing
    , _eiInstanceId = Nothing
    }

-- | The domain.
eiDomain :: Lens' ElasticIp (Maybe Text)
eiDomain = lens _eiDomain (\s a -> s { _eiDomain = a })

-- | The ID of the instance that the address is attached to.
eiInstanceId :: Lens' ElasticIp (Maybe Text)
eiInstanceId = lens _eiInstanceId (\s a -> s { _eiInstanceId = a })

-- | The IP address.
eiIp :: Lens' ElasticIp (Maybe Text)
eiIp = lens _eiIp (\s a -> s { _eiIp = a })

-- | The name.
eiName :: Lens' ElasticIp (Maybe Text)
eiName = lens _eiName (\s a -> s { _eiName = a })

-- | The AWS region. For more information, see Regions and Endpoints.
eiRegion :: Lens' ElasticIp (Maybe Text)
eiRegion = lens _eiRegion (\s a -> s { _eiRegion = a })

data UserProfile = UserProfile
    { _upAllowSelfManagement :: Maybe Bool
    , _upIamUserArn          :: Maybe Text
    , _upName                :: Maybe Text
    , _upSshPublicKey        :: Maybe Text
    , _upSshUsername         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UserProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upAllowSelfManagement' @::@ 'Maybe' 'Bool'
--
-- * 'upIamUserArn' @::@ 'Maybe' 'Text'
--
-- * 'upName' @::@ 'Maybe' 'Text'
--
-- * 'upSshPublicKey' @::@ 'Maybe' 'Text'
--
-- * 'upSshUsername' @::@ 'Maybe' 'Text'
--
userProfile :: UserProfile
userProfile = UserProfile
    { _upIamUserArn          = Nothing
    , _upName                = Nothing
    , _upSshUsername         = Nothing
    , _upSshPublicKey        = Nothing
    , _upAllowSelfManagement = Nothing
    }

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see Managing User Permissions.
upAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
upAllowSelfManagement =
    lens _upAllowSelfManagement (\s a -> s { _upAllowSelfManagement = a })

-- | The user's IAM ARN.
upIamUserArn :: Lens' UserProfile (Maybe Text)
upIamUserArn = lens _upIamUserArn (\s a -> s { _upIamUserArn = a })

-- | The user's name.
upName :: Lens' UserProfile (Maybe Text)
upName = lens _upName (\s a -> s { _upName = a })

-- | The user's SSH public key.
upSshPublicKey :: Lens' UserProfile (Maybe Text)
upSshPublicKey = lens _upSshPublicKey (\s a -> s { _upSshPublicKey = a })

-- | The user's SSH user name.
upSshUsername :: Lens' UserProfile (Maybe Text)
upSshUsername = lens _upSshUsername (\s a -> s { _upSshUsername = a })

data AutoScalingType
    = Load  -- ^ load
    | Timer -- ^ timer
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AutoScalingType

instance FromText AutoScalingType where
    parser = match "load"  Load
         <|> match "timer" Timer

instance ToText AutoScalingType where
    toText = \case
        Load  -> "load"
        Timer -> "timer"

data Source = Source
    { _sPassword :: Maybe Text
    , _sRevision :: Maybe Text
    , _sSshKey   :: Maybe Text
    , _sType     :: Maybe Text
    , _sUrl      :: Maybe Text
    , _sUsername :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Source' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sPassword' @::@ 'Maybe' 'Text'
--
-- * 'sRevision' @::@ 'Maybe' 'Text'
--
-- * 'sSshKey' @::@ 'Maybe' 'Text'
--
-- * 'sType' @::@ 'Maybe' 'Text'
--
-- * 'sUrl' @::@ 'Maybe' 'Text'
--
-- * 'sUsername' @::@ 'Maybe' 'Text'
--
source :: Source
source = Source
    { _sType     = Nothing
    , _sUrl      = Nothing
    , _sUsername = Nothing
    , _sPassword = Nothing
    , _sSshKey   = Nothing
    , _sRevision = Nothing
    }

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Password to the appropriate IAM secret access key. For HTTP bundles and
-- Subversion repositories, set Password to the password. For more
-- information on how to safely handle IAM credentials, see .
sPassword :: Lens' Source (Maybe Text)
sPassword = lens _sPassword (\s a -> s { _sPassword = a })

-- | The application's version. AWS OpsWorks enables you to easily deploy new
-- versions of an application. One of the simplest approaches is to have
-- branches or revisions in your repository that represent different
-- versions that can potentially be deployed.
sRevision :: Lens' Source (Maybe Text)
sRevision = lens _sRevision (\s a -> s { _sRevision = a })

-- | The repository's SSH key.
sSshKey :: Lens' Source (Maybe Text)
sSshKey = lens _sSshKey (\s a -> s { _sSshKey = a })

-- | The repository type.
sType :: Lens' Source (Maybe Text)
sType = lens _sType (\s a -> s { _sType = a })

-- | The source URL.
sUrl :: Lens' Source (Maybe Text)
sUrl = lens _sUrl (\s a -> s { _sUrl = a })

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Username to the appropriate IAM access key ID. For HTTP bundles, Git
-- repositories, and Subversion repositories, set Username to the user name.
sUsername :: Lens' Source (Maybe Text)
sUsername = lens _sUsername (\s a -> s { _sUsername = a })

data DataSource = DataSource
    { _dsArn          :: Maybe Text
    , _dsDatabaseName :: Maybe Text
    , _dsType         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DataSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsArn' @::@ 'Maybe' 'Text'
--
-- * 'dsDatabaseName' @::@ 'Maybe' 'Text'
--
-- * 'dsType' @::@ 'Maybe' 'Text'
--
dataSource :: DataSource
dataSource = DataSource
    { _dsType         = Nothing
    , _dsArn          = Nothing
    , _dsDatabaseName = Nothing
    }

-- | The data source's ARN.
dsArn :: Lens' DataSource (Maybe Text)
dsArn = lens _dsArn (\s a -> s { _dsArn = a })

-- | The database name.
dsDatabaseName :: Lens' DataSource (Maybe Text)
dsDatabaseName = lens _dsDatabaseName (\s a -> s { _dsDatabaseName = a })

-- | The data source's type, AutoSelectOpsworksMysqlInstance,
-- OpsworksMysqlInstance, or RdsDbInstance.
dsType :: Lens' DataSource (Maybe Text)
dsType = lens _dsType (\s a -> s { _dsType = a })

data Architecture
    = I386  -- ^ i386
    | X8664 -- ^ x86_64
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Architecture

instance FromText Architecture where
    parser = match "i386"   I386
         <|> match "x86_64" X8664

instance ToText Architecture where
    toText = \case
        I386  -> "i386"
        X8664 -> "x86_64"

data StackConfigurationManager = StackConfigurationManager
    { _scmName    :: Maybe Text
    , _scmVersion :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'StackConfigurationManager' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scmName' @::@ 'Maybe' 'Text'
--
-- * 'scmVersion' @::@ 'Maybe' 'Text'
--
stackConfigurationManager :: StackConfigurationManager
stackConfigurationManager = StackConfigurationManager
    { _scmName    = Nothing
    , _scmVersion = Nothing
    }

-- | The name. This parameter must be set to "Chef".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\s a -> s { _scmName = a })

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\s a -> s { _scmVersion = a })

data ServiceError = ServiceError
    { _seCreatedAt      :: Maybe Text
    , _seInstanceId     :: Maybe Text
    , _seMessage        :: Maybe Text
    , _seServiceErrorId :: Maybe Text
    , _seStackId        :: Maybe Text
    , _seType           :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ServiceError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'seInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'seMessage' @::@ 'Maybe' 'Text'
--
-- * 'seServiceErrorId' @::@ 'Maybe' 'Text'
--
-- * 'seStackId' @::@ 'Maybe' 'Text'
--
-- * 'seType' @::@ 'Maybe' 'Text'
--
serviceError :: ServiceError
serviceError = ServiceError
    { _seServiceErrorId = Nothing
    , _seStackId        = Nothing
    , _seInstanceId     = Nothing
    , _seType           = Nothing
    , _seMessage        = Nothing
    , _seCreatedAt      = Nothing
    }

-- | When the error occurred.
seCreatedAt :: Lens' ServiceError (Maybe Text)
seCreatedAt = lens _seCreatedAt (\s a -> s { _seCreatedAt = a })

-- | The instance ID.
seInstanceId :: Lens' ServiceError (Maybe Text)
seInstanceId = lens _seInstanceId (\s a -> s { _seInstanceId = a })

-- | A message that describes the error.
seMessage :: Lens' ServiceError (Maybe Text)
seMessage = lens _seMessage (\s a -> s { _seMessage = a })

-- | The error ID.
seServiceErrorId :: Lens' ServiceError (Maybe Text)
seServiceErrorId = lens _seServiceErrorId (\s a -> s { _seServiceErrorId = a })

-- | The stack ID.
seStackId :: Lens' ServiceError (Maybe Text)
seStackId = lens _seStackId (\s a -> s { _seStackId = a })

-- | The error type.
seType :: Lens' ServiceError (Maybe Text)
seType = lens _seType (\s a -> s { _seType = a })

data LayerAttributesKeys
    = BundlerVersion              -- ^ BundlerVersion
    | EnableHaproxyStats          -- ^ EnableHaproxyStats
    | GangliaPassword             -- ^ GangliaPassword
    | GangliaUrl                  -- ^ GangliaUrl
    | GangliaUser                 -- ^ GangliaUser
    | HaproxyHealthCheckMethod    -- ^ HaproxyHealthCheckMethod
    | HaproxyHealthCheckUrl       -- ^ HaproxyHealthCheckUrl
    | HaproxyStatsPassword        -- ^ HaproxyStatsPassword
    | HaproxyStatsUrl             -- ^ HaproxyStatsUrl
    | HaproxyStatsUser            -- ^ HaproxyStatsUser
    | JavaAppServer               -- ^ JavaAppServer
    | JavaAppServerVersion        -- ^ JavaAppServerVersion
    | Jvm                         -- ^ Jvm
    | JvmOptions                  -- ^ JvmOptions
    | JvmVersion                  -- ^ JvmVersion
    | ManageBundler               -- ^ ManageBundler
    | MemcachedMemory             -- ^ MemcachedMemory
    | MysqlRootPassword           -- ^ MysqlRootPassword
    | MysqlRootPasswordUbiquitous -- ^ MysqlRootPasswordUbiquitous
    | NodejsVersion               -- ^ NodejsVersion
    | PassengerVersion            -- ^ PassengerVersion
    | RailsStack                  -- ^ RailsStack
    | RubyVersion                 -- ^ RubyVersion
    | RubygemsVersion             -- ^ RubygemsVersion
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable LayerAttributesKeys

instance FromText LayerAttributesKeys where
    parser = match "BundlerVersion"              BundlerVersion
         <|> match "EnableHaproxyStats"          EnableHaproxyStats
         <|> match "GangliaPassword"             GangliaPassword
         <|> match "GangliaUrl"                  GangliaUrl
         <|> match "GangliaUser"                 GangliaUser
         <|> match "HaproxyHealthCheckMethod"    HaproxyHealthCheckMethod
         <|> match "HaproxyHealthCheckUrl"       HaproxyHealthCheckUrl
         <|> match "HaproxyStatsPassword"        HaproxyStatsPassword
         <|> match "HaproxyStatsUrl"             HaproxyStatsUrl
         <|> match "HaproxyStatsUser"            HaproxyStatsUser
         <|> match "JavaAppServer"               JavaAppServer
         <|> match "JavaAppServerVersion"        JavaAppServerVersion
         <|> match "Jvm"                         Jvm
         <|> match "JvmOptions"                  JvmOptions
         <|> match "JvmVersion"                  JvmVersion
         <|> match "ManageBundler"               ManageBundler
         <|> match "MemcachedMemory"             MemcachedMemory
         <|> match "MysqlRootPassword"           MysqlRootPassword
         <|> match "MysqlRootPasswordUbiquitous" MysqlRootPasswordUbiquitous
         <|> match "NodejsVersion"               NodejsVersion
         <|> match "PassengerVersion"            PassengerVersion
         <|> match "RailsStack"                  RailsStack
         <|> match "RubyVersion"                 RubyVersion
         <|> match "RubygemsVersion"             RubygemsVersion

instance ToText LayerAttributesKeys where
    toText = \case
        BundlerVersion              -> "BundlerVersion"
        EnableHaproxyStats          -> "EnableHaproxyStats"
        GangliaPassword             -> "GangliaPassword"
        GangliaUrl                  -> "GangliaUrl"
        GangliaUser                 -> "GangliaUser"
        HaproxyHealthCheckMethod    -> "HaproxyHealthCheckMethod"
        HaproxyHealthCheckUrl       -> "HaproxyHealthCheckUrl"
        HaproxyStatsPassword        -> "HaproxyStatsPassword"
        HaproxyStatsUrl             -> "HaproxyStatsUrl"
        HaproxyStatsUser            -> "HaproxyStatsUser"
        JavaAppServer               -> "JavaAppServer"
        JavaAppServerVersion        -> "JavaAppServerVersion"
        Jvm                         -> "Jvm"
        JvmOptions                  -> "JvmOptions"
        JvmVersion                  -> "JvmVersion"
        ManageBundler               -> "ManageBundler"
        MemcachedMemory             -> "MemcachedMemory"
        MysqlRootPassword           -> "MysqlRootPassword"
        MysqlRootPasswordUbiquitous -> "MysqlRootPasswordUbiquitous"
        NodejsVersion               -> "NodejsVersion"
        PassengerVersion            -> "PassengerVersion"
        RailsStack                  -> "RailsStack"
        RubyVersion                 -> "RubyVersion"
        RubygemsVersion             -> "RubygemsVersion"

data VolumeConfiguration = VolumeConfiguration
    { _vcIops          :: Maybe Int
    , _vcMountPoint    :: Text
    , _vcNumberOfDisks :: Int
    , _vcRaidLevel     :: Maybe Int
    , _vcSize          :: Int
    , _vcVolumeType    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcIops' @::@ 'Maybe' 'Int'
--
-- * 'vcMountPoint' @::@ 'Text'
--
-- * 'vcNumberOfDisks' @::@ 'Int'
--
-- * 'vcRaidLevel' @::@ 'Maybe' 'Int'
--
-- * 'vcSize' @::@ 'Int'
--
-- * 'vcVolumeType' @::@ 'Maybe' 'Text'
--
volumeConfiguration :: Text -- ^ 'vcMountPoint'
                    -> Int -- ^ 'vcNumberOfDisks'
                    -> Int -- ^ 'vcSize'
                    -> VolumeConfiguration
volumeConfiguration p1 p2 p3 = VolumeConfiguration
    { _vcMountPoint    = p1
    , _vcNumberOfDisks = p2
    , _vcSize          = p3
    , _vcRaidLevel     = Nothing
    , _vcVolumeType    = Nothing
    , _vcIops          = Nothing
    }

-- | For PIOPS volumes, the IOPS per disk.
vcIops :: Lens' VolumeConfiguration (Maybe Int)
vcIops = lens _vcIops (\s a -> s { _vcIops = a })

-- | The volume mount point. For example "/dev/sdh".
vcMountPoint :: Lens' VolumeConfiguration Text
vcMountPoint = lens _vcMountPoint (\s a -> s { _vcMountPoint = a })

-- | The number of disks in the volume.
vcNumberOfDisks :: Lens' VolumeConfiguration Int
vcNumberOfDisks = lens _vcNumberOfDisks (\s a -> s { _vcNumberOfDisks = a })

-- | The volume RAID level.
vcRaidLevel :: Lens' VolumeConfiguration (Maybe Int)
vcRaidLevel = lens _vcRaidLevel (\s a -> s { _vcRaidLevel = a })

-- | The volume size.
vcSize :: Lens' VolumeConfiguration Int
vcSize = lens _vcSize (\s a -> s { _vcSize = a })

-- | The volume type, standard or PIOPS.
vcVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vcVolumeType = lens _vcVolumeType (\s a -> s { _vcVolumeType = a })

data Permission = Permission
    { _pAllowSsh   :: Maybe Bool
    , _pAllowSudo  :: Maybe Bool
    , _pIamUserArn :: Maybe Text
    , _pLevel      :: Maybe Text
    , _pStackId    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Permission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAllowSsh' @::@ 'Maybe' 'Bool'
--
-- * 'pAllowSudo' @::@ 'Maybe' 'Bool'
--
-- * 'pIamUserArn' @::@ 'Maybe' 'Text'
--
-- * 'pLevel' @::@ 'Maybe' 'Text'
--
-- * 'pStackId' @::@ 'Maybe' 'Text'
--
permission :: Permission
permission = Permission
    { _pStackId    = Nothing
    , _pIamUserArn = Nothing
    , _pAllowSsh   = Nothing
    , _pAllowSudo  = Nothing
    , _pLevel      = Nothing
    }

-- | Whether the user can use SSH.
pAllowSsh :: Lens' Permission (Maybe Bool)
pAllowSsh = lens _pAllowSsh (\s a -> s { _pAllowSsh = a })

-- | Whether the user can use sudo.
pAllowSudo :: Lens' Permission (Maybe Bool)
pAllowSudo = lens _pAllowSudo (\s a -> s { _pAllowSudo = a })

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see Using Identifiers.
pIamUserArn :: Lens' Permission (Maybe Text)
pIamUserArn = lens _pIamUserArn (\s a -> s { _pIamUserArn = a })

-- | The user's permission level, which must be the following: deny show
-- deploy manage iam_only For more information on the permissions associated
-- with these levels, see Managing User Permissions.
pLevel :: Lens' Permission (Maybe Text)
pLevel = lens _pLevel (\s a -> s { _pLevel = a })

-- | A stack ID.
pStackId :: Lens' Permission (Maybe Text)
pStackId = lens _pStackId (\s a -> s { _pStackId = a })

data Layer = Layer
    { _lAttributes                :: Map Text Text
    , _lAutoAssignElasticIps      :: Maybe Bool
    , _lAutoAssignPublicIps       :: Maybe Bool
    , _lCreatedAt                 :: Maybe Text
    , _lCustomInstanceProfileArn  :: Maybe Text
    , _lCustomRecipes             :: Maybe Recipes
    , _lCustomSecurityGroupIds    :: [Text]
    , _lDefaultRecipes            :: Maybe Recipes
    , _lDefaultSecurityGroupNames :: [Text]
    , _lEnableAutoHealing         :: Maybe Bool
    , _lInstallUpdatesOnBoot      :: Maybe Bool
    , _lLayerId                   :: Maybe Text
    , _lName                      :: Maybe Text
    , _lPackages                  :: [Text]
    , _lShortname                 :: Maybe Text
    , _lStackId                   :: Maybe Text
    , _lType                      :: Maybe Text
    , _lUseEbsOptimizedInstances  :: Maybe Bool
    , _lVolumeConfigurations      :: [VolumeConfiguration]
    } deriving (Eq, Show, Generic)

-- | 'Layer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'lAutoAssignElasticIps' @::@ 'Maybe' 'Bool'
--
-- * 'lAutoAssignPublicIps' @::@ 'Maybe' 'Bool'
--
-- * 'lCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'lCustomInstanceProfileArn' @::@ 'Maybe' 'Text'
--
-- * 'lCustomRecipes' @::@ 'Maybe' 'Recipes'
--
-- * 'lCustomSecurityGroupIds' @::@ ['Text']
--
-- * 'lDefaultRecipes' @::@ 'Maybe' 'Recipes'
--
-- * 'lDefaultSecurityGroupNames' @::@ ['Text']
--
-- * 'lEnableAutoHealing' @::@ 'Maybe' 'Bool'
--
-- * 'lInstallUpdatesOnBoot' @::@ 'Maybe' 'Bool'
--
-- * 'lLayerId' @::@ 'Maybe' 'Text'
--
-- * 'lName' @::@ 'Maybe' 'Text'
--
-- * 'lPackages' @::@ ['Text']
--
-- * 'lShortname' @::@ 'Maybe' 'Text'
--
-- * 'lStackId' @::@ 'Maybe' 'Text'
--
-- * 'lType' @::@ 'Maybe' 'Text'
--
-- * 'lUseEbsOptimizedInstances' @::@ 'Maybe' 'Bool'
--
-- * 'lVolumeConfigurations' @::@ ['VolumeConfiguration']
--
layer :: Layer
layer = Layer
    { _lStackId                   = Nothing
    , _lLayerId                   = Nothing
    , _lType                      = Nothing
    , _lName                      = Nothing
    , _lShortname                 = Nothing
    , _lAttributes                = mempty
    , _lCustomInstanceProfileArn  = Nothing
    , _lCustomSecurityGroupIds    = mempty
    , _lDefaultSecurityGroupNames = mempty
    , _lPackages                  = mempty
    , _lVolumeConfigurations      = mempty
    , _lEnableAutoHealing         = Nothing
    , _lAutoAssignElasticIps      = Nothing
    , _lAutoAssignPublicIps       = Nothing
    , _lDefaultRecipes            = Nothing
    , _lCustomRecipes             = Nothing
    , _lCreatedAt                 = Nothing
    , _lInstallUpdatesOnBoot      = Nothing
    , _lUseEbsOptimizedInstances  = Nothing
    }

-- | The layer attributes.
lAttributes :: Lens' Layer (HashMap Text Text)
lAttributes = lens _lAttributes (\s a -> s { _lAttributes = a })
    . _Map

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
lAutoAssignElasticIps :: Lens' Layer (Maybe Bool)
lAutoAssignElasticIps =
    lens _lAutoAssignElasticIps (\s a -> s { _lAutoAssignElasticIps = a })

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
lAutoAssignPublicIps :: Lens' Layer (Maybe Bool)
lAutoAssignPublicIps =
    lens _lAutoAssignPublicIps (\s a -> s { _lAutoAssignPublicIps = a })

-- | Date when the layer was created.
lCreatedAt :: Lens' Layer (Maybe Text)
lCreatedAt = lens _lCreatedAt (\s a -> s { _lCreatedAt = a })

-- | The ARN of the default IAM profile to be used for the layer's EC2
-- instances. For more information about IAM ARNs, see Using Identifiers.
lCustomInstanceProfileArn :: Lens' Layer (Maybe Text)
lCustomInstanceProfileArn =
    lens _lCustomInstanceProfileArn
        (\s a -> s { _lCustomInstanceProfileArn = a })

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
lCustomRecipes :: Lens' Layer (Maybe Recipes)
lCustomRecipes = lens _lCustomRecipes (\s a -> s { _lCustomRecipes = a })

-- | An array containing the layer's custom security group IDs.
lCustomSecurityGroupIds :: Lens' Layer [Text]
lCustomSecurityGroupIds =
    lens _lCustomSecurityGroupIds (\s a -> s { _lCustomSecurityGroupIds = a })

lDefaultRecipes :: Lens' Layer (Maybe Recipes)
lDefaultRecipes = lens _lDefaultRecipes (\s a -> s { _lDefaultRecipes = a })

-- | An array containing the layer's security group names.
lDefaultSecurityGroupNames :: Lens' Layer [Text]
lDefaultSecurityGroupNames =
    lens _lDefaultSecurityGroupNames
        (\s a -> s { _lDefaultSecurityGroupNames = a })

-- | Whether auto healing is disabled for the layer.
lEnableAutoHealing :: Lens' Layer (Maybe Bool)
lEnableAutoHealing =
    lens _lEnableAutoHealing (\s a -> s { _lEnableAutoHealing = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux)
-- or apt-get (Ubuntu) on the instances.
lInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
lInstallUpdatesOnBoot =
    lens _lInstallUpdatesOnBoot (\s a -> s { _lInstallUpdatesOnBoot = a })

-- | The layer ID.
lLayerId :: Lens' Layer (Maybe Text)
lLayerId = lens _lLayerId (\s a -> s { _lLayerId = a })

-- | The layer name.
lName :: Lens' Layer (Maybe Text)
lName = lens _lName (\s a -> s { _lName = a })

-- | An array of Package objects that describe the layer's packages.
lPackages :: Lens' Layer [Text]
lPackages = lens _lPackages (\s a -> s { _lPackages = a })

-- | The layer short name.
lShortname :: Lens' Layer (Maybe Text)
lShortname = lens _lShortname (\s a -> s { _lShortname = a })

-- | The layer stack ID.
lStackId :: Lens' Layer (Maybe Text)
lStackId = lens _lStackId (\s a -> s { _lStackId = a })

-- | The layer type, which must be one of the following: Custom
-- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster
-- NodeJsAppServer PhpAppServer RailsAppServer WebServer.
lType :: Lens' Layer (Maybe Text)
lType = lens _lType (\s a -> s { _lType = a })

-- | Whether the layer uses Amazon EBS-optimized instances.
lUseEbsOptimizedInstances :: Lens' Layer (Maybe Bool)
lUseEbsOptimizedInstances =
    lens _lUseEbsOptimizedInstances
        (\s a -> s { _lUseEbsOptimizedInstances = a })

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
lVolumeConfigurations :: Lens' Layer [VolumeConfiguration]
lVolumeConfigurations =
    lens _lVolumeConfigurations (\s a -> s { _lVolumeConfigurations = a })

data Recipes = Recipes
    { _rConfigure :: [Text]
    , _rDeploy    :: [Text]
    , _rSetup     :: [Text]
    , _rShutdown  :: [Text]
    , _rUndeploy  :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'Recipes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rConfigure' @::@ ['Text']
--
-- * 'rDeploy' @::@ ['Text']
--
-- * 'rSetup' @::@ ['Text']
--
-- * 'rShutdown' @::@ ['Text']
--
-- * 'rUndeploy' @::@ ['Text']
--
recipes :: Recipes
recipes = Recipes
    { _rSetup     = mempty
    , _rConfigure = mempty
    , _rDeploy    = mempty
    , _rUndeploy  = mempty
    , _rShutdown  = mempty
    }

-- | An array of custom recipe names to be run following a configure event.
rConfigure :: Lens' Recipes [Text]
rConfigure = lens _rConfigure (\s a -> s { _rConfigure = a })

-- | An array of custom recipe names to be run following a deploy event.
rDeploy :: Lens' Recipes [Text]
rDeploy = lens _rDeploy (\s a -> s { _rDeploy = a })

-- | An array of custom recipe names to be run following a setup event.
rSetup :: Lens' Recipes [Text]
rSetup = lens _rSetup (\s a -> s { _rSetup = a })

-- | An array of custom recipe names to be run following a shutdown event.
rShutdown :: Lens' Recipes [Text]
rShutdown = lens _rShutdown (\s a -> s { _rShutdown = a })

-- | An array of custom recipe names to be run following a undeploy event.
rUndeploy :: Lens' Recipes [Text]
rUndeploy = lens _rUndeploy (\s a -> s { _rUndeploy = a })

data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { _tbascAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
    , _tbascInstanceId          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'TimeBasedAutoScalingConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tbascAutoScalingSchedule' @::@ 'Maybe' 'WeeklyAutoScalingSchedule'
--
-- * 'tbascInstanceId' @::@ 'Maybe' 'Text'
--
timeBasedAutoScalingConfiguration :: TimeBasedAutoScalingConfiguration
timeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { _tbascInstanceId          = Nothing
    , _tbascAutoScalingSchedule = Nothing
    }

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
tbascAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule =
    lens _tbascAutoScalingSchedule
        (\s a -> s { _tbascAutoScalingSchedule = a })

-- | The instance ID.
tbascInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbascInstanceId = lens _tbascInstanceId (\s a -> s { _tbascInstanceId = a })

data SelfUserProfile = SelfUserProfile
    { _supIamUserArn   :: Maybe Text
    , _supName         :: Maybe Text
    , _supSshPublicKey :: Maybe Text
    , _supSshUsername  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SelfUserProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'supIamUserArn' @::@ 'Maybe' 'Text'
--
-- * 'supName' @::@ 'Maybe' 'Text'
--
-- * 'supSshPublicKey' @::@ 'Maybe' 'Text'
--
-- * 'supSshUsername' @::@ 'Maybe' 'Text'
--
selfUserProfile :: SelfUserProfile
selfUserProfile = SelfUserProfile
    { _supIamUserArn   = Nothing
    , _supName         = Nothing
    , _supSshUsername  = Nothing
    , _supSshPublicKey = Nothing
    }

-- | The user's IAM ARN.
supIamUserArn :: Lens' SelfUserProfile (Maybe Text)
supIamUserArn = lens _supIamUserArn (\s a -> s { _supIamUserArn = a })

-- | The user's name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName = lens _supName (\s a -> s { _supName = a })

-- | The user's SSH public key.
supSshPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSshPublicKey = lens _supSshPublicKey (\s a -> s { _supSshPublicKey = a })

-- | The user's SSH user name.
supSshUsername :: Lens' SelfUserProfile (Maybe Text)
supSshUsername = lens _supSshUsername (\s a -> s { _supSshUsername = a })

data RootDeviceType
    = Ebs           -- ^ ebs
    | InstanceStore -- ^ instance-store
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RootDeviceType

instance FromText RootDeviceType where
    parser = match "ebs"            Ebs
         <|> match "instance-store" InstanceStore

instance ToText RootDeviceType where
    toText = \case
        Ebs           -> "ebs"
        InstanceStore -> "instance-store"

data Stack = Stack
    { _sArn                       :: Maybe Text
    , _sAttributes                :: Map Text Text
    , _sChefConfiguration         :: Maybe ChefConfiguration
    , _sConfigurationManager      :: Maybe StackConfigurationManager
    , _sCreatedAt                 :: Maybe Text
    , _sCustomCookbooksSource     :: Maybe Source
    , _sCustomJson                :: Maybe Text
    , _sDefaultAvailabilityZone   :: Maybe Text
    , _sDefaultInstanceProfileArn :: Maybe Text
    , _sDefaultOs                 :: Maybe Text
    , _sDefaultRootDeviceType     :: Maybe Text
    , _sDefaultSshKeyName         :: Maybe Text
    , _sDefaultSubnetId           :: Maybe Text
    , _sHostnameTheme             :: Maybe Text
    , _sName                      :: Maybe Text
    , _sRegion                    :: Maybe Text
    , _sServiceRoleArn            :: Maybe Text
    , _sStackId                   :: Maybe Text
    , _sUseCustomCookbooks        :: Maybe Bool
    , _sUseOpsworksSecurityGroups :: Maybe Bool
    , _sVpcId                     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Stack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sArn' @::@ 'Maybe' 'Text'
--
-- * 'sAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'sChefConfiguration' @::@ 'Maybe' 'ChefConfiguration'
--
-- * 'sConfigurationManager' @::@ 'Maybe' 'StackConfigurationManager'
--
-- * 'sCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'sCustomCookbooksSource' @::@ 'Maybe' 'Source'
--
-- * 'sCustomJson' @::@ 'Maybe' 'Text'
--
-- * 'sDefaultAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'sDefaultInstanceProfileArn' @::@ 'Maybe' 'Text'
--
-- * 'sDefaultOs' @::@ 'Maybe' 'Text'
--
-- * 'sDefaultRootDeviceType' @::@ 'Maybe' 'Text'
--
-- * 'sDefaultSshKeyName' @::@ 'Maybe' 'Text'
--
-- * 'sDefaultSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'sHostnameTheme' @::@ 'Maybe' 'Text'
--
-- * 'sName' @::@ 'Maybe' 'Text'
--
-- * 'sRegion' @::@ 'Maybe' 'Text'
--
-- * 'sServiceRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'sStackId' @::@ 'Maybe' 'Text'
--
-- * 'sUseCustomCookbooks' @::@ 'Maybe' 'Bool'
--
-- * 'sUseOpsworksSecurityGroups' @::@ 'Maybe' 'Bool'
--
-- * 'sVpcId' @::@ 'Maybe' 'Text'
--
stack :: Stack
stack = Stack
    { _sStackId                   = Nothing
    , _sName                      = Nothing
    , _sArn                       = Nothing
    , _sRegion                    = Nothing
    , _sVpcId                     = Nothing
    , _sAttributes                = mempty
    , _sServiceRoleArn            = Nothing
    , _sDefaultInstanceProfileArn = Nothing
    , _sDefaultOs                 = Nothing
    , _sHostnameTheme             = Nothing
    , _sDefaultAvailabilityZone   = Nothing
    , _sDefaultSubnetId           = Nothing
    , _sCustomJson                = Nothing
    , _sConfigurationManager      = Nothing
    , _sChefConfiguration         = Nothing
    , _sUseCustomCookbooks        = Nothing
    , _sUseOpsworksSecurityGroups = Nothing
    , _sCustomCookbooksSource     = Nothing
    , _sDefaultSshKeyName         = Nothing
    , _sCreatedAt                 = Nothing
    , _sDefaultRootDeviceType     = Nothing
    }

-- | The stack's ARN.
sArn :: Lens' Stack (Maybe Text)
sArn = lens _sArn (\s a -> s { _sArn = a })

-- | The stack's attributes.
sAttributes :: Lens' Stack (HashMap Text Text)
sAttributes = lens _sAttributes (\s a -> s { _sAttributes = a })
    . _Map

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version. For more information, see Create a New Stack.
sChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
sChefConfiguration =
    lens _sChefConfiguration (\s a -> s { _sChefConfiguration = a })

-- | The configuration manager.
sConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
sConfigurationManager =
    lens _sConfigurationManager (\s a -> s { _sConfigurationManager = a })

-- | Date when the stack was created.
sCreatedAt :: Lens' Stack (Maybe Text)
sCreatedAt = lens _sCreatedAt (\s a -> s { _sCreatedAt = a })

sCustomCookbooksSource :: Lens' Stack (Maybe Source)
sCustomCookbooksSource =
    lens _sCustomCookbooksSource (\s a -> s { _sCustomCookbooksSource = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as
-- '"'.: "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more
-- information on custom JSON, see Use Custom JSON to Modify the Stack
-- Configuration JSON.
sCustomJson :: Lens' Stack (Maybe Text)
sCustomJson = lens _sCustomJson (\s a -> s { _sCustomJson = a })

-- | The stack's default Availability Zone. For more information, see Regions
-- and Endpoints.
sDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
sDefaultAvailabilityZone =
    lens _sDefaultAvailabilityZone
        (\s a -> s { _sDefaultAvailabilityZone = a })

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
sDefaultInstanceProfileArn :: Lens' Stack (Maybe Text)
sDefaultInstanceProfileArn =
    lens _sDefaultInstanceProfileArn
        (\s a -> s { _sDefaultInstanceProfileArn = a })

-- | The stack's default operating system, which must be set to Amazon Linux
-- or Ubuntu 12.04 LTS. The default option is Amazon Linux.
sDefaultOs :: Lens' Stack (Maybe Text)
sDefaultOs = lens _sDefaultOs (\s a -> s { _sDefaultOs = a })

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
sDefaultRootDeviceType :: Lens' Stack (Maybe Text)
sDefaultRootDeviceType =
    lens _sDefaultRootDeviceType (\s a -> s { _sDefaultRootDeviceType = a })

-- | A default SSH key for the stack's instances. You can override this value
-- when you create or update an instance.
sDefaultSshKeyName :: Lens' Stack (Maybe Text)
sDefaultSshKeyName =
    lens _sDefaultSshKeyName (\s a -> s { _sDefaultSshKeyName = a })

-- | The default subnet ID, if the stack is running in a VPC.
sDefaultSubnetId :: Lens' Stack (Maybe Text)
sDefaultSubnetId = lens _sDefaultSubnetId (\s a -> s { _sDefaultSubnetId = a })

-- | The stack host name theme, with spaces replaced by underscores.
sHostnameTheme :: Lens' Stack (Maybe Text)
sHostnameTheme = lens _sHostnameTheme (\s a -> s { _sHostnameTheme = a })

-- | The stack name.
sName :: Lens' Stack (Maybe Text)
sName = lens _sName (\s a -> s { _sName = a })

-- | The stack AWS region, such as "us-east-1". For more information about AWS
-- regions, see Regions and Endpoints.
sRegion :: Lens' Stack (Maybe Text)
sRegion = lens _sRegion (\s a -> s { _sRegion = a })

-- | The stack AWS Identity and Access Management (IAM) role.
sServiceRoleArn :: Lens' Stack (Maybe Text)
sServiceRoleArn = lens _sServiceRoleArn (\s a -> s { _sServiceRoleArn = a })

-- | The stack ID.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\s a -> s { _sStackId = a })

-- | Whether the stack uses custom cookbooks.
sUseCustomCookbooks :: Lens' Stack (Maybe Bool)
sUseCustomCookbooks =
    lens _sUseCustomCookbooks (\s a -> s { _sUseCustomCookbooks = a })

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack's layers.
sUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
sUseOpsworksSecurityGroups =
    lens _sUseOpsworksSecurityGroups
        (\s a -> s { _sUseOpsworksSecurityGroups = a })

-- | The VPC ID, if the stack is running in a VPC.
sVpcId :: Lens' Stack (Maybe Text)
sVpcId = lens _sVpcId (\s a -> s { _sVpcId = a })

data DeploymentCommand = DeploymentCommand
    { _dcArgs :: Map Text [Text]
    , _dcName :: Text
    } deriving (Eq, Show, Generic)

-- | 'DeploymentCommand' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcArgs' @::@ 'HashMap' 'Text' ['Text']
--
-- * 'dcName' @::@ 'Text'
--
deploymentCommand :: Text -- ^ 'dcName'
                  -> DeploymentCommand
deploymentCommand p1 = DeploymentCommand
    { _dcName = p1
    , _dcArgs = mempty
    }

-- | The arguments of those commands that take arguments. It should be set to
-- a JSON object with the following format: {"arg_name":["value1", "value2",
-- ...]}.
dcArgs :: Lens' DeploymentCommand (HashMap Text [Text])
dcArgs = lens _dcArgs (\s a -> s { _dcArgs = a })
    . _Map

-- | Specifies the operation. You can specify only one command. For stacks,
-- the following commands are available: execute_recipes: Execute one or
-- more recipes. To specify the recipes, set an Args parameter named recipes
-- to the list of recipes to be executed. For example, to execute
-- phpapp::appsetup, set Args to {"recipes":["phpapp::appsetup"]}.
-- install_dependencies: Install the stack's dependencies.
-- update_custom_cookbooks: Update the stack's custom cookbooks.
-- update_dependencies: Update the stack's dependencies. For apps, the
-- following commands are available: deploy: Deploy an app. Rails apps have
-- an optional Args parameter named migrate. Set Args to
-- {"migrate":["true"]} to migrate the database. The default setting is
-- {"migrate":["false"]}. rollback Roll the app back to the previous
-- version. When you update an app, AWS OpsWorks stores the previous
-- version, up to a maximum of five versions. You can use this command to
-- roll an app back as many as four versions. start: Start the app's web or
-- application server. stop: Stop the app's web or application server.
-- restart: Restart the app's web or application server. undeploy: Undeploy
-- the app.
dcName :: Lens' DeploymentCommand Text
dcName = lens _dcName (\s a -> s { _dcName = a })

data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassFriday    :: Map Text Text
    , _wassMonday    :: Map Text Text
    , _wassSaturday  :: Map Text Text
    , _wassSunday    :: Map Text Text
    , _wassThursday  :: Map Text Text
    , _wassTuesday   :: Map Text Text
    , _wassWednesday :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'WeeklyAutoScalingSchedule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wassFriday' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'wassMonday' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'wassSaturday' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'wassSunday' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'wassThursday' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'wassTuesday' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'wassWednesday' @::@ 'HashMap' 'Text' 'Text'
--
weeklyAutoScalingSchedule :: WeeklyAutoScalingSchedule
weeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassMonday    = mempty
    , _wassTuesday   = mempty
    , _wassWednesday = mempty
    , _wassThursday  = mempty
    , _wassFriday    = mempty
    , _wassSaturday  = mempty
    , _wassSunday    = mempty
    }

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassFriday = lens _wassFriday (\s a -> s { _wassFriday = a })
    . _Map

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassMonday = lens _wassMonday (\s a -> s { _wassMonday = a })
    . _Map

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSaturday = lens _wassSaturday (\s a -> s { _wassSaturday = a })
    . _Map

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSunday = lens _wassSunday (\s a -> s { _wassSunday = a })
    . _Map

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassThursday = lens _wassThursday (\s a -> s { _wassThursday = a })
    . _Map

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassTuesday = lens _wassTuesday (\s a -> s { _wassTuesday = a })
    . _Map

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassWednesday = lens _wassWednesday (\s a -> s { _wassWednesday = a })
    . _Map

data DeploymentCommandName
    = Deploy                -- ^ deploy
    | ExecuteRecipes        -- ^ execute_recipes
    | InstallDependencies   -- ^ install_dependencies
    | Restart               -- ^ restart
    | Rollback              -- ^ rollback
    | Start                 -- ^ start
    | Stop                  -- ^ stop
    | Undeploy              -- ^ undeploy
    | UpdateCustomCookbooks -- ^ update_custom_cookbooks
    | UpdateDependencies    -- ^ update_dependencies
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DeploymentCommandName

instance FromText DeploymentCommandName where
    parser = match "deploy"                  Deploy
         <|> match "execute_recipes"         ExecuteRecipes
         <|> match "install_dependencies"    InstallDependencies
         <|> match "restart"                 Restart
         <|> match "rollback"                Rollback
         <|> match "start"                   Start
         <|> match "stop"                    Stop
         <|> match "undeploy"                Undeploy
         <|> match "update_custom_cookbooks" UpdateCustomCookbooks
         <|> match "update_dependencies"     UpdateDependencies

instance ToText DeploymentCommandName where
    toText = \case
        Deploy                -> "deploy"
        ExecuteRecipes        -> "execute_recipes"
        InstallDependencies   -> "install_dependencies"
        Restart               -> "restart"
        Rollback              -> "rollback"
        Start                 -> "start"
        Stop                  -> "stop"
        Undeploy              -> "undeploy"
        UpdateCustomCookbooks -> "update_custom_cookbooks"
        UpdateDependencies    -> "update_dependencies"

data Instance = Instance
    { _iAmiId                    :: Maybe Text
    , _iArchitecture             :: Maybe Text
    , _iAutoScalingType          :: Maybe Text
    , _iAvailabilityZone         :: Maybe Text
    , _iCreatedAt                :: Maybe Text
    , _iEbsOptimized             :: Maybe Bool
    , _iEc2InstanceId            :: Maybe Text
    , _iElasticIp                :: Maybe Text
    , _iHostname                 :: Maybe Text
    , _iInstallUpdatesOnBoot     :: Maybe Bool
    , _iInstanceId               :: Maybe Text
    , _iInstanceProfileArn       :: Maybe Text
    , _iInstanceType             :: Maybe Text
    , _iLastServiceErrorId       :: Maybe Text
    , _iLayerIds                 :: [Text]
    , _iOs                       :: Maybe Text
    , _iPrivateDns               :: Maybe Text
    , _iPrivateIp                :: Maybe Text
    , _iPublicDns                :: Maybe Text
    , _iPublicIp                 :: Maybe Text
    , _iRootDeviceType           :: Maybe Text
    , _iRootDeviceVolumeId       :: Maybe Text
    , _iSecurityGroupIds         :: [Text]
    , _iSshHostDsaKeyFingerprint :: Maybe Text
    , _iSshHostRsaKeyFingerprint :: Maybe Text
    , _iSshKeyName               :: Maybe Text
    , _iStackId                  :: Maybe Text
    , _iStatus                   :: Maybe Text
    , _iSubnetId                 :: Maybe Text
    , _iVirtualizationType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iAmiId' @::@ 'Maybe' 'Text'
--
-- * 'iArchitecture' @::@ 'Maybe' 'Text'
--
-- * 'iAutoScalingType' @::@ 'Maybe' 'Text'
--
-- * 'iAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'iCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'iEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'iEc2InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iElasticIp' @::@ 'Maybe' 'Text'
--
-- * 'iHostname' @::@ 'Maybe' 'Text'
--
-- * 'iInstallUpdatesOnBoot' @::@ 'Maybe' 'Bool'
--
-- * 'iInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iInstanceProfileArn' @::@ 'Maybe' 'Text'
--
-- * 'iInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'iLastServiceErrorId' @::@ 'Maybe' 'Text'
--
-- * 'iLayerIds' @::@ ['Text']
--
-- * 'iOs' @::@ 'Maybe' 'Text'
--
-- * 'iPrivateDns' @::@ 'Maybe' 'Text'
--
-- * 'iPrivateIp' @::@ 'Maybe' 'Text'
--
-- * 'iPublicDns' @::@ 'Maybe' 'Text'
--
-- * 'iPublicIp' @::@ 'Maybe' 'Text'
--
-- * 'iRootDeviceType' @::@ 'Maybe' 'Text'
--
-- * 'iRootDeviceVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'iSecurityGroupIds' @::@ ['Text']
--
-- * 'iSshHostDsaKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'iSshHostRsaKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'iSshKeyName' @::@ 'Maybe' 'Text'
--
-- * 'iStackId' @::@ 'Maybe' 'Text'
--
-- * 'iStatus' @::@ 'Maybe' 'Text'
--
-- * 'iSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'iVirtualizationType' @::@ 'Maybe' 'Text'
--
instance' :: Instance
instance' = Instance
    { _iInstanceId               = Nothing
    , _iEc2InstanceId            = Nothing
    , _iVirtualizationType       = Nothing
    , _iHostname                 = Nothing
    , _iStackId                  = Nothing
    , _iLayerIds                 = mempty
    , _iSecurityGroupIds         = mempty
    , _iInstanceType             = Nothing
    , _iInstanceProfileArn       = Nothing
    , _iStatus                   = Nothing
    , _iOs                       = Nothing
    , _iAmiId                    = Nothing
    , _iAvailabilityZone         = Nothing
    , _iSubnetId                 = Nothing
    , _iPublicDns                = Nothing
    , _iPrivateDns               = Nothing
    , _iPublicIp                 = Nothing
    , _iPrivateIp                = Nothing
    , _iElasticIp                = Nothing
    , _iAutoScalingType          = Nothing
    , _iSshKeyName               = Nothing
    , _iSshHostRsaKeyFingerprint = Nothing
    , _iSshHostDsaKeyFingerprint = Nothing
    , _iCreatedAt                = Nothing
    , _iLastServiceErrorId       = Nothing
    , _iArchitecture             = Nothing
    , _iRootDeviceType           = Nothing
    , _iRootDeviceVolumeId       = Nothing
    , _iInstallUpdatesOnBoot     = Nothing
    , _iEbsOptimized             = Nothing
    }

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu
-- 12.04 LTS. For more information, see Instances.
iAmiId :: Lens' Instance (Maybe Text)
iAmiId = lens _iAmiId (\s a -> s { _iAmiId = a })

-- | The instance architecture, "i386" or "x86_64".
iArchitecture :: Lens' Instance (Maybe Text)
iArchitecture = lens _iArchitecture (\s a -> s { _iArchitecture = a })

-- | The instance's auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is
-- started and stopped based on a specified schedule. LoadBasedAutoScaling:
-- A load-based auto scaling instance, which is started and stopped based on
-- load metrics.
iAutoScalingType :: Lens' Instance (Maybe Text)
iAutoScalingType = lens _iAutoScalingType (\s a -> s { _iAutoScalingType = a })

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
iAvailabilityZone :: Lens' Instance (Maybe Text)
iAvailabilityZone =
    lens _iAvailabilityZone (\s a -> s { _iAvailabilityZone = a })

-- | The time that the instance was created.
iCreatedAt :: Lens' Instance (Maybe Text)
iCreatedAt = lens _iCreatedAt (\s a -> s { _iCreatedAt = a })

-- | Whether this is an Amazon EBS-optimized instance.
iEbsOptimized :: Lens' Instance (Maybe Bool)
iEbsOptimized = lens _iEbsOptimized (\s a -> s { _iEbsOptimized = a })

-- | The ID of the associated Amazon EC2 instance.
iEc2InstanceId :: Lens' Instance (Maybe Text)
iEc2InstanceId = lens _iEc2InstanceId (\s a -> s { _iEc2InstanceId = a })

-- | The instance Elastic IP address .
iElasticIp :: Lens' Instance (Maybe Text)
iElasticIp = lens _iElasticIp (\s a -> s { _iElasticIp = a })

-- | The instance host name.
iHostname :: Lens' Instance (Maybe Text)
iHostname = lens _iHostname (\s a -> s { _iHostname = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux)
-- or apt-get (Ubuntu) on the instances.
iInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
iInstallUpdatesOnBoot =
    lens _iInstallUpdatesOnBoot (\s a -> s { _iInstallUpdatesOnBoot = a })

-- | The instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

-- | The ARN of the instance's IAM profile. For more information about IAM
-- ARNs, see Using Identifiers.
iInstanceProfileArn :: Lens' Instance (Maybe Text)
iInstanceProfileArn =
    lens _iInstanceProfileArn (\s a -> s { _iInstanceProfileArn = a })

-- | The instance type. AWS OpsWorks supports all instance types except
-- Cluster Compute, Cluster GPU, and High Memory Cluster. For more
-- information, see Instance Families and Types. The parameter values that
-- specify the various types are in the API Name column of the Available
-- Instance Types table.
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\s a -> s { _iInstanceType = a })

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
iLastServiceErrorId :: Lens' Instance (Maybe Text)
iLastServiceErrorId =
    lens _iLastServiceErrorId (\s a -> s { _iLastServiceErrorId = a })

-- | An array containing the instance layer IDs.
iLayerIds :: Lens' Instance [Text]
iLayerIds = lens _iLayerIds (\s a -> s { _iLayerIds = a })

-- | The instance operating system.
iOs :: Lens' Instance (Maybe Text)
iOs = lens _iOs (\s a -> s { _iOs = a })

-- | The instance private DNS name.
iPrivateDns :: Lens' Instance (Maybe Text)
iPrivateDns = lens _iPrivateDns (\s a -> s { _iPrivateDns = a })

-- | The instance private IP address.
iPrivateIp :: Lens' Instance (Maybe Text)
iPrivateIp = lens _iPrivateIp (\s a -> s { _iPrivateIp = a })

-- | The instance public DNS name.
iPublicDns :: Lens' Instance (Maybe Text)
iPublicDns = lens _iPublicDns (\s a -> s { _iPublicDns = a })

-- | The instance public IP address.
iPublicIp :: Lens' Instance (Maybe Text)
iPublicIp = lens _iPublicIp (\s a -> s { _iPublicIp = a })

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
iRootDeviceType :: Lens' Instance (Maybe Text)
iRootDeviceType = lens _iRootDeviceType (\s a -> s { _iRootDeviceType = a })

-- | The root device volume ID.
iRootDeviceVolumeId :: Lens' Instance (Maybe Text)
iRootDeviceVolumeId =
    lens _iRootDeviceVolumeId (\s a -> s { _iRootDeviceVolumeId = a })

-- | An array containing the instance security group IDs.
iSecurityGroupIds :: Lens' Instance [Text]
iSecurityGroupIds =
    lens _iSecurityGroupIds (\s a -> s { _iSecurityGroupIds = a })

-- | The SSH key's DSA fingerprint.
iSshHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSshHostDsaKeyFingerprint =
    lens _iSshHostDsaKeyFingerprint
        (\s a -> s { _iSshHostDsaKeyFingerprint = a })

-- | The SSH key's RSA fingerprint.
iSshHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSshHostRsaKeyFingerprint =
    lens _iSshHostRsaKeyFingerprint
        (\s a -> s { _iSshHostRsaKeyFingerprint = a })

-- | The instance SSH key name.
iSshKeyName :: Lens' Instance (Maybe Text)
iSshKeyName = lens _iSshKeyName (\s a -> s { _iSshKeyName = a })

-- | The stack ID.
iStackId :: Lens' Instance (Maybe Text)
iStackId = lens _iStackId (\s a -> s { _iStackId = a })

-- | The instance status: requested booting running_setup online setup_failed
-- start_failed terminating terminated stopped connection_lost.
iStatus :: Lens' Instance (Maybe Text)
iStatus = lens _iStatus (\s a -> s { _iStatus = a })

-- | The instance's subnet ID, if the stack is running in a VPC.
iSubnetId :: Lens' Instance (Maybe Text)
iSubnetId = lens _iSubnetId (\s a -> s { _iSubnetId = a })

-- | The instance's virtualization type, paravirtual or hvm.
iVirtualizationType :: Lens' Instance (Maybe Text)
iVirtualizationType =
    lens _iVirtualizationType (\s a -> s { _iVirtualizationType = a })

data Deployment = Deployment
    { _dAppId        :: Maybe Text
    , _dCommand      :: Maybe DeploymentCommand
    , _dComment      :: Maybe Text
    , _dCompletedAt  :: Maybe Text
    , _dCreatedAt    :: Maybe Text
    , _dCustomJson   :: Maybe Text
    , _dDeploymentId :: Maybe Text
    , _dDuration     :: Maybe Int
    , _dIamUserArn   :: Maybe Text
    , _dInstanceIds  :: [Text]
    , _dStackId      :: Maybe Text
    , _dStatus       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Deployment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dAppId' @::@ 'Maybe' 'Text'
--
-- * 'dCommand' @::@ 'Maybe' 'DeploymentCommand'
--
-- * 'dComment' @::@ 'Maybe' 'Text'
--
-- * 'dCompletedAt' @::@ 'Maybe' 'Text'
--
-- * 'dCreatedAt' @::@ 'Maybe' 'Text'
--
-- * 'dCustomJson' @::@ 'Maybe' 'Text'
--
-- * 'dDeploymentId' @::@ 'Maybe' 'Text'
--
-- * 'dDuration' @::@ 'Maybe' 'Int'
--
-- * 'dIamUserArn' @::@ 'Maybe' 'Text'
--
-- * 'dInstanceIds' @::@ ['Text']
--
-- * 'dStackId' @::@ 'Maybe' 'Text'
--
-- * 'dStatus' @::@ 'Maybe' 'Text'
--
deployment :: Deployment
deployment = Deployment
    { _dDeploymentId = Nothing
    , _dStackId      = Nothing
    , _dAppId        = Nothing
    , _dCreatedAt    = Nothing
    , _dCompletedAt  = Nothing
    , _dDuration     = Nothing
    , _dIamUserArn   = Nothing
    , _dComment      = Nothing
    , _dCommand      = Nothing
    , _dStatus       = Nothing
    , _dCustomJson   = Nothing
    , _dInstanceIds  = mempty
    }

-- | The app ID.
dAppId :: Lens' Deployment (Maybe Text)
dAppId = lens _dAppId (\s a -> s { _dAppId = a })

dCommand :: Lens' Deployment (Maybe DeploymentCommand)
dCommand = lens _dCommand (\s a -> s { _dCommand = a })

-- | A user-defined comment.
dComment :: Lens' Deployment (Maybe Text)
dComment = lens _dComment (\s a -> s { _dComment = a })

-- | Date when the deployment completed.
dCompletedAt :: Lens' Deployment (Maybe Text)
dCompletedAt = lens _dCompletedAt (\s a -> s { _dCompletedAt = a })

-- | Date when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\s a -> s { _dCreatedAt = a })

-- | A string that contains user-defined custom JSON. It is used to override
-- the corresponding default stack configuration JSON values for stack. The
-- string should be in the following format and must escape characters such
-- as '"'.: "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more
-- information on custom JSON, see Use Custom JSON to Modify the Stack
-- Configuration JSON.
dCustomJson :: Lens' Deployment (Maybe Text)
dCustomJson = lens _dCustomJson (\s a -> s { _dCustomJson = a })

-- | The deployment ID.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\s a -> s { _dDeploymentId = a })

-- | The deployment duration.
dDuration :: Lens' Deployment (Maybe Int)
dDuration = lens _dDuration (\s a -> s { _dDuration = a })

-- | The user's IAM ARN.
dIamUserArn :: Lens' Deployment (Maybe Text)
dIamUserArn = lens _dIamUserArn (\s a -> s { _dIamUserArn = a })

-- | The IDs of the target instances.
dInstanceIds :: Lens' Deployment [Text]
dInstanceIds = lens _dInstanceIds (\s a -> s { _dInstanceIds = a })

-- | The stack ID.
dStackId :: Lens' Deployment (Maybe Text)
dStackId = lens _dStackId (\s a -> s { _dStackId = a })

-- | The deployment status: running successful failed.
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\s a -> s { _dStatus = a })

data InstancesCount = InstancesCount
    { _icBooting        :: Maybe Int
    , _icConnectionLost :: Maybe Int
    , _icOnline         :: Maybe Int
    , _icPending        :: Maybe Int
    , _icRebooting      :: Maybe Int
    , _icRequested      :: Maybe Int
    , _icRunningSetup   :: Maybe Int
    , _icSetupFailed    :: Maybe Int
    , _icShuttingDown   :: Maybe Int
    , _icStartFailed    :: Maybe Int
    , _icStopped        :: Maybe Int
    , _icStopping       :: Maybe Int
    , _icTerminated     :: Maybe Int
    , _icTerminating    :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstancesCount' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icBooting' @::@ 'Maybe' 'Int'
--
-- * 'icConnectionLost' @::@ 'Maybe' 'Int'
--
-- * 'icOnline' @::@ 'Maybe' 'Int'
--
-- * 'icPending' @::@ 'Maybe' 'Int'
--
-- * 'icRebooting' @::@ 'Maybe' 'Int'
--
-- * 'icRequested' @::@ 'Maybe' 'Int'
--
-- * 'icRunningSetup' @::@ 'Maybe' 'Int'
--
-- * 'icSetupFailed' @::@ 'Maybe' 'Int'
--
-- * 'icShuttingDown' @::@ 'Maybe' 'Int'
--
-- * 'icStartFailed' @::@ 'Maybe' 'Int'
--
-- * 'icStopped' @::@ 'Maybe' 'Int'
--
-- * 'icStopping' @::@ 'Maybe' 'Int'
--
-- * 'icTerminated' @::@ 'Maybe' 'Int'
--
-- * 'icTerminating' @::@ 'Maybe' 'Int'
--
instancesCount :: InstancesCount
instancesCount = InstancesCount
    { _icBooting        = Nothing
    , _icConnectionLost = Nothing
    , _icOnline         = Nothing
    , _icPending        = Nothing
    , _icRebooting      = Nothing
    , _icRequested      = Nothing
    , _icRunningSetup   = Nothing
    , _icSetupFailed    = Nothing
    , _icShuttingDown   = Nothing
    , _icStartFailed    = Nothing
    , _icStopped        = Nothing
    , _icStopping       = Nothing
    , _icTerminated     = Nothing
    , _icTerminating    = Nothing
    }

-- | The number of instances with booting status.
icBooting :: Lens' InstancesCount (Maybe Int)
icBooting = lens _icBooting (\s a -> s { _icBooting = a })

-- | The number of instances with connection_lost status.
icConnectionLost :: Lens' InstancesCount (Maybe Int)
icConnectionLost = lens _icConnectionLost (\s a -> s { _icConnectionLost = a })

-- | The number of instances with online status.
icOnline :: Lens' InstancesCount (Maybe Int)
icOnline = lens _icOnline (\s a -> s { _icOnline = a })

-- | The number of instances with pending status.
icPending :: Lens' InstancesCount (Maybe Int)
icPending = lens _icPending (\s a -> s { _icPending = a })

-- | The number of instances with rebooting status.
icRebooting :: Lens' InstancesCount (Maybe Int)
icRebooting = lens _icRebooting (\s a -> s { _icRebooting = a })

-- | The number of instances with requested status.
icRequested :: Lens' InstancesCount (Maybe Int)
icRequested = lens _icRequested (\s a -> s { _icRequested = a })

-- | The number of instances with running_setup status.
icRunningSetup :: Lens' InstancesCount (Maybe Int)
icRunningSetup = lens _icRunningSetup (\s a -> s { _icRunningSetup = a })

-- | The number of instances with setup_failed status.
icSetupFailed :: Lens' InstancesCount (Maybe Int)
icSetupFailed = lens _icSetupFailed (\s a -> s { _icSetupFailed = a })

-- | The number of instances with shutting_down status.
icShuttingDown :: Lens' InstancesCount (Maybe Int)
icShuttingDown = lens _icShuttingDown (\s a -> s { _icShuttingDown = a })

-- | The number of instances with start_failed status.
icStartFailed :: Lens' InstancesCount (Maybe Int)
icStartFailed = lens _icStartFailed (\s a -> s { _icStartFailed = a })

-- | The number of instances with stopped status.
icStopped :: Lens' InstancesCount (Maybe Int)
icStopped = lens _icStopped (\s a -> s { _icStopped = a })

-- | The number of instances with stopping status.
icStopping :: Lens' InstancesCount (Maybe Int)
icStopping = lens _icStopping (\s a -> s { _icStopping = a })

-- | The number of instances with terminated status.
icTerminated :: Lens' InstancesCount (Maybe Int)
icTerminated = lens _icTerminated (\s a -> s { _icTerminated = a })

-- | The number of instances with terminating status.
icTerminating :: Lens' InstancesCount (Maybe Int)
icTerminating = lens _icTerminating (\s a -> s { _icTerminating = a })

data AppType
    = Nodejs -- ^ nodejs
    | Other  -- ^ other
    | Php    -- ^ php
    | Rails  -- ^ rails
    | Static -- ^ static
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AppType

instance FromText AppType where
    parser = match "nodejs" Nodejs
         <|> match "other"  Other
         <|> match "php"    Php
         <|> match "rails"  Rails
         <|> match "static" Static

instance ToText AppType where
    toText = \case
        Nodejs -> "nodejs"
        Other  -> "other"
        Php    -> "php"
        Rails  -> "rails"
        Static -> "static"
