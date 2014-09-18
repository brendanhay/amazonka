{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS OpsWorks provides a simple and flexible way to create and manage stacks
-- and applications. With AWS OpsWorks, you can provision AWS resources,
-- manage their configuration, deploy applications to those resources, and
-- monitor their health.
module Network.AWS.OpsWorks.Types
    (
    -- * Service
      OpsWorks
    -- ** Errors
    , OpsWorksError (..)
    , _OpsWorksClient
    , _OpsWorksSerializer
    , _OpsWorksService
    , _ResourceNotFoundException
    , _ValidationException
    -- * AppAttributesKeys
    , AppAttributesKeys (..)

    -- * AppType
    , AppType (..)

    -- * Architecture
    , Architecture (..)

    -- * AutoScalingType
    , AutoScalingType (..)

    -- * DeploymentCommandName
    , DeploymentCommandName (..)

    -- * LayerAttributesKeys
    , LayerAttributesKeys (..)

    -- * LayerType
    , LayerType (..)

    -- * RootDeviceType
    , RootDeviceType (..)

    -- * SourceType
    , SourceType (..)

    -- * StackAttributesKeys
    , StackAttributesKeys (..)

    -- * App
    , App
    , app
    , aAppId
    , aStackId
    , aShortname
    , aName
    , aDescription
    , aDataSources
    , aType
    , aAppSource
    , aDomains
    , aEnableSsl
    , aSslConfiguration
    , aAttributes
    , aCreatedAt

    -- * AutoScalingThresholds
    , AutoScalingThresholds
    , autoScalingThresholds
    , astInstanceCount
    , astThresholdsWaitTime
    , astIgnoreMetricsTime
    , astCpuThreshold
    , astMemoryThreshold
    , astLoadThreshold

    -- * ChefConfiguration
    , ChefConfiguration
    , chefConfiguration
    , ccManageBerkshelf
    , ccBerkshelfVersion

    -- * Command
    , Command
    , command
    , cCommandId
    , cInstanceId
    , cDeploymentId
    , cCreatedAt
    , cAcknowledgedAt
    , cCompletedAt
    , cStatus
    , cExitCode
    , cLogUrl
    , cType

    -- * DataSource
    , DataSource
    , dataSource
    , dsType
    , dsArn
    , dsDatabaseName

    -- * Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dStackId
    , dAppId
    , dCreatedAt
    , dCompletedAt
    , dDuration
    , dIamUserArn
    , dComment
    , dCommand
    , dStatus
    , dCustomJson
    , dInstanceIds

    -- * DeploymentCommand
    , DeploymentCommand
    , deploymentCommand
    , dcName
    , dcArgs

    -- * ElasticIp
    , ElasticIp
    , elasticIp
    , eiIp
    , eiName
    , eiDomain
    , eiRegion
    , eiInstanceId

    -- * ElasticLoadBalancer
    , ElasticLoadBalancer
    , elasticLoadBalancer
    , elbElasticLoadBalancerName
    , elbRegion
    , elbDnsName
    , elbStackId
    , elbLayerId
    , elbVpcId
    , elbAvailabilityZones
    , elbSubnetIds
    , elbEc2InstanceIds

    -- * Instance
    , Instance
    , instance'
    , iInstanceId
    , iEc2InstanceId
    , iVirtualizationType
    , iHostname
    , iStackId
    , iLayerIds
    , iSecurityGroupIds
    , iInstanceType
    , iInstanceProfileArn
    , iStatus
    , iOs
    , iAmiId
    , iAvailabilityZone
    , iSubnetId
    , iPublicDns
    , iPrivateDns
    , iPublicIp
    , iPrivateIp
    , iElasticIp
    , iAutoScalingType
    , iSshKeyName
    , iSshHostRsaKeyFingerprint
    , iSshHostDsaKeyFingerprint
    , iCreatedAt
    , iLastServiceErrorId
    , iArchitecture
    , iRootDeviceType
    , iRootDeviceVolumeId
    , iInstallUpdatesOnBoot
    , iEbsOptimized

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

    -- * Layer
    , Layer
    , layer
    , lStackId
    , lLayerId
    , lType
    , lName
    , lShortname
    , lAttributes
    , lCustomInstanceProfileArn
    , lCustomSecurityGroupIds
    , lDefaultSecurityGroupNames
    , lPackages
    , lVolumeConfigurations
    , lEnableAutoHealing
    , lAutoAssignElasticIps
    , lAutoAssignPublicIps
    , lDefaultRecipes
    , lCustomRecipes
    , lCreatedAt
    , lInstallUpdatesOnBoot
    , lUseEbsOptimizedInstances

    -- * LoadBasedAutoScalingConfiguration
    , LoadBasedAutoScalingConfiguration
    , loadBasedAutoScalingConfiguration
    , lbascLayerId
    , lbascEnable
    , lbascUpScaling
    , lbascDownScaling

    -- * Permission
    , Permission
    , permission
    , pStackId
    , pIamUserArn
    , pAllowSsh
    , pAllowSudo
    , pLevel

    -- * RaidArray
    , RaidArray
    , raidArray
    , raRaidArrayId
    , raInstanceId
    , raName
    , raRaidLevel
    , raNumberOfDisks
    , raSize
    , raDevice
    , raMountPoint
    , raAvailabilityZone
    , raCreatedAt
    , raVolumeType
    , raIops

    -- * RdsDbInstance
    , RdsDbInstance
    , rdsDbInstance
    , rdiRdsDbInstanceArn
    , rdiDbInstanceIdentifier
    , rdiDbUser
    , rdiDbPassword
    , rdiRegion
    , rdiAddress
    , rdiEngine
    , rdiStackId
    , rdiMissingOnRds

    -- * Recipes
    , Recipes
    , recipes
    , rSetup
    , rConfigure
    , rDeploy
    , rUndeploy
    , rShutdown

    -- * SelfUserProfile
    , SelfUserProfile
    , selfUserProfile
    , supIamUserArn
    , supName
    , supSshUsername
    , supSshPublicKey

    -- * ServiceError
    , ServiceError
    , serviceError'
    , seServiceErrorId
    , seStackId
    , seInstanceId
    , seType
    , seMessage
    , seCreatedAt

    -- * Source
    , Source
    , source
    , sType
    , sUrl
    , sUsername
    , sPassword
    , sSshKey
    , sRevision

    -- * SslConfiguration
    , SslConfiguration
    , sslConfiguration
    , scCertificate
    , scPrivateKey
    , scChain

    -- * Stack
    , Stack
    , stack
    , srStackId
    , srName
    , srArn
    , srRegion
    , srVpcId
    , srAttributes
    , srServiceRoleArn
    , srDefaultInstanceProfileArn
    , srDefaultOs
    , srHostnameTheme
    , srDefaultAvailabilityZone
    , srDefaultSubnetId
    , srCustomJson
    , srConfigurationManager
    , srChefConfiguration
    , srUseCustomCookbooks
    , srUseOpsworksSecurityGroups
    , srCustomCookbooksSource
    , srDefaultSshKeyName
    , srCreatedAt
    , srDefaultRootDeviceType

    -- * StackConfigurationManager
    , StackConfigurationManager
    , stackConfigurationManager
    , scmName
    , scmVersion

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssStackId
    , ssName
    , ssArn
    , ssLayersCount
    , ssAppsCount
    , ssInstancesCount

    -- * TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration
    , timeBasedAutoScalingConfiguration
    , tbascInstanceId
    , tbascAutoScalingSchedule

    -- * UserProfile
    , UserProfile
    , userProfile
    , upIamUserArn
    , upName
    , upSshUsername
    , upSshPublicKey
    , upAllowSelfManagement

    -- * Volume
    , Volume
    , volume
    , vVolumeId
    , vEc2VolumeId
    , vName
    , vRaidArrayId
    , vInstanceId
    , vStatus
    , vSize
    , vDevice
    , vMountPoint
    , vRegion
    , vAvailabilityZone
    , vVolumeType
    , vIops

    -- * VolumeConfiguration
    , VolumeConfiguration
    , volumeConfiguration
    , vcMountPoint
    , vcRaidLevel
    , vcNumberOfDisks
    , vcSize
    , vcVolumeType
    , vcIops

    -- * WeeklyAutoScalingSchedule
    , WeeklyAutoScalingSchedule
    , weeklyAutoScalingSchedule
    , wassMonday
    , wassTuesday
    , wassWednesday
    , wassThursday
    , wassFriday
    , wassSaturday
    , wassSunday
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-02-18@) of the
-- @AWS OpsWorks@ service.
data OpsWorks deriving (Typeable)

instance AWSService OpsWorks where
    type Sg OpsWorks = V4
    type Er OpsWorks = OpsWorksError

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "opsworks"
        , _svcVersion  = "2013-02-18"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'OpsWorks' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data OpsWorksError
    = OpsWorksClient HttpException
    | OpsWorksSerializer String
    | OpsWorksService String
      -- | Indicates that a resource was not found.
    | ResourceNotFoundException
        { _rnfeMessage :: Maybe Text
        }
      -- | Indicates that a request was invalid.
    | ValidationException
        { _veMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError OpsWorksError where
    awsError = const "OpsWorksError"

instance AWSServiceError OpsWorksError where
    serviceError    = OpsWorksService
    clientError     = OpsWorksClient
    serializerError = OpsWorksSerializer

instance Exception OpsWorksError

-- | See: 'OpsWorksClient'
_OpsWorksClient :: Prism' OpsWorksError HttpException
_OpsWorksClient = prism
    OpsWorksClient
    (\case
        OpsWorksClient p1 -> Right p1
        x -> Left x)

-- | See: 'OpsWorksSerializer'
_OpsWorksSerializer :: Prism' OpsWorksError String
_OpsWorksSerializer = prism
    OpsWorksSerializer
    (\case
        OpsWorksSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'OpsWorksService'
_OpsWorksService :: Prism' OpsWorksError String
_OpsWorksService = prism
    OpsWorksService
    (\case
        OpsWorksService p1 -> Right p1
        x -> Left x)

-- | Indicates that a resource was not found.
--
-- See: 'ResourceNotFoundException'
_ResourceNotFoundException :: Prism' OpsWorksError (Maybe Text)
_ResourceNotFoundException = prism
    ResourceNotFoundException
    (\case
        ResourceNotFoundException p1 -> Right p1
        x -> Left x)

-- | Indicates that a request was invalid.
--
-- See: 'ValidationException'
_ValidationException :: Prism' OpsWorksError (Maybe Text)
_ValidationException = prism
    ValidationException
    (\case
        ValidationException p1 -> Right p1
        x -> Left x)

data AppAttributesKeys
    = AppAttributesKeysAutoBundleOnDeploy -- ^ AutoBundleOnDeploy
    | AppAttributesKeysDocumentRoot -- ^ DocumentRoot
    | AppAttributesKeysRailsEnv -- ^ RailsEnv
      deriving (Eq, Show, Generic)

instance Hashable AppAttributesKeys

instance FromText AppAttributesKeys where
    parser = match "AutoBundleOnDeploy" AppAttributesKeysAutoBundleOnDeploy
         <|> match "DocumentRoot" AppAttributesKeysDocumentRoot
         <|> match "RailsEnv" AppAttributesKeysRailsEnv

instance ToText AppAttributesKeys where
    toText AppAttributesKeysAutoBundleOnDeploy = "AutoBundleOnDeploy"
    toText AppAttributesKeysDocumentRoot = "DocumentRoot"
    toText AppAttributesKeysRailsEnv = "RailsEnv"

instance ToByteString AppAttributesKeys where
    toBS AppAttributesKeysAutoBundleOnDeploy = "AutoBundleOnDeploy"
    toBS AppAttributesKeysDocumentRoot = "DocumentRoot"
    toBS AppAttributesKeysRailsEnv = "RailsEnv"

instance ToHeader AppAttributesKeys where
    toHeader k = toHeader k . toBS

instance ToQuery AppAttributesKeys where
    toQuery = toQuery . toBS

instance FromJSON AppAttributesKeys

instance ToJSON AppAttributesKeys

data AppType
    = AppTypeNodejs -- ^ nodejs
    | AppTypeOther -- ^ other
    | AppTypePhp -- ^ php
    | AppTypeRails -- ^ rails
    | AppTypeStatic -- ^ static
      deriving (Eq, Show, Generic)

instance Hashable AppType

instance FromText AppType where
    parser = match "nodejs" AppTypeNodejs
         <|> match "other" AppTypeOther
         <|> match "php" AppTypePhp
         <|> match "rails" AppTypeRails
         <|> match "static" AppTypeStatic

instance ToText AppType where
    toText AppTypeNodejs = "nodejs"
    toText AppTypeOther = "other"
    toText AppTypePhp = "php"
    toText AppTypeRails = "rails"
    toText AppTypeStatic = "static"

instance ToByteString AppType where
    toBS AppTypeNodejs = "nodejs"
    toBS AppTypeOther = "other"
    toBS AppTypePhp = "php"
    toBS AppTypeRails = "rails"
    toBS AppTypeStatic = "static"

instance ToHeader AppType where
    toHeader k = toHeader k . toBS

instance ToQuery AppType where
    toQuery = toQuery . toBS

instance FromJSON AppType

instance ToJSON AppType

data Architecture
    = ArchitectureI386 -- ^ i386
    | ArchitectureX8664 -- ^ x86_64
      deriving (Eq, Show, Generic)

instance Hashable Architecture

instance FromText Architecture where
    parser = match "i386" ArchitectureI386
         <|> match "x86_64" ArchitectureX8664

instance ToText Architecture where
    toText ArchitectureI386 = "i386"
    toText ArchitectureX8664 = "x86_64"

instance ToByteString Architecture where
    toBS ArchitectureI386 = "i386"
    toBS ArchitectureX8664 = "x86_64"

instance ToHeader Architecture where
    toHeader k = toHeader k . toBS

instance ToQuery Architecture where
    toQuery = toQuery . toBS

instance FromJSON Architecture

instance ToJSON Architecture

data AutoScalingType
    = AutoScalingTypeLoad -- ^ load
    | AutoScalingTypeTimer -- ^ timer
      deriving (Eq, Show, Generic)

instance Hashable AutoScalingType

instance FromText AutoScalingType where
    parser = match "load" AutoScalingTypeLoad
         <|> match "timer" AutoScalingTypeTimer

instance ToText AutoScalingType where
    toText AutoScalingTypeLoad = "load"
    toText AutoScalingTypeTimer = "timer"

instance ToByteString AutoScalingType where
    toBS AutoScalingTypeLoad = "load"
    toBS AutoScalingTypeTimer = "timer"

instance ToHeader AutoScalingType where
    toHeader k = toHeader k . toBS

instance ToQuery AutoScalingType where
    toQuery = toQuery . toBS

instance FromJSON AutoScalingType

instance ToJSON AutoScalingType

data DeploymentCommandName
    = DeploymentCommandNameDeploy -- ^ deploy
    | DeploymentCommandNameExecuteRecipes -- ^ execute_recipes
    | DeploymentCommandNameInstallDependencies -- ^ install_dependencies
    | DeploymentCommandNameRestart -- ^ restart
    | DeploymentCommandNameRollback -- ^ rollback
    | DeploymentCommandNameStart -- ^ start
    | DeploymentCommandNameStop -- ^ stop
    | DeploymentCommandNameUndeploy -- ^ undeploy
    | DeploymentCommandNameUpdateCustomCookbooks -- ^ update_custom_cookbooks
    | DeploymentCommandNameUpdateDependencies -- ^ update_dependencies
      deriving (Eq, Show, Generic)

instance Hashable DeploymentCommandName

instance FromText DeploymentCommandName where
    parser = match "deploy" DeploymentCommandNameDeploy
         <|> match "execute_recipes" DeploymentCommandNameExecuteRecipes
         <|> match "install_dependencies" DeploymentCommandNameInstallDependencies
         <|> match "restart" DeploymentCommandNameRestart
         <|> match "rollback" DeploymentCommandNameRollback
         <|> match "start" DeploymentCommandNameStart
         <|> match "stop" DeploymentCommandNameStop
         <|> match "undeploy" DeploymentCommandNameUndeploy
         <|> match "update_custom_cookbooks" DeploymentCommandNameUpdateCustomCookbooks
         <|> match "update_dependencies" DeploymentCommandNameUpdateDependencies

instance ToText DeploymentCommandName where
    toText DeploymentCommandNameDeploy = "deploy"
    toText DeploymentCommandNameExecuteRecipes = "execute_recipes"
    toText DeploymentCommandNameInstallDependencies = "install_dependencies"
    toText DeploymentCommandNameRestart = "restart"
    toText DeploymentCommandNameRollback = "rollback"
    toText DeploymentCommandNameStart = "start"
    toText DeploymentCommandNameStop = "stop"
    toText DeploymentCommandNameUndeploy = "undeploy"
    toText DeploymentCommandNameUpdateCustomCookbooks = "update_custom_cookbooks"
    toText DeploymentCommandNameUpdateDependencies = "update_dependencies"

instance ToByteString DeploymentCommandName where
    toBS DeploymentCommandNameDeploy = "deploy"
    toBS DeploymentCommandNameExecuteRecipes = "execute_recipes"
    toBS DeploymentCommandNameInstallDependencies = "install_dependencies"
    toBS DeploymentCommandNameRestart = "restart"
    toBS DeploymentCommandNameRollback = "rollback"
    toBS DeploymentCommandNameStart = "start"
    toBS DeploymentCommandNameStop = "stop"
    toBS DeploymentCommandNameUndeploy = "undeploy"
    toBS DeploymentCommandNameUpdateCustomCookbooks = "update_custom_cookbooks"
    toBS DeploymentCommandNameUpdateDependencies = "update_dependencies"

instance ToHeader DeploymentCommandName where
    toHeader k = toHeader k . toBS

instance ToQuery DeploymentCommandName where
    toQuery = toQuery . toBS

instance FromJSON DeploymentCommandName

instance ToJSON DeploymentCommandName

data LayerAttributesKeys
    = LayerAttributesKeysBundlerVersion -- ^ BundlerVersion
    | LayerAttributesKeysEnableHaproxyStats -- ^ EnableHaproxyStats
    | LayerAttributesKeysGangliaPassword -- ^ GangliaPassword
    | LayerAttributesKeysGangliaUrl -- ^ GangliaUrl
    | LayerAttributesKeysGangliaUser -- ^ GangliaUser
    | LayerAttributesKeysHaproxyHealthCheckMethod -- ^ HaproxyHealthCheckMethod
    | LayerAttributesKeysHaproxyHealthCheckUrl -- ^ HaproxyHealthCheckUrl
    | LayerAttributesKeysHaproxyStatsPassword -- ^ HaproxyStatsPassword
    | LayerAttributesKeysHaproxyStatsUrl -- ^ HaproxyStatsUrl
    | LayerAttributesKeysHaproxyStatsUser -- ^ HaproxyStatsUser
    | LayerAttributesKeysJavaAppServer -- ^ JavaAppServer
    | LayerAttributesKeysJavaAppServerVersion -- ^ JavaAppServerVersion
    | LayerAttributesKeysJvm -- ^ Jvm
    | LayerAttributesKeysJvmOptions -- ^ JvmOptions
    | LayerAttributesKeysJvmVersion -- ^ JvmVersion
    | LayerAttributesKeysManageBundler -- ^ ManageBundler
    | LayerAttributesKeysMemcachedMemory -- ^ MemcachedMemory
    | LayerAttributesKeysMysqlRootPassword -- ^ MysqlRootPassword
    | LayerAttributesKeysMysqlRootPasswordUbiquitous -- ^ MysqlRootPasswordUbiquitous
    | LayerAttributesKeysNodejsVersion -- ^ NodejsVersion
    | LayerAttributesKeysPassengerVersion -- ^ PassengerVersion
    | LayerAttributesKeysRailsStack -- ^ RailsStack
    | LayerAttributesKeysRubyVersion -- ^ RubyVersion
    | LayerAttributesKeysRubygemsVersion -- ^ RubygemsVersion
      deriving (Eq, Show, Generic)

instance Hashable LayerAttributesKeys

instance FromText LayerAttributesKeys where
    parser = match "BundlerVersion" LayerAttributesKeysBundlerVersion
         <|> match "EnableHaproxyStats" LayerAttributesKeysEnableHaproxyStats
         <|> match "GangliaPassword" LayerAttributesKeysGangliaPassword
         <|> match "GangliaUrl" LayerAttributesKeysGangliaUrl
         <|> match "GangliaUser" LayerAttributesKeysGangliaUser
         <|> match "HaproxyHealthCheckMethod" LayerAttributesKeysHaproxyHealthCheckMethod
         <|> match "HaproxyHealthCheckUrl" LayerAttributesKeysHaproxyHealthCheckUrl
         <|> match "HaproxyStatsPassword" LayerAttributesKeysHaproxyStatsPassword
         <|> match "HaproxyStatsUrl" LayerAttributesKeysHaproxyStatsUrl
         <|> match "HaproxyStatsUser" LayerAttributesKeysHaproxyStatsUser
         <|> match "JavaAppServer" LayerAttributesKeysJavaAppServer
         <|> match "JavaAppServerVersion" LayerAttributesKeysJavaAppServerVersion
         <|> match "Jvm" LayerAttributesKeysJvm
         <|> match "JvmOptions" LayerAttributesKeysJvmOptions
         <|> match "JvmVersion" LayerAttributesKeysJvmVersion
         <|> match "ManageBundler" LayerAttributesKeysManageBundler
         <|> match "MemcachedMemory" LayerAttributesKeysMemcachedMemory
         <|> match "MysqlRootPassword" LayerAttributesKeysMysqlRootPassword
         <|> match "MysqlRootPasswordUbiquitous" LayerAttributesKeysMysqlRootPasswordUbiquitous
         <|> match "NodejsVersion" LayerAttributesKeysNodejsVersion
         <|> match "PassengerVersion" LayerAttributesKeysPassengerVersion
         <|> match "RailsStack" LayerAttributesKeysRailsStack
         <|> match "RubyVersion" LayerAttributesKeysRubyVersion
         <|> match "RubygemsVersion" LayerAttributesKeysRubygemsVersion

instance ToText LayerAttributesKeys where
    toText LayerAttributesKeysBundlerVersion = "BundlerVersion"
    toText LayerAttributesKeysEnableHaproxyStats = "EnableHaproxyStats"
    toText LayerAttributesKeysGangliaPassword = "GangliaPassword"
    toText LayerAttributesKeysGangliaUrl = "GangliaUrl"
    toText LayerAttributesKeysGangliaUser = "GangliaUser"
    toText LayerAttributesKeysHaproxyHealthCheckMethod = "HaproxyHealthCheckMethod"
    toText LayerAttributesKeysHaproxyHealthCheckUrl = "HaproxyHealthCheckUrl"
    toText LayerAttributesKeysHaproxyStatsPassword = "HaproxyStatsPassword"
    toText LayerAttributesKeysHaproxyStatsUrl = "HaproxyStatsUrl"
    toText LayerAttributesKeysHaproxyStatsUser = "HaproxyStatsUser"
    toText LayerAttributesKeysJavaAppServer = "JavaAppServer"
    toText LayerAttributesKeysJavaAppServerVersion = "JavaAppServerVersion"
    toText LayerAttributesKeysJvm = "Jvm"
    toText LayerAttributesKeysJvmOptions = "JvmOptions"
    toText LayerAttributesKeysJvmVersion = "JvmVersion"
    toText LayerAttributesKeysManageBundler = "ManageBundler"
    toText LayerAttributesKeysMemcachedMemory = "MemcachedMemory"
    toText LayerAttributesKeysMysqlRootPassword = "MysqlRootPassword"
    toText LayerAttributesKeysMysqlRootPasswordUbiquitous = "MysqlRootPasswordUbiquitous"
    toText LayerAttributesKeysNodejsVersion = "NodejsVersion"
    toText LayerAttributesKeysPassengerVersion = "PassengerVersion"
    toText LayerAttributesKeysRailsStack = "RailsStack"
    toText LayerAttributesKeysRubyVersion = "RubyVersion"
    toText LayerAttributesKeysRubygemsVersion = "RubygemsVersion"

instance ToByteString LayerAttributesKeys where
    toBS LayerAttributesKeysBundlerVersion = "BundlerVersion"
    toBS LayerAttributesKeysEnableHaproxyStats = "EnableHaproxyStats"
    toBS LayerAttributesKeysGangliaPassword = "GangliaPassword"
    toBS LayerAttributesKeysGangliaUrl = "GangliaUrl"
    toBS LayerAttributesKeysGangliaUser = "GangliaUser"
    toBS LayerAttributesKeysHaproxyHealthCheckMethod = "HaproxyHealthCheckMethod"
    toBS LayerAttributesKeysHaproxyHealthCheckUrl = "HaproxyHealthCheckUrl"
    toBS LayerAttributesKeysHaproxyStatsPassword = "HaproxyStatsPassword"
    toBS LayerAttributesKeysHaproxyStatsUrl = "HaproxyStatsUrl"
    toBS LayerAttributesKeysHaproxyStatsUser = "HaproxyStatsUser"
    toBS LayerAttributesKeysJavaAppServer = "JavaAppServer"
    toBS LayerAttributesKeysJavaAppServerVersion = "JavaAppServerVersion"
    toBS LayerAttributesKeysJvm = "Jvm"
    toBS LayerAttributesKeysJvmOptions = "JvmOptions"
    toBS LayerAttributesKeysJvmVersion = "JvmVersion"
    toBS LayerAttributesKeysManageBundler = "ManageBundler"
    toBS LayerAttributesKeysMemcachedMemory = "MemcachedMemory"
    toBS LayerAttributesKeysMysqlRootPassword = "MysqlRootPassword"
    toBS LayerAttributesKeysMysqlRootPasswordUbiquitous = "MysqlRootPasswordUbiquitous"
    toBS LayerAttributesKeysNodejsVersion = "NodejsVersion"
    toBS LayerAttributesKeysPassengerVersion = "PassengerVersion"
    toBS LayerAttributesKeysRailsStack = "RailsStack"
    toBS LayerAttributesKeysRubyVersion = "RubyVersion"
    toBS LayerAttributesKeysRubygemsVersion = "RubygemsVersion"

instance ToHeader LayerAttributesKeys where
    toHeader k = toHeader k . toBS

instance ToQuery LayerAttributesKeys where
    toQuery = toQuery . toBS

instance FromJSON LayerAttributesKeys

instance ToJSON LayerAttributesKeys

data LayerType
    = LayerTypeCustom -- ^ custom
    | LayerTypeDbMaster -- ^ db-master
    | LayerTypeLb -- ^ lb
    | LayerTypeMemcached -- ^ memcached
    | LayerTypeMonitoringMaster -- ^ monitoring-master
    | LayerTypeNodejsApp -- ^ nodejs-app
    | LayerTypePhpApp -- ^ php-app
    | LayerTypeRailsApp -- ^ rails-app
    | LayerTypeWeb -- ^ web
      deriving (Eq, Show, Generic)

instance Hashable LayerType

instance FromText LayerType where
    parser = match "custom" LayerTypeCustom
         <|> match "db-master" LayerTypeDbMaster
         <|> match "lb" LayerTypeLb
         <|> match "memcached" LayerTypeMemcached
         <|> match "monitoring-master" LayerTypeMonitoringMaster
         <|> match "nodejs-app" LayerTypeNodejsApp
         <|> match "php-app" LayerTypePhpApp
         <|> match "rails-app" LayerTypeRailsApp
         <|> match "web" LayerTypeWeb

instance ToText LayerType where
    toText LayerTypeCustom = "custom"
    toText LayerTypeDbMaster = "db-master"
    toText LayerTypeLb = "lb"
    toText LayerTypeMemcached = "memcached"
    toText LayerTypeMonitoringMaster = "monitoring-master"
    toText LayerTypeNodejsApp = "nodejs-app"
    toText LayerTypePhpApp = "php-app"
    toText LayerTypeRailsApp = "rails-app"
    toText LayerTypeWeb = "web"

instance ToByteString LayerType where
    toBS LayerTypeCustom = "custom"
    toBS LayerTypeDbMaster = "db-master"
    toBS LayerTypeLb = "lb"
    toBS LayerTypeMemcached = "memcached"
    toBS LayerTypeMonitoringMaster = "monitoring-master"
    toBS LayerTypeNodejsApp = "nodejs-app"
    toBS LayerTypePhpApp = "php-app"
    toBS LayerTypeRailsApp = "rails-app"
    toBS LayerTypeWeb = "web"

instance ToHeader LayerType where
    toHeader k = toHeader k . toBS

instance ToQuery LayerType where
    toQuery = toQuery . toBS

instance FromJSON LayerType

instance ToJSON LayerType

data RootDeviceType
    = RootDeviceTypeEbs -- ^ ebs
    | RootDeviceTypeInstanceStore -- ^ instance-store
      deriving (Eq, Show, Generic)

instance Hashable RootDeviceType

instance FromText RootDeviceType where
    parser = match "ebs" RootDeviceTypeEbs
         <|> match "instance-store" RootDeviceTypeInstanceStore

instance ToText RootDeviceType where
    toText RootDeviceTypeEbs = "ebs"
    toText RootDeviceTypeInstanceStore = "instance-store"

instance ToByteString RootDeviceType where
    toBS RootDeviceTypeEbs = "ebs"
    toBS RootDeviceTypeInstanceStore = "instance-store"

instance ToHeader RootDeviceType where
    toHeader k = toHeader k . toBS

instance ToQuery RootDeviceType where
    toQuery = toQuery . toBS

instance FromJSON RootDeviceType

instance ToJSON RootDeviceType

data SourceType
    = SourceTypeArchive -- ^ archive
    | SourceTypeGit -- ^ git
    | SourceTypeS3 -- ^ s3
    | SourceTypeSvn -- ^ svn
      deriving (Eq, Show, Generic)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "archive" SourceTypeArchive
         <|> match "git" SourceTypeGit
         <|> match "s3" SourceTypeS3
         <|> match "svn" SourceTypeSvn

instance ToText SourceType where
    toText SourceTypeArchive = "archive"
    toText SourceTypeGit = "git"
    toText SourceTypeS3 = "s3"
    toText SourceTypeSvn = "svn"

instance ToByteString SourceType where
    toBS SourceTypeArchive = "archive"
    toBS SourceTypeGit = "git"
    toBS SourceTypeS3 = "s3"
    toBS SourceTypeSvn = "svn"

instance ToHeader SourceType where
    toHeader k = toHeader k . toBS

instance ToQuery SourceType where
    toQuery = toQuery . toBS

instance FromJSON SourceType

instance ToJSON SourceType

data StackAttributesKeys
    = StackAttributesKeysColor -- ^ Color
      deriving (Eq, Show, Generic)

instance Hashable StackAttributesKeys

instance FromText StackAttributesKeys where
    parser = match "Color" StackAttributesKeysColor

instance ToText StackAttributesKeys where
    toText StackAttributesKeysColor = "Color"

instance ToByteString StackAttributesKeys where
    toBS StackAttributesKeysColor = "Color"

instance ToHeader StackAttributesKeys where
    toHeader k = toHeader k . toBS

instance ToQuery StackAttributesKeys where
    toQuery = toQuery . toBS

instance FromJSON StackAttributesKeys

instance ToJSON StackAttributesKeys

-- | A description of the app.
data App = App
    { _aAppId :: Maybe Text
    , _aStackId :: Maybe Text
    , _aShortname :: Maybe Text
    , _aName :: Maybe Text
    , _aDescription :: Maybe Text
    , _aDataSources :: [DataSource]
    , _aType :: Maybe AppType
    , _aAppSource :: Maybe Source
    , _aDomains :: [Text]
    , _aEnableSsl :: Maybe Bool
    , _aSslConfiguration :: Maybe SslConfiguration
    , _aAttributes :: Map AppAttributesKeys Text
    , _aCreatedAt :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'App' data type.
--
-- 'App' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AppId ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @Shortname ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @DataSources ::@ @[DataSource]@
--
-- * @Type ::@ @Maybe AppType@
--
-- * @AppSource ::@ @Maybe Source@
--
-- * @Domains ::@ @[Text]@
--
-- * @EnableSsl ::@ @Maybe Bool@
--
-- * @SslConfiguration ::@ @Maybe SslConfiguration@
--
-- * @Attributes ::@ @Map AppAttributesKeys Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
app :: App
app = App
    { _aAppId = Nothing
    , _aStackId = Nothing
    , _aShortname = Nothing
    , _aName = Nothing
    , _aDescription = Nothing
    , _aDataSources = mempty
    , _aType = Nothing
    , _aAppSource = Nothing
    , _aDomains = mempty
    , _aEnableSsl = Nothing
    , _aSslConfiguration = Nothing
    , _aAttributes = mempty
    , _aCreatedAt = Nothing
    }

-- | The app ID.
aAppId :: Lens' App (Maybe Text)
aAppId = lens _aAppId (\s a -> s { _aAppId = a })

-- | The app stack ID.
aStackId :: Lens' App (Maybe Text)
aStackId = lens _aStackId (\s a -> s { _aStackId = a })

-- | The app's short name.
aShortname :: Lens' App (Maybe Text)
aShortname = lens _aShortname (\s a -> s { _aShortname = a })

-- | The app name.
aName :: Lens' App (Maybe Text)
aName = lens _aName (\s a -> s { _aName = a })

-- | A description of the app.
aDescription :: Lens' App (Maybe Text)
aDescription = lens _aDescription (\s a -> s { _aDescription = a })

-- | The app's data sources.
aDataSources :: Lens' App [DataSource]
aDataSources = lens _aDataSources (\s a -> s { _aDataSources = a })

-- | The app type.
aType :: Lens' App (Maybe AppType)
aType = lens _aType (\s a -> s { _aType = a })

-- | A Source object that describes the app repository.
aAppSource :: Lens' App (Maybe Source)
aAppSource = lens _aAppSource (\s a -> s { _aAppSource = a })

-- | The app vhost settings with multiple domains separated by commas. For
-- example: 'www.example.com, example.com'.
aDomains :: Lens' App [Text]
aDomains = lens _aDomains (\s a -> s { _aDomains = a })

-- | Whether to enable SSL for the app.
aEnableSsl :: Lens' App (Maybe Bool)
aEnableSsl = lens _aEnableSsl (\s a -> s { _aEnableSsl = a })

-- | An SslConfiguration object with the SSL configuration.
aSslConfiguration :: Lens' App (Maybe SslConfiguration)
aSslConfiguration =
    lens _aSslConfiguration (\s a -> s { _aSslConfiguration = a })

-- | The stack attributes.
aAttributes :: Lens' App (Map AppAttributesKeys Text)
aAttributes = lens _aAttributes (\s a -> s { _aAttributes = a })

-- | When the app was created.
aCreatedAt :: Lens' App (Maybe Text)
aCreatedAt = lens _aCreatedAt (\s a -> s { _aCreatedAt = a })

instance FromJSON App

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the number
-- of instances.
data AutoScalingThresholds = AutoScalingThresholds
    { _astInstanceCount :: Maybe Integer
    , _astThresholdsWaitTime :: Maybe Integer
    , _astIgnoreMetricsTime :: Maybe Integer
    , _astCpuThreshold :: Maybe Double
    , _astMemoryThreshold :: Maybe Double
    , _astLoadThreshold :: Maybe Double
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AutoScalingThresholds' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceCount ::@ @Maybe Integer@
--
-- * @ThresholdsWaitTime ::@ @Maybe Integer@
--
-- * @IgnoreMetricsTime ::@ @Maybe Integer@
--
-- * @CpuThreshold ::@ @Maybe Double@
--
-- * @MemoryThreshold ::@ @Maybe Double@
--
-- * @LoadThreshold ::@ @Maybe Double@
--
autoScalingThresholds :: AutoScalingThresholds
autoScalingThresholds = AutoScalingThresholds
    { _astInstanceCount = Nothing
    , _astThresholdsWaitTime = Nothing
    , _astIgnoreMetricsTime = Nothing
    , _astCpuThreshold = Nothing
    , _astMemoryThreshold = Nothing
    , _astLoadThreshold = Nothing
    }

-- | The number of instances to add or remove when the load exceeds a threshold.
astInstanceCount :: Lens' AutoScalingThresholds (Maybe Integer)
astInstanceCount =
    lens _astInstanceCount (\s a -> s { _astInstanceCount = a })

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
astThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Integer)
astThresholdsWaitTime =
    lens _astThresholdsWaitTime (\s a -> s { _astThresholdsWaitTime = a })

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks should ignore metrics and not raise any additional scaling events.
-- For example, AWS OpsWorks adds new instances following an upscaling event
-- but the instances won't start reducing the load until they have been booted
-- and configured. There is no point in raising additional scaling events
-- during that operation, which typically takes several minutes.
-- IgnoreMetricsTime allows you to direct AWS OpsWorks to not raise any
-- scaling events long enough to get the new instances online.
astIgnoreMetricsTime :: Lens' AutoScalingThresholds (Maybe Integer)
astIgnoreMetricsTime =
    lens _astIgnoreMetricsTime (\s a -> s { _astIgnoreMetricsTime = a })

-- | The CPU utilization threshold, as a percent of the available CPU.
astCpuThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astCpuThreshold = lens _astCpuThreshold (\s a -> s { _astCpuThreshold = a })

-- | The memory utilization threshold, as a percent of the available memory.
astMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astMemoryThreshold =
    lens _astMemoryThreshold (\s a -> s { _astMemoryThreshold = a })

-- | The load threshold. For more information about how load is computed, see
-- Load (computing).
astLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astLoadThreshold =
    lens _astLoadThreshold (\s a -> s { _astLoadThreshold = a })

instance FromJSON AutoScalingThresholds

instance ToJSON AutoScalingThresholds

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
data ChefConfiguration = ChefConfiguration
    { _ccManageBerkshelf :: Maybe Bool
    , _ccBerkshelfVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChefConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ManageBerkshelf ::@ @Maybe Bool@
--
-- * @BerkshelfVersion ::@ @Maybe Text@
--
chefConfiguration :: ChefConfiguration
chefConfiguration = ChefConfiguration
    { _ccManageBerkshelf = Nothing
    , _ccBerkshelfVersion = Nothing
    }

-- | Whether to enable Berkshelf.
ccManageBerkshelf :: Lens' ChefConfiguration (Maybe Bool)
ccManageBerkshelf =
    lens _ccManageBerkshelf (\s a -> s { _ccManageBerkshelf = a })

-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion =
    lens _ccBerkshelfVersion (\s a -> s { _ccBerkshelfVersion = a })

instance FromJSON ChefConfiguration

instance ToJSON ChefConfiguration

-- | Describes a command.
data Command = Command
    { _cCommandId :: Maybe Text
    , _cInstanceId :: Maybe Text
    , _cDeploymentId :: Maybe Text
    , _cCreatedAt :: Maybe Text
    , _cAcknowledgedAt :: Maybe Text
    , _cCompletedAt :: Maybe Text
    , _cStatus :: Maybe Text
    , _cExitCode :: Maybe Integer
    , _cLogUrl :: Maybe Text
    , _cType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Command' data type.
--
-- 'Command' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CommandId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @DeploymentId ::@ @Maybe Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
-- * @AcknowledgedAt ::@ @Maybe Text@
--
-- * @CompletedAt ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @ExitCode ::@ @Maybe Integer@
--
-- * @LogUrl ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe Text@
--
command :: Command
command = Command
    { _cCommandId = Nothing
    , _cInstanceId = Nothing
    , _cDeploymentId = Nothing
    , _cCreatedAt = Nothing
    , _cAcknowledgedAt = Nothing
    , _cCompletedAt = Nothing
    , _cStatus = Nothing
    , _cExitCode = Nothing
    , _cLogUrl = Nothing
    , _cType = Nothing
    }

-- | The command ID.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\s a -> s { _cCommandId = a })

-- | The ID of the instance where the command was executed.
cInstanceId :: Lens' Command (Maybe Text)
cInstanceId = lens _cInstanceId (\s a -> s { _cInstanceId = a })

-- | The command deployment ID.
cDeploymentId :: Lens' Command (Maybe Text)
cDeploymentId = lens _cDeploymentId (\s a -> s { _cDeploymentId = a })

-- | Date and time when the command was run.
cCreatedAt :: Lens' Command (Maybe Text)
cCreatedAt = lens _cCreatedAt (\s a -> s { _cCreatedAt = a })

-- | Date and time when the command was acknowledged.
cAcknowledgedAt :: Lens' Command (Maybe Text)
cAcknowledgedAt = lens _cAcknowledgedAt (\s a -> s { _cAcknowledgedAt = a })

-- | Date when the command completed.
cCompletedAt :: Lens' Command (Maybe Text)
cCompletedAt = lens _cCompletedAt (\s a -> s { _cCompletedAt = a })

-- | The command status: failed successful skipped pending.
cStatus :: Lens' Command (Maybe Text)
cStatus = lens _cStatus (\s a -> s { _cStatus = a })

-- | The command exit code.
cExitCode :: Lens' Command (Maybe Integer)
cExitCode = lens _cExitCode (\s a -> s { _cExitCode = a })

-- | The URL of the command log.
cLogUrl :: Lens' Command (Maybe Text)
cLogUrl = lens _cLogUrl (\s a -> s { _cLogUrl = a })

-- | The command type: deploy rollback start stop restart undeploy
-- update_dependencies install_dependencies update_custom_cookbooks
-- execute_recipes.
cType :: Lens' Command (Maybe Text)
cType = lens _cType (\s a -> s { _cType = a })

instance FromJSON Command

-- | Describes an app's data source.
data DataSource = DataSource
    { _dsType :: Maybe Text
    , _dsArn :: Maybe Text
    , _dsDatabaseName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DataSource' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Type ::@ @Maybe Text@
--
-- * @Arn ::@ @Maybe Text@
--
-- * @DatabaseName ::@ @Maybe Text@
--
dataSource :: DataSource
dataSource = DataSource
    { _dsType = Nothing
    , _dsArn = Nothing
    , _dsDatabaseName = Nothing
    }

-- | The data source's type, AutoSelectOpsworksMysqlInstance,
-- OpsworksMysqlInstance, or RdsDbInstance.
dsType :: Lens' DataSource (Maybe Text)
dsType = lens _dsType (\s a -> s { _dsType = a })

-- | The data source's ARN.
dsArn :: Lens' DataSource (Maybe Text)
dsArn = lens _dsArn (\s a -> s { _dsArn = a })

-- | The database name.
dsDatabaseName :: Lens' DataSource (Maybe Text)
dsDatabaseName = lens _dsDatabaseName (\s a -> s { _dsDatabaseName = a })

instance FromJSON DataSource

instance ToJSON DataSource

-- | Describes a deployment of a stack or app.
data Deployment = Deployment
    { _dDeploymentId :: Maybe Text
    , _dStackId :: Maybe Text
    , _dAppId :: Maybe Text
    , _dCreatedAt :: Maybe Text
    , _dCompletedAt :: Maybe Text
    , _dDuration :: Maybe Integer
    , _dIamUserArn :: Maybe Text
    , _dComment :: Maybe Text
    , _dCommand :: Maybe DeploymentCommand
    , _dStatus :: Maybe Text
    , _dCustomJson :: Maybe Text
    , _dInstanceIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Deployment' data type.
--
-- 'Deployment' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DeploymentId ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @AppId ::@ @Maybe Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
-- * @CompletedAt ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @IamUserArn ::@ @Maybe Text@
--
-- * @Comment ::@ @Maybe Text@
--
-- * @Command ::@ @Maybe DeploymentCommand@
--
-- * @Status ::@ @Maybe Text@
--
-- * @CustomJson ::@ @Maybe Text@
--
-- * @InstanceIds ::@ @[Text]@
--
deployment :: Deployment
deployment = Deployment
    { _dDeploymentId = Nothing
    , _dStackId = Nothing
    , _dAppId = Nothing
    , _dCreatedAt = Nothing
    , _dCompletedAt = Nothing
    , _dDuration = Nothing
    , _dIamUserArn = Nothing
    , _dComment = Nothing
    , _dCommand = Nothing
    , _dStatus = Nothing
    , _dCustomJson = Nothing
    , _dInstanceIds = mempty
    }

-- | The deployment ID.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\s a -> s { _dDeploymentId = a })

-- | The stack ID.
dStackId :: Lens' Deployment (Maybe Text)
dStackId = lens _dStackId (\s a -> s { _dStackId = a })

-- | The app ID.
dAppId :: Lens' Deployment (Maybe Text)
dAppId = lens _dAppId (\s a -> s { _dAppId = a })

-- | Date when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\s a -> s { _dCreatedAt = a })

-- | Date when the deployment completed.
dCompletedAt :: Lens' Deployment (Maybe Text)
dCompletedAt = lens _dCompletedAt (\s a -> s { _dCompletedAt = a })

-- | The deployment duration.
dDuration :: Lens' Deployment (Maybe Integer)
dDuration = lens _dDuration (\s a -> s { _dDuration = a })

-- | The user's IAM ARN.
dIamUserArn :: Lens' Deployment (Maybe Text)
dIamUserArn = lens _dIamUserArn (\s a -> s { _dIamUserArn = a })

-- | A user-defined comment.
dComment :: Lens' Deployment (Maybe Text)
dComment = lens _dComment (\s a -> s { _dComment = a })

-- | Used to specify a deployment operation.
dCommand :: Lens' Deployment (Maybe DeploymentCommand)
dCommand = lens _dCommand (\s a -> s { _dCommand = a })

-- | The deployment status: running successful failed.
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\s a -> s { _dStatus = a })

-- | A string that contains user-defined custom JSON. It is used to override the
-- corresponding default stack configuration JSON values for stack. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
dCustomJson :: Lens' Deployment (Maybe Text)
dCustomJson = lens _dCustomJson (\s a -> s { _dCustomJson = a })

-- | The IDs of the target instances.
dInstanceIds :: Lens' Deployment [Text]
dInstanceIds = lens _dInstanceIds (\s a -> s { _dInstanceIds = a })

instance FromJSON Deployment

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
data DeploymentCommand = DeploymentCommand
    { _dcName :: DeploymentCommandName
    , _dcArgs :: Map Text [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeploymentCommand' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @DeploymentCommandName@
--
-- * @Args ::@ @Map Text [Text]@
--
deploymentCommand :: DeploymentCommandName -- ^ 'dcName'
                    -> DeploymentCommand
deploymentCommand p1 = DeploymentCommand
    { _dcName = p1
    , _dcArgs = mempty
    }

-- | Specifies the operation. You can specify only one command. For stacks, the
-- following commands are available: execute_recipes: Execute one or more
-- recipes. To specify the recipes, set an Args parameter named recipes to the
-- list of recipes to be executed. For example, to execute phpapp::appsetup,
-- set Args to {"recipes":["phpapp::appsetup"]}. install_dependencies: Install
-- the stack's dependencies. update_custom_cookbooks: Update the stack's
-- custom cookbooks. update_dependencies: Update the stack's dependencies. For
-- apps, the following commands are available: deploy: Deploy an app. Rails
-- apps have an optional Args parameter named migrate. Set Args to
-- {"migrate":["true"]} to migrate the database. The default setting is
-- {"migrate":["false"]}. rollback Roll the app back to the previous version.
-- When you update an app, AWS OpsWorks stores the previous version, up to a
-- maximum of five versions. You can use this command to roll an app back as
-- many as four versions. start: Start the app's web or application server.
-- stop: Stop the app's web or application server. restart: Restart the app's
-- web or application server. undeploy: Undeploy the app.
dcName :: Lens' DeploymentCommand DeploymentCommandName
dcName = lens _dcName (\s a -> s { _dcName = a })

-- | The arguments of those commands that take arguments. It should be set to a
-- JSON object with the following format: {"arg_name":["value1", "value2",
-- ...]}.
dcArgs :: Lens' DeploymentCommand (Map Text [Text])
dcArgs = lens _dcArgs (\s a -> s { _dcArgs = a })

instance FromJSON DeploymentCommand

instance ToJSON DeploymentCommand

-- | Describes an Elastic IP address.
data ElasticIp = ElasticIp
    { _eiIp :: Maybe Text
    , _eiName :: Maybe Text
    , _eiDomain :: Maybe Text
    , _eiRegion :: Maybe Text
    , _eiInstanceId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ElasticIp' data type.
--
-- 'ElasticIp' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Ip ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Domain ::@ @Maybe Text@
--
-- * @Region ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
elasticIp :: ElasticIp
elasticIp = ElasticIp
    { _eiIp = Nothing
    , _eiName = Nothing
    , _eiDomain = Nothing
    , _eiRegion = Nothing
    , _eiInstanceId = Nothing
    }

-- | The IP address.
eiIp :: Lens' ElasticIp (Maybe Text)
eiIp = lens _eiIp (\s a -> s { _eiIp = a })

-- | The name.
eiName :: Lens' ElasticIp (Maybe Text)
eiName = lens _eiName (\s a -> s { _eiName = a })

-- | The domain.
eiDomain :: Lens' ElasticIp (Maybe Text)
eiDomain = lens _eiDomain (\s a -> s { _eiDomain = a })

-- | The AWS region. For more information, see Regions and Endpoints.
eiRegion :: Lens' ElasticIp (Maybe Text)
eiRegion = lens _eiRegion (\s a -> s { _eiRegion = a })

-- | The ID of the instance that the address is attached to.
eiInstanceId :: Lens' ElasticIp (Maybe Text)
eiInstanceId = lens _eiInstanceId (\s a -> s { _eiInstanceId = a })

instance FromJSON ElasticIp

-- | Describes an Elastic Load Balancing instance.
data ElasticLoadBalancer = ElasticLoadBalancer
    { _elbElasticLoadBalancerName :: Maybe Text
    , _elbRegion :: Maybe Text
    , _elbDnsName :: Maybe Text
    , _elbStackId :: Maybe Text
    , _elbLayerId :: Maybe Text
    , _elbVpcId :: Maybe Text
    , _elbAvailabilityZones :: [Text]
    , _elbSubnetIds :: [Text]
    , _elbEc2InstanceIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ElasticLoadBalancer' data type.
--
-- 'ElasticLoadBalancer' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ElasticLoadBalancerName ::@ @Maybe Text@
--
-- * @Region ::@ @Maybe Text@
--
-- * @DnsName ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LayerId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @AvailabilityZones ::@ @[Text]@
--
-- * @SubnetIds ::@ @[Text]@
--
-- * @Ec2InstanceIds ::@ @[Text]@
--
elasticLoadBalancer :: ElasticLoadBalancer
elasticLoadBalancer = ElasticLoadBalancer
    { _elbElasticLoadBalancerName = Nothing
    , _elbRegion = Nothing
    , _elbDnsName = Nothing
    , _elbStackId = Nothing
    , _elbLayerId = Nothing
    , _elbVpcId = Nothing
    , _elbAvailabilityZones = mempty
    , _elbSubnetIds = mempty
    , _elbEc2InstanceIds = mempty
    }

-- | The Elastic Load Balancing instance's name.
elbElasticLoadBalancerName :: Lens' ElasticLoadBalancer (Maybe Text)
elbElasticLoadBalancerName =
    lens _elbElasticLoadBalancerName
         (\s a -> s { _elbElasticLoadBalancerName = a })

-- | The instance's AWS region.
elbRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elbRegion = lens _elbRegion (\s a -> s { _elbRegion = a })

-- | The instance's public DNS name.
elbDnsName :: Lens' ElasticLoadBalancer (Maybe Text)
elbDnsName = lens _elbDnsName (\s a -> s { _elbDnsName = a })

-- | The ID of the stack that the instance is associated with.
elbStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elbStackId = lens _elbStackId (\s a -> s { _elbStackId = a })

-- | The ID of the layer that the instance is attached to.
elbLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elbLayerId = lens _elbLayerId (\s a -> s { _elbLayerId = a })

-- | The VPC ID.
elbVpcId :: Lens' ElasticLoadBalancer (Maybe Text)
elbVpcId = lens _elbVpcId (\s a -> s { _elbVpcId = a })

-- | A list of Availability Zones.
elbAvailabilityZones :: Lens' ElasticLoadBalancer [Text]
elbAvailabilityZones =
    lens _elbAvailabilityZones (\s a -> s { _elbAvailabilityZones = a })

-- | A list of subnet IDs, if the stack is running in a VPC.
elbSubnetIds :: Lens' ElasticLoadBalancer [Text]
elbSubnetIds = lens _elbSubnetIds (\s a -> s { _elbSubnetIds = a })

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elbEc2InstanceIds :: Lens' ElasticLoadBalancer [Text]
elbEc2InstanceIds =
    lens _elbEc2InstanceIds (\s a -> s { _elbEc2InstanceIds = a })

instance FromJSON ElasticLoadBalancer

-- | Describes an instance.
data Instance = Instance
    { _iInstanceId :: Maybe Text
    , _iEc2InstanceId :: Maybe Text
    , _iVirtualizationType :: Maybe Text
    , _iHostname :: Maybe Text
    , _iStackId :: Maybe Text
    , _iLayerIds :: [Text]
    , _iSecurityGroupIds :: [Text]
    , _iInstanceType :: Maybe Text
    , _iInstanceProfileArn :: Maybe Text
    , _iStatus :: Maybe Text
    , _iOs :: Maybe Text
    , _iAmiId :: Maybe Text
    , _iAvailabilityZone :: Maybe Text
    , _iSubnetId :: Maybe Text
    , _iPublicDns :: Maybe Text
    , _iPrivateDns :: Maybe Text
    , _iPublicIp :: Maybe Text
    , _iPrivateIp :: Maybe Text
    , _iElasticIp :: Maybe Text
    , _iAutoScalingType :: Maybe AutoScalingType
    , _iSshKeyName :: Maybe Text
    , _iSshHostRsaKeyFingerprint :: Maybe Text
    , _iSshHostDsaKeyFingerprint :: Maybe Text
    , _iCreatedAt :: Maybe Text
    , _iLastServiceErrorId :: Maybe Text
    , _iArchitecture :: Maybe Architecture
    , _iRootDeviceType :: Maybe RootDeviceType
    , _iRootDeviceVolumeId :: Maybe Text
    , _iInstallUpdatesOnBoot :: Maybe Bool
    , _iEbsOptimized :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type.
--
-- 'Instance' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Ec2InstanceId ::@ @Maybe Text@
--
-- * @VirtualizationType ::@ @Maybe Text@
--
-- * @Hostname ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LayerIds ::@ @[Text]@
--
-- * @SecurityGroupIds ::@ @[Text]@
--
-- * @InstanceType ::@ @Maybe Text@
--
-- * @InstanceProfileArn ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @Os ::@ @Maybe Text@
--
-- * @AmiId ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @PublicDns ::@ @Maybe Text@
--
-- * @PrivateDns ::@ @Maybe Text@
--
-- * @PublicIp ::@ @Maybe Text@
--
-- * @PrivateIp ::@ @Maybe Text@
--
-- * @ElasticIp ::@ @Maybe Text@
--
-- * @AutoScalingType ::@ @Maybe AutoScalingType@
--
-- * @SshKeyName ::@ @Maybe Text@
--
-- * @SshHostRsaKeyFingerprint ::@ @Maybe Text@
--
-- * @SshHostDsaKeyFingerprint ::@ @Maybe Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
-- * @LastServiceErrorId ::@ @Maybe Text@
--
-- * @Architecture ::@ @Maybe Architecture@
--
-- * @RootDeviceType ::@ @Maybe RootDeviceType@
--
-- * @RootDeviceVolumeId ::@ @Maybe Text@
--
-- * @InstallUpdatesOnBoot ::@ @Maybe Bool@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
instance' :: Instance
instance' = Instance
    { _iInstanceId = Nothing
    , _iEc2InstanceId = Nothing
    , _iVirtualizationType = Nothing
    , _iHostname = Nothing
    , _iStackId = Nothing
    , _iLayerIds = mempty
    , _iSecurityGroupIds = mempty
    , _iInstanceType = Nothing
    , _iInstanceProfileArn = Nothing
    , _iStatus = Nothing
    , _iOs = Nothing
    , _iAmiId = Nothing
    , _iAvailabilityZone = Nothing
    , _iSubnetId = Nothing
    , _iPublicDns = Nothing
    , _iPrivateDns = Nothing
    , _iPublicIp = Nothing
    , _iPrivateIp = Nothing
    , _iElasticIp = Nothing
    , _iAutoScalingType = Nothing
    , _iSshKeyName = Nothing
    , _iSshHostRsaKeyFingerprint = Nothing
    , _iSshHostDsaKeyFingerprint = Nothing
    , _iCreatedAt = Nothing
    , _iLastServiceErrorId = Nothing
    , _iArchitecture = Nothing
    , _iRootDeviceType = Nothing
    , _iRootDeviceVolumeId = Nothing
    , _iInstallUpdatesOnBoot = Nothing
    , _iEbsOptimized = Nothing
    }

-- | The instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

-- | The ID of the associated Amazon EC2 instance.
iEc2InstanceId :: Lens' Instance (Maybe Text)
iEc2InstanceId = lens _iEc2InstanceId (\s a -> s { _iEc2InstanceId = a })

-- | The instance's virtualization type, paravirtual or hvm.
iVirtualizationType :: Lens' Instance (Maybe Text)
iVirtualizationType =
    lens _iVirtualizationType (\s a -> s { _iVirtualizationType = a })

-- | The instance host name.
iHostname :: Lens' Instance (Maybe Text)
iHostname = lens _iHostname (\s a -> s { _iHostname = a })

-- | The stack ID.
iStackId :: Lens' Instance (Maybe Text)
iStackId = lens _iStackId (\s a -> s { _iStackId = a })

-- | An array containing the instance layer IDs.
iLayerIds :: Lens' Instance [Text]
iLayerIds = lens _iLayerIds (\s a -> s { _iLayerIds = a })

-- | An array containing the instance security group IDs.
iSecurityGroupIds :: Lens' Instance [Text]
iSecurityGroupIds =
    lens _iSecurityGroupIds (\s a -> s { _iSecurityGroupIds = a })

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that specify the various
-- types are in the API Name column of the Available Instance Types table.
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\s a -> s { _iInstanceType = a })

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs,
-- see Using Identifiers.
iInstanceProfileArn :: Lens' Instance (Maybe Text)
iInstanceProfileArn =
    lens _iInstanceProfileArn (\s a -> s { _iInstanceProfileArn = a })

-- | The instance status: requested booting running_setup online setup_failed
-- start_failed terminating terminated stopped connection_lost.
iStatus :: Lens' Instance (Maybe Text)
iStatus = lens _iStatus (\s a -> s { _iStatus = a })

-- | The instance operating system.
iOs :: Lens' Instance (Maybe Text)
iOs = lens _iOs (\s a -> s { _iOs = a })

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
-- For more information, see Instances.
iAmiId :: Lens' Instance (Maybe Text)
iAmiId = lens _iAmiId (\s a -> s { _iAmiId = a })

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
iAvailabilityZone :: Lens' Instance (Maybe Text)
iAvailabilityZone =
    lens _iAvailabilityZone (\s a -> s { _iAvailabilityZone = a })

-- | The instance's subnet ID, if the stack is running in a VPC.
iSubnetId :: Lens' Instance (Maybe Text)
iSubnetId = lens _iSubnetId (\s a -> s { _iSubnetId = a })

-- | The instance public DNS name.
iPublicDns :: Lens' Instance (Maybe Text)
iPublicDns = lens _iPublicDns (\s a -> s { _iPublicDns = a })

-- | The instance private DNS name.
iPrivateDns :: Lens' Instance (Maybe Text)
iPrivateDns = lens _iPrivateDns (\s a -> s { _iPrivateDns = a })

-- | The instance public IP address.
iPublicIp :: Lens' Instance (Maybe Text)
iPublicIp = lens _iPublicIp (\s a -> s { _iPublicIp = a })

-- | The instance private IP address.
iPrivateIp :: Lens' Instance (Maybe Text)
iPrivateIp = lens _iPrivateIp (\s a -> s { _iPrivateIp = a })

-- | The instance Elastic IP address .
iElasticIp :: Lens' Instance (Maybe Text)
iElasticIp = lens _iElasticIp (\s a -> s { _iElasticIp = a })

-- | The instance's auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. LoadBasedAutoScaling: A
-- load-based auto scaling instance, which is started and stopped based on
-- load metrics.
iAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
iAutoScalingType =
    lens _iAutoScalingType (\s a -> s { _iAutoScalingType = a })

-- | The instance SSH key name.
iSshKeyName :: Lens' Instance (Maybe Text)
iSshKeyName = lens _iSshKeyName (\s a -> s { _iSshKeyName = a })

-- | The SSH key's RSA fingerprint.
iSshHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSshHostRsaKeyFingerprint =
    lens _iSshHostRsaKeyFingerprint
         (\s a -> s { _iSshHostRsaKeyFingerprint = a })

-- | The SSH key's DSA fingerprint.
iSshHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSshHostDsaKeyFingerprint =
    lens _iSshHostDsaKeyFingerprint
         (\s a -> s { _iSshHostDsaKeyFingerprint = a })

-- | The time that the instance was created.
iCreatedAt :: Lens' Instance (Maybe Text)
iCreatedAt = lens _iCreatedAt (\s a -> s { _iCreatedAt = a })

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
iLastServiceErrorId :: Lens' Instance (Maybe Text)
iLastServiceErrorId =
    lens _iLastServiceErrorId (\s a -> s { _iLastServiceErrorId = a })

-- | The instance architecture, "i386" or "x86_64".
iArchitecture :: Lens' Instance (Maybe Architecture)
iArchitecture = lens _iArchitecture (\s a -> s { _iArchitecture = a })

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
iRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
iRootDeviceType = lens _iRootDeviceType (\s a -> s { _iRootDeviceType = a })

-- | The root device volume ID.
iRootDeviceVolumeId :: Lens' Instance (Maybe Text)
iRootDeviceVolumeId =
    lens _iRootDeviceVolumeId (\s a -> s { _iRootDeviceVolumeId = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
iInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
iInstallUpdatesOnBoot =
    lens _iInstallUpdatesOnBoot (\s a -> s { _iInstallUpdatesOnBoot = a })

-- | Whether this is an Amazon EBS-optimized instance.
iEbsOptimized :: Lens' Instance (Maybe Bool)
iEbsOptimized = lens _iEbsOptimized (\s a -> s { _iEbsOptimized = a })

instance FromJSON Instance

-- | An InstancesCount object with the number of instances in each status.
data InstancesCount = InstancesCount
    { _icBooting :: Maybe Integer
    , _icConnectionLost :: Maybe Integer
    , _icOnline :: Maybe Integer
    , _icPending :: Maybe Integer
    , _icRebooting :: Maybe Integer
    , _icRequested :: Maybe Integer
    , _icRunningSetup :: Maybe Integer
    , _icSetupFailed :: Maybe Integer
    , _icShuttingDown :: Maybe Integer
    , _icStartFailed :: Maybe Integer
    , _icStopped :: Maybe Integer
    , _icStopping :: Maybe Integer
    , _icTerminated :: Maybe Integer
    , _icTerminating :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstancesCount' data type.
--
-- 'InstancesCount' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Booting ::@ @Maybe Integer@
--
-- * @ConnectionLost ::@ @Maybe Integer@
--
-- * @Online ::@ @Maybe Integer@
--
-- * @Pending ::@ @Maybe Integer@
--
-- * @Rebooting ::@ @Maybe Integer@
--
-- * @Requested ::@ @Maybe Integer@
--
-- * @RunningSetup ::@ @Maybe Integer@
--
-- * @SetupFailed ::@ @Maybe Integer@
--
-- * @ShuttingDown ::@ @Maybe Integer@
--
-- * @StartFailed ::@ @Maybe Integer@
--
-- * @Stopped ::@ @Maybe Integer@
--
-- * @Stopping ::@ @Maybe Integer@
--
-- * @Terminated ::@ @Maybe Integer@
--
-- * @Terminating ::@ @Maybe Integer@
--
instancesCount :: InstancesCount
instancesCount = InstancesCount
    { _icBooting = Nothing
    , _icConnectionLost = Nothing
    , _icOnline = Nothing
    , _icPending = Nothing
    , _icRebooting = Nothing
    , _icRequested = Nothing
    , _icRunningSetup = Nothing
    , _icSetupFailed = Nothing
    , _icShuttingDown = Nothing
    , _icStartFailed = Nothing
    , _icStopped = Nothing
    , _icStopping = Nothing
    , _icTerminated = Nothing
    , _icTerminating = Nothing
    }

-- | The number of instances with booting status.
icBooting :: Lens' InstancesCount (Maybe Integer)
icBooting = lens _icBooting (\s a -> s { _icBooting = a })

-- | The number of instances with connection_lost status.
icConnectionLost :: Lens' InstancesCount (Maybe Integer)
icConnectionLost =
    lens _icConnectionLost (\s a -> s { _icConnectionLost = a })

-- | The number of instances with online status.
icOnline :: Lens' InstancesCount (Maybe Integer)
icOnline = lens _icOnline (\s a -> s { _icOnline = a })

-- | The number of instances with pending status.
icPending :: Lens' InstancesCount (Maybe Integer)
icPending = lens _icPending (\s a -> s { _icPending = a })

-- | The number of instances with rebooting status.
icRebooting :: Lens' InstancesCount (Maybe Integer)
icRebooting = lens _icRebooting (\s a -> s { _icRebooting = a })

-- | The number of instances with requested status.
icRequested :: Lens' InstancesCount (Maybe Integer)
icRequested = lens _icRequested (\s a -> s { _icRequested = a })

-- | The number of instances with running_setup status.
icRunningSetup :: Lens' InstancesCount (Maybe Integer)
icRunningSetup = lens _icRunningSetup (\s a -> s { _icRunningSetup = a })

-- | The number of instances with setup_failed status.
icSetupFailed :: Lens' InstancesCount (Maybe Integer)
icSetupFailed = lens _icSetupFailed (\s a -> s { _icSetupFailed = a })

-- | The number of instances with shutting_down status.
icShuttingDown :: Lens' InstancesCount (Maybe Integer)
icShuttingDown = lens _icShuttingDown (\s a -> s { _icShuttingDown = a })

-- | The number of instances with start_failed status.
icStartFailed :: Lens' InstancesCount (Maybe Integer)
icStartFailed = lens _icStartFailed (\s a -> s { _icStartFailed = a })

-- | The number of instances with stopped status.
icStopped :: Lens' InstancesCount (Maybe Integer)
icStopped = lens _icStopped (\s a -> s { _icStopped = a })

-- | The number of instances with stopping status.
icStopping :: Lens' InstancesCount (Maybe Integer)
icStopping = lens _icStopping (\s a -> s { _icStopping = a })

-- | The number of instances with terminated status.
icTerminated :: Lens' InstancesCount (Maybe Integer)
icTerminated = lens _icTerminated (\s a -> s { _icTerminated = a })

-- | The number of instances with terminating status.
icTerminating :: Lens' InstancesCount (Maybe Integer)
icTerminating = lens _icTerminating (\s a -> s { _icTerminating = a })

instance FromJSON InstancesCount

-- | Describes a layer.
data Layer = Layer
    { _lStackId :: Maybe Text
    , _lLayerId :: Maybe Text
    , _lType :: Maybe LayerType
    , _lName :: Maybe Text
    , _lShortname :: Maybe Text
    , _lAttributes :: Map LayerAttributesKeys Text
    , _lCustomInstanceProfileArn :: Maybe Text
    , _lCustomSecurityGroupIds :: [Text]
    , _lDefaultSecurityGroupNames :: [Text]
    , _lPackages :: [Text]
    , _lVolumeConfigurations :: [VolumeConfiguration]
    , _lEnableAutoHealing :: Maybe Bool
    , _lAutoAssignElasticIps :: Maybe Bool
    , _lAutoAssignPublicIps :: Maybe Bool
    , _lDefaultRecipes :: Maybe Recipes
    , _lCustomRecipes :: Maybe Recipes
    , _lCreatedAt :: Maybe Text
    , _lInstallUpdatesOnBoot :: Maybe Bool
    , _lUseEbsOptimizedInstances :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Layer' data type.
--
-- 'Layer' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LayerId ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe LayerType@
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
-- * @DefaultSecurityGroupNames ::@ @[Text]@
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
-- * @DefaultRecipes ::@ @Maybe Recipes@
--
-- * @CustomRecipes ::@ @Maybe Recipes@
--
-- * @CreatedAt ::@ @Maybe Text@
--
-- * @InstallUpdatesOnBoot ::@ @Maybe Bool@
--
-- * @UseEbsOptimizedInstances ::@ @Maybe Bool@
--
layer :: Layer
layer = Layer
    { _lStackId = Nothing
    , _lLayerId = Nothing
    , _lType = Nothing
    , _lName = Nothing
    , _lShortname = Nothing
    , _lAttributes = mempty
    , _lCustomInstanceProfileArn = Nothing
    , _lCustomSecurityGroupIds = mempty
    , _lDefaultSecurityGroupNames = mempty
    , _lPackages = mempty
    , _lVolumeConfigurations = mempty
    , _lEnableAutoHealing = Nothing
    , _lAutoAssignElasticIps = Nothing
    , _lAutoAssignPublicIps = Nothing
    , _lDefaultRecipes = Nothing
    , _lCustomRecipes = Nothing
    , _lCreatedAt = Nothing
    , _lInstallUpdatesOnBoot = Nothing
    , _lUseEbsOptimizedInstances = Nothing
    }

-- | The layer stack ID.
lStackId :: Lens' Layer (Maybe Text)
lStackId = lens _lStackId (\s a -> s { _lStackId = a })

-- | The layer ID.
lLayerId :: Lens' Layer (Maybe Text)
lLayerId = lens _lLayerId (\s a -> s { _lLayerId = a })

-- | The layer type, which must be one of the following: Custom
-- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster NodeJsAppServer
-- PhpAppServer RailsAppServer WebServer.
lType :: Lens' Layer (Maybe LayerType)
lType = lens _lType (\s a -> s { _lType = a })

-- | The layer name.
lName :: Lens' Layer (Maybe Text)
lName = lens _lName (\s a -> s { _lName = a })

-- | The layer short name.
lShortname :: Lens' Layer (Maybe Text)
lShortname = lens _lShortname (\s a -> s { _lShortname = a })

-- | The layer attributes.
lAttributes :: Lens' Layer (Map LayerAttributesKeys Text)
lAttributes = lens _lAttributes (\s a -> s { _lAttributes = a })

-- | The ARN of the default IAM profile to be used for the layer's EC2
-- instances. For more information about IAM ARNs, see Using Identifiers.
lCustomInstanceProfileArn :: Lens' Layer (Maybe Text)
lCustomInstanceProfileArn =
    lens _lCustomInstanceProfileArn
         (\s a -> s { _lCustomInstanceProfileArn = a })

-- | An array containing the layer's custom security group IDs.
lCustomSecurityGroupIds :: Lens' Layer [Text]
lCustomSecurityGroupIds =
    lens _lCustomSecurityGroupIds
         (\s a -> s { _lCustomSecurityGroupIds = a })

-- | An array containing the layer's security group names.
lDefaultSecurityGroupNames :: Lens' Layer [Text]
lDefaultSecurityGroupNames =
    lens _lDefaultSecurityGroupNames
         (\s a -> s { _lDefaultSecurityGroupNames = a })

-- | An array of Package objects that describe the layer's packages.
lPackages :: Lens' Layer [Text]
lPackages = lens _lPackages (\s a -> s { _lPackages = a })

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
lVolumeConfigurations :: Lens' Layer [VolumeConfiguration]
lVolumeConfigurations =
    lens _lVolumeConfigurations (\s a -> s { _lVolumeConfigurations = a })

-- | Whether auto healing is disabled for the layer.
lEnableAutoHealing :: Lens' Layer (Maybe Bool)
lEnableAutoHealing =
    lens _lEnableAutoHealing (\s a -> s { _lEnableAutoHealing = a })

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

-- | AWS OpsWorks supports five lifecycle events, setup, configuration, deploy,
-- undeploy, and shutdown. For each layer, AWS OpsWorks runs a set of standard
-- recipes for each event. In addition, you can provide custom recipes for any
-- or all layers and events. AWS OpsWorks runs custom event recipes after the
-- standard recipes. LayerCustomRecipes specifies the custom recipes for a
-- particular layer to be run in response to each of the five events. To
-- specify a recipe, use the cookbook's directory name in the repository
-- followed by two colons and the recipe name, which is the recipe's file name
-- without the .rb extension. For example: phpapp2::dbsetup specifies the
-- dbsetup.rb recipe in the repository's phpapp2 folder.
lDefaultRecipes :: Lens' Layer (Maybe Recipes)
lDefaultRecipes = lens _lDefaultRecipes (\s a -> s { _lDefaultRecipes = a })

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
lCustomRecipes :: Lens' Layer (Maybe Recipes)
lCustomRecipes = lens _lCustomRecipes (\s a -> s { _lCustomRecipes = a })

-- | Date when the layer was created.
lCreatedAt :: Lens' Layer (Maybe Text)
lCreatedAt = lens _lCreatedAt (\s a -> s { _lCreatedAt = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
lInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
lInstallUpdatesOnBoot =
    lens _lInstallUpdatesOnBoot (\s a -> s { _lInstallUpdatesOnBoot = a })

-- | Whether the layer uses Amazon EBS-optimized instances.
lUseEbsOptimizedInstances :: Lens' Layer (Maybe Bool)
lUseEbsOptimizedInstances =
    lens _lUseEbsOptimizedInstances
         (\s a -> s { _lUseEbsOptimizedInstances = a })

instance FromJSON Layer

-- | Describes a layer's load-based auto scaling configuration.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { _lbascLayerId :: Maybe Text
    , _lbascEnable :: Maybe Bool
    , _lbascUpScaling :: Maybe AutoScalingThresholds
    , _lbascDownScaling :: Maybe AutoScalingThresholds
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBasedAutoScalingConfiguration' data type.
--
-- 'LoadBasedAutoScalingConfiguration' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LayerId ::@ @Maybe Text@
--
-- * @Enable ::@ @Maybe Bool@
--
-- * @UpScaling ::@ @Maybe AutoScalingThresholds@
--
-- * @DownScaling ::@ @Maybe AutoScalingThresholds@
--
loadBasedAutoScalingConfiguration :: LoadBasedAutoScalingConfiguration
loadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { _lbascLayerId = Nothing
    , _lbascEnable = Nothing
    , _lbascUpScaling = Nothing
    , _lbascDownScaling = Nothing
    }

-- | The layer ID.
lbascLayerId :: Lens' LoadBasedAutoScalingConfiguration (Maybe Text)
lbascLayerId = lens _lbascLayerId (\s a -> s { _lbascLayerId = a })

-- | Whether load-based auto scaling is enabled for the layer.
lbascEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbascEnable = lens _lbascEnable (\s a -> s { _lbascEnable = a })

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the number
-- of instances.
lbascUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascUpScaling = lens _lbascUpScaling (\s a -> s { _lbascUpScaling = a })

-- | A LoadBasedAutoscalingInstruction object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the number
-- of instances.
lbascDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascDownScaling =
    lens _lbascDownScaling (\s a -> s { _lbascDownScaling = a })

instance FromJSON LoadBasedAutoScalingConfiguration

-- | Describes stack or user permissions.
data Permission = Permission
    { _pStackId :: Maybe Text
    , _pIamUserArn :: Maybe Text
    , _pAllowSsh :: Maybe Bool
    , _pAllowSudo :: Maybe Bool
    , _pLevel :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Permission' data type.
--
-- 'Permission' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @IamUserArn ::@ @Maybe Text@
--
-- * @AllowSsh ::@ @Maybe Bool@
--
-- * @AllowSudo ::@ @Maybe Bool@
--
-- * @Level ::@ @Maybe Text@
--
permission :: Permission
permission = Permission
    { _pStackId = Nothing
    , _pIamUserArn = Nothing
    , _pAllowSsh = Nothing
    , _pAllowSudo = Nothing
    , _pLevel = Nothing
    }

-- | A stack ID.
pStackId :: Lens' Permission (Maybe Text)
pStackId = lens _pStackId (\s a -> s { _pStackId = a })

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see Using Identifiers.
pIamUserArn :: Lens' Permission (Maybe Text)
pIamUserArn = lens _pIamUserArn (\s a -> s { _pIamUserArn = a })

-- | Whether the user can use SSH.
pAllowSsh :: Lens' Permission (Maybe Bool)
pAllowSsh = lens _pAllowSsh (\s a -> s { _pAllowSsh = a })

-- | Whether the user can use sudo.
pAllowSudo :: Lens' Permission (Maybe Bool)
pAllowSudo = lens _pAllowSudo (\s a -> s { _pAllowSudo = a })

-- | The user's permission level, which must be the following: deny show deploy
-- manage iam_only For more information on the permissions associated with
-- these levels, see Managing User Permissions.
pLevel :: Lens' Permission (Maybe Text)
pLevel = lens _pLevel (\s a -> s { _pLevel = a })

instance FromJSON Permission

-- | Describes an instance's RAID array.
data RaidArray = RaidArray
    { _raRaidArrayId :: Maybe Text
    , _raInstanceId :: Maybe Text
    , _raName :: Maybe Text
    , _raRaidLevel :: Maybe Integer
    , _raNumberOfDisks :: Maybe Integer
    , _raSize :: Maybe Integer
    , _raDevice :: Maybe Text
    , _raMountPoint :: Maybe Text
    , _raAvailabilityZone :: Maybe Text
    , _raCreatedAt :: Maybe Text
    , _raVolumeType :: Maybe Text
    , _raIops :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RaidArray' data type.
--
-- 'RaidArray' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RaidArrayId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @RaidLevel ::@ @Maybe Integer@
--
-- * @NumberOfDisks ::@ @Maybe Integer@
--
-- * @Size ::@ @Maybe Integer@
--
-- * @Device ::@ @Maybe Text@
--
-- * @MountPoint ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
-- * @VolumeType ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
raidArray :: RaidArray
raidArray = RaidArray
    { _raRaidArrayId = Nothing
    , _raInstanceId = Nothing
    , _raName = Nothing
    , _raRaidLevel = Nothing
    , _raNumberOfDisks = Nothing
    , _raSize = Nothing
    , _raDevice = Nothing
    , _raMountPoint = Nothing
    , _raAvailabilityZone = Nothing
    , _raCreatedAt = Nothing
    , _raVolumeType = Nothing
    , _raIops = Nothing
    }

-- | The array ID.
raRaidArrayId :: Lens' RaidArray (Maybe Text)
raRaidArrayId = lens _raRaidArrayId (\s a -> s { _raRaidArrayId = a })

-- | The instance ID.
raInstanceId :: Lens' RaidArray (Maybe Text)
raInstanceId = lens _raInstanceId (\s a -> s { _raInstanceId = a })

-- | The array name.
raName :: Lens' RaidArray (Maybe Text)
raName = lens _raName (\s a -> s { _raName = a })

-- | The RAID level.
raRaidLevel :: Lens' RaidArray (Maybe Integer)
raRaidLevel = lens _raRaidLevel (\s a -> s { _raRaidLevel = a })

-- | The number of disks in the array.
raNumberOfDisks :: Lens' RaidArray (Maybe Integer)
raNumberOfDisks = lens _raNumberOfDisks (\s a -> s { _raNumberOfDisks = a })

-- | The array's size.
raSize :: Lens' RaidArray (Maybe Integer)
raSize = lens _raSize (\s a -> s { _raSize = a })

-- | The array's Linux device. For example /dev/mdadm0.
raDevice :: Lens' RaidArray (Maybe Text)
raDevice = lens _raDevice (\s a -> s { _raDevice = a })

-- | The array's mount point.
raMountPoint :: Lens' RaidArray (Maybe Text)
raMountPoint = lens _raMountPoint (\s a -> s { _raMountPoint = a })

-- | The array's Availability Zone. For more information, see Regions and
-- Endpoints.
raAvailabilityZone :: Lens' RaidArray (Maybe Text)
raAvailabilityZone =
    lens _raAvailabilityZone (\s a -> s { _raAvailabilityZone = a })

-- | When the RAID array was created.
raCreatedAt :: Lens' RaidArray (Maybe Text)
raCreatedAt = lens _raCreatedAt (\s a -> s { _raCreatedAt = a })

-- | The volume type, standard or PIOPS.
raVolumeType :: Lens' RaidArray (Maybe Text)
raVolumeType = lens _raVolumeType (\s a -> s { _raVolumeType = a })

-- | For PIOPS volumes, the IOPS per disk.
raIops :: Lens' RaidArray (Maybe Integer)
raIops = lens _raIops (\s a -> s { _raIops = a })

instance FromJSON RaidArray

-- | Describes an Amazon RDS instance.
data RdsDbInstance = RdsDbInstance
    { _rdiRdsDbInstanceArn :: Maybe Text
    , _rdiDbInstanceIdentifier :: Maybe Text
    , _rdiDbUser :: Maybe Text
    , _rdiDbPassword :: Maybe Text
    , _rdiRegion :: Maybe Text
    , _rdiAddress :: Maybe Text
    , _rdiEngine :: Maybe Text
    , _rdiStackId :: Maybe Text
    , _rdiMissingOnRds :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RdsDbInstance' data type.
--
-- 'RdsDbInstance' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RdsDbInstanceArn ::@ @Maybe Text@
--
-- * @DbInstanceIdentifier ::@ @Maybe Text@
--
-- * @DbUser ::@ @Maybe Text@
--
-- * @DbPassword ::@ @Maybe Text@
--
-- * @Region ::@ @Maybe Text@
--
-- * @Address ::@ @Maybe Text@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @MissingOnRds ::@ @Maybe Bool@
--
rdsDbInstance :: RdsDbInstance
rdsDbInstance = RdsDbInstance
    { _rdiRdsDbInstanceArn = Nothing
    , _rdiDbInstanceIdentifier = Nothing
    , _rdiDbUser = Nothing
    , _rdiDbPassword = Nothing
    , _rdiRegion = Nothing
    , _rdiAddress = Nothing
    , _rdiEngine = Nothing
    , _rdiStackId = Nothing
    , _rdiMissingOnRds = Nothing
    }

-- | The instance's ARN.
rdiRdsDbInstanceArn :: Lens' RdsDbInstance (Maybe Text)
rdiRdsDbInstanceArn =
    lens _rdiRdsDbInstanceArn (\s a -> s { _rdiRdsDbInstanceArn = a })

-- | The DB instance identifier.
rdiDbInstanceIdentifier :: Lens' RdsDbInstance (Maybe Text)
rdiDbInstanceIdentifier =
    lens _rdiDbInstanceIdentifier
         (\s a -> s { _rdiDbInstanceIdentifier = a })

-- | The master user name.
rdiDbUser :: Lens' RdsDbInstance (Maybe Text)
rdiDbUser = lens _rdiDbUser (\s a -> s { _rdiDbUser = a })

-- | The database password.
rdiDbPassword :: Lens' RdsDbInstance (Maybe Text)
rdiDbPassword = lens _rdiDbPassword (\s a -> s { _rdiDbPassword = a })

-- | The instance's AWS region.
rdiRegion :: Lens' RdsDbInstance (Maybe Text)
rdiRegion = lens _rdiRegion (\s a -> s { _rdiRegion = a })

-- | The instance's address.
rdiAddress :: Lens' RdsDbInstance (Maybe Text)
rdiAddress = lens _rdiAddress (\s a -> s { _rdiAddress = a })

-- | The instance's database engine.
rdiEngine :: Lens' RdsDbInstance (Maybe Text)
rdiEngine = lens _rdiEngine (\s a -> s { _rdiEngine = a })

-- | The ID of the stack that the instance is registered with.
rdiStackId :: Lens' RdsDbInstance (Maybe Text)
rdiStackId = lens _rdiStackId (\s a -> s { _rdiStackId = a })

-- | Set to true if AWS OpsWorks was unable to discover the Amazon RDS instance.
-- AWS OpsWorks attempts to discover the instance only once. If this value is
-- set to true, you must deregister the instance and then register it again.
rdiMissingOnRds :: Lens' RdsDbInstance (Maybe Bool)
rdiMissingOnRds = lens _rdiMissingOnRds (\s a -> s { _rdiMissingOnRds = a })

instance FromJSON RdsDbInstance

-- | A LayerCustomRecipes object that specifies the layer custom recipes.
data Recipes = Recipes
    { _rSetup :: [Text]
    , _rConfigure :: [Text]
    , _rDeploy :: [Text]
    , _rUndeploy :: [Text]
    , _rShutdown :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Recipes' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Setup ::@ @[Text]@
--
-- * @Configure ::@ @[Text]@
--
-- * @Deploy ::@ @[Text]@
--
-- * @Undeploy ::@ @[Text]@
--
-- * @Shutdown ::@ @[Text]@
--
recipes :: Recipes
recipes = Recipes
    { _rSetup = mempty
    , _rConfigure = mempty
    , _rDeploy = mempty
    , _rUndeploy = mempty
    , _rShutdown = mempty
    }

-- | An array of custom recipe names to be run following a setup event.
rSetup :: Lens' Recipes [Text]
rSetup = lens _rSetup (\s a -> s { _rSetup = a })

-- | An array of custom recipe names to be run following a configure event.
rConfigure :: Lens' Recipes [Text]
rConfigure = lens _rConfigure (\s a -> s { _rConfigure = a })

-- | An array of custom recipe names to be run following a deploy event.
rDeploy :: Lens' Recipes [Text]
rDeploy = lens _rDeploy (\s a -> s { _rDeploy = a })

-- | An array of custom recipe names to be run following a undeploy event.
rUndeploy :: Lens' Recipes [Text]
rUndeploy = lens _rUndeploy (\s a -> s { _rUndeploy = a })

-- | An array of custom recipe names to be run following a shutdown event.
rShutdown :: Lens' Recipes [Text]
rShutdown = lens _rShutdown (\s a -> s { _rShutdown = a })

instance FromJSON Recipes

instance ToJSON Recipes

-- | A UserProfile object that describes the user's SSH information.
data SelfUserProfile = SelfUserProfile
    { _supIamUserArn :: Maybe Text
    , _supName :: Maybe Text
    , _supSshUsername :: Maybe Text
    , _supSshPublicKey :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SelfUserProfile' data type.
--
-- 'SelfUserProfile' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IamUserArn ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @SshUsername ::@ @Maybe Text@
--
-- * @SshPublicKey ::@ @Maybe Text@
--
selfUserProfile :: SelfUserProfile
selfUserProfile = SelfUserProfile
    { _supIamUserArn = Nothing
    , _supName = Nothing
    , _supSshUsername = Nothing
    , _supSshPublicKey = Nothing
    }

-- | The user's IAM ARN.
supIamUserArn :: Lens' SelfUserProfile (Maybe Text)
supIamUserArn = lens _supIamUserArn (\s a -> s { _supIamUserArn = a })

-- | The user's name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName = lens _supName (\s a -> s { _supName = a })

-- | The user's SSH user name.
supSshUsername :: Lens' SelfUserProfile (Maybe Text)
supSshUsername = lens _supSshUsername (\s a -> s { _supSshUsername = a })

-- | The user's SSH public key.
supSshPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSshPublicKey = lens _supSshPublicKey (\s a -> s { _supSshPublicKey = a })

instance FromJSON SelfUserProfile

-- | Describes an AWS OpsWorks service error.
data ServiceError = ServiceError
    { _seServiceErrorId :: Maybe Text
    , _seStackId :: Maybe Text
    , _seInstanceId :: Maybe Text
    , _seType :: Maybe Text
    , _seMessage :: Maybe Text
    , _seCreatedAt :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ServiceError' data type.
--
-- 'ServiceError' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServiceErrorId ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
serviceError' :: ServiceError
serviceError' = ServiceError
    { _seServiceErrorId = Nothing
    , _seStackId = Nothing
    , _seInstanceId = Nothing
    , _seType = Nothing
    , _seMessage = Nothing
    , _seCreatedAt = Nothing
    }

-- | The error ID.
seServiceErrorId :: Lens' ServiceError (Maybe Text)
seServiceErrorId =
    lens _seServiceErrorId (\s a -> s { _seServiceErrorId = a })

-- | The stack ID.
seStackId :: Lens' ServiceError (Maybe Text)
seStackId = lens _seStackId (\s a -> s { _seStackId = a })

-- | The instance ID.
seInstanceId :: Lens' ServiceError (Maybe Text)
seInstanceId = lens _seInstanceId (\s a -> s { _seInstanceId = a })

-- | The error type.
seType :: Lens' ServiceError (Maybe Text)
seType = lens _seType (\s a -> s { _seType = a })

-- | A message that describes the error.
seMessage :: Lens' ServiceError (Maybe Text)
seMessage = lens _seMessage (\s a -> s { _seMessage = a })

-- | When the error occurred.
seCreatedAt :: Lens' ServiceError (Maybe Text)
seCreatedAt = lens _seCreatedAt (\s a -> s { _seCreatedAt = a })

instance FromJSON ServiceError

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
data Source = Source
    { _sType :: Maybe SourceType
    , _sUrl :: Maybe Text
    , _sUsername :: Maybe Text
    , _sPassword :: Maybe Text
    , _sSshKey :: Maybe Text
    , _sRevision :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Source' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Type ::@ @Maybe SourceType@
--
-- * @Url ::@ @Maybe Text@
--
-- * @Username ::@ @Maybe Text@
--
-- * @Password ::@ @Maybe Text@
--
-- * @SshKey ::@ @Maybe Text@
--
-- * @Revision ::@ @Maybe Text@
--
source :: Source
source = Source
    { _sType = Nothing
    , _sUrl = Nothing
    , _sUsername = Nothing
    , _sPassword = Nothing
    , _sSshKey = Nothing
    , _sRevision = Nothing
    }

-- | The repository type.
sType :: Lens' Source (Maybe SourceType)
sType = lens _sType (\s a -> s { _sType = a })

-- | The source URL.
sUrl :: Lens' Source (Maybe Text)
sUrl = lens _sUrl (\s a -> s { _sUrl = a })

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Username to the appropriate IAM access key ID. For HTTP bundles, Git
-- repositories, and Subversion repositories, set Username to the user name.
sUsername :: Lens' Source (Maybe Text)
sUsername = lens _sUsername (\s a -> s { _sUsername = a })

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Password to the appropriate IAM secret access key. For HTTP bundles and
-- Subversion repositories, set Password to the password. For more information
-- on how to safely handle IAM credentials, see .
sPassword :: Lens' Source (Maybe Text)
sPassword = lens _sPassword (\s a -> s { _sPassword = a })

-- | The repository's SSH key.
sSshKey :: Lens' Source (Maybe Text)
sSshKey = lens _sSshKey (\s a -> s { _sSshKey = a })

-- | The application's version. AWS OpsWorks enables you to easily deploy new
-- versions of an application. One of the simplest approaches is to have
-- branches or revisions in your repository that represent different versions
-- that can potentially be deployed.
sRevision :: Lens' Source (Maybe Text)
sRevision = lens _sRevision (\s a -> s { _sRevision = a })

instance FromJSON Source

instance ToJSON Source

-- | An SslConfiguration object with the SSL configuration.
data SslConfiguration = SslConfiguration
    { _scCertificate :: Text
    , _scPrivateKey :: Text
    , _scChain :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SslConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Certificate ::@ @Text@
--
-- * @PrivateKey ::@ @Text@
--
-- * @Chain ::@ @Maybe Text@
--
sslConfiguration :: Text -- ^ 'scCertificate'
                   -> Text -- ^ 'scPrivateKey'
                   -> SslConfiguration
sslConfiguration p1 p2 = SslConfiguration
    { _scCertificate = p1
    , _scPrivateKey = p2
    , _scChain = Nothing
    }

-- | The contents of the certificate's domain.crt file.
scCertificate :: Lens' SslConfiguration Text
scCertificate = lens _scCertificate (\s a -> s { _scCertificate = a })

-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey :: Lens' SslConfiguration Text
scPrivateKey = lens _scPrivateKey (\s a -> s { _scPrivateKey = a })

-- | Optional. Can be used to specify an intermediate certificate authority key
-- or client authentication.
scChain :: Lens' SslConfiguration (Maybe Text)
scChain = lens _scChain (\s a -> s { _scChain = a })

instance FromJSON SslConfiguration

instance ToJSON SslConfiguration

-- | Describes a stack.
data Stack = Stack
    { _srStackId :: Maybe Text
    , _srName :: Maybe Text
    , _srArn :: Maybe Text
    , _srRegion :: Maybe Text
    , _srVpcId :: Maybe Text
    , _srAttributes :: Map StackAttributesKeys Text
    , _srServiceRoleArn :: Maybe Text
    , _srDefaultInstanceProfileArn :: Maybe Text
    , _srDefaultOs :: Maybe Text
    , _srHostnameTheme :: Maybe Text
    , _srDefaultAvailabilityZone :: Maybe Text
    , _srDefaultSubnetId :: Maybe Text
    , _srCustomJson :: Maybe Text
    , _srConfigurationManager :: Maybe StackConfigurationManager
    , _srChefConfiguration :: Maybe ChefConfiguration
    , _srUseCustomCookbooks :: Maybe Bool
    , _srUseOpsworksSecurityGroups :: Maybe Bool
    , _srCustomCookbooksSource :: Maybe Source
    , _srDefaultSshKeyName :: Maybe Text
    , _srCreatedAt :: Maybe Text
    , _srDefaultRootDeviceType :: Maybe RootDeviceType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Stack' data type.
--
-- 'Stack' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Arn ::@ @Maybe Text@
--
-- * @Region ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map StackAttributesKeys Text@
--
-- * @ServiceRoleArn ::@ @Maybe Text@
--
-- * @DefaultInstanceProfileArn ::@ @Maybe Text@
--
-- * @DefaultOs ::@ @Maybe Text@
--
-- * @HostnameTheme ::@ @Maybe Text@
--
-- * @DefaultAvailabilityZone ::@ @Maybe Text@
--
-- * @DefaultSubnetId ::@ @Maybe Text@
--
-- * @CustomJson ::@ @Maybe Text@
--
-- * @ConfigurationManager ::@ @Maybe StackConfigurationManager@
--
-- * @ChefConfiguration ::@ @Maybe ChefConfiguration@
--
-- * @UseCustomCookbooks ::@ @Maybe Bool@
--
-- * @UseOpsworksSecurityGroups ::@ @Maybe Bool@
--
-- * @CustomCookbooksSource ::@ @Maybe Source@
--
-- * @DefaultSshKeyName ::@ @Maybe Text@
--
-- * @CreatedAt ::@ @Maybe Text@
--
-- * @DefaultRootDeviceType ::@ @Maybe RootDeviceType@
--
stack :: Stack
stack = Stack
    { _srStackId = Nothing
    , _srName = Nothing
    , _srArn = Nothing
    , _srRegion = Nothing
    , _srVpcId = Nothing
    , _srAttributes = mempty
    , _srServiceRoleArn = Nothing
    , _srDefaultInstanceProfileArn = Nothing
    , _srDefaultOs = Nothing
    , _srHostnameTheme = Nothing
    , _srDefaultAvailabilityZone = Nothing
    , _srDefaultSubnetId = Nothing
    , _srCustomJson = Nothing
    , _srConfigurationManager = Nothing
    , _srChefConfiguration = Nothing
    , _srUseCustomCookbooks = Nothing
    , _srUseOpsworksSecurityGroups = Nothing
    , _srCustomCookbooksSource = Nothing
    , _srDefaultSshKeyName = Nothing
    , _srCreatedAt = Nothing
    , _srDefaultRootDeviceType = Nothing
    }

-- | The stack ID.
srStackId :: Lens' Stack (Maybe Text)
srStackId = lens _srStackId (\s a -> s { _srStackId = a })

-- | The stack name.
srName :: Lens' Stack (Maybe Text)
srName = lens _srName (\s a -> s { _srName = a })

-- | The stack's ARN.
srArn :: Lens' Stack (Maybe Text)
srArn = lens _srArn (\s a -> s { _srArn = a })

-- | The stack AWS region, such as "us-east-1". For more information about AWS
-- regions, see Regions and Endpoints.
srRegion :: Lens' Stack (Maybe Text)
srRegion = lens _srRegion (\s a -> s { _srRegion = a })

-- | The VPC ID, if the stack is running in a VPC.
srVpcId :: Lens' Stack (Maybe Text)
srVpcId = lens _srVpcId (\s a -> s { _srVpcId = a })

-- | The stack's attributes.
srAttributes :: Lens' Stack (Map StackAttributesKeys Text)
srAttributes = lens _srAttributes (\s a -> s { _srAttributes = a })

-- | The stack AWS Identity and Access Management (IAM) role.
srServiceRoleArn :: Lens' Stack (Maybe Text)
srServiceRoleArn =
    lens _srServiceRoleArn (\s a -> s { _srServiceRoleArn = a })

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
srDefaultInstanceProfileArn :: Lens' Stack (Maybe Text)
srDefaultInstanceProfileArn =
    lens _srDefaultInstanceProfileArn
         (\s a -> s { _srDefaultInstanceProfileArn = a })

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
srDefaultOs :: Lens' Stack (Maybe Text)
srDefaultOs = lens _srDefaultOs (\s a -> s { _srDefaultOs = a })

-- | The stack host name theme, with spaces replaced by underscores.
srHostnameTheme :: Lens' Stack (Maybe Text)
srHostnameTheme = lens _srHostnameTheme (\s a -> s { _srHostnameTheme = a })

-- | The stack's default Availability Zone. For more information, see Regions
-- and Endpoints.
srDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
srDefaultAvailabilityZone =
    lens _srDefaultAvailabilityZone
         (\s a -> s { _srDefaultAvailabilityZone = a })

-- | The default subnet ID, if the stack is running in a VPC.
srDefaultSubnetId :: Lens' Stack (Maybe Text)
srDefaultSubnetId =
    lens _srDefaultSubnetId (\s a -> s { _srDefaultSubnetId = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
srCustomJson :: Lens' Stack (Maybe Text)
srCustomJson = lens _srCustomJson (\s a -> s { _srCustomJson = a })

-- | The configuration manager.
srConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
srConfigurationManager =
    lens _srConfigurationManager (\s a -> s { _srConfigurationManager = a })

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version. For more information, see Create a New Stack.
srChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
srChefConfiguration =
    lens _srChefConfiguration (\s a -> s { _srChefConfiguration = a })

-- | Whether the stack uses custom cookbooks.
srUseCustomCookbooks :: Lens' Stack (Maybe Bool)
srUseCustomCookbooks =
    lens _srUseCustomCookbooks (\s a -> s { _srUseCustomCookbooks = a })

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack's layers.
srUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
srUseOpsworksSecurityGroups =
    lens _srUseOpsworksSecurityGroups
         (\s a -> s { _srUseOpsworksSecurityGroups = a })

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
srCustomCookbooksSource :: Lens' Stack (Maybe Source)
srCustomCookbooksSource =
    lens _srCustomCookbooksSource
         (\s a -> s { _srCustomCookbooksSource = a })

-- | A default SSH key for the stack's instances. You can override this value
-- when you create or update an instance.
srDefaultSshKeyName :: Lens' Stack (Maybe Text)
srDefaultSshKeyName =
    lens _srDefaultSshKeyName (\s a -> s { _srDefaultSshKeyName = a })

-- | Date when the stack was created.
srCreatedAt :: Lens' Stack (Maybe Text)
srCreatedAt = lens _srCreatedAt (\s a -> s { _srCreatedAt = a })

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
srDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
srDefaultRootDeviceType =
    lens _srDefaultRootDeviceType
         (\s a -> s { _srDefaultRootDeviceType = a })

instance FromJSON Stack

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
data StackConfigurationManager = StackConfigurationManager
    { _scmName :: Maybe Text
    , _scmVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackConfigurationManager' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Version ::@ @Maybe Text@
--
stackConfigurationManager :: StackConfigurationManager
stackConfigurationManager = StackConfigurationManager
    { _scmName = Nothing
    , _scmVersion = Nothing
    }

-- | The name. This parameter must be set to "Chef".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\s a -> s { _scmName = a })

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\s a -> s { _scmVersion = a })

instance FromJSON StackConfigurationManager

instance ToJSON StackConfigurationManager

-- | A StackSummary object that contains the results.
data StackSummary = StackSummary
    { _ssStackId :: Maybe Text
    , _ssName :: Maybe Text
    , _ssArn :: Maybe Text
    , _ssLayersCount :: Maybe Integer
    , _ssAppsCount :: Maybe Integer
    , _ssInstancesCount :: Maybe InstancesCount
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackSummary' data type.
--
-- 'StackSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Arn ::@ @Maybe Text@
--
-- * @LayersCount ::@ @Maybe Integer@
--
-- * @AppsCount ::@ @Maybe Integer@
--
-- * @InstancesCount ::@ @Maybe InstancesCount@
--
stackSummary :: StackSummary
stackSummary = StackSummary
    { _ssStackId = Nothing
    , _ssName = Nothing
    , _ssArn = Nothing
    , _ssLayersCount = Nothing
    , _ssAppsCount = Nothing
    , _ssInstancesCount = Nothing
    }

-- | The stack ID.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\s a -> s { _ssStackId = a })

-- | The stack name.
ssName :: Lens' StackSummary (Maybe Text)
ssName = lens _ssName (\s a -> s { _ssName = a })

-- | The stack's ARN.
ssArn :: Lens' StackSummary (Maybe Text)
ssArn = lens _ssArn (\s a -> s { _ssArn = a })

-- | The number of layers.
ssLayersCount :: Lens' StackSummary (Maybe Integer)
ssLayersCount = lens _ssLayersCount (\s a -> s { _ssLayersCount = a })

-- | The number of apps.
ssAppsCount :: Lens' StackSummary (Maybe Integer)
ssAppsCount = lens _ssAppsCount (\s a -> s { _ssAppsCount = a })

-- | An InstancesCount object with the number of instances in each status.
ssInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssInstancesCount =
    lens _ssInstancesCount (\s a -> s { _ssInstancesCount = a })

instance FromJSON StackSummary

-- | Describes an instance's time-based auto scaling configuration.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { _tbascInstanceId :: Maybe Text
    , _tbascAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TimeBasedAutoScalingConfiguration' data type.
--
-- 'TimeBasedAutoScalingConfiguration' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @AutoScalingSchedule ::@ @Maybe WeeklyAutoScalingSchedule@
--
timeBasedAutoScalingConfiguration :: TimeBasedAutoScalingConfiguration
timeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { _tbascInstanceId = Nothing
    , _tbascAutoScalingSchedule = Nothing
    }

-- | The instance ID.
tbascInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbascInstanceId = lens _tbascInstanceId (\s a -> s { _tbascInstanceId = a })

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
tbascAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule =
    lens _tbascAutoScalingSchedule
         (\s a -> s { _tbascAutoScalingSchedule = a })

instance FromJSON TimeBasedAutoScalingConfiguration

-- | Describes a user's SSH information.
data UserProfile = UserProfile
    { _upIamUserArn :: Maybe Text
    , _upName :: Maybe Text
    , _upSshUsername :: Maybe Text
    , _upSshPublicKey :: Maybe Text
    , _upAllowSelfManagement :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'UserProfile' data type.
--
-- 'UserProfile' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IamUserArn ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @SshUsername ::@ @Maybe Text@
--
-- * @SshPublicKey ::@ @Maybe Text@
--
-- * @AllowSelfManagement ::@ @Maybe Bool@
--
userProfile :: UserProfile
userProfile = UserProfile
    { _upIamUserArn = Nothing
    , _upName = Nothing
    , _upSshUsername = Nothing
    , _upSshPublicKey = Nothing
    , _upAllowSelfManagement = Nothing
    }

-- | The user's IAM ARN.
upIamUserArn :: Lens' UserProfile (Maybe Text)
upIamUserArn = lens _upIamUserArn (\s a -> s { _upIamUserArn = a })

-- | The user's name.
upName :: Lens' UserProfile (Maybe Text)
upName = lens _upName (\s a -> s { _upName = a })

-- | The user's SSH user name.
upSshUsername :: Lens' UserProfile (Maybe Text)
upSshUsername = lens _upSshUsername (\s a -> s { _upSshUsername = a })

-- | The user's SSH public key.
upSshPublicKey :: Lens' UserProfile (Maybe Text)
upSshPublicKey = lens _upSshPublicKey (\s a -> s { _upSshPublicKey = a })

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Managing User Permissions.
upAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
upAllowSelfManagement =
    lens _upAllowSelfManagement (\s a -> s { _upAllowSelfManagement = a })

instance FromJSON UserProfile

-- | Describes an instance's Amazon EBS volume.
data Volume = Volume
    { _vVolumeId :: Maybe Text
    , _vEc2VolumeId :: Maybe Text
    , _vName :: Maybe Text
    , _vRaidArrayId :: Maybe Text
    , _vInstanceId :: Maybe Text
    , _vStatus :: Maybe Text
    , _vSize :: Maybe Integer
    , _vDevice :: Maybe Text
    , _vMountPoint :: Maybe Text
    , _vRegion :: Maybe Text
    , _vAvailabilityZone :: Maybe Text
    , _vVolumeType :: Maybe Text
    , _vIops :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Volume' data type.
--
-- 'Volume' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @Ec2VolumeId ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @RaidArrayId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @Size ::@ @Maybe Integer@
--
-- * @Device ::@ @Maybe Text@
--
-- * @MountPoint ::@ @Maybe Text@
--
-- * @Region ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @VolumeType ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
volume :: Volume
volume = Volume
    { _vVolumeId = Nothing
    , _vEc2VolumeId = Nothing
    , _vName = Nothing
    , _vRaidArrayId = Nothing
    , _vInstanceId = Nothing
    , _vStatus = Nothing
    , _vSize = Nothing
    , _vDevice = Nothing
    , _vMountPoint = Nothing
    , _vRegion = Nothing
    , _vAvailabilityZone = Nothing
    , _vVolumeType = Nothing
    , _vIops = Nothing
    }

-- | The volume ID.
vVolumeId :: Lens' Volume (Maybe Text)
vVolumeId = lens _vVolumeId (\s a -> s { _vVolumeId = a })

-- | The Amazon EC2 volume ID.
vEc2VolumeId :: Lens' Volume (Maybe Text)
vEc2VolumeId = lens _vEc2VolumeId (\s a -> s { _vEc2VolumeId = a })

-- | The volume name.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\s a -> s { _vName = a })

-- | The RAID array ID.
vRaidArrayId :: Lens' Volume (Maybe Text)
vRaidArrayId = lens _vRaidArrayId (\s a -> s { _vRaidArrayId = a })

-- | The instance ID.
vInstanceId :: Lens' Volume (Maybe Text)
vInstanceId = lens _vInstanceId (\s a -> s { _vInstanceId = a })

-- | The value returned by DescribeVolumes.
vStatus :: Lens' Volume (Maybe Text)
vStatus = lens _vStatus (\s a -> s { _vStatus = a })

-- | The volume size.
vSize :: Lens' Volume (Maybe Integer)
vSize = lens _vSize (\s a -> s { _vSize = a })

-- | The device name.
vDevice :: Lens' Volume (Maybe Text)
vDevice = lens _vDevice (\s a -> s { _vDevice = a })

-- | The volume mount point. For example "/dev/sdh".
vMountPoint :: Lens' Volume (Maybe Text)
vMountPoint = lens _vMountPoint (\s a -> s { _vMountPoint = a })

-- | The AWS region. For more information about AWS regions, see Regions and
-- Endpoints.
vRegion :: Lens' Volume (Maybe Text)
vRegion = lens _vRegion (\s a -> s { _vRegion = a })

-- | The volume Availability Zone. For more information, see Regions and
-- Endpoints.
vAvailabilityZone :: Lens' Volume (Maybe Text)
vAvailabilityZone =
    lens _vAvailabilityZone (\s a -> s { _vAvailabilityZone = a })

-- | The volume type, standard or PIOPS.
vVolumeType :: Lens' Volume (Maybe Text)
vVolumeType = lens _vVolumeType (\s a -> s { _vVolumeType = a })

-- | For PIOPS volumes, the IOPS per disk.
vIops :: Lens' Volume (Maybe Integer)
vIops = lens _vIops (\s a -> s { _vIops = a })

instance FromJSON Volume

-- | Describes an Amazon EBS volume configuration.
data VolumeConfiguration = VolumeConfiguration
    { _vcMountPoint :: Text
    , _vcRaidLevel :: Maybe Integer
    , _vcNumberOfDisks :: !Integer
    , _vcSize :: !Integer
    , _vcVolumeType :: Maybe Text
    , _vcIops :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MountPoint ::@ @Text@
--
-- * @RaidLevel ::@ @Maybe Integer@
--
-- * @NumberOfDisks ::@ @Integer@
--
-- * @Size ::@ @Integer@
--
-- * @VolumeType ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
volumeConfiguration :: Text -- ^ 'vcMountPoint'
                      -> Integer -- ^ 'vcNumberOfDisks'
                      -> Integer -- ^ 'vcSize'
                      -> VolumeConfiguration
volumeConfiguration p1 p3 p4 = VolumeConfiguration
    { _vcMountPoint = p1
    , _vcRaidLevel = Nothing
    , _vcNumberOfDisks = p3
    , _vcSize = p4
    , _vcVolumeType = Nothing
    , _vcIops = Nothing
    }

-- | The volume mount point. For example "/dev/sdh".
vcMountPoint :: Lens' VolumeConfiguration Text
vcMountPoint = lens _vcMountPoint (\s a -> s { _vcMountPoint = a })

-- | The volume RAID level.
vcRaidLevel :: Lens' VolumeConfiguration (Maybe Integer)
vcRaidLevel = lens _vcRaidLevel (\s a -> s { _vcRaidLevel = a })

-- | The number of disks in the volume.
vcNumberOfDisks :: Lens' VolumeConfiguration Integer
vcNumberOfDisks = lens _vcNumberOfDisks (\s a -> s { _vcNumberOfDisks = a })

-- | The volume size.
vcSize :: Lens' VolumeConfiguration Integer
vcSize = lens _vcSize (\s a -> s { _vcSize = a })

-- | The volume type, standard or PIOPS.
vcVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vcVolumeType = lens _vcVolumeType (\s a -> s { _vcVolumeType = a })

-- | For PIOPS volumes, the IOPS per disk.
vcIops :: Lens' VolumeConfiguration (Maybe Integer)
vcIops = lens _vcIops (\s a -> s { _vcIops = a })

instance FromJSON VolumeConfiguration

instance ToJSON VolumeConfiguration

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassMonday :: Map Text Text
    , _wassTuesday :: Map Text Text
    , _wassWednesday :: Map Text Text
    , _wassThursday :: Map Text Text
    , _wassFriday :: Map Text Text
    , _wassSaturday :: Map Text Text
    , _wassSunday :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WeeklyAutoScalingSchedule' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Monday ::@ @Map Text Text@
--
-- * @Tuesday ::@ @Map Text Text@
--
-- * @Wednesday ::@ @Map Text Text@
--
-- * @Thursday ::@ @Map Text Text@
--
-- * @Friday ::@ @Map Text Text@
--
-- * @Saturday ::@ @Map Text Text@
--
-- * @Sunday ::@ @Map Text Text@
--
weeklyAutoScalingSchedule :: WeeklyAutoScalingSchedule
weeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassMonday = mempty
    , _wassTuesday = mempty
    , _wassWednesday = mempty
    , _wassThursday = mempty
    , _wassFriday = mempty
    , _wassSaturday = mempty
    , _wassSunday = mempty
    }

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassMonday = lens _wassMonday (\s a -> s { _wassMonday = a })

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassTuesday = lens _wassTuesday (\s a -> s { _wassTuesday = a })

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassWednesday = lens _wassWednesday (\s a -> s { _wassWednesday = a })

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassThursday = lens _wassThursday (\s a -> s { _wassThursday = a })

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassFriday = lens _wassFriday (\s a -> s { _wassFriday = a })

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassSaturday = lens _wassSaturday (\s a -> s { _wassSaturday = a })

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassSunday = lens _wassSunday (\s a -> s { _wassSunday = a })

instance FromJSON WeeklyAutoScalingSchedule

instance ToJSON WeeklyAutoScalingSchedule
