{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.Types
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
module Network.AWS.OpsWorks.V2013_02_18.Types
    (
    -- * Service
      OpsWorks
    -- ** Errors
    , Er (..)
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
    , apAppId
    , apStackId
    , apShortname
    , apName
    , apDescription
    , apDataSources
    , apType
    , apAppSource
    , apDomains
    , apEnableSsl
    , apSslConfiguration
    , apAttributes
    , apCreatedAt

    -- * AutoScalingThresholds
    , AutoScalingThresholds
    , mkAutoScalingThresholds
    , asvInstanceCount
    , asvThresholdsWaitTime
    , asvIgnoreMetricsTime
    , asvCpuThreshold
    , asvMemoryThreshold
    , asvLoadThreshold

    -- * ChefConfiguration
    , ChefConfiguration
    , mkChefConfiguration
    , ccManageBerkshelf
    , ccBerkshelfVersion

    -- * Command
    , Command
    , ceCommandId
    , ceInstanceId
    , ceDeploymentId
    , ceCreatedAt
    , ceAcknowledgedAt
    , ceCompletedAt
    , ceStatus
    , ceExitCode
    , ceLogUrl
    , ceType

    -- * DataSource
    , DataSource
    , mkDataSource
    , dtType
    , dtArn
    , dtDatabaseName

    -- * Deployment
    , Deployment
    , ddtDeploymentId
    , ddtStackId
    , ddtAppId
    , ddtCreatedAt
    , ddtCompletedAt
    , ddtDuration
    , ddtIamUserArn
    , ddtComment
    , ddtCommand
    , ddtStatus
    , ddtCustomJson
    , ddtInstanceIds

    -- * DeploymentCommand
    , DeploymentCommand
    , mkDeploymentCommand
    , dcName
    , dcArgs

    -- * ElasticIp
    , ElasticIp
    , ejIp
    , ejName
    , ejDomain
    , ejRegion
    , ejInstanceId

    -- * ElasticLoadBalancer
    , ElasticLoadBalancer
    , elcElasticLoadBalancerName
    , elcRegion
    , elcDnsName
    , elcStackId
    , elcLayerId
    , elcVpcId
    , elcAvailabilityZones
    , elcSubnetIds
    , elcEc2InstanceIds

    -- * Instance
    , Instance
    , ieInstanceId
    , ieEc2InstanceId
    , ieVirtualizationType
    , ieHostname
    , ieStackId
    , ieLayerIds
    , ieSecurityGroupIds
    , ieInstanceType
    , ieInstanceProfileArn
    , ieStatus
    , ieOs
    , ieAmiId
    , ieAvailabilityZone
    , ieSubnetId
    , iePublicDns
    , iePrivateDns
    , iePublicIp
    , iePrivateIp
    , ieElasticIp
    , ieAutoScalingType
    , ieSshKeyName
    , ieSshHostRsaKeyFingerprint
    , ieSshHostDsaKeyFingerprint
    , ieCreatedAt
    , ieLastServiceErrorId
    , ieArchitecture
    , ieRootDeviceType
    , ieRootDeviceVolumeId
    , ieInstallUpdatesOnBoot
    , ieEbsOptimized

    -- * InstancesCount
    , InstancesCount
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
    , lrStackId
    , lrLayerId
    , lrType
    , lrName
    , lrShortname
    , lrAttributes
    , lrCustomInstanceProfileArn
    , lrCustomSecurityGroupIds
    , lrDefaultSecurityGroupNames
    , lrPackages
    , lrVolumeConfigurations
    , lrEnableAutoHealing
    , lrAutoAssignElasticIps
    , lrAutoAssignPublicIps
    , lrDefaultRecipes
    , lrCustomRecipes
    , lrCreatedAt
    , lrInstallUpdatesOnBoot
    , lrUseEbsOptimizedInstances

    -- * LoadBasedAutoScalingConfiguration
    , LoadBasedAutoScalingConfiguration
    , lbasdLayerId
    , lbasdEnable
    , lbasdUpScaling
    , lbasdDownScaling

    -- * Permission
    , Permission
    , pnStackId
    , pnIamUserArn
    , pnAllowSsh
    , pnAllowSudo
    , pnLevel

    -- * RaidArray
    , RaidArray
    , rbRaidArrayId
    , rbInstanceId
    , rbName
    , rbRaidLevel
    , rbNumberOfDisks
    , rbSize
    , rbDevice
    , rbMountPoint
    , rbAvailabilityZone
    , rbCreatedAt
    , rbVolumeType
    , rbIops

    -- * RdsDbInstance
    , RdsDbInstance
    , rdjRdsDbInstanceArn
    , rdjDbInstanceIdentifier
    , rdjDbUser
    , rdjDbPassword
    , rdjRegion
    , rdjAddress
    , rdjEngine
    , rdjStackId
    , rdjMissingOnRds

    -- * Recipes
    , Recipes
    , mkRecipes
    , rSetup
    , rConfigure
    , rDeploy
    , rUndeploy
    , rShutdown

    -- * SelfUserProfile
    , SelfUserProfile
    , supIamUserArn
    , supName
    , supSshUsername
    , supSshPublicKey

    -- * ServiceError
    , ServiceError
    , sssssssssssssssxServiceErrorId
    , sssssssssssssssxStackId
    , sssssssssssssssxInstanceId
    , sssssssssssssssxType
    , sssssssssssssssxMessage
    , sssssssssssssssxCreatedAt

    -- * Source
    , Source
    , mkSource
    , seType
    , seUrl
    , seUsername
    , sePassword
    , seSshKey
    , seRevision

    -- * SslConfiguration
    , SslConfiguration
    , mkSslConfiguration
    , scCertificate
    , scPrivateKey
    , scChain

    -- * Stack
    , Stack
    , ssssssssssssssssoStackId
    , ssssssssssssssssoName
    , ssssssssssssssssoArn
    , ssssssssssssssssoRegion
    , ssssssssssssssssoVpcId
    , ssssssssssssssssoAttributes
    , ssssssssssssssssoServiceRoleArn
    , ssssssssssssssssoDefaultInstanceProfileArn
    , ssssssssssssssssoDefaultOs
    , ssssssssssssssssoHostnameTheme
    , ssssssssssssssssoDefaultAvailabilityZone
    , ssssssssssssssssoDefaultSubnetId
    , ssssssssssssssssoCustomJson
    , ssssssssssssssssoConfigurationManager
    , ssssssssssssssssoChefConfiguration
    , ssssssssssssssssoUseCustomCookbooks
    , ssssssssssssssssoUseOpsworksSecurityGroups
    , ssssssssssssssssoCustomCookbooksSource
    , ssssssssssssssssoDefaultSshKeyName
    , ssssssssssssssssoCreatedAt
    , ssssssssssssssssoDefaultRootDeviceType

    -- * StackConfigurationManager
    , StackConfigurationManager
    , mkStackConfigurationManager
    , scmName
    , scmVersion

    -- * StackSummary
    , StackSummary
    , ssssssssssssssssyStackId
    , ssssssssssssssssyName
    , ssssssssssssssssyArn
    , ssssssssssssssssyLayersCount
    , ssssssssssssssssyAppsCount
    , ssssssssssssssssyInstancesCount

    -- * TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration
    , tbasdInstanceId
    , tbasdAutoScalingSchedule

    -- * UserProfile
    , UserProfile
    , uxIamUserArn
    , uxName
    , uxSshUsername
    , uxSshPublicKey
    , uxAllowSelfManagement

    -- * Volume
    , Volume
    , vgVolumeId
    , vgEc2VolumeId
    , vgName
    , vgRaidArrayId
    , vgInstanceId
    , vgStatus
    , vgSize
    , vgDevice
    , vgMountPoint
    , vgRegion
    , vgAvailabilityZone
    , vgVolumeType
    , vgIops

    -- * VolumeConfiguration
    , VolumeConfiguration
    , mkVolumeConfiguration
    , vdMountPoint
    , vdRaidLevel
    , vdNumberOfDisks
    , vdSize
    , vdVolumeType
    , vdIops

    -- * WeeklyAutoScalingSchedule
    , WeeklyAutoScalingSchedule
    , mkWeeklyAutoScalingSchedule
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
    data Er OpsWorks
        = OpsWorksClient HttpException
        | OpsWorksSerializer String
        | OpsWorksService String
        | ResourceNotFoundException
            { _rnfeMessage :: Maybe Text
            }
        | ValidationException
            { _veMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "opsworks"
        , _svcVersion  = "2013-02-18"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er OpsWorks)
deriving instance Generic (Er OpsWorks)

instance AWSError (Er OpsWorks) where
    awsError = const "OpsWorksError"

instance AWSServiceError (Er OpsWorks) where
    serviceError    = OpsWorksService
    clientError     = OpsWorksClient
    serializerError = OpsWorksSerializer

instance Exception (Er OpsWorks)

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

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS OpsWorks
-- deploys an application to those instances that are members of the
-- corresponding layer.
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

-- | The instance architecture. The default option is x86_64. Instance types do
-- not necessarily support both architectures. For a list of the architectures
-- that are supported by the different instance types, see Instance Families
-- and Types.
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

-- | The instance auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. To specify the schedule, call
-- SetTimeBasedAutoScaling. LoadBasedAutoScaling: A load-based auto scaling
-- instance, which is started and stopped based on load metrics. To use
-- load-based auto scaling, you must enable it for the instance layer and
-- configure the thresholds by calling SetLoadBasedAutoScaling.
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

-- | The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers. This parameter must be
-- set to one of the following: custom: A custom layer db-master: A MySQL
-- layer java-app: A Java App Server layer rails-app: A Rails App Server layer
-- lb: An HAProxy layer memcached: A Memcached layer monitoring-master: A
-- Ganglia layer nodejs-app: A Node.js App Server layer php-app: A PHP App
-- Server layer web: A Static Web Server layer.
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

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
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

-- | The repository type.
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
    { _apAppId :: Maybe Text
      -- ^ The app ID.
    , _apStackId :: Maybe Text
      -- ^ The app stack ID.
    , _apShortname :: Maybe Text
      -- ^ The app's short name.
    , _apName :: Maybe Text
      -- ^ The app name.
    , _apDescription :: Maybe Text
      -- ^ A description of the app.
    , _apDataSources :: [DataSource]
      -- ^ The app's data sources.
    , _apType :: Maybe AppType
      -- ^ The app type.
    , _apAppSource :: Maybe Source
      -- ^ A Source object that describes the app repository.
    , _apDomains :: [Text]
      -- ^ The app vhost settings with multiple domains separated by commas.
      -- For example: 'www.example.com, example.com'.
    , _apEnableSsl :: Maybe Bool
      -- ^ Whether to enable SSL for the app.
    , _apSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , _apAttributes :: Map AppAttributesKeys Text
      -- ^ The stack attributes.
    , _apCreatedAt :: Maybe Text
      -- ^ When the app was created.
    } deriving (Show, Generic)

-- | The app ID.
apAppId :: Lens' App (Maybe Text)
apAppId = lens _apAppId (\s a -> s { _apAppId = a })
{-# INLINE apAppId #-}

-- | The app stack ID.
apStackId :: Lens' App (Maybe Text)
apStackId = lens _apStackId (\s a -> s { _apStackId = a })
{-# INLINE apStackId #-}

-- | The app's short name.
apShortname :: Lens' App (Maybe Text)
apShortname = lens _apShortname (\s a -> s { _apShortname = a })
{-# INLINE apShortname #-}

-- | The app name.
apName :: Lens' App (Maybe Text)
apName = lens _apName (\s a -> s { _apName = a })
{-# INLINE apName #-}

-- | A description of the app.
apDescription :: Lens' App (Maybe Text)
apDescription = lens _apDescription (\s a -> s { _apDescription = a })
{-# INLINE apDescription #-}

-- | The app's data sources.
apDataSources :: Lens' App ([DataSource])
apDataSources = lens _apDataSources (\s a -> s { _apDataSources = a })
{-# INLINE apDataSources #-}

-- | The app type.
apType :: Lens' App (Maybe AppType)
apType = lens _apType (\s a -> s { _apType = a })
{-# INLINE apType #-}

-- | A Source object that describes the app repository.
apAppSource :: Lens' App (Maybe Source)
apAppSource = lens _apAppSource (\s a -> s { _apAppSource = a })
{-# INLINE apAppSource #-}

-- | The app vhost settings with multiple domains separated by commas. For
-- example: 'www.example.com, example.com'.
apDomains :: Lens' App ([Text])
apDomains = lens _apDomains (\s a -> s { _apDomains = a })
{-# INLINE apDomains #-}

-- | Whether to enable SSL for the app.
apEnableSsl :: Lens' App (Maybe Bool)
apEnableSsl = lens _apEnableSsl (\s a -> s { _apEnableSsl = a })
{-# INLINE apEnableSsl #-}

-- | An SslConfiguration object with the SSL configuration.
apSslConfiguration :: Lens' App (Maybe SslConfiguration)
apSslConfiguration = lens _apSslConfiguration (\s a -> s { _apSslConfiguration = a })
{-# INLINE apSslConfiguration #-}

-- | The stack attributes.
apAttributes :: Lens' App (Map AppAttributesKeys Text)
apAttributes = lens _apAttributes (\s a -> s { _apAttributes = a })
{-# INLINE apAttributes #-}

-- | When the app was created.
apCreatedAt :: Lens' App (Maybe Text)
apCreatedAt = lens _apCreatedAt (\s a -> s { _apCreatedAt = a })
{-# INLINE apCreatedAt #-}

instance FromJSON App

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the number
-- of instances.
data AutoScalingThresholds = AutoScalingThresholds
    { _asvInstanceCount :: Maybe Integer
      -- ^ The number of instances to add or remove when the load exceeds a
      -- threshold.
    , _asvThresholdsWaitTime :: Maybe Integer
      -- ^ The amount of time, in minutes, that the load must exceed a
      -- threshold before more instances are added or removed.
    , _asvIgnoreMetricsTime :: Maybe Integer
      -- ^ The amount of time (in minutes) after a scaling event occurs that
      -- AWS OpsWorks should ignore metrics and not raise any additional
      -- scaling events. For example, AWS OpsWorks adds new instances
      -- following an upscaling event but the instances won't start
      -- reducing the load until they have been booted and configured.
      -- There is no point in raising additional scaling events during
      -- that operation, which typically takes several minutes.
      -- IgnoreMetricsTime allows you to direct AWS OpsWorks to not raise
      -- any scaling events long enough to get the new instances online.
    , _asvCpuThreshold :: Maybe Double
      -- ^ The CPU utilization threshold, as a percent of the available CPU.
    , _asvMemoryThreshold :: Maybe Double
      -- ^ The memory utilization threshold, as a percent of the available
      -- memory.
    , _asvLoadThreshold :: Maybe Double
      -- ^ The load threshold. For more information about how load is
      -- computed, see Load (computing).
    } deriving (Show, Generic)

-- | The number of instances to add or remove when the load exceeds a threshold.
asvInstanceCount :: Lens' AutoScalingThresholds (Maybe Integer)
asvInstanceCount = lens _asvInstanceCount (\s a -> s { _asvInstanceCount = a })
{-# INLINE asvInstanceCount #-}

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
asvThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Integer)
asvThresholdsWaitTime = lens _asvThresholdsWaitTime (\s a -> s { _asvThresholdsWaitTime = a })
{-# INLINE asvThresholdsWaitTime #-}

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks should ignore metrics and not raise any additional scaling events.
-- For example, AWS OpsWorks adds new instances following an upscaling event
-- but the instances won't start reducing the load until they have been booted
-- and configured. There is no point in raising additional scaling events
-- during that operation, which typically takes several minutes.
-- IgnoreMetricsTime allows you to direct AWS OpsWorks to not raise any
-- scaling events long enough to get the new instances online.
asvIgnoreMetricsTime :: Lens' AutoScalingThresholds (Maybe Integer)
asvIgnoreMetricsTime = lens _asvIgnoreMetricsTime (\s a -> s { _asvIgnoreMetricsTime = a })
{-# INLINE asvIgnoreMetricsTime #-}

-- | The CPU utilization threshold, as a percent of the available CPU.
asvCpuThreshold :: Lens' AutoScalingThresholds (Maybe Double)
asvCpuThreshold = lens _asvCpuThreshold (\s a -> s { _asvCpuThreshold = a })
{-# INLINE asvCpuThreshold #-}

-- | The memory utilization threshold, as a percent of the available memory.
asvMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
asvMemoryThreshold = lens _asvMemoryThreshold (\s a -> s { _asvMemoryThreshold = a })
{-# INLINE asvMemoryThreshold #-}

-- | The load threshold. For more information about how load is computed, see
-- Load (computing).
asvLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
asvLoadThreshold = lens _asvLoadThreshold (\s a -> s { _asvLoadThreshold = a })
{-# INLINE asvLoadThreshold #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AutoScalingThresholds' data type to populate a request.
mkAutoScalingThresholds :: AutoScalingThresholds
mkAutoScalingThresholds = AutoScalingThresholds
    { _asvInstanceCount = Nothing
    , _asvThresholdsWaitTime = Nothing
    , _asvIgnoreMetricsTime = Nothing
    , _asvCpuThreshold = Nothing
    , _asvMemoryThreshold = Nothing
    , _asvLoadThreshold = Nothing
    }
{-# INLINE mkAutoScalingThresholds #-}

instance FromJSON AutoScalingThresholds

instance ToJSON AutoScalingThresholds

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
data ChefConfiguration = ChefConfiguration
    { _ccManageBerkshelf :: Maybe Bool
      -- ^ Whether to enable Berkshelf.
    , _ccBerkshelfVersion :: Maybe Text
      -- ^ The Berkshelf version.
    } deriving (Show, Generic)

-- | Whether to enable Berkshelf.
ccManageBerkshelf :: Lens' ChefConfiguration (Maybe Bool)
ccManageBerkshelf = lens _ccManageBerkshelf (\s a -> s { _ccManageBerkshelf = a })
{-# INLINE ccManageBerkshelf #-}

-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion = lens _ccBerkshelfVersion (\s a -> s { _ccBerkshelfVersion = a })
{-# INLINE ccBerkshelfVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChefConfiguration' data type to populate a request.
mkChefConfiguration :: ChefConfiguration
mkChefConfiguration = ChefConfiguration
    { _ccManageBerkshelf = Nothing
    , _ccBerkshelfVersion = Nothing
    }
{-# INLINE mkChefConfiguration #-}

instance FromJSON ChefConfiguration

instance ToJSON ChefConfiguration

-- | Describes a command.
data Command = Command
    { _ceCommandId :: Maybe Text
      -- ^ The command ID.
    , _ceInstanceId :: Maybe Text
      -- ^ The ID of the instance where the command was executed.
    , _ceDeploymentId :: Maybe Text
      -- ^ The command deployment ID.
    , _ceCreatedAt :: Maybe Text
      -- ^ Date and time when the command was run.
    , _ceAcknowledgedAt :: Maybe Text
      -- ^ Date and time when the command was acknowledged.
    , _ceCompletedAt :: Maybe Text
      -- ^ Date when the command completed.
    , _ceStatus :: Maybe Text
      -- ^ The command status: failed successful skipped pending.
    , _ceExitCode :: Maybe Integer
      -- ^ The command exit code.
    , _ceLogUrl :: Maybe Text
      -- ^ The URL of the command log.
    , _ceType :: Maybe Text
      -- ^ The command type: deploy rollback start stop restart undeploy
      -- update_dependencies install_dependencies update_custom_cookbooks
      -- execute_recipes.
    } deriving (Show, Generic)

-- | The command ID.
ceCommandId :: Lens' Command (Maybe Text)
ceCommandId = lens _ceCommandId (\s a -> s { _ceCommandId = a })
{-# INLINE ceCommandId #-}

-- | The ID of the instance where the command was executed.
ceInstanceId :: Lens' Command (Maybe Text)
ceInstanceId = lens _ceInstanceId (\s a -> s { _ceInstanceId = a })
{-# INLINE ceInstanceId #-}

-- | The command deployment ID.
ceDeploymentId :: Lens' Command (Maybe Text)
ceDeploymentId = lens _ceDeploymentId (\s a -> s { _ceDeploymentId = a })
{-# INLINE ceDeploymentId #-}

-- | Date and time when the command was run.
ceCreatedAt :: Lens' Command (Maybe Text)
ceCreatedAt = lens _ceCreatedAt (\s a -> s { _ceCreatedAt = a })
{-# INLINE ceCreatedAt #-}

-- | Date and time when the command was acknowledged.
ceAcknowledgedAt :: Lens' Command (Maybe Text)
ceAcknowledgedAt = lens _ceAcknowledgedAt (\s a -> s { _ceAcknowledgedAt = a })
{-# INLINE ceAcknowledgedAt #-}

-- | Date when the command completed.
ceCompletedAt :: Lens' Command (Maybe Text)
ceCompletedAt = lens _ceCompletedAt (\s a -> s { _ceCompletedAt = a })
{-# INLINE ceCompletedAt #-}

-- | The command status: failed successful skipped pending.
ceStatus :: Lens' Command (Maybe Text)
ceStatus = lens _ceStatus (\s a -> s { _ceStatus = a })
{-# INLINE ceStatus #-}

-- | The command exit code.
ceExitCode :: Lens' Command (Maybe Integer)
ceExitCode = lens _ceExitCode (\s a -> s { _ceExitCode = a })
{-# INLINE ceExitCode #-}

-- | The URL of the command log.
ceLogUrl :: Lens' Command (Maybe Text)
ceLogUrl = lens _ceLogUrl (\s a -> s { _ceLogUrl = a })
{-# INLINE ceLogUrl #-}

-- | The command type: deploy rollback start stop restart undeploy
-- update_dependencies install_dependencies update_custom_cookbooks
-- execute_recipes.
ceType :: Lens' Command (Maybe Text)
ceType = lens _ceType (\s a -> s { _ceType = a })
{-# INLINE ceType #-}

instance FromJSON Command

-- | Describes an app's data source.
data DataSource = DataSource
    { _dtType :: Maybe Text
      -- ^ The data source's type, AutoSelectOpsworksMysqlInstance,
      -- OpsworksMysqlInstance, or RdsDbInstance.
    , _dtArn :: Maybe Text
      -- ^ The data source's ARN.
    , _dtDatabaseName :: Maybe Text
      -- ^ The database name.
    } deriving (Show, Generic)

-- | The data source's type, AutoSelectOpsworksMysqlInstance,
-- OpsworksMysqlInstance, or RdsDbInstance.
dtType :: Lens' DataSource (Maybe Text)
dtType = lens _dtType (\s a -> s { _dtType = a })
{-# INLINE dtType #-}

-- | The data source's ARN.
dtArn :: Lens' DataSource (Maybe Text)
dtArn = lens _dtArn (\s a -> s { _dtArn = a })
{-# INLINE dtArn #-}

-- | The database name.
dtDatabaseName :: Lens' DataSource (Maybe Text)
dtDatabaseName = lens _dtDatabaseName (\s a -> s { _dtDatabaseName = a })
{-# INLINE dtDatabaseName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DataSource' data type to populate a request.
mkDataSource :: DataSource
mkDataSource = DataSource
    { _dtType = Nothing
    , _dtArn = Nothing
    , _dtDatabaseName = Nothing
    }
{-# INLINE mkDataSource #-}

instance FromJSON DataSource

instance ToJSON DataSource

-- | Describes a deployment of a stack or app.
data Deployment = Deployment
    { _ddtDeploymentId :: Maybe Text
      -- ^ The deployment ID.
    , _ddtStackId :: Maybe Text
      -- ^ The stack ID.
    , _ddtAppId :: Maybe Text
      -- ^ The app ID.
    , _ddtCreatedAt :: Maybe Text
      -- ^ Date when the deployment was created.
    , _ddtCompletedAt :: Maybe Text
      -- ^ Date when the deployment completed.
    , _ddtDuration :: Maybe Integer
      -- ^ The deployment duration.
    , _ddtIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , _ddtComment :: Maybe Text
      -- ^ A user-defined comment.
    , _ddtCommand :: Maybe DeploymentCommand
      -- ^ Used to specify a deployment operation.
    , _ddtStatus :: Maybe Text
      -- ^ The deployment status: running successful failed.
    , _ddtCustomJson :: Maybe Text
      -- ^ A string that contains user-defined custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values for stack. The string should be in the following format
      -- and must escape characters such as '"'.: "{\"key1\": \"value1\",
      -- \"key2\": \"value2\",...}" For more information on custom JSON,
      -- see Use Custom JSON to Modify the Stack Configuration JSON.
    , _ddtInstanceIds :: [Text]
      -- ^ The IDs of the target instances.
    } deriving (Show, Generic)

-- | The deployment ID.
ddtDeploymentId :: Lens' Deployment (Maybe Text)
ddtDeploymentId = lens _ddtDeploymentId (\s a -> s { _ddtDeploymentId = a })
{-# INLINE ddtDeploymentId #-}

-- | The stack ID.
ddtStackId :: Lens' Deployment (Maybe Text)
ddtStackId = lens _ddtStackId (\s a -> s { _ddtStackId = a })
{-# INLINE ddtStackId #-}

-- | The app ID.
ddtAppId :: Lens' Deployment (Maybe Text)
ddtAppId = lens _ddtAppId (\s a -> s { _ddtAppId = a })
{-# INLINE ddtAppId #-}

-- | Date when the deployment was created.
ddtCreatedAt :: Lens' Deployment (Maybe Text)
ddtCreatedAt = lens _ddtCreatedAt (\s a -> s { _ddtCreatedAt = a })
{-# INLINE ddtCreatedAt #-}

-- | Date when the deployment completed.
ddtCompletedAt :: Lens' Deployment (Maybe Text)
ddtCompletedAt = lens _ddtCompletedAt (\s a -> s { _ddtCompletedAt = a })
{-# INLINE ddtCompletedAt #-}

-- | The deployment duration.
ddtDuration :: Lens' Deployment (Maybe Integer)
ddtDuration = lens _ddtDuration (\s a -> s { _ddtDuration = a })
{-# INLINE ddtDuration #-}

-- | The user's IAM ARN.
ddtIamUserArn :: Lens' Deployment (Maybe Text)
ddtIamUserArn = lens _ddtIamUserArn (\s a -> s { _ddtIamUserArn = a })
{-# INLINE ddtIamUserArn #-}

-- | A user-defined comment.
ddtComment :: Lens' Deployment (Maybe Text)
ddtComment = lens _ddtComment (\s a -> s { _ddtComment = a })
{-# INLINE ddtComment #-}

-- | Used to specify a deployment operation.
ddtCommand :: Lens' Deployment (Maybe DeploymentCommand)
ddtCommand = lens _ddtCommand (\s a -> s { _ddtCommand = a })
{-# INLINE ddtCommand #-}

-- | The deployment status: running successful failed.
ddtStatus :: Lens' Deployment (Maybe Text)
ddtStatus = lens _ddtStatus (\s a -> s { _ddtStatus = a })
{-# INLINE ddtStatus #-}

-- | A string that contains user-defined custom JSON. It is used to override the
-- corresponding default stack configuration JSON values for stack. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
ddtCustomJson :: Lens' Deployment (Maybe Text)
ddtCustomJson = lens _ddtCustomJson (\s a -> s { _ddtCustomJson = a })
{-# INLINE ddtCustomJson #-}

-- | The IDs of the target instances.
ddtInstanceIds :: Lens' Deployment ([Text])
ddtInstanceIds = lens _ddtInstanceIds (\s a -> s { _ddtInstanceIds = a })
{-# INLINE ddtInstanceIds #-}

instance FromJSON Deployment

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
data DeploymentCommand = DeploymentCommand
    { _dcName :: DeploymentCommandName
      -- ^ Specifies the operation. You can specify only one command. For
      -- stacks, the following commands are available: execute_recipes:
      -- Execute one or more recipes. To specify the recipes, set an Args
      -- parameter named recipes to the list of recipes to be executed.
      -- For example, to execute phpapp::appsetup, set Args to
      -- {"recipes":["phpapp::appsetup"]}. install_dependencies: Install
      -- the stack's dependencies. update_custom_cookbooks: Update the
      -- stack's custom cookbooks. update_dependencies: Update the stack's
      -- dependencies. For apps, the following commands are available:
      -- deploy: Deploy an app. Rails apps have an optional Args parameter
      -- named migrate. Set Args to {"migrate":["true"]} to migrate the
      -- database. The default setting is {"migrate":["false"]}. rollback
      -- Roll the app back to the previous version. When you update an
      -- app, AWS OpsWorks stores the previous version, up to a maximum of
      -- five versions. You can use this command to roll an app back as
      -- many as four versions. start: Start the app's web or application
      -- server. stop: Stop the app's web or application server. restart:
      -- Restart the app's web or application server. undeploy: Undeploy
      -- the app.
    , _dcArgs :: Map Text [Text]
      -- ^ The arguments of those commands that take arguments. It should be
      -- set to a JSON object with the following format:
      -- {"arg_name":["value1", "value2", ...]}.
    } deriving (Show, Generic)

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
dcName :: Lens' DeploymentCommand (DeploymentCommandName)
dcName = lens _dcName (\s a -> s { _dcName = a })
{-# INLINE dcName #-}

-- | The arguments of those commands that take arguments. It should be set to a
-- JSON object with the following format: {"arg_name":["value1", "value2",
-- ...]}.
dcArgs :: Lens' DeploymentCommand (Map Text [Text])
dcArgs = lens _dcArgs (\s a -> s { _dcArgs = a })
{-# INLINE dcArgs #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeploymentCommand' data type to populate a request.
mkDeploymentCommand :: DeploymentCommandName -- ^ 'dcName'
                    -> DeploymentCommand
mkDeploymentCommand p1 = DeploymentCommand
    { _dcName = p1
    , _dcArgs = mempty
    }
{-# INLINE mkDeploymentCommand #-}

instance FromJSON DeploymentCommand

instance ToJSON DeploymentCommand

-- | Describes an Elastic IP address.
data ElasticIp = ElasticIp
    { _ejIp :: Maybe Text
      -- ^ The IP address.
    , _ejName :: Maybe Text
      -- ^ The name.
    , _ejDomain :: Maybe Text
      -- ^ The domain.
    , _ejRegion :: Maybe Text
      -- ^ The AWS region. For more information, see Regions and Endpoints.
    , _ejInstanceId :: Maybe Text
      -- ^ The ID of the instance that the address is attached to.
    } deriving (Show, Generic)

-- | The IP address.
ejIp :: Lens' ElasticIp (Maybe Text)
ejIp = lens _ejIp (\s a -> s { _ejIp = a })
{-# INLINE ejIp #-}

-- | The name.
ejName :: Lens' ElasticIp (Maybe Text)
ejName = lens _ejName (\s a -> s { _ejName = a })
{-# INLINE ejName #-}

-- | The domain.
ejDomain :: Lens' ElasticIp (Maybe Text)
ejDomain = lens _ejDomain (\s a -> s { _ejDomain = a })
{-# INLINE ejDomain #-}

-- | The AWS region. For more information, see Regions and Endpoints.
ejRegion :: Lens' ElasticIp (Maybe Text)
ejRegion = lens _ejRegion (\s a -> s { _ejRegion = a })
{-# INLINE ejRegion #-}

-- | The ID of the instance that the address is attached to.
ejInstanceId :: Lens' ElasticIp (Maybe Text)
ejInstanceId = lens _ejInstanceId (\s a -> s { _ejInstanceId = a })
{-# INLINE ejInstanceId #-}

instance FromJSON ElasticIp

-- | Describes an Elastic Load Balancing instance.
data ElasticLoadBalancer = ElasticLoadBalancer
    { _elcElasticLoadBalancerName :: Maybe Text
      -- ^ The Elastic Load Balancing instance's name.
    , _elcRegion :: Maybe Text
      -- ^ The instance's AWS region.
    , _elcDnsName :: Maybe Text
      -- ^ The instance's public DNS name.
    , _elcStackId :: Maybe Text
      -- ^ The ID of the stack that the instance is associated with.
    , _elcLayerId :: Maybe Text
      -- ^ The ID of the layer that the instance is attached to.
    , _elcVpcId :: Maybe Text
      -- ^ The VPC ID.
    , _elcAvailabilityZones :: [Text]
      -- ^ A list of Availability Zones.
    , _elcSubnetIds :: [Text]
      -- ^ A list of subnet IDs, if the stack is running in a VPC.
    , _elcEc2InstanceIds :: [Text]
      -- ^ A list of the EC2 instances that the Elastic Load Balancing
      -- instance is managing traffic for.
    } deriving (Show, Generic)

-- | The Elastic Load Balancing instance's name.
elcElasticLoadBalancerName :: Lens' ElasticLoadBalancer (Maybe Text)
elcElasticLoadBalancerName = lens _elcElasticLoadBalancerName (\s a -> s { _elcElasticLoadBalancerName = a })
{-# INLINE elcElasticLoadBalancerName #-}

-- | The instance's AWS region.
elcRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elcRegion = lens _elcRegion (\s a -> s { _elcRegion = a })
{-# INLINE elcRegion #-}

-- | The instance's public DNS name.
elcDnsName :: Lens' ElasticLoadBalancer (Maybe Text)
elcDnsName = lens _elcDnsName (\s a -> s { _elcDnsName = a })
{-# INLINE elcDnsName #-}

-- | The ID of the stack that the instance is associated with.
elcStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elcStackId = lens _elcStackId (\s a -> s { _elcStackId = a })
{-# INLINE elcStackId #-}

-- | The ID of the layer that the instance is attached to.
elcLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elcLayerId = lens _elcLayerId (\s a -> s { _elcLayerId = a })
{-# INLINE elcLayerId #-}

-- | The VPC ID.
elcVpcId :: Lens' ElasticLoadBalancer (Maybe Text)
elcVpcId = lens _elcVpcId (\s a -> s { _elcVpcId = a })
{-# INLINE elcVpcId #-}

-- | A list of Availability Zones.
elcAvailabilityZones :: Lens' ElasticLoadBalancer ([Text])
elcAvailabilityZones = lens _elcAvailabilityZones (\s a -> s { _elcAvailabilityZones = a })
{-# INLINE elcAvailabilityZones #-}

-- | A list of subnet IDs, if the stack is running in a VPC.
elcSubnetIds :: Lens' ElasticLoadBalancer ([Text])
elcSubnetIds = lens _elcSubnetIds (\s a -> s { _elcSubnetIds = a })
{-# INLINE elcSubnetIds #-}

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elcEc2InstanceIds :: Lens' ElasticLoadBalancer ([Text])
elcEc2InstanceIds = lens _elcEc2InstanceIds (\s a -> s { _elcEc2InstanceIds = a })
{-# INLINE elcEc2InstanceIds #-}

instance FromJSON ElasticLoadBalancer

-- | Describes an instance.
data Instance = Instance
    { _ieInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _ieEc2InstanceId :: Maybe Text
      -- ^ The ID of the associated Amazon EC2 instance.
    , _ieVirtualizationType :: Maybe Text
      -- ^ The instance's virtualization type, paravirtual or hvm.
    , _ieHostname :: Maybe Text
      -- ^ The instance host name.
    , _ieStackId :: Maybe Text
      -- ^ The stack ID.
    , _ieLayerIds :: [Text]
      -- ^ An array containing the instance layer IDs.
    , _ieSecurityGroupIds :: [Text]
      -- ^ An array containing the instance security group IDs.
    , _ieInstanceType :: Maybe Text
      -- ^ The instance type. AWS OpsWorks supports all instance types
      -- except Cluster Compute, Cluster GPU, and High Memory Cluster. For
      -- more information, see Instance Families and Types. The parameter
      -- values that specify the various types are in the API Name column
      -- of the Available Instance Types table.
    , _ieInstanceProfileArn :: Maybe Text
      -- ^ The ARN of the instance's IAM profile. For more information about
      -- IAM ARNs, see Using Identifiers.
    , _ieStatus :: Maybe Text
      -- ^ The instance status: requested booting running_setup online
      -- setup_failed start_failed terminating terminated stopped
      -- connection_lost.
    , _ieOs :: Maybe Text
      -- ^ The instance operating system.
    , _ieAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should
      -- be based on one of the standard AWS OpsWorks APIs: Amazon Linux
      -- or Ubuntu 12.04 LTS. For more information, see Instances.
    , _ieAvailabilityZone :: Maybe Text
      -- ^ The instance Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _ieSubnetId :: Maybe Text
      -- ^ The instance's subnet ID, if the stack is running in a VPC.
    , _iePublicDns :: Maybe Text
      -- ^ The instance public DNS name.
    , _iePrivateDns :: Maybe Text
      -- ^ The instance private DNS name.
    , _iePublicIp :: Maybe Text
      -- ^ The instance public IP address.
    , _iePrivateIp :: Maybe Text
      -- ^ The instance private IP address.
    , _ieElasticIp :: Maybe Text
      -- ^ The instance Elastic IP address .
    , _ieAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance's auto scaling type, which has three possible
      -- values: AlwaysRunning: A 24/7 instance, which is not affected by
      -- auto scaling. TimeBasedAutoScaling: A time-based auto scaling
      -- instance, which is started and stopped based on a specified
      -- schedule. LoadBasedAutoScaling: A load-based auto scaling
      -- instance, which is started and stopped based on load metrics.
    , _ieSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    , _ieSshHostRsaKeyFingerprint :: Maybe Text
      -- ^ The SSH key's RSA fingerprint.
    , _ieSshHostDsaKeyFingerprint :: Maybe Text
      -- ^ The SSH key's DSA fingerprint.
    , _ieCreatedAt :: Maybe Text
      -- ^ The time that the instance was created.
    , _ieLastServiceErrorId :: Maybe Text
      -- ^ The ID of the last service error. For more information, call
      -- DescribeServiceErrors.
    , _ieArchitecture :: Maybe Architecture
      -- ^ The instance architecture, "i386" or "x86_64".
    , _ieRootDeviceType :: Maybe RootDeviceType
      -- ^ The instance root device type. For more information, see Storage
      -- for the Root Device.
    , _ieRootDeviceVolumeId :: Maybe Text
      -- ^ The root device volume ID.
    , _ieInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. If this value is set
      -- to false, you must then update your instances manually by using
      -- CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the
      -- instances. We strongly recommend using the default value of true,
      -- to ensure that your instances have the latest security updates.
    , _ieEbsOptimized :: Maybe Bool
      -- ^ Whether this is an Amazon EBS-optimized instance.
    } deriving (Show, Generic)

-- | The instance ID.
ieInstanceId :: Lens' Instance (Maybe Text)
ieInstanceId = lens _ieInstanceId (\s a -> s { _ieInstanceId = a })
{-# INLINE ieInstanceId #-}

-- | The ID of the associated Amazon EC2 instance.
ieEc2InstanceId :: Lens' Instance (Maybe Text)
ieEc2InstanceId = lens _ieEc2InstanceId (\s a -> s { _ieEc2InstanceId = a })
{-# INLINE ieEc2InstanceId #-}

-- | The instance's virtualization type, paravirtual or hvm.
ieVirtualizationType :: Lens' Instance (Maybe Text)
ieVirtualizationType = lens _ieVirtualizationType (\s a -> s { _ieVirtualizationType = a })
{-# INLINE ieVirtualizationType #-}

-- | The instance host name.
ieHostname :: Lens' Instance (Maybe Text)
ieHostname = lens _ieHostname (\s a -> s { _ieHostname = a })
{-# INLINE ieHostname #-}

-- | The stack ID.
ieStackId :: Lens' Instance (Maybe Text)
ieStackId = lens _ieStackId (\s a -> s { _ieStackId = a })
{-# INLINE ieStackId #-}

-- | An array containing the instance layer IDs.
ieLayerIds :: Lens' Instance ([Text])
ieLayerIds = lens _ieLayerIds (\s a -> s { _ieLayerIds = a })
{-# INLINE ieLayerIds #-}

-- | An array containing the instance security group IDs.
ieSecurityGroupIds :: Lens' Instance ([Text])
ieSecurityGroupIds = lens _ieSecurityGroupIds (\s a -> s { _ieSecurityGroupIds = a })
{-# INLINE ieSecurityGroupIds #-}

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that specify the various
-- types are in the API Name column of the Available Instance Types table.
ieInstanceType :: Lens' Instance (Maybe Text)
ieInstanceType = lens _ieInstanceType (\s a -> s { _ieInstanceType = a })
{-# INLINE ieInstanceType #-}

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs,
-- see Using Identifiers.
ieInstanceProfileArn :: Lens' Instance (Maybe Text)
ieInstanceProfileArn = lens _ieInstanceProfileArn (\s a -> s { _ieInstanceProfileArn = a })
{-# INLINE ieInstanceProfileArn #-}

-- | The instance status: requested booting running_setup online setup_failed
-- start_failed terminating terminated stopped connection_lost.
ieStatus :: Lens' Instance (Maybe Text)
ieStatus = lens _ieStatus (\s a -> s { _ieStatus = a })
{-# INLINE ieStatus #-}

-- | The instance operating system.
ieOs :: Lens' Instance (Maybe Text)
ieOs = lens _ieOs (\s a -> s { _ieOs = a })
{-# INLINE ieOs #-}

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
-- For more information, see Instances.
ieAmiId :: Lens' Instance (Maybe Text)
ieAmiId = lens _ieAmiId (\s a -> s { _ieAmiId = a })
{-# INLINE ieAmiId #-}

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
ieAvailabilityZone :: Lens' Instance (Maybe Text)
ieAvailabilityZone = lens _ieAvailabilityZone (\s a -> s { _ieAvailabilityZone = a })
{-# INLINE ieAvailabilityZone #-}

-- | The instance's subnet ID, if the stack is running in a VPC.
ieSubnetId :: Lens' Instance (Maybe Text)
ieSubnetId = lens _ieSubnetId (\s a -> s { _ieSubnetId = a })
{-# INLINE ieSubnetId #-}

-- | The instance public DNS name.
iePublicDns :: Lens' Instance (Maybe Text)
iePublicDns = lens _iePublicDns (\s a -> s { _iePublicDns = a })
{-# INLINE iePublicDns #-}

-- | The instance private DNS name.
iePrivateDns :: Lens' Instance (Maybe Text)
iePrivateDns = lens _iePrivateDns (\s a -> s { _iePrivateDns = a })
{-# INLINE iePrivateDns #-}

-- | The instance public IP address.
iePublicIp :: Lens' Instance (Maybe Text)
iePublicIp = lens _iePublicIp (\s a -> s { _iePublicIp = a })
{-# INLINE iePublicIp #-}

-- | The instance private IP address.
iePrivateIp :: Lens' Instance (Maybe Text)
iePrivateIp = lens _iePrivateIp (\s a -> s { _iePrivateIp = a })
{-# INLINE iePrivateIp #-}

-- | The instance Elastic IP address .
ieElasticIp :: Lens' Instance (Maybe Text)
ieElasticIp = lens _ieElasticIp (\s a -> s { _ieElasticIp = a })
{-# INLINE ieElasticIp #-}

-- | The instance's auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. LoadBasedAutoScaling: A
-- load-based auto scaling instance, which is started and stopped based on
-- load metrics.
ieAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
ieAutoScalingType = lens _ieAutoScalingType (\s a -> s { _ieAutoScalingType = a })
{-# INLINE ieAutoScalingType #-}

-- | The instance SSH key name.
ieSshKeyName :: Lens' Instance (Maybe Text)
ieSshKeyName = lens _ieSshKeyName (\s a -> s { _ieSshKeyName = a })
{-# INLINE ieSshKeyName #-}

-- | The SSH key's RSA fingerprint.
ieSshHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
ieSshHostRsaKeyFingerprint = lens _ieSshHostRsaKeyFingerprint (\s a -> s { _ieSshHostRsaKeyFingerprint = a })
{-# INLINE ieSshHostRsaKeyFingerprint #-}

-- | The SSH key's DSA fingerprint.
ieSshHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
ieSshHostDsaKeyFingerprint = lens _ieSshHostDsaKeyFingerprint (\s a -> s { _ieSshHostDsaKeyFingerprint = a })
{-# INLINE ieSshHostDsaKeyFingerprint #-}

-- | The time that the instance was created.
ieCreatedAt :: Lens' Instance (Maybe Text)
ieCreatedAt = lens _ieCreatedAt (\s a -> s { _ieCreatedAt = a })
{-# INLINE ieCreatedAt #-}

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
ieLastServiceErrorId :: Lens' Instance (Maybe Text)
ieLastServiceErrorId = lens _ieLastServiceErrorId (\s a -> s { _ieLastServiceErrorId = a })
{-# INLINE ieLastServiceErrorId #-}

-- | The instance architecture, "i386" or "x86_64".
ieArchitecture :: Lens' Instance (Maybe Architecture)
ieArchitecture = lens _ieArchitecture (\s a -> s { _ieArchitecture = a })
{-# INLINE ieArchitecture #-}

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
ieRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
ieRootDeviceType = lens _ieRootDeviceType (\s a -> s { _ieRootDeviceType = a })
{-# INLINE ieRootDeviceType #-}

-- | The root device volume ID.
ieRootDeviceVolumeId :: Lens' Instance (Maybe Text)
ieRootDeviceVolumeId = lens _ieRootDeviceVolumeId (\s a -> s { _ieRootDeviceVolumeId = a })
{-# INLINE ieRootDeviceVolumeId #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
ieInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
ieInstallUpdatesOnBoot = lens _ieInstallUpdatesOnBoot (\s a -> s { _ieInstallUpdatesOnBoot = a })
{-# INLINE ieInstallUpdatesOnBoot #-}

-- | Whether this is an Amazon EBS-optimized instance.
ieEbsOptimized :: Lens' Instance (Maybe Bool)
ieEbsOptimized = lens _ieEbsOptimized (\s a -> s { _ieEbsOptimized = a })
{-# INLINE ieEbsOptimized #-}

instance FromJSON Instance

-- | An InstancesCount object with the number of instances in each status.
data InstancesCount = InstancesCount
    { _icBooting :: Maybe Integer
      -- ^ The number of instances with booting status.
    , _icConnectionLost :: Maybe Integer
      -- ^ The number of instances with connection_lost status.
    , _icOnline :: Maybe Integer
      -- ^ The number of instances with online status.
    , _icPending :: Maybe Integer
      -- ^ The number of instances with pending status.
    , _icRebooting :: Maybe Integer
      -- ^ The number of instances with rebooting status.
    , _icRequested :: Maybe Integer
      -- ^ The number of instances with requested status.
    , _icRunningSetup :: Maybe Integer
      -- ^ The number of instances with running_setup status.
    , _icSetupFailed :: Maybe Integer
      -- ^ The number of instances with setup_failed status.
    , _icShuttingDown :: Maybe Integer
      -- ^ The number of instances with shutting_down status.
    , _icStartFailed :: Maybe Integer
      -- ^ The number of instances with start_failed status.
    , _icStopped :: Maybe Integer
      -- ^ The number of instances with stopped status.
    , _icStopping :: Maybe Integer
      -- ^ The number of instances with stopping status.
    , _icTerminated :: Maybe Integer
      -- ^ The number of instances with terminated status.
    , _icTerminating :: Maybe Integer
      -- ^ The number of instances with terminating status.
    } deriving (Show, Generic)

-- | The number of instances with booting status.
icBooting :: Lens' InstancesCount (Maybe Integer)
icBooting = lens _icBooting (\s a -> s { _icBooting = a })
{-# INLINE icBooting #-}

-- | The number of instances with connection_lost status.
icConnectionLost :: Lens' InstancesCount (Maybe Integer)
icConnectionLost = lens _icConnectionLost (\s a -> s { _icConnectionLost = a })
{-# INLINE icConnectionLost #-}

-- | The number of instances with online status.
icOnline :: Lens' InstancesCount (Maybe Integer)
icOnline = lens _icOnline (\s a -> s { _icOnline = a })
{-# INLINE icOnline #-}

-- | The number of instances with pending status.
icPending :: Lens' InstancesCount (Maybe Integer)
icPending = lens _icPending (\s a -> s { _icPending = a })
{-# INLINE icPending #-}

-- | The number of instances with rebooting status.
icRebooting :: Lens' InstancesCount (Maybe Integer)
icRebooting = lens _icRebooting (\s a -> s { _icRebooting = a })
{-# INLINE icRebooting #-}

-- | The number of instances with requested status.
icRequested :: Lens' InstancesCount (Maybe Integer)
icRequested = lens _icRequested (\s a -> s { _icRequested = a })
{-# INLINE icRequested #-}

-- | The number of instances with running_setup status.
icRunningSetup :: Lens' InstancesCount (Maybe Integer)
icRunningSetup = lens _icRunningSetup (\s a -> s { _icRunningSetup = a })
{-# INLINE icRunningSetup #-}

-- | The number of instances with setup_failed status.
icSetupFailed :: Lens' InstancesCount (Maybe Integer)
icSetupFailed = lens _icSetupFailed (\s a -> s { _icSetupFailed = a })
{-# INLINE icSetupFailed #-}

-- | The number of instances with shutting_down status.
icShuttingDown :: Lens' InstancesCount (Maybe Integer)
icShuttingDown = lens _icShuttingDown (\s a -> s { _icShuttingDown = a })
{-# INLINE icShuttingDown #-}

-- | The number of instances with start_failed status.
icStartFailed :: Lens' InstancesCount (Maybe Integer)
icStartFailed = lens _icStartFailed (\s a -> s { _icStartFailed = a })
{-# INLINE icStartFailed #-}

-- | The number of instances with stopped status.
icStopped :: Lens' InstancesCount (Maybe Integer)
icStopped = lens _icStopped (\s a -> s { _icStopped = a })
{-# INLINE icStopped #-}

-- | The number of instances with stopping status.
icStopping :: Lens' InstancesCount (Maybe Integer)
icStopping = lens _icStopping (\s a -> s { _icStopping = a })
{-# INLINE icStopping #-}

-- | The number of instances with terminated status.
icTerminated :: Lens' InstancesCount (Maybe Integer)
icTerminated = lens _icTerminated (\s a -> s { _icTerminated = a })
{-# INLINE icTerminated #-}

-- | The number of instances with terminating status.
icTerminating :: Lens' InstancesCount (Maybe Integer)
icTerminating = lens _icTerminating (\s a -> s { _icTerminating = a })
{-# INLINE icTerminating #-}

instance FromJSON InstancesCount

-- | Describes a layer.
data Layer = Layer
    { _lrStackId :: Maybe Text
      -- ^ The layer stack ID.
    , _lrLayerId :: Maybe Text
      -- ^ The layer ID.
    , _lrType :: Maybe LayerType
      -- ^ The layer type, which must be one of the following: Custom
      -- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster
      -- NodeJsAppServer PhpAppServer RailsAppServer WebServer.
    , _lrName :: Maybe Text
      -- ^ The layer name.
    , _lrShortname :: Maybe Text
      -- ^ The layer short name.
    , _lrAttributes :: Map LayerAttributesKeys Text
      -- ^ The layer attributes.
    , _lrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of the default IAM profile to be used for the layer's EC2
      -- instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , _lrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer's custom security group IDs.
    , _lrDefaultSecurityGroupNames :: [Text]
      -- ^ An array containing the layer's security group names.
    , _lrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer's packages.
    , _lrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon
      -- EBS volumes.
    , _lrEnableAutoHealing :: Maybe Bool
      -- ^ Whether auto healing is disabled for the layer.
    , _lrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    , _lrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _lrDefaultRecipes :: Maybe Recipes
      -- ^ AWS OpsWorks supports five lifecycle events, setup,
      -- configuration, deploy, undeploy, and shutdown. For each layer,
      -- AWS OpsWorks runs a set of standard recipes for each event. In
      -- addition, you can provide custom recipes for any or all layers
      -- and events. AWS OpsWorks runs custom event recipes after the
      -- standard recipes. LayerCustomRecipes specifies the custom recipes
      -- for a particular layer to be run in response to each of the five
      -- events. To specify a recipe, use the cookbook's directory name in
      -- the repository followed by two colons and the recipe name, which
      -- is the recipe's file name without the .rb extension. For example:
      -- phpapp2::dbsetup specifies the dbsetup.rb recipe in the
      -- repository's phpapp2 folder.
    , _lrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer's custom
      -- recipes.
    , _lrCreatedAt :: Maybe Text
      -- ^ Date when the layer was created.
    , _lrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. If this value is set
      -- to false, you must then update your instances manually by using
      -- CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the
      -- instances. We strongly recommend using the default value of true,
      -- to ensure that your instances have the latest security updates.
    , _lrUseEbsOptimizedInstances :: Maybe Bool
      -- ^ Whether the layer uses Amazon EBS-optimized instances.
    } deriving (Show, Generic)

-- | The layer stack ID.
lrStackId :: Lens' Layer (Maybe Text)
lrStackId = lens _lrStackId (\s a -> s { _lrStackId = a })
{-# INLINE lrStackId #-}

-- | The layer ID.
lrLayerId :: Lens' Layer (Maybe Text)
lrLayerId = lens _lrLayerId (\s a -> s { _lrLayerId = a })
{-# INLINE lrLayerId #-}

-- | The layer type, which must be one of the following: Custom
-- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster NodeJsAppServer
-- PhpAppServer RailsAppServer WebServer.
lrType :: Lens' Layer (Maybe LayerType)
lrType = lens _lrType (\s a -> s { _lrType = a })
{-# INLINE lrType #-}

-- | The layer name.
lrName :: Lens' Layer (Maybe Text)
lrName = lens _lrName (\s a -> s { _lrName = a })
{-# INLINE lrName #-}

-- | The layer short name.
lrShortname :: Lens' Layer (Maybe Text)
lrShortname = lens _lrShortname (\s a -> s { _lrShortname = a })
{-# INLINE lrShortname #-}

-- | The layer attributes.
lrAttributes :: Lens' Layer (Map LayerAttributesKeys Text)
lrAttributes = lens _lrAttributes (\s a -> s { _lrAttributes = a })
{-# INLINE lrAttributes #-}

-- | The ARN of the default IAM profile to be used for the layer's EC2
-- instances. For more information about IAM ARNs, see Using Identifiers.
lrCustomInstanceProfileArn :: Lens' Layer (Maybe Text)
lrCustomInstanceProfileArn = lens _lrCustomInstanceProfileArn (\s a -> s { _lrCustomInstanceProfileArn = a })
{-# INLINE lrCustomInstanceProfileArn #-}

-- | An array containing the layer's custom security group IDs.
lrCustomSecurityGroupIds :: Lens' Layer ([Text])
lrCustomSecurityGroupIds = lens _lrCustomSecurityGroupIds (\s a -> s { _lrCustomSecurityGroupIds = a })
{-# INLINE lrCustomSecurityGroupIds #-}

-- | An array containing the layer's security group names.
lrDefaultSecurityGroupNames :: Lens' Layer ([Text])
lrDefaultSecurityGroupNames = lens _lrDefaultSecurityGroupNames (\s a -> s { _lrDefaultSecurityGroupNames = a })
{-# INLINE lrDefaultSecurityGroupNames #-}

-- | An array of Package objects that describe the layer's packages.
lrPackages :: Lens' Layer ([Text])
lrPackages = lens _lrPackages (\s a -> s { _lrPackages = a })
{-# INLINE lrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
lrVolumeConfigurations :: Lens' Layer ([VolumeConfiguration])
lrVolumeConfigurations = lens _lrVolumeConfigurations (\s a -> s { _lrVolumeConfigurations = a })
{-# INLINE lrVolumeConfigurations #-}

-- | Whether auto healing is disabled for the layer.
lrEnableAutoHealing :: Lens' Layer (Maybe Bool)
lrEnableAutoHealing = lens _lrEnableAutoHealing (\s a -> s { _lrEnableAutoHealing = a })
{-# INLINE lrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
lrAutoAssignElasticIps :: Lens' Layer (Maybe Bool)
lrAutoAssignElasticIps = lens _lrAutoAssignElasticIps (\s a -> s { _lrAutoAssignElasticIps = a })
{-# INLINE lrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
lrAutoAssignPublicIps :: Lens' Layer (Maybe Bool)
lrAutoAssignPublicIps = lens _lrAutoAssignPublicIps (\s a -> s { _lrAutoAssignPublicIps = a })
{-# INLINE lrAutoAssignPublicIps #-}

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
lrDefaultRecipes :: Lens' Layer (Maybe Recipes)
lrDefaultRecipes = lens _lrDefaultRecipes (\s a -> s { _lrDefaultRecipes = a })
{-# INLINE lrDefaultRecipes #-}

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
lrCustomRecipes :: Lens' Layer (Maybe Recipes)
lrCustomRecipes = lens _lrCustomRecipes (\s a -> s { _lrCustomRecipes = a })
{-# INLINE lrCustomRecipes #-}

-- | Date when the layer was created.
lrCreatedAt :: Lens' Layer (Maybe Text)
lrCreatedAt = lens _lrCreatedAt (\s a -> s { _lrCreatedAt = a })
{-# INLINE lrCreatedAt #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
lrInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
lrInstallUpdatesOnBoot = lens _lrInstallUpdatesOnBoot (\s a -> s { _lrInstallUpdatesOnBoot = a })
{-# INLINE lrInstallUpdatesOnBoot #-}

-- | Whether the layer uses Amazon EBS-optimized instances.
lrUseEbsOptimizedInstances :: Lens' Layer (Maybe Bool)
lrUseEbsOptimizedInstances = lens _lrUseEbsOptimizedInstances (\s a -> s { _lrUseEbsOptimizedInstances = a })
{-# INLINE lrUseEbsOptimizedInstances #-}

instance FromJSON Layer

-- | Describes a layer's load-based auto scaling configuration.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { _lbasdLayerId :: Maybe Text
      -- ^ The layer ID.
    , _lbasdEnable :: Maybe Bool
      -- ^ Whether load-based auto scaling is enabled for the layer.
    , _lbasdUpScaling :: Maybe AutoScalingThresholds
      -- ^ A LoadBasedAutoscalingInstruction object that describes the
      -- upscaling configuration, which defines how and when AWS OpsWorks
      -- increases the number of instances.
    , _lbasdDownScaling :: Maybe AutoScalingThresholds
      -- ^ A LoadBasedAutoscalingInstruction object that describes the
      -- downscaling configuration, which defines how and when AWS
      -- OpsWorks reduces the number of instances.
    } deriving (Show, Generic)

-- | The layer ID.
lbasdLayerId :: Lens' LoadBasedAutoScalingConfiguration (Maybe Text)
lbasdLayerId = lens _lbasdLayerId (\s a -> s { _lbasdLayerId = a })
{-# INLINE lbasdLayerId #-}

-- | Whether load-based auto scaling is enabled for the layer.
lbasdEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbasdEnable = lens _lbasdEnable (\s a -> s { _lbasdEnable = a })
{-# INLINE lbasdEnable #-}

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the number
-- of instances.
lbasdUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbasdUpScaling = lens _lbasdUpScaling (\s a -> s { _lbasdUpScaling = a })
{-# INLINE lbasdUpScaling #-}

-- | A LoadBasedAutoscalingInstruction object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the number
-- of instances.
lbasdDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbasdDownScaling = lens _lbasdDownScaling (\s a -> s { _lbasdDownScaling = a })
{-# INLINE lbasdDownScaling #-}

instance FromJSON LoadBasedAutoScalingConfiguration

-- | Describes stack or user permissions.
data Permission = Permission
    { _pnStackId :: Maybe Text
      -- ^ A stack ID.
    , _pnIamUserArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for an AWS Identity and Access
      -- Management (IAM) role. For more information about IAM ARNs, see
      -- Using Identifiers.
    , _pnAllowSsh :: Maybe Bool
      -- ^ Whether the user can use SSH.
    , _pnAllowSudo :: Maybe Bool
      -- ^ Whether the user can use sudo.
    , _pnLevel :: Maybe Text
      -- ^ The user's permission level, which must be the following: deny
      -- show deploy manage iam_only For more information on the
      -- permissions associated with these levels, see Managing User
      -- Permissions.
    } deriving (Show, Generic)

-- | A stack ID.
pnStackId :: Lens' Permission (Maybe Text)
pnStackId = lens _pnStackId (\s a -> s { _pnStackId = a })
{-# INLINE pnStackId #-}

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see Using Identifiers.
pnIamUserArn :: Lens' Permission (Maybe Text)
pnIamUserArn = lens _pnIamUserArn (\s a -> s { _pnIamUserArn = a })
{-# INLINE pnIamUserArn #-}

-- | Whether the user can use SSH.
pnAllowSsh :: Lens' Permission (Maybe Bool)
pnAllowSsh = lens _pnAllowSsh (\s a -> s { _pnAllowSsh = a })
{-# INLINE pnAllowSsh #-}

-- | Whether the user can use sudo.
pnAllowSudo :: Lens' Permission (Maybe Bool)
pnAllowSudo = lens _pnAllowSudo (\s a -> s { _pnAllowSudo = a })
{-# INLINE pnAllowSudo #-}

-- | The user's permission level, which must be the following: deny show deploy
-- manage iam_only For more information on the permissions associated with
-- these levels, see Managing User Permissions.
pnLevel :: Lens' Permission (Maybe Text)
pnLevel = lens _pnLevel (\s a -> s { _pnLevel = a })
{-# INLINE pnLevel #-}

instance FromJSON Permission

-- | Describes an instance's RAID array.
data RaidArray = RaidArray
    { _rbRaidArrayId :: Maybe Text
      -- ^ The array ID.
    , _rbInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _rbName :: Maybe Text
      -- ^ The array name.
    , _rbRaidLevel :: Maybe Integer
      -- ^ The RAID level.
    , _rbNumberOfDisks :: Maybe Integer
      -- ^ The number of disks in the array.
    , _rbSize :: Maybe Integer
      -- ^ The array's size.
    , _rbDevice :: Maybe Text
      -- ^ The array's Linux device. For example /dev/mdadm0.
    , _rbMountPoint :: Maybe Text
      -- ^ The array's mount point.
    , _rbAvailabilityZone :: Maybe Text
      -- ^ The array's Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _rbCreatedAt :: Maybe Text
      -- ^ When the RAID array was created.
    , _rbVolumeType :: Maybe Text
      -- ^ The volume type, standard or PIOPS.
    , _rbIops :: Maybe Integer
      -- ^ For PIOPS volumes, the IOPS per disk.
    } deriving (Show, Generic)

-- | The array ID.
rbRaidArrayId :: Lens' RaidArray (Maybe Text)
rbRaidArrayId = lens _rbRaidArrayId (\s a -> s { _rbRaidArrayId = a })
{-# INLINE rbRaidArrayId #-}

-- | The instance ID.
rbInstanceId :: Lens' RaidArray (Maybe Text)
rbInstanceId = lens _rbInstanceId (\s a -> s { _rbInstanceId = a })
{-# INLINE rbInstanceId #-}

-- | The array name.
rbName :: Lens' RaidArray (Maybe Text)
rbName = lens _rbName (\s a -> s { _rbName = a })
{-# INLINE rbName #-}

-- | The RAID level.
rbRaidLevel :: Lens' RaidArray (Maybe Integer)
rbRaidLevel = lens _rbRaidLevel (\s a -> s { _rbRaidLevel = a })
{-# INLINE rbRaidLevel #-}

-- | The number of disks in the array.
rbNumberOfDisks :: Lens' RaidArray (Maybe Integer)
rbNumberOfDisks = lens _rbNumberOfDisks (\s a -> s { _rbNumberOfDisks = a })
{-# INLINE rbNumberOfDisks #-}

-- | The array's size.
rbSize :: Lens' RaidArray (Maybe Integer)
rbSize = lens _rbSize (\s a -> s { _rbSize = a })
{-# INLINE rbSize #-}

-- | The array's Linux device. For example /dev/mdadm0.
rbDevice :: Lens' RaidArray (Maybe Text)
rbDevice = lens _rbDevice (\s a -> s { _rbDevice = a })
{-# INLINE rbDevice #-}

-- | The array's mount point.
rbMountPoint :: Lens' RaidArray (Maybe Text)
rbMountPoint = lens _rbMountPoint (\s a -> s { _rbMountPoint = a })
{-# INLINE rbMountPoint #-}

-- | The array's Availability Zone. For more information, see Regions and
-- Endpoints.
rbAvailabilityZone :: Lens' RaidArray (Maybe Text)
rbAvailabilityZone = lens _rbAvailabilityZone (\s a -> s { _rbAvailabilityZone = a })
{-# INLINE rbAvailabilityZone #-}

-- | When the RAID array was created.
rbCreatedAt :: Lens' RaidArray (Maybe Text)
rbCreatedAt = lens _rbCreatedAt (\s a -> s { _rbCreatedAt = a })
{-# INLINE rbCreatedAt #-}

-- | The volume type, standard or PIOPS.
rbVolumeType :: Lens' RaidArray (Maybe Text)
rbVolumeType = lens _rbVolumeType (\s a -> s { _rbVolumeType = a })
{-# INLINE rbVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
rbIops :: Lens' RaidArray (Maybe Integer)
rbIops = lens _rbIops (\s a -> s { _rbIops = a })
{-# INLINE rbIops #-}

instance FromJSON RaidArray

-- | Describes an Amazon RDS instance.
data RdsDbInstance = RdsDbInstance
    { _rdjRdsDbInstanceArn :: Maybe Text
      -- ^ The instance's ARN.
    , _rdjDbInstanceIdentifier :: Maybe Text
      -- ^ The DB instance identifier.
    , _rdjDbUser :: Maybe Text
      -- ^ The master user name.
    , _rdjDbPassword :: Maybe Text
      -- ^ The database password.
    , _rdjRegion :: Maybe Text
      -- ^ The instance's AWS region.
    , _rdjAddress :: Maybe Text
      -- ^ The instance's address.
    , _rdjEngine :: Maybe Text
      -- ^ The instance's database engine.
    , _rdjStackId :: Maybe Text
      -- ^ The ID of the stack that the instance is registered with.
    , _rdjMissingOnRds :: Maybe Bool
      -- ^ Set to true if AWS OpsWorks was unable to discover the Amazon RDS
      -- instance. AWS OpsWorks attempts to discover the instance only
      -- once. If this value is set to true, you must deregister the
      -- instance and then register it again.
    } deriving (Show, Generic)

-- | The instance's ARN.
rdjRdsDbInstanceArn :: Lens' RdsDbInstance (Maybe Text)
rdjRdsDbInstanceArn = lens _rdjRdsDbInstanceArn (\s a -> s { _rdjRdsDbInstanceArn = a })
{-# INLINE rdjRdsDbInstanceArn #-}

-- | The DB instance identifier.
rdjDbInstanceIdentifier :: Lens' RdsDbInstance (Maybe Text)
rdjDbInstanceIdentifier = lens _rdjDbInstanceIdentifier (\s a -> s { _rdjDbInstanceIdentifier = a })
{-# INLINE rdjDbInstanceIdentifier #-}

-- | The master user name.
rdjDbUser :: Lens' RdsDbInstance (Maybe Text)
rdjDbUser = lens _rdjDbUser (\s a -> s { _rdjDbUser = a })
{-# INLINE rdjDbUser #-}

-- | The database password.
rdjDbPassword :: Lens' RdsDbInstance (Maybe Text)
rdjDbPassword = lens _rdjDbPassword (\s a -> s { _rdjDbPassword = a })
{-# INLINE rdjDbPassword #-}

-- | The instance's AWS region.
rdjRegion :: Lens' RdsDbInstance (Maybe Text)
rdjRegion = lens _rdjRegion (\s a -> s { _rdjRegion = a })
{-# INLINE rdjRegion #-}

-- | The instance's address.
rdjAddress :: Lens' RdsDbInstance (Maybe Text)
rdjAddress = lens _rdjAddress (\s a -> s { _rdjAddress = a })
{-# INLINE rdjAddress #-}

-- | The instance's database engine.
rdjEngine :: Lens' RdsDbInstance (Maybe Text)
rdjEngine = lens _rdjEngine (\s a -> s { _rdjEngine = a })
{-# INLINE rdjEngine #-}

-- | The ID of the stack that the instance is registered with.
rdjStackId :: Lens' RdsDbInstance (Maybe Text)
rdjStackId = lens _rdjStackId (\s a -> s { _rdjStackId = a })
{-# INLINE rdjStackId #-}

-- | Set to true if AWS OpsWorks was unable to discover the Amazon RDS instance.
-- AWS OpsWorks attempts to discover the instance only once. If this value is
-- set to true, you must deregister the instance and then register it again.
rdjMissingOnRds :: Lens' RdsDbInstance (Maybe Bool)
rdjMissingOnRds = lens _rdjMissingOnRds (\s a -> s { _rdjMissingOnRds = a })
{-# INLINE rdjMissingOnRds #-}

instance FromJSON RdsDbInstance

-- | A LayerCustomRecipes object that specifies the layer custom recipes.
data Recipes = Recipes
    { _rSetup :: [Text]
      -- ^ An array of custom recipe names to be run following a setup
      -- event.
    , _rConfigure :: [Text]
      -- ^ An array of custom recipe names to be run following a configure
      -- event.
    , _rDeploy :: [Text]
      -- ^ An array of custom recipe names to be run following a deploy
      -- event.
    , _rUndeploy :: [Text]
      -- ^ An array of custom recipe names to be run following a undeploy
      -- event.
    , _rShutdown :: [Text]
      -- ^ An array of custom recipe names to be run following a shutdown
      -- event.
    } deriving (Show, Generic)

-- | An array of custom recipe names to be run following a setup event.
rSetup :: Lens' Recipes ([Text])
rSetup = lens _rSetup (\s a -> s { _rSetup = a })
{-# INLINE rSetup #-}

-- | An array of custom recipe names to be run following a configure event.
rConfigure :: Lens' Recipes ([Text])
rConfigure = lens _rConfigure (\s a -> s { _rConfigure = a })
{-# INLINE rConfigure #-}

-- | An array of custom recipe names to be run following a deploy event.
rDeploy :: Lens' Recipes ([Text])
rDeploy = lens _rDeploy (\s a -> s { _rDeploy = a })
{-# INLINE rDeploy #-}

-- | An array of custom recipe names to be run following a undeploy event.
rUndeploy :: Lens' Recipes ([Text])
rUndeploy = lens _rUndeploy (\s a -> s { _rUndeploy = a })
{-# INLINE rUndeploy #-}

-- | An array of custom recipe names to be run following a shutdown event.
rShutdown :: Lens' Recipes ([Text])
rShutdown = lens _rShutdown (\s a -> s { _rShutdown = a })
{-# INLINE rShutdown #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Recipes' data type to populate a request.
mkRecipes :: Recipes
mkRecipes = Recipes
    { _rSetup = mempty
    , _rConfigure = mempty
    , _rDeploy = mempty
    , _rUndeploy = mempty
    , _rShutdown = mempty
    }
{-# INLINE mkRecipes #-}

instance FromJSON Recipes

instance ToJSON Recipes

-- | A UserProfile object that describes the user's SSH information.
data SelfUserProfile = SelfUserProfile
    { _supIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , _supName :: Maybe Text
      -- ^ The user's name.
    , _supSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    , _supSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    } deriving (Show, Generic)

-- | The user's IAM ARN.
supIamUserArn :: Lens' SelfUserProfile (Maybe Text)
supIamUserArn = lens _supIamUserArn (\s a -> s { _supIamUserArn = a })
{-# INLINE supIamUserArn #-}

-- | The user's name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName = lens _supName (\s a -> s { _supName = a })
{-# INLINE supName #-}

-- | The user's SSH user name.
supSshUsername :: Lens' SelfUserProfile (Maybe Text)
supSshUsername = lens _supSshUsername (\s a -> s { _supSshUsername = a })
{-# INLINE supSshUsername #-}

-- | The user's SSH public key.
supSshPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSshPublicKey = lens _supSshPublicKey (\s a -> s { _supSshPublicKey = a })
{-# INLINE supSshPublicKey #-}

instance FromJSON SelfUserProfile

-- | Describes an AWS OpsWorks service error.
data ServiceError = ServiceError
    { _sssssssssssssssxServiceErrorId :: Maybe Text
      -- ^ The error ID.
    , _sssssssssssssssxStackId :: Maybe Text
      -- ^ The stack ID.
    , _sssssssssssssssxInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _sssssssssssssssxType :: Maybe Text
      -- ^ The error type.
    , _sssssssssssssssxMessage :: Maybe Text
      -- ^ A message that describes the error.
    , _sssssssssssssssxCreatedAt :: Maybe Text
      -- ^ When the error occurred.
    } deriving (Show, Generic)

-- | The error ID.
sssssssssssssssxServiceErrorId :: Lens' ServiceError (Maybe Text)
sssssssssssssssxServiceErrorId = lens _sssssssssssssssxServiceErrorId (\s a -> s { _sssssssssssssssxServiceErrorId = a })
{-# INLINE sssssssssssssssxServiceErrorId #-}

-- | The stack ID.
sssssssssssssssxStackId :: Lens' ServiceError (Maybe Text)
sssssssssssssssxStackId = lens _sssssssssssssssxStackId (\s a -> s { _sssssssssssssssxStackId = a })
{-# INLINE sssssssssssssssxStackId #-}

-- | The instance ID.
sssssssssssssssxInstanceId :: Lens' ServiceError (Maybe Text)
sssssssssssssssxInstanceId = lens _sssssssssssssssxInstanceId (\s a -> s { _sssssssssssssssxInstanceId = a })
{-# INLINE sssssssssssssssxInstanceId #-}

-- | The error type.
sssssssssssssssxType :: Lens' ServiceError (Maybe Text)
sssssssssssssssxType = lens _sssssssssssssssxType (\s a -> s { _sssssssssssssssxType = a })
{-# INLINE sssssssssssssssxType #-}

-- | A message that describes the error.
sssssssssssssssxMessage :: Lens' ServiceError (Maybe Text)
sssssssssssssssxMessage = lens _sssssssssssssssxMessage (\s a -> s { _sssssssssssssssxMessage = a })
{-# INLINE sssssssssssssssxMessage #-}

-- | When the error occurred.
sssssssssssssssxCreatedAt :: Lens' ServiceError (Maybe Text)
sssssssssssssssxCreatedAt = lens _sssssssssssssssxCreatedAt (\s a -> s { _sssssssssssssssxCreatedAt = a })
{-# INLINE sssssssssssssssxCreatedAt #-}

instance FromJSON ServiceError

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
data Source = Source
    { _seType :: Maybe SourceType
      -- ^ The repository type.
    , _seUrl :: Maybe Text
      -- ^ The source URL.
    , _seUsername :: Maybe Text
      -- ^ This parameter depends on the repository type. For Amazon S3
      -- bundles, set Username to the appropriate IAM access key ID. For
      -- HTTP bundles, Git repositories, and Subversion repositories, set
      -- Username to the user name.
    , _sePassword :: Maybe Text
      -- ^ This parameter depends on the repository type. For Amazon S3
      -- bundles, set Password to the appropriate IAM secret access key.
      -- For HTTP bundles and Subversion repositories, set Password to the
      -- password. For more information on how to safely handle IAM
      -- credentials, see .
    , _seSshKey :: Maybe Text
      -- ^ The repository's SSH key.
    , _seRevision :: Maybe Text
      -- ^ The application's version. AWS OpsWorks enables you to easily
      -- deploy new versions of an application. One of the simplest
      -- approaches is to have branches or revisions in your repository
      -- that represent different versions that can potentially be
      -- deployed.
    } deriving (Show, Generic)

-- | The repository type.
seType :: Lens' Source (Maybe SourceType)
seType = lens _seType (\s a -> s { _seType = a })
{-# INLINE seType #-}

-- | The source URL.
seUrl :: Lens' Source (Maybe Text)
seUrl = lens _seUrl (\s a -> s { _seUrl = a })
{-# INLINE seUrl #-}

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Username to the appropriate IAM access key ID. For HTTP bundles, Git
-- repositories, and Subversion repositories, set Username to the user name.
seUsername :: Lens' Source (Maybe Text)
seUsername = lens _seUsername (\s a -> s { _seUsername = a })
{-# INLINE seUsername #-}

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Password to the appropriate IAM secret access key. For HTTP bundles and
-- Subversion repositories, set Password to the password. For more information
-- on how to safely handle IAM credentials, see .
sePassword :: Lens' Source (Maybe Text)
sePassword = lens _sePassword (\s a -> s { _sePassword = a })
{-# INLINE sePassword #-}

-- | The repository's SSH key.
seSshKey :: Lens' Source (Maybe Text)
seSshKey = lens _seSshKey (\s a -> s { _seSshKey = a })
{-# INLINE seSshKey #-}

-- | The application's version. AWS OpsWorks enables you to easily deploy new
-- versions of an application. One of the simplest approaches is to have
-- branches or revisions in your repository that represent different versions
-- that can potentially be deployed.
seRevision :: Lens' Source (Maybe Text)
seRevision = lens _seRevision (\s a -> s { _seRevision = a })
{-# INLINE seRevision #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Source' data type to populate a request.
mkSource :: Source
mkSource = Source
    { _seType = Nothing
    , _seUrl = Nothing
    , _seUsername = Nothing
    , _sePassword = Nothing
    , _seSshKey = Nothing
    , _seRevision = Nothing
    }
{-# INLINE mkSource #-}

instance FromJSON Source

instance ToJSON Source

-- | An SslConfiguration object with the SSL configuration.
data SslConfiguration = SslConfiguration
    { _scCertificate :: Text
      -- ^ The contents of the certificate's domain.crt file.
    , _scPrivateKey :: Text
      -- ^ The private key; the contents of the certificate's domain.kex
      -- file.
    , _scChain :: Maybe Text
      -- ^ Optional. Can be used to specify an intermediate certificate
      -- authority key or client authentication.
    } deriving (Show, Generic)

-- | The contents of the certificate's domain.crt file.
scCertificate :: Lens' SslConfiguration (Text)
scCertificate = lens _scCertificate (\s a -> s { _scCertificate = a })
{-# INLINE scCertificate #-}

-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey :: Lens' SslConfiguration (Text)
scPrivateKey = lens _scPrivateKey (\s a -> s { _scPrivateKey = a })
{-# INLINE scPrivateKey #-}

-- | Optional. Can be used to specify an intermediate certificate authority key
-- or client authentication.
scChain :: Lens' SslConfiguration (Maybe Text)
scChain = lens _scChain (\s a -> s { _scChain = a })
{-# INLINE scChain #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SslConfiguration' data type to populate a request.
mkSslConfiguration :: Text -- ^ 'scCertificate'
                   -> Text -- ^ 'scPrivateKey'
                   -> SslConfiguration
mkSslConfiguration p1 p2 = SslConfiguration
    { _scCertificate = p1
    , _scPrivateKey = p2
    , _scChain = Nothing
    }
{-# INLINE mkSslConfiguration #-}

instance FromJSON SslConfiguration

instance ToJSON SslConfiguration

-- | Describes a stack.
data Stack = Stack
    { _ssssssssssssssssoStackId :: Maybe Text
      -- ^ The stack ID.
    , _ssssssssssssssssoName :: Maybe Text
      -- ^ The stack name.
    , _ssssssssssssssssoArn :: Maybe Text
      -- ^ The stack's ARN.
    , _ssssssssssssssssoRegion :: Maybe Text
      -- ^ The stack AWS region, such as "us-east-1". For more information
      -- about AWS regions, see Regions and Endpoints.
    , _ssssssssssssssssoVpcId :: Maybe Text
      -- ^ The VPC ID, if the stack is running in a VPC.
    , _ssssssssssssssssoAttributes :: Map StackAttributesKeys Text
      -- ^ The stack's attributes.
    , _ssssssssssssssssoServiceRoleArn :: Maybe Text
      -- ^ The stack AWS Identity and Access Management (IAM) role.
    , _ssssssssssssssssoDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of
      -- the stack's EC2 instances. For more information about IAM ARNs,
      -- see Using Identifiers.
    , _ssssssssssssssssoDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon
      -- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , _ssssssssssssssssoHostnameTheme :: Maybe Text
      -- ^ The stack host name theme, with spaces replaced by underscores.
    , _ssssssssssssssssoDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone. For more information, see
      -- Regions and Endpoints.
    , _ssssssssssssssssoDefaultSubnetId :: Maybe Text
      -- ^ The default subnet ID, if the stack is running in a VPC.
    , _ssssssssssssssssoCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _ssssssssssssssssoConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager.
    , _ssssssssssssssssoChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version. For more information, see
      -- Create a New Stack.
    , _ssssssssssssssssoUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , _ssssssssssssssssoUseOpsworksSecurityGroups :: Maybe Bool
      -- ^ Whether the stack automatically associates the AWS OpsWorks
      -- built-in security groups with the stack's layers.
    , _ssssssssssssssssoCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _ssssssssssssssssoDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack's instances. You can override
      -- this value when you create or update an instance.
    , _ssssssssssssssssoCreatedAt :: Maybe Text
      -- ^ Date when the stack was created.
    , _ssssssssssssssssoDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the stack, but you can override it when you
      -- create an instance. For more information, see Storage for the
      -- Root Device.
    } deriving (Show, Generic)

-- | The stack ID.
ssssssssssssssssoStackId :: Lens' Stack (Maybe Text)
ssssssssssssssssoStackId = lens _ssssssssssssssssoStackId (\s a -> s { _ssssssssssssssssoStackId = a })
{-# INLINE ssssssssssssssssoStackId #-}

-- | The stack name.
ssssssssssssssssoName :: Lens' Stack (Maybe Text)
ssssssssssssssssoName = lens _ssssssssssssssssoName (\s a -> s { _ssssssssssssssssoName = a })
{-# INLINE ssssssssssssssssoName #-}

-- | The stack's ARN.
ssssssssssssssssoArn :: Lens' Stack (Maybe Text)
ssssssssssssssssoArn = lens _ssssssssssssssssoArn (\s a -> s { _ssssssssssssssssoArn = a })
{-# INLINE ssssssssssssssssoArn #-}

-- | The stack AWS region, such as "us-east-1". For more information about AWS
-- regions, see Regions and Endpoints.
ssssssssssssssssoRegion :: Lens' Stack (Maybe Text)
ssssssssssssssssoRegion = lens _ssssssssssssssssoRegion (\s a -> s { _ssssssssssssssssoRegion = a })
{-# INLINE ssssssssssssssssoRegion #-}

-- | The VPC ID, if the stack is running in a VPC.
ssssssssssssssssoVpcId :: Lens' Stack (Maybe Text)
ssssssssssssssssoVpcId = lens _ssssssssssssssssoVpcId (\s a -> s { _ssssssssssssssssoVpcId = a })
{-# INLINE ssssssssssssssssoVpcId #-}

-- | The stack's attributes.
ssssssssssssssssoAttributes :: Lens' Stack (Map StackAttributesKeys Text)
ssssssssssssssssoAttributes = lens _ssssssssssssssssoAttributes (\s a -> s { _ssssssssssssssssoAttributes = a })
{-# INLINE ssssssssssssssssoAttributes #-}

-- | The stack AWS Identity and Access Management (IAM) role.
ssssssssssssssssoServiceRoleArn :: Lens' Stack (Maybe Text)
ssssssssssssssssoServiceRoleArn = lens _ssssssssssssssssoServiceRoleArn (\s a -> s { _ssssssssssssssssoServiceRoleArn = a })
{-# INLINE ssssssssssssssssoServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
ssssssssssssssssoDefaultInstanceProfileArn :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultInstanceProfileArn = lens _ssssssssssssssssoDefaultInstanceProfileArn (\s a -> s { _ssssssssssssssssoDefaultInstanceProfileArn = a })
{-# INLINE ssssssssssssssssoDefaultInstanceProfileArn #-}

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
ssssssssssssssssoDefaultOs :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultOs = lens _ssssssssssssssssoDefaultOs (\s a -> s { _ssssssssssssssssoDefaultOs = a })
{-# INLINE ssssssssssssssssoDefaultOs #-}

-- | The stack host name theme, with spaces replaced by underscores.
ssssssssssssssssoHostnameTheme :: Lens' Stack (Maybe Text)
ssssssssssssssssoHostnameTheme = lens _ssssssssssssssssoHostnameTheme (\s a -> s { _ssssssssssssssssoHostnameTheme = a })
{-# INLINE ssssssssssssssssoHostnameTheme #-}

-- | The stack's default Availability Zone. For more information, see Regions
-- and Endpoints.
ssssssssssssssssoDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultAvailabilityZone = lens _ssssssssssssssssoDefaultAvailabilityZone (\s a -> s { _ssssssssssssssssoDefaultAvailabilityZone = a })
{-# INLINE ssssssssssssssssoDefaultAvailabilityZone #-}

-- | The default subnet ID, if the stack is running in a VPC.
ssssssssssssssssoDefaultSubnetId :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultSubnetId = lens _ssssssssssssssssoDefaultSubnetId (\s a -> s { _ssssssssssssssssoDefaultSubnetId = a })
{-# INLINE ssssssssssssssssoDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
ssssssssssssssssoCustomJson :: Lens' Stack (Maybe Text)
ssssssssssssssssoCustomJson = lens _ssssssssssssssssoCustomJson (\s a -> s { _ssssssssssssssssoCustomJson = a })
{-# INLINE ssssssssssssssssoCustomJson #-}

-- | The configuration manager.
ssssssssssssssssoConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
ssssssssssssssssoConfigurationManager = lens _ssssssssssssssssoConfigurationManager (\s a -> s { _ssssssssssssssssoConfigurationManager = a })
{-# INLINE ssssssssssssssssoConfigurationManager #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version. For more information, see Create a New Stack.
ssssssssssssssssoChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
ssssssssssssssssoChefConfiguration = lens _ssssssssssssssssoChefConfiguration (\s a -> s { _ssssssssssssssssoChefConfiguration = a })
{-# INLINE ssssssssssssssssoChefConfiguration #-}

-- | Whether the stack uses custom cookbooks.
ssssssssssssssssoUseCustomCookbooks :: Lens' Stack (Maybe Bool)
ssssssssssssssssoUseCustomCookbooks = lens _ssssssssssssssssoUseCustomCookbooks (\s a -> s { _ssssssssssssssssoUseCustomCookbooks = a })
{-# INLINE ssssssssssssssssoUseCustomCookbooks #-}

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack's layers.
ssssssssssssssssoUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
ssssssssssssssssoUseOpsworksSecurityGroups = lens _ssssssssssssssssoUseOpsworksSecurityGroups (\s a -> s { _ssssssssssssssssoUseOpsworksSecurityGroups = a })
{-# INLINE ssssssssssssssssoUseOpsworksSecurityGroups #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
ssssssssssssssssoCustomCookbooksSource :: Lens' Stack (Maybe Source)
ssssssssssssssssoCustomCookbooksSource = lens _ssssssssssssssssoCustomCookbooksSource (\s a -> s { _ssssssssssssssssoCustomCookbooksSource = a })
{-# INLINE ssssssssssssssssoCustomCookbooksSource #-}

-- | A default SSH key for the stack's instances. You can override this value
-- when you create or update an instance.
ssssssssssssssssoDefaultSshKeyName :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultSshKeyName = lens _ssssssssssssssssoDefaultSshKeyName (\s a -> s { _ssssssssssssssssoDefaultSshKeyName = a })
{-# INLINE ssssssssssssssssoDefaultSshKeyName #-}

-- | Date when the stack was created.
ssssssssssssssssoCreatedAt :: Lens' Stack (Maybe Text)
ssssssssssssssssoCreatedAt = lens _ssssssssssssssssoCreatedAt (\s a -> s { _ssssssssssssssssoCreatedAt = a })
{-# INLINE ssssssssssssssssoCreatedAt #-}

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
ssssssssssssssssoDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
ssssssssssssssssoDefaultRootDeviceType = lens _ssssssssssssssssoDefaultRootDeviceType (\s a -> s { _ssssssssssssssssoDefaultRootDeviceType = a })
{-# INLINE ssssssssssssssssoDefaultRootDeviceType #-}

instance FromJSON Stack

-- | The configuration manager. When you clone a stack we recommend that you use
-- the configuration manager to specify the Chef version, 0.9, 11.4, or 11.10.
-- The default value is currently 11.4.
data StackConfigurationManager = StackConfigurationManager
    { _scmName :: Maybe Text
      -- ^ The name. This parameter must be set to "Chef".
    , _scmVersion :: Maybe Text
      -- ^ The Chef version. This parameter must be set to 0.9, 11.4, or
      -- 11.10. The default value is 11.4.
    } deriving (Show, Generic)

-- | The name. This parameter must be set to "Chef".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\s a -> s { _scmName = a })
{-# INLINE scmName #-}

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\s a -> s { _scmVersion = a })
{-# INLINE scmVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StackConfigurationManager' data type to populate a request.
mkStackConfigurationManager :: StackConfigurationManager
mkStackConfigurationManager = StackConfigurationManager
    { _scmName = Nothing
    , _scmVersion = Nothing
    }
{-# INLINE mkStackConfigurationManager #-}

instance FromJSON StackConfigurationManager

instance ToJSON StackConfigurationManager

-- | A StackSummary object that contains the results.
data StackSummary = StackSummary
    { _ssssssssssssssssyStackId :: Maybe Text
      -- ^ The stack ID.
    , _ssssssssssssssssyName :: Maybe Text
      -- ^ The stack name.
    , _ssssssssssssssssyArn :: Maybe Text
      -- ^ The stack's ARN.
    , _ssssssssssssssssyLayersCount :: Maybe Integer
      -- ^ The number of layers.
    , _ssssssssssssssssyAppsCount :: Maybe Integer
      -- ^ The number of apps.
    , _ssssssssssssssssyInstancesCount :: Maybe InstancesCount
      -- ^ An InstancesCount object with the number of instances in each
      -- status.
    } deriving (Show, Generic)

-- | The stack ID.
ssssssssssssssssyStackId :: Lens' StackSummary (Maybe Text)
ssssssssssssssssyStackId = lens _ssssssssssssssssyStackId (\s a -> s { _ssssssssssssssssyStackId = a })
{-# INLINE ssssssssssssssssyStackId #-}

-- | The stack name.
ssssssssssssssssyName :: Lens' StackSummary (Maybe Text)
ssssssssssssssssyName = lens _ssssssssssssssssyName (\s a -> s { _ssssssssssssssssyName = a })
{-# INLINE ssssssssssssssssyName #-}

-- | The stack's ARN.
ssssssssssssssssyArn :: Lens' StackSummary (Maybe Text)
ssssssssssssssssyArn = lens _ssssssssssssssssyArn (\s a -> s { _ssssssssssssssssyArn = a })
{-# INLINE ssssssssssssssssyArn #-}

-- | The number of layers.
ssssssssssssssssyLayersCount :: Lens' StackSummary (Maybe Integer)
ssssssssssssssssyLayersCount = lens _ssssssssssssssssyLayersCount (\s a -> s { _ssssssssssssssssyLayersCount = a })
{-# INLINE ssssssssssssssssyLayersCount #-}

-- | The number of apps.
ssssssssssssssssyAppsCount :: Lens' StackSummary (Maybe Integer)
ssssssssssssssssyAppsCount = lens _ssssssssssssssssyAppsCount (\s a -> s { _ssssssssssssssssyAppsCount = a })
{-# INLINE ssssssssssssssssyAppsCount #-}

-- | An InstancesCount object with the number of instances in each status.
ssssssssssssssssyInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssssssssssssssssyInstancesCount = lens _ssssssssssssssssyInstancesCount (\s a -> s { _ssssssssssssssssyInstancesCount = a })
{-# INLINE ssssssssssssssssyInstancesCount #-}

instance FromJSON StackSummary

-- | Describes an instance's time-based auto scaling configuration.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { _tbasdInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _tbasdAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
      -- ^ A WeeklyAutoScalingSchedule object with the instance schedule.
    } deriving (Show, Generic)

-- | The instance ID.
tbasdInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbasdInstanceId = lens _tbasdInstanceId (\s a -> s { _tbasdInstanceId = a })
{-# INLINE tbasdInstanceId #-}

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
tbasdAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbasdAutoScalingSchedule = lens _tbasdAutoScalingSchedule (\s a -> s { _tbasdAutoScalingSchedule = a })
{-# INLINE tbasdAutoScalingSchedule #-}

instance FromJSON TimeBasedAutoScalingConfiguration

-- | Describes a user's SSH information.
data UserProfile = UserProfile
    { _uxIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , _uxName :: Maybe Text
      -- ^ The user's name.
    , _uxSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    , _uxSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    , _uxAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My
      -- Settings page. For more information, see Managing User
      -- Permissions.
    } deriving (Show, Generic)

-- | The user's IAM ARN.
uxIamUserArn :: Lens' UserProfile (Maybe Text)
uxIamUserArn = lens _uxIamUserArn (\s a -> s { _uxIamUserArn = a })
{-# INLINE uxIamUserArn #-}

-- | The user's name.
uxName :: Lens' UserProfile (Maybe Text)
uxName = lens _uxName (\s a -> s { _uxName = a })
{-# INLINE uxName #-}

-- | The user's SSH user name.
uxSshUsername :: Lens' UserProfile (Maybe Text)
uxSshUsername = lens _uxSshUsername (\s a -> s { _uxSshUsername = a })
{-# INLINE uxSshUsername #-}

-- | The user's SSH public key.
uxSshPublicKey :: Lens' UserProfile (Maybe Text)
uxSshPublicKey = lens _uxSshPublicKey (\s a -> s { _uxSshPublicKey = a })
{-# INLINE uxSshPublicKey #-}

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Managing User Permissions.
uxAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
uxAllowSelfManagement = lens _uxAllowSelfManagement (\s a -> s { _uxAllowSelfManagement = a })
{-# INLINE uxAllowSelfManagement #-}

instance FromJSON UserProfile

-- | Describes an instance's Amazon EBS volume.
data Volume = Volume
    { _vgVolumeId :: Maybe Text
      -- ^ The volume ID.
    , _vgEc2VolumeId :: Maybe Text
      -- ^ The Amazon EC2 volume ID.
    , _vgName :: Maybe Text
      -- ^ The volume name.
    , _vgRaidArrayId :: Maybe Text
      -- ^ The RAID array ID.
    , _vgInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _vgStatus :: Maybe Text
      -- ^ The value returned by DescribeVolumes.
    , _vgSize :: Maybe Integer
      -- ^ The volume size.
    , _vgDevice :: Maybe Text
      -- ^ The device name.
    , _vgMountPoint :: Maybe Text
      -- ^ The volume mount point. For example "/dev/sdh".
    , _vgRegion :: Maybe Text
      -- ^ The AWS region. For more information about AWS regions, see
      -- Regions and Endpoints.
    , _vgAvailabilityZone :: Maybe Text
      -- ^ The volume Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _vgVolumeType :: Maybe Text
      -- ^ The volume type, standard or PIOPS.
    , _vgIops :: Maybe Integer
      -- ^ For PIOPS volumes, the IOPS per disk.
    } deriving (Show, Generic)

-- | The volume ID.
vgVolumeId :: Lens' Volume (Maybe Text)
vgVolumeId = lens _vgVolumeId (\s a -> s { _vgVolumeId = a })
{-# INLINE vgVolumeId #-}

-- | The Amazon EC2 volume ID.
vgEc2VolumeId :: Lens' Volume (Maybe Text)
vgEc2VolumeId = lens _vgEc2VolumeId (\s a -> s { _vgEc2VolumeId = a })
{-# INLINE vgEc2VolumeId #-}

-- | The volume name.
vgName :: Lens' Volume (Maybe Text)
vgName = lens _vgName (\s a -> s { _vgName = a })
{-# INLINE vgName #-}

-- | The RAID array ID.
vgRaidArrayId :: Lens' Volume (Maybe Text)
vgRaidArrayId = lens _vgRaidArrayId (\s a -> s { _vgRaidArrayId = a })
{-# INLINE vgRaidArrayId #-}

-- | The instance ID.
vgInstanceId :: Lens' Volume (Maybe Text)
vgInstanceId = lens _vgInstanceId (\s a -> s { _vgInstanceId = a })
{-# INLINE vgInstanceId #-}

-- | The value returned by DescribeVolumes.
vgStatus :: Lens' Volume (Maybe Text)
vgStatus = lens _vgStatus (\s a -> s { _vgStatus = a })
{-# INLINE vgStatus #-}

-- | The volume size.
vgSize :: Lens' Volume (Maybe Integer)
vgSize = lens _vgSize (\s a -> s { _vgSize = a })
{-# INLINE vgSize #-}

-- | The device name.
vgDevice :: Lens' Volume (Maybe Text)
vgDevice = lens _vgDevice (\s a -> s { _vgDevice = a })
{-# INLINE vgDevice #-}

-- | The volume mount point. For example "/dev/sdh".
vgMountPoint :: Lens' Volume (Maybe Text)
vgMountPoint = lens _vgMountPoint (\s a -> s { _vgMountPoint = a })
{-# INLINE vgMountPoint #-}

-- | The AWS region. For more information about AWS regions, see Regions and
-- Endpoints.
vgRegion :: Lens' Volume (Maybe Text)
vgRegion = lens _vgRegion (\s a -> s { _vgRegion = a })
{-# INLINE vgRegion #-}

-- | The volume Availability Zone. For more information, see Regions and
-- Endpoints.
vgAvailabilityZone :: Lens' Volume (Maybe Text)
vgAvailabilityZone = lens _vgAvailabilityZone (\s a -> s { _vgAvailabilityZone = a })
{-# INLINE vgAvailabilityZone #-}

-- | The volume type, standard or PIOPS.
vgVolumeType :: Lens' Volume (Maybe Text)
vgVolumeType = lens _vgVolumeType (\s a -> s { _vgVolumeType = a })
{-# INLINE vgVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
vgIops :: Lens' Volume (Maybe Integer)
vgIops = lens _vgIops (\s a -> s { _vgIops = a })
{-# INLINE vgIops #-}

instance FromJSON Volume

-- | Describes an Amazon EBS volume configuration.
data VolumeConfiguration = VolumeConfiguration
    { _vdMountPoint :: Text
      -- ^ The volume mount point. For example "/dev/sdh".
    , _vdRaidLevel :: Maybe Integer
      -- ^ The volume RAID level.
    , _vdNumberOfDisks :: Integer
      -- ^ The number of disks in the volume.
    , _vdSize :: Integer
      -- ^ The volume size.
    , _vdVolumeType :: Maybe Text
      -- ^ The volume type, standard or PIOPS.
    , _vdIops :: Maybe Integer
      -- ^ For PIOPS volumes, the IOPS per disk.
    } deriving (Show, Generic)

-- | The volume mount point. For example "/dev/sdh".
vdMountPoint :: Lens' VolumeConfiguration (Text)
vdMountPoint = lens _vdMountPoint (\s a -> s { _vdMountPoint = a })
{-# INLINE vdMountPoint #-}

-- | The volume RAID level.
vdRaidLevel :: Lens' VolumeConfiguration (Maybe Integer)
vdRaidLevel = lens _vdRaidLevel (\s a -> s { _vdRaidLevel = a })
{-# INLINE vdRaidLevel #-}

-- | The number of disks in the volume.
vdNumberOfDisks :: Lens' VolumeConfiguration (Integer)
vdNumberOfDisks = lens _vdNumberOfDisks (\s a -> s { _vdNumberOfDisks = a })
{-# INLINE vdNumberOfDisks #-}

-- | The volume size.
vdSize :: Lens' VolumeConfiguration (Integer)
vdSize = lens _vdSize (\s a -> s { _vdSize = a })
{-# INLINE vdSize #-}

-- | The volume type, standard or PIOPS.
vdVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vdVolumeType = lens _vdVolumeType (\s a -> s { _vdVolumeType = a })
{-# INLINE vdVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
vdIops :: Lens' VolumeConfiguration (Maybe Integer)
vdIops = lens _vdIops (\s a -> s { _vdIops = a })
{-# INLINE vdIops #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeConfiguration' data type to populate a request.
mkVolumeConfiguration :: Text -- ^ 'vdMountPoint'
                      -> Integer -- ^ 'vdNumberOfDisks'
                      -> Integer -- ^ 'vdSize'
                      -> VolumeConfiguration
mkVolumeConfiguration p1 p2 p3 = VolumeConfiguration
    { _vdMountPoint = p1
    , _vdRaidLevel = Nothing
    , _vdNumberOfDisks = p3
    , _vdSize = p4
    , _vdVolumeType = Nothing
    , _vdIops = Nothing
    }
{-# INLINE mkVolumeConfiguration #-}

instance FromJSON VolumeConfiguration

instance ToJSON VolumeConfiguration

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassMonday :: Map Text Text
      -- ^ The schedule for Monday.
    , _wassTuesday :: Map Text Text
      -- ^ The schedule for Tuesday.
    , _wassWednesday :: Map Text Text
      -- ^ The schedule for Wednesday.
    , _wassThursday :: Map Text Text
      -- ^ The schedule for Thursday.
    , _wassFriday :: Map Text Text
      -- ^ The schedule for Friday.
    , _wassSaturday :: Map Text Text
      -- ^ The schedule for Saturday.
    , _wassSunday :: Map Text Text
      -- ^ The schedule for Sunday.
    } deriving (Show, Generic)

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassMonday = lens _wassMonday (\s a -> s { _wassMonday = a })
{-# INLINE wassMonday #-}

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassTuesday = lens _wassTuesday (\s a -> s { _wassTuesday = a })
{-# INLINE wassTuesday #-}

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassWednesday = lens _wassWednesday (\s a -> s { _wassWednesday = a })
{-# INLINE wassWednesday #-}

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassThursday = lens _wassThursday (\s a -> s { _wassThursday = a })
{-# INLINE wassThursday #-}

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassFriday = lens _wassFriday (\s a -> s { _wassFriday = a })
{-# INLINE wassFriday #-}

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassSaturday = lens _wassSaturday (\s a -> s { _wassSaturday = a })
{-# INLINE wassSaturday #-}

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassSunday = lens _wassSunday (\s a -> s { _wassSunday = a })
{-# INLINE wassSunday #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WeeklyAutoScalingSchedule' data type to populate a request.
mkWeeklyAutoScalingSchedule :: WeeklyAutoScalingSchedule
mkWeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassMonday = mempty
    , _wassTuesday = mempty
    , _wassWednesday = mempty
    , _wassThursday = mempty
    , _wassFriday = mempty
    , _wassSaturday = mempty
    , _wassSunday = mempty
    }
{-# INLINE mkWeeklyAutoScalingSchedule #-}

instance FromJSON WeeklyAutoScalingSchedule

instance ToJSON WeeklyAutoScalingSchedule
