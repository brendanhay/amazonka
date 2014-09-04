{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , App (..)
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
    , AutoScalingThresholds (..)
    , asvInstanceCount
    , asvThresholdsWaitTime
    , asvIgnoreMetricsTime
    , asvCpuThreshold
    , asvMemoryThreshold
    , asvLoadThreshold

    -- * ChefConfiguration
    , ChefConfiguration (..)
    , ccManageBerkshelf
    , ccBerkshelfVersion

    -- * Command
    , Command (..)
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
    , DataSource (..)
    , dtType
    , dtArn
    , dtDatabaseName

    -- * Deployment
    , Deployment (..)
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
    , DeploymentCommand (..)
    , dcName
    , dcArgs

    -- * ElasticIp
    , ElasticIp (..)
    , ejIp
    , ejName
    , ejDomain
    , ejRegion
    , ejInstanceId

    -- * ElasticLoadBalancer
    , ElasticLoadBalancer (..)
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
    , Instance (..)
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
    , InstancesCount (..)
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
    , Layer (..)
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
    , LoadBasedAutoScalingConfiguration (..)
    , lbasdLayerId
    , lbasdEnable
    , lbasdUpScaling
    , lbasdDownScaling

    -- * Permission
    , Permission (..)
    , pnStackId
    , pnIamUserArn
    , pnAllowSsh
    , pnAllowSudo
    , pnLevel

    -- * RaidArray
    , RaidArray (..)
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
    , RdsDbInstance (..)
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
    , Recipes (..)
    , rSetup
    , rConfigure
    , rDeploy
    , rUndeploy
    , rShutdown

    -- * SelfUserProfile
    , SelfUserProfile (..)
    , supIamUserArn
    , supName
    , supSshUsername
    , supSshPublicKey

    -- * ServiceError
    , ServiceError (..)
    , sssssssssssssssxServiceErrorId
    , sssssssssssssssxStackId
    , sssssssssssssssxInstanceId
    , sssssssssssssssxType
    , sssssssssssssssxMessage
    , sssssssssssssssxCreatedAt

    -- * Source
    , Source (..)
    , seType
    , seUrl
    , seUsername
    , sePassword
    , seSshKey
    , seRevision

    -- * SslConfiguration
    , SslConfiguration (..)
    , scCertificate
    , scPrivateKey
    , scChain

    -- * Stack
    , Stack (..)
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
    , StackConfigurationManager (..)
    , scmName
    , scmVersion

    -- * StackSummary
    , StackSummary (..)
    , ssssssssssssssssyStackId
    , ssssssssssssssssyName
    , ssssssssssssssssyArn
    , ssssssssssssssssyLayersCount
    , ssssssssssssssssyAppsCount
    , ssssssssssssssssyInstancesCount

    -- * TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration (..)
    , tbasdInstanceId
    , tbasdAutoScalingSchedule

    -- * UserProfile
    , UserProfile (..)
    , uxIamUserArn
    , uxName
    , uxSshUsername
    , uxSshPublicKey
    , uxAllowSelfManagement

    -- * Volume
    , Volume (..)
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
    , VolumeConfiguration (..)
    , vdMountPoint
    , vdRaidLevel
    , vdNumberOfDisks
    , vdSize
    , vdVolumeType
    , vdIops

    -- * WeeklyAutoScalingSchedule
    , WeeklyAutoScalingSchedule (..)
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
apAppId f x =
    f (_apAppId x)
        <&> \y -> x { _apAppId = y }
{-# INLINE apAppId #-}

-- | The app stack ID.
apStackId :: Lens' App (Maybe Text)
apStackId f x =
    f (_apStackId x)
        <&> \y -> x { _apStackId = y }
{-# INLINE apStackId #-}

-- | The app's short name.
apShortname :: Lens' App (Maybe Text)
apShortname f x =
    f (_apShortname x)
        <&> \y -> x { _apShortname = y }
{-# INLINE apShortname #-}

-- | The app name.
apName :: Lens' App (Maybe Text)
apName f x =
    f (_apName x)
        <&> \y -> x { _apName = y }
{-# INLINE apName #-}

-- | A description of the app.
apDescription :: Lens' App (Maybe Text)
apDescription f x =
    f (_apDescription x)
        <&> \y -> x { _apDescription = y }
{-# INLINE apDescription #-}

-- | The app's data sources.
apDataSources :: Lens' App ([DataSource])
apDataSources f x =
    f (_apDataSources x)
        <&> \y -> x { _apDataSources = y }
{-# INLINE apDataSources #-}

-- | The app type.
apType :: Lens' App (Maybe AppType)
apType f x =
    f (_apType x)
        <&> \y -> x { _apType = y }
{-# INLINE apType #-}

-- | A Source object that describes the app repository.
apAppSource :: Lens' App (Maybe Source)
apAppSource f x =
    f (_apAppSource x)
        <&> \y -> x { _apAppSource = y }
{-# INLINE apAppSource #-}

-- | The app vhost settings with multiple domains separated by commas. For
-- example: 'www.example.com, example.com'.
apDomains :: Lens' App ([Text])
apDomains f x =
    f (_apDomains x)
        <&> \y -> x { _apDomains = y }
{-# INLINE apDomains #-}

-- | Whether to enable SSL for the app.
apEnableSsl :: Lens' App (Maybe Bool)
apEnableSsl f x =
    f (_apEnableSsl x)
        <&> \y -> x { _apEnableSsl = y }
{-# INLINE apEnableSsl #-}

-- | An SslConfiguration object with the SSL configuration.
apSslConfiguration :: Lens' App (Maybe SslConfiguration)
apSslConfiguration f x =
    f (_apSslConfiguration x)
        <&> \y -> x { _apSslConfiguration = y }
{-# INLINE apSslConfiguration #-}

-- | The stack attributes.
apAttributes :: Lens' App (Map AppAttributesKeys Text)
apAttributes f x =
    f (_apAttributes x)
        <&> \y -> x { _apAttributes = y }
{-# INLINE apAttributes #-}

-- | When the app was created.
apCreatedAt :: Lens' App (Maybe Text)
apCreatedAt f x =
    f (_apCreatedAt x)
        <&> \y -> x { _apCreatedAt = y }
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
asvInstanceCount f x =
    f (_asvInstanceCount x)
        <&> \y -> x { _asvInstanceCount = y }
{-# INLINE asvInstanceCount #-}

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
asvThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Integer)
asvThresholdsWaitTime f x =
    f (_asvThresholdsWaitTime x)
        <&> \y -> x { _asvThresholdsWaitTime = y }
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
asvIgnoreMetricsTime f x =
    f (_asvIgnoreMetricsTime x)
        <&> \y -> x { _asvIgnoreMetricsTime = y }
{-# INLINE asvIgnoreMetricsTime #-}

-- | The CPU utilization threshold, as a percent of the available CPU.
asvCpuThreshold :: Lens' AutoScalingThresholds (Maybe Double)
asvCpuThreshold f x =
    f (_asvCpuThreshold x)
        <&> \y -> x { _asvCpuThreshold = y }
{-# INLINE asvCpuThreshold #-}

-- | The memory utilization threshold, as a percent of the available memory.
asvMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
asvMemoryThreshold f x =
    f (_asvMemoryThreshold x)
        <&> \y -> x { _asvMemoryThreshold = y }
{-# INLINE asvMemoryThreshold #-}

-- | The load threshold. For more information about how load is computed, see
-- Load (computing).
asvLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
asvLoadThreshold f x =
    f (_asvLoadThreshold x)
        <&> \y -> x { _asvLoadThreshold = y }
{-# INLINE asvLoadThreshold #-}

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
ccManageBerkshelf f x =
    f (_ccManageBerkshelf x)
        <&> \y -> x { _ccManageBerkshelf = y }
{-# INLINE ccManageBerkshelf #-}

-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion f x =
    f (_ccBerkshelfVersion x)
        <&> \y -> x { _ccBerkshelfVersion = y }
{-# INLINE ccBerkshelfVersion #-}

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
ceCommandId f x =
    f (_ceCommandId x)
        <&> \y -> x { _ceCommandId = y }
{-# INLINE ceCommandId #-}

-- | The ID of the instance where the command was executed.
ceInstanceId :: Lens' Command (Maybe Text)
ceInstanceId f x =
    f (_ceInstanceId x)
        <&> \y -> x { _ceInstanceId = y }
{-# INLINE ceInstanceId #-}

-- | The command deployment ID.
ceDeploymentId :: Lens' Command (Maybe Text)
ceDeploymentId f x =
    f (_ceDeploymentId x)
        <&> \y -> x { _ceDeploymentId = y }
{-# INLINE ceDeploymentId #-}

-- | Date and time when the command was run.
ceCreatedAt :: Lens' Command (Maybe Text)
ceCreatedAt f x =
    f (_ceCreatedAt x)
        <&> \y -> x { _ceCreatedAt = y }
{-# INLINE ceCreatedAt #-}

-- | Date and time when the command was acknowledged.
ceAcknowledgedAt :: Lens' Command (Maybe Text)
ceAcknowledgedAt f x =
    f (_ceAcknowledgedAt x)
        <&> \y -> x { _ceAcknowledgedAt = y }
{-# INLINE ceAcknowledgedAt #-}

-- | Date when the command completed.
ceCompletedAt :: Lens' Command (Maybe Text)
ceCompletedAt f x =
    f (_ceCompletedAt x)
        <&> \y -> x { _ceCompletedAt = y }
{-# INLINE ceCompletedAt #-}

-- | The command status: failed successful skipped pending.
ceStatus :: Lens' Command (Maybe Text)
ceStatus f x =
    f (_ceStatus x)
        <&> \y -> x { _ceStatus = y }
{-# INLINE ceStatus #-}

-- | The command exit code.
ceExitCode :: Lens' Command (Maybe Integer)
ceExitCode f x =
    f (_ceExitCode x)
        <&> \y -> x { _ceExitCode = y }
{-# INLINE ceExitCode #-}

-- | The URL of the command log.
ceLogUrl :: Lens' Command (Maybe Text)
ceLogUrl f x =
    f (_ceLogUrl x)
        <&> \y -> x { _ceLogUrl = y }
{-# INLINE ceLogUrl #-}

-- | The command type: deploy rollback start stop restart undeploy
-- update_dependencies install_dependencies update_custom_cookbooks
-- execute_recipes.
ceType :: Lens' Command (Maybe Text)
ceType f x =
    f (_ceType x)
        <&> \y -> x { _ceType = y }
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
dtType f x =
    f (_dtType x)
        <&> \y -> x { _dtType = y }
{-# INLINE dtType #-}

-- | The data source's ARN.
dtArn :: Lens' DataSource (Maybe Text)
dtArn f x =
    f (_dtArn x)
        <&> \y -> x { _dtArn = y }
{-# INLINE dtArn #-}

-- | The database name.
dtDatabaseName :: Lens' DataSource (Maybe Text)
dtDatabaseName f x =
    f (_dtDatabaseName x)
        <&> \y -> x { _dtDatabaseName = y }
{-# INLINE dtDatabaseName #-}

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
ddtDeploymentId f x =
    f (_ddtDeploymentId x)
        <&> \y -> x { _ddtDeploymentId = y }
{-# INLINE ddtDeploymentId #-}

-- | The stack ID.
ddtStackId :: Lens' Deployment (Maybe Text)
ddtStackId f x =
    f (_ddtStackId x)
        <&> \y -> x { _ddtStackId = y }
{-# INLINE ddtStackId #-}

-- | The app ID.
ddtAppId :: Lens' Deployment (Maybe Text)
ddtAppId f x =
    f (_ddtAppId x)
        <&> \y -> x { _ddtAppId = y }
{-# INLINE ddtAppId #-}

-- | Date when the deployment was created.
ddtCreatedAt :: Lens' Deployment (Maybe Text)
ddtCreatedAt f x =
    f (_ddtCreatedAt x)
        <&> \y -> x { _ddtCreatedAt = y }
{-# INLINE ddtCreatedAt #-}

-- | Date when the deployment completed.
ddtCompletedAt :: Lens' Deployment (Maybe Text)
ddtCompletedAt f x =
    f (_ddtCompletedAt x)
        <&> \y -> x { _ddtCompletedAt = y }
{-# INLINE ddtCompletedAt #-}

-- | The deployment duration.
ddtDuration :: Lens' Deployment (Maybe Integer)
ddtDuration f x =
    f (_ddtDuration x)
        <&> \y -> x { _ddtDuration = y }
{-# INLINE ddtDuration #-}

-- | The user's IAM ARN.
ddtIamUserArn :: Lens' Deployment (Maybe Text)
ddtIamUserArn f x =
    f (_ddtIamUserArn x)
        <&> \y -> x { _ddtIamUserArn = y }
{-# INLINE ddtIamUserArn #-}

-- | A user-defined comment.
ddtComment :: Lens' Deployment (Maybe Text)
ddtComment f x =
    f (_ddtComment x)
        <&> \y -> x { _ddtComment = y }
{-# INLINE ddtComment #-}

-- | Used to specify a deployment operation.
ddtCommand :: Lens' Deployment (Maybe DeploymentCommand)
ddtCommand f x =
    f (_ddtCommand x)
        <&> \y -> x { _ddtCommand = y }
{-# INLINE ddtCommand #-}

-- | The deployment status: running successful failed.
ddtStatus :: Lens' Deployment (Maybe Text)
ddtStatus f x =
    f (_ddtStatus x)
        <&> \y -> x { _ddtStatus = y }
{-# INLINE ddtStatus #-}

-- | A string that contains user-defined custom JSON. It is used to override the
-- corresponding default stack configuration JSON values for stack. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
ddtCustomJson :: Lens' Deployment (Maybe Text)
ddtCustomJson f x =
    f (_ddtCustomJson x)
        <&> \y -> x { _ddtCustomJson = y }
{-# INLINE ddtCustomJson #-}

-- | The IDs of the target instances.
ddtInstanceIds :: Lens' Deployment ([Text])
ddtInstanceIds f x =
    f (_ddtInstanceIds x)
        <&> \y -> x { _ddtInstanceIds = y }
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
dcName f x =
    f (_dcName x)
        <&> \y -> x { _dcName = y }
{-# INLINE dcName #-}

-- | The arguments of those commands that take arguments. It should be set to a
-- JSON object with the following format: {"arg_name":["value1", "value2",
-- ...]}.
dcArgs :: Lens' DeploymentCommand (Map Text [Text])
dcArgs f x =
    f (_dcArgs x)
        <&> \y -> x { _dcArgs = y }
{-# INLINE dcArgs #-}

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
ejIp f x =
    f (_ejIp x)
        <&> \y -> x { _ejIp = y }
{-# INLINE ejIp #-}

-- | The name.
ejName :: Lens' ElasticIp (Maybe Text)
ejName f x =
    f (_ejName x)
        <&> \y -> x { _ejName = y }
{-# INLINE ejName #-}

-- | The domain.
ejDomain :: Lens' ElasticIp (Maybe Text)
ejDomain f x =
    f (_ejDomain x)
        <&> \y -> x { _ejDomain = y }
{-# INLINE ejDomain #-}

-- | The AWS region. For more information, see Regions and Endpoints.
ejRegion :: Lens' ElasticIp (Maybe Text)
ejRegion f x =
    f (_ejRegion x)
        <&> \y -> x { _ejRegion = y }
{-# INLINE ejRegion #-}

-- | The ID of the instance that the address is attached to.
ejInstanceId :: Lens' ElasticIp (Maybe Text)
ejInstanceId f x =
    f (_ejInstanceId x)
        <&> \y -> x { _ejInstanceId = y }
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
elcElasticLoadBalancerName f x =
    f (_elcElasticLoadBalancerName x)
        <&> \y -> x { _elcElasticLoadBalancerName = y }
{-# INLINE elcElasticLoadBalancerName #-}

-- | The instance's AWS region.
elcRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elcRegion f x =
    f (_elcRegion x)
        <&> \y -> x { _elcRegion = y }
{-# INLINE elcRegion #-}

-- | The instance's public DNS name.
elcDnsName :: Lens' ElasticLoadBalancer (Maybe Text)
elcDnsName f x =
    f (_elcDnsName x)
        <&> \y -> x { _elcDnsName = y }
{-# INLINE elcDnsName #-}

-- | The ID of the stack that the instance is associated with.
elcStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elcStackId f x =
    f (_elcStackId x)
        <&> \y -> x { _elcStackId = y }
{-# INLINE elcStackId #-}

-- | The ID of the layer that the instance is attached to.
elcLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elcLayerId f x =
    f (_elcLayerId x)
        <&> \y -> x { _elcLayerId = y }
{-# INLINE elcLayerId #-}

-- | The VPC ID.
elcVpcId :: Lens' ElasticLoadBalancer (Maybe Text)
elcVpcId f x =
    f (_elcVpcId x)
        <&> \y -> x { _elcVpcId = y }
{-# INLINE elcVpcId #-}

-- | A list of Availability Zones.
elcAvailabilityZones :: Lens' ElasticLoadBalancer ([Text])
elcAvailabilityZones f x =
    f (_elcAvailabilityZones x)
        <&> \y -> x { _elcAvailabilityZones = y }
{-# INLINE elcAvailabilityZones #-}

-- | A list of subnet IDs, if the stack is running in a VPC.
elcSubnetIds :: Lens' ElasticLoadBalancer ([Text])
elcSubnetIds f x =
    f (_elcSubnetIds x)
        <&> \y -> x { _elcSubnetIds = y }
{-# INLINE elcSubnetIds #-}

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elcEc2InstanceIds :: Lens' ElasticLoadBalancer ([Text])
elcEc2InstanceIds f x =
    f (_elcEc2InstanceIds x)
        <&> \y -> x { _elcEc2InstanceIds = y }
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
ieInstanceId f x =
    f (_ieInstanceId x)
        <&> \y -> x { _ieInstanceId = y }
{-# INLINE ieInstanceId #-}

-- | The ID of the associated Amazon EC2 instance.
ieEc2InstanceId :: Lens' Instance (Maybe Text)
ieEc2InstanceId f x =
    f (_ieEc2InstanceId x)
        <&> \y -> x { _ieEc2InstanceId = y }
{-# INLINE ieEc2InstanceId #-}

-- | The instance's virtualization type, paravirtual or hvm.
ieVirtualizationType :: Lens' Instance (Maybe Text)
ieVirtualizationType f x =
    f (_ieVirtualizationType x)
        <&> \y -> x { _ieVirtualizationType = y }
{-# INLINE ieVirtualizationType #-}

-- | The instance host name.
ieHostname :: Lens' Instance (Maybe Text)
ieHostname f x =
    f (_ieHostname x)
        <&> \y -> x { _ieHostname = y }
{-# INLINE ieHostname #-}

-- | The stack ID.
ieStackId :: Lens' Instance (Maybe Text)
ieStackId f x =
    f (_ieStackId x)
        <&> \y -> x { _ieStackId = y }
{-# INLINE ieStackId #-}

-- | An array containing the instance layer IDs.
ieLayerIds :: Lens' Instance ([Text])
ieLayerIds f x =
    f (_ieLayerIds x)
        <&> \y -> x { _ieLayerIds = y }
{-# INLINE ieLayerIds #-}

-- | An array containing the instance security group IDs.
ieSecurityGroupIds :: Lens' Instance ([Text])
ieSecurityGroupIds f x =
    f (_ieSecurityGroupIds x)
        <&> \y -> x { _ieSecurityGroupIds = y }
{-# INLINE ieSecurityGroupIds #-}

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that specify the various
-- types are in the API Name column of the Available Instance Types table.
ieInstanceType :: Lens' Instance (Maybe Text)
ieInstanceType f x =
    f (_ieInstanceType x)
        <&> \y -> x { _ieInstanceType = y }
{-# INLINE ieInstanceType #-}

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs,
-- see Using Identifiers.
ieInstanceProfileArn :: Lens' Instance (Maybe Text)
ieInstanceProfileArn f x =
    f (_ieInstanceProfileArn x)
        <&> \y -> x { _ieInstanceProfileArn = y }
{-# INLINE ieInstanceProfileArn #-}

-- | The instance status: requested booting running_setup online setup_failed
-- start_failed terminating terminated stopped connection_lost.
ieStatus :: Lens' Instance (Maybe Text)
ieStatus f x =
    f (_ieStatus x)
        <&> \y -> x { _ieStatus = y }
{-# INLINE ieStatus #-}

-- | The instance operating system.
ieOs :: Lens' Instance (Maybe Text)
ieOs f x =
    f (_ieOs x)
        <&> \y -> x { _ieOs = y }
{-# INLINE ieOs #-}

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
-- For more information, see Instances.
ieAmiId :: Lens' Instance (Maybe Text)
ieAmiId f x =
    f (_ieAmiId x)
        <&> \y -> x { _ieAmiId = y }
{-# INLINE ieAmiId #-}

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
ieAvailabilityZone :: Lens' Instance (Maybe Text)
ieAvailabilityZone f x =
    f (_ieAvailabilityZone x)
        <&> \y -> x { _ieAvailabilityZone = y }
{-# INLINE ieAvailabilityZone #-}

-- | The instance's subnet ID, if the stack is running in a VPC.
ieSubnetId :: Lens' Instance (Maybe Text)
ieSubnetId f x =
    f (_ieSubnetId x)
        <&> \y -> x { _ieSubnetId = y }
{-# INLINE ieSubnetId #-}

-- | The instance public DNS name.
iePublicDns :: Lens' Instance (Maybe Text)
iePublicDns f x =
    f (_iePublicDns x)
        <&> \y -> x { _iePublicDns = y }
{-# INLINE iePublicDns #-}

-- | The instance private DNS name.
iePrivateDns :: Lens' Instance (Maybe Text)
iePrivateDns f x =
    f (_iePrivateDns x)
        <&> \y -> x { _iePrivateDns = y }
{-# INLINE iePrivateDns #-}

-- | The instance public IP address.
iePublicIp :: Lens' Instance (Maybe Text)
iePublicIp f x =
    f (_iePublicIp x)
        <&> \y -> x { _iePublicIp = y }
{-# INLINE iePublicIp #-}

-- | The instance private IP address.
iePrivateIp :: Lens' Instance (Maybe Text)
iePrivateIp f x =
    f (_iePrivateIp x)
        <&> \y -> x { _iePrivateIp = y }
{-# INLINE iePrivateIp #-}

-- | The instance Elastic IP address .
ieElasticIp :: Lens' Instance (Maybe Text)
ieElasticIp f x =
    f (_ieElasticIp x)
        <&> \y -> x { _ieElasticIp = y }
{-# INLINE ieElasticIp #-}

-- | The instance's auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. LoadBasedAutoScaling: A
-- load-based auto scaling instance, which is started and stopped based on
-- load metrics.
ieAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
ieAutoScalingType f x =
    f (_ieAutoScalingType x)
        <&> \y -> x { _ieAutoScalingType = y }
{-# INLINE ieAutoScalingType #-}

-- | The instance SSH key name.
ieSshKeyName :: Lens' Instance (Maybe Text)
ieSshKeyName f x =
    f (_ieSshKeyName x)
        <&> \y -> x { _ieSshKeyName = y }
{-# INLINE ieSshKeyName #-}

-- | The SSH key's RSA fingerprint.
ieSshHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
ieSshHostRsaKeyFingerprint f x =
    f (_ieSshHostRsaKeyFingerprint x)
        <&> \y -> x { _ieSshHostRsaKeyFingerprint = y }
{-# INLINE ieSshHostRsaKeyFingerprint #-}

-- | The SSH key's DSA fingerprint.
ieSshHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
ieSshHostDsaKeyFingerprint f x =
    f (_ieSshHostDsaKeyFingerprint x)
        <&> \y -> x { _ieSshHostDsaKeyFingerprint = y }
{-# INLINE ieSshHostDsaKeyFingerprint #-}

-- | The time that the instance was created.
ieCreatedAt :: Lens' Instance (Maybe Text)
ieCreatedAt f x =
    f (_ieCreatedAt x)
        <&> \y -> x { _ieCreatedAt = y }
{-# INLINE ieCreatedAt #-}

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
ieLastServiceErrorId :: Lens' Instance (Maybe Text)
ieLastServiceErrorId f x =
    f (_ieLastServiceErrorId x)
        <&> \y -> x { _ieLastServiceErrorId = y }
{-# INLINE ieLastServiceErrorId #-}

-- | The instance architecture, "i386" or "x86_64".
ieArchitecture :: Lens' Instance (Maybe Architecture)
ieArchitecture f x =
    f (_ieArchitecture x)
        <&> \y -> x { _ieArchitecture = y }
{-# INLINE ieArchitecture #-}

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
ieRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
ieRootDeviceType f x =
    f (_ieRootDeviceType x)
        <&> \y -> x { _ieRootDeviceType = y }
{-# INLINE ieRootDeviceType #-}

-- | The root device volume ID.
ieRootDeviceVolumeId :: Lens' Instance (Maybe Text)
ieRootDeviceVolumeId f x =
    f (_ieRootDeviceVolumeId x)
        <&> \y -> x { _ieRootDeviceVolumeId = y }
{-# INLINE ieRootDeviceVolumeId #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
ieInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
ieInstallUpdatesOnBoot f x =
    f (_ieInstallUpdatesOnBoot x)
        <&> \y -> x { _ieInstallUpdatesOnBoot = y }
{-# INLINE ieInstallUpdatesOnBoot #-}

-- | Whether this is an Amazon EBS-optimized instance.
ieEbsOptimized :: Lens' Instance (Maybe Bool)
ieEbsOptimized f x =
    f (_ieEbsOptimized x)
        <&> \y -> x { _ieEbsOptimized = y }
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
icBooting f x =
    f (_icBooting x)
        <&> \y -> x { _icBooting = y }
{-# INLINE icBooting #-}

-- | The number of instances with connection_lost status.
icConnectionLost :: Lens' InstancesCount (Maybe Integer)
icConnectionLost f x =
    f (_icConnectionLost x)
        <&> \y -> x { _icConnectionLost = y }
{-# INLINE icConnectionLost #-}

-- | The number of instances with online status.
icOnline :: Lens' InstancesCount (Maybe Integer)
icOnline f x =
    f (_icOnline x)
        <&> \y -> x { _icOnline = y }
{-# INLINE icOnline #-}

-- | The number of instances with pending status.
icPending :: Lens' InstancesCount (Maybe Integer)
icPending f x =
    f (_icPending x)
        <&> \y -> x { _icPending = y }
{-# INLINE icPending #-}

-- | The number of instances with rebooting status.
icRebooting :: Lens' InstancesCount (Maybe Integer)
icRebooting f x =
    f (_icRebooting x)
        <&> \y -> x { _icRebooting = y }
{-# INLINE icRebooting #-}

-- | The number of instances with requested status.
icRequested :: Lens' InstancesCount (Maybe Integer)
icRequested f x =
    f (_icRequested x)
        <&> \y -> x { _icRequested = y }
{-# INLINE icRequested #-}

-- | The number of instances with running_setup status.
icRunningSetup :: Lens' InstancesCount (Maybe Integer)
icRunningSetup f x =
    f (_icRunningSetup x)
        <&> \y -> x { _icRunningSetup = y }
{-# INLINE icRunningSetup #-}

-- | The number of instances with setup_failed status.
icSetupFailed :: Lens' InstancesCount (Maybe Integer)
icSetupFailed f x =
    f (_icSetupFailed x)
        <&> \y -> x { _icSetupFailed = y }
{-# INLINE icSetupFailed #-}

-- | The number of instances with shutting_down status.
icShuttingDown :: Lens' InstancesCount (Maybe Integer)
icShuttingDown f x =
    f (_icShuttingDown x)
        <&> \y -> x { _icShuttingDown = y }
{-# INLINE icShuttingDown #-}

-- | The number of instances with start_failed status.
icStartFailed :: Lens' InstancesCount (Maybe Integer)
icStartFailed f x =
    f (_icStartFailed x)
        <&> \y -> x { _icStartFailed = y }
{-# INLINE icStartFailed #-}

-- | The number of instances with stopped status.
icStopped :: Lens' InstancesCount (Maybe Integer)
icStopped f x =
    f (_icStopped x)
        <&> \y -> x { _icStopped = y }
{-# INLINE icStopped #-}

-- | The number of instances with stopping status.
icStopping :: Lens' InstancesCount (Maybe Integer)
icStopping f x =
    f (_icStopping x)
        <&> \y -> x { _icStopping = y }
{-# INLINE icStopping #-}

-- | The number of instances with terminated status.
icTerminated :: Lens' InstancesCount (Maybe Integer)
icTerminated f x =
    f (_icTerminated x)
        <&> \y -> x { _icTerminated = y }
{-# INLINE icTerminated #-}

-- | The number of instances with terminating status.
icTerminating :: Lens' InstancesCount (Maybe Integer)
icTerminating f x =
    f (_icTerminating x)
        <&> \y -> x { _icTerminating = y }
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
lrStackId f x =
    f (_lrStackId x)
        <&> \y -> x { _lrStackId = y }
{-# INLINE lrStackId #-}

-- | The layer ID.
lrLayerId :: Lens' Layer (Maybe Text)
lrLayerId f x =
    f (_lrLayerId x)
        <&> \y -> x { _lrLayerId = y }
{-# INLINE lrLayerId #-}

-- | The layer type, which must be one of the following: Custom
-- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster NodeJsAppServer
-- PhpAppServer RailsAppServer WebServer.
lrType :: Lens' Layer (Maybe LayerType)
lrType f x =
    f (_lrType x)
        <&> \y -> x { _lrType = y }
{-# INLINE lrType #-}

-- | The layer name.
lrName :: Lens' Layer (Maybe Text)
lrName f x =
    f (_lrName x)
        <&> \y -> x { _lrName = y }
{-# INLINE lrName #-}

-- | The layer short name.
lrShortname :: Lens' Layer (Maybe Text)
lrShortname f x =
    f (_lrShortname x)
        <&> \y -> x { _lrShortname = y }
{-# INLINE lrShortname #-}

-- | The layer attributes.
lrAttributes :: Lens' Layer (Map LayerAttributesKeys Text)
lrAttributes f x =
    f (_lrAttributes x)
        <&> \y -> x { _lrAttributes = y }
{-# INLINE lrAttributes #-}

-- | The ARN of the default IAM profile to be used for the layer's EC2
-- instances. For more information about IAM ARNs, see Using Identifiers.
lrCustomInstanceProfileArn :: Lens' Layer (Maybe Text)
lrCustomInstanceProfileArn f x =
    f (_lrCustomInstanceProfileArn x)
        <&> \y -> x { _lrCustomInstanceProfileArn = y }
{-# INLINE lrCustomInstanceProfileArn #-}

-- | An array containing the layer's custom security group IDs.
lrCustomSecurityGroupIds :: Lens' Layer ([Text])
lrCustomSecurityGroupIds f x =
    f (_lrCustomSecurityGroupIds x)
        <&> \y -> x { _lrCustomSecurityGroupIds = y }
{-# INLINE lrCustomSecurityGroupIds #-}

-- | An array containing the layer's security group names.
lrDefaultSecurityGroupNames :: Lens' Layer ([Text])
lrDefaultSecurityGroupNames f x =
    f (_lrDefaultSecurityGroupNames x)
        <&> \y -> x { _lrDefaultSecurityGroupNames = y }
{-# INLINE lrDefaultSecurityGroupNames #-}

-- | An array of Package objects that describe the layer's packages.
lrPackages :: Lens' Layer ([Text])
lrPackages f x =
    f (_lrPackages x)
        <&> \y -> x { _lrPackages = y }
{-# INLINE lrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
lrVolumeConfigurations :: Lens' Layer ([VolumeConfiguration])
lrVolumeConfigurations f x =
    f (_lrVolumeConfigurations x)
        <&> \y -> x { _lrVolumeConfigurations = y }
{-# INLINE lrVolumeConfigurations #-}

-- | Whether auto healing is disabled for the layer.
lrEnableAutoHealing :: Lens' Layer (Maybe Bool)
lrEnableAutoHealing f x =
    f (_lrEnableAutoHealing x)
        <&> \y -> x { _lrEnableAutoHealing = y }
{-# INLINE lrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
lrAutoAssignElasticIps :: Lens' Layer (Maybe Bool)
lrAutoAssignElasticIps f x =
    f (_lrAutoAssignElasticIps x)
        <&> \y -> x { _lrAutoAssignElasticIps = y }
{-# INLINE lrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
lrAutoAssignPublicIps :: Lens' Layer (Maybe Bool)
lrAutoAssignPublicIps f x =
    f (_lrAutoAssignPublicIps x)
        <&> \y -> x { _lrAutoAssignPublicIps = y }
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
lrDefaultRecipes f x =
    f (_lrDefaultRecipes x)
        <&> \y -> x { _lrDefaultRecipes = y }
{-# INLINE lrDefaultRecipes #-}

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
lrCustomRecipes :: Lens' Layer (Maybe Recipes)
lrCustomRecipes f x =
    f (_lrCustomRecipes x)
        <&> \y -> x { _lrCustomRecipes = y }
{-# INLINE lrCustomRecipes #-}

-- | Date when the layer was created.
lrCreatedAt :: Lens' Layer (Maybe Text)
lrCreatedAt f x =
    f (_lrCreatedAt x)
        <&> \y -> x { _lrCreatedAt = y }
{-# INLINE lrCreatedAt #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
lrInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
lrInstallUpdatesOnBoot f x =
    f (_lrInstallUpdatesOnBoot x)
        <&> \y -> x { _lrInstallUpdatesOnBoot = y }
{-# INLINE lrInstallUpdatesOnBoot #-}

-- | Whether the layer uses Amazon EBS-optimized instances.
lrUseEbsOptimizedInstances :: Lens' Layer (Maybe Bool)
lrUseEbsOptimizedInstances f x =
    f (_lrUseEbsOptimizedInstances x)
        <&> \y -> x { _lrUseEbsOptimizedInstances = y }
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
lbasdLayerId f x =
    f (_lbasdLayerId x)
        <&> \y -> x { _lbasdLayerId = y }
{-# INLINE lbasdLayerId #-}

-- | Whether load-based auto scaling is enabled for the layer.
lbasdEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbasdEnable f x =
    f (_lbasdEnable x)
        <&> \y -> x { _lbasdEnable = y }
{-# INLINE lbasdEnable #-}

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the number
-- of instances.
lbasdUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbasdUpScaling f x =
    f (_lbasdUpScaling x)
        <&> \y -> x { _lbasdUpScaling = y }
{-# INLINE lbasdUpScaling #-}

-- | A LoadBasedAutoscalingInstruction object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the number
-- of instances.
lbasdDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbasdDownScaling f x =
    f (_lbasdDownScaling x)
        <&> \y -> x { _lbasdDownScaling = y }
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
pnStackId f x =
    f (_pnStackId x)
        <&> \y -> x { _pnStackId = y }
{-# INLINE pnStackId #-}

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see Using Identifiers.
pnIamUserArn :: Lens' Permission (Maybe Text)
pnIamUserArn f x =
    f (_pnIamUserArn x)
        <&> \y -> x { _pnIamUserArn = y }
{-# INLINE pnIamUserArn #-}

-- | Whether the user can use SSH.
pnAllowSsh :: Lens' Permission (Maybe Bool)
pnAllowSsh f x =
    f (_pnAllowSsh x)
        <&> \y -> x { _pnAllowSsh = y }
{-# INLINE pnAllowSsh #-}

-- | Whether the user can use sudo.
pnAllowSudo :: Lens' Permission (Maybe Bool)
pnAllowSudo f x =
    f (_pnAllowSudo x)
        <&> \y -> x { _pnAllowSudo = y }
{-# INLINE pnAllowSudo #-}

-- | The user's permission level, which must be the following: deny show deploy
-- manage iam_only For more information on the permissions associated with
-- these levels, see Managing User Permissions.
pnLevel :: Lens' Permission (Maybe Text)
pnLevel f x =
    f (_pnLevel x)
        <&> \y -> x { _pnLevel = y }
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
rbRaidArrayId f x =
    f (_rbRaidArrayId x)
        <&> \y -> x { _rbRaidArrayId = y }
{-# INLINE rbRaidArrayId #-}

-- | The instance ID.
rbInstanceId :: Lens' RaidArray (Maybe Text)
rbInstanceId f x =
    f (_rbInstanceId x)
        <&> \y -> x { _rbInstanceId = y }
{-# INLINE rbInstanceId #-}

-- | The array name.
rbName :: Lens' RaidArray (Maybe Text)
rbName f x =
    f (_rbName x)
        <&> \y -> x { _rbName = y }
{-# INLINE rbName #-}

-- | The RAID level.
rbRaidLevel :: Lens' RaidArray (Maybe Integer)
rbRaidLevel f x =
    f (_rbRaidLevel x)
        <&> \y -> x { _rbRaidLevel = y }
{-# INLINE rbRaidLevel #-}

-- | The number of disks in the array.
rbNumberOfDisks :: Lens' RaidArray (Maybe Integer)
rbNumberOfDisks f x =
    f (_rbNumberOfDisks x)
        <&> \y -> x { _rbNumberOfDisks = y }
{-# INLINE rbNumberOfDisks #-}

-- | The array's size.
rbSize :: Lens' RaidArray (Maybe Integer)
rbSize f x =
    f (_rbSize x)
        <&> \y -> x { _rbSize = y }
{-# INLINE rbSize #-}

-- | The array's Linux device. For example /dev/mdadm0.
rbDevice :: Lens' RaidArray (Maybe Text)
rbDevice f x =
    f (_rbDevice x)
        <&> \y -> x { _rbDevice = y }
{-# INLINE rbDevice #-}

-- | The array's mount point.
rbMountPoint :: Lens' RaidArray (Maybe Text)
rbMountPoint f x =
    f (_rbMountPoint x)
        <&> \y -> x { _rbMountPoint = y }
{-# INLINE rbMountPoint #-}

-- | The array's Availability Zone. For more information, see Regions and
-- Endpoints.
rbAvailabilityZone :: Lens' RaidArray (Maybe Text)
rbAvailabilityZone f x =
    f (_rbAvailabilityZone x)
        <&> \y -> x { _rbAvailabilityZone = y }
{-# INLINE rbAvailabilityZone #-}

-- | When the RAID array was created.
rbCreatedAt :: Lens' RaidArray (Maybe Text)
rbCreatedAt f x =
    f (_rbCreatedAt x)
        <&> \y -> x { _rbCreatedAt = y }
{-# INLINE rbCreatedAt #-}

-- | The volume type, standard or PIOPS.
rbVolumeType :: Lens' RaidArray (Maybe Text)
rbVolumeType f x =
    f (_rbVolumeType x)
        <&> \y -> x { _rbVolumeType = y }
{-# INLINE rbVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
rbIops :: Lens' RaidArray (Maybe Integer)
rbIops f x =
    f (_rbIops x)
        <&> \y -> x { _rbIops = y }
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
rdjRdsDbInstanceArn f x =
    f (_rdjRdsDbInstanceArn x)
        <&> \y -> x { _rdjRdsDbInstanceArn = y }
{-# INLINE rdjRdsDbInstanceArn #-}

-- | The DB instance identifier.
rdjDbInstanceIdentifier :: Lens' RdsDbInstance (Maybe Text)
rdjDbInstanceIdentifier f x =
    f (_rdjDbInstanceIdentifier x)
        <&> \y -> x { _rdjDbInstanceIdentifier = y }
{-# INLINE rdjDbInstanceIdentifier #-}

-- | The master user name.
rdjDbUser :: Lens' RdsDbInstance (Maybe Text)
rdjDbUser f x =
    f (_rdjDbUser x)
        <&> \y -> x { _rdjDbUser = y }
{-# INLINE rdjDbUser #-}

-- | The database password.
rdjDbPassword :: Lens' RdsDbInstance (Maybe Text)
rdjDbPassword f x =
    f (_rdjDbPassword x)
        <&> \y -> x { _rdjDbPassword = y }
{-# INLINE rdjDbPassword #-}

-- | The instance's AWS region.
rdjRegion :: Lens' RdsDbInstance (Maybe Text)
rdjRegion f x =
    f (_rdjRegion x)
        <&> \y -> x { _rdjRegion = y }
{-# INLINE rdjRegion #-}

-- | The instance's address.
rdjAddress :: Lens' RdsDbInstance (Maybe Text)
rdjAddress f x =
    f (_rdjAddress x)
        <&> \y -> x { _rdjAddress = y }
{-# INLINE rdjAddress #-}

-- | The instance's database engine.
rdjEngine :: Lens' RdsDbInstance (Maybe Text)
rdjEngine f x =
    f (_rdjEngine x)
        <&> \y -> x { _rdjEngine = y }
{-# INLINE rdjEngine #-}

-- | The ID of the stack that the instance is registered with.
rdjStackId :: Lens' RdsDbInstance (Maybe Text)
rdjStackId f x =
    f (_rdjStackId x)
        <&> \y -> x { _rdjStackId = y }
{-# INLINE rdjStackId #-}

-- | Set to true if AWS OpsWorks was unable to discover the Amazon RDS instance.
-- AWS OpsWorks attempts to discover the instance only once. If this value is
-- set to true, you must deregister the instance and then register it again.
rdjMissingOnRds :: Lens' RdsDbInstance (Maybe Bool)
rdjMissingOnRds f x =
    f (_rdjMissingOnRds x)
        <&> \y -> x { _rdjMissingOnRds = y }
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
rSetup f x =
    f (_rSetup x)
        <&> \y -> x { _rSetup = y }
{-# INLINE rSetup #-}

-- | An array of custom recipe names to be run following a configure event.
rConfigure :: Lens' Recipes ([Text])
rConfigure f x =
    f (_rConfigure x)
        <&> \y -> x { _rConfigure = y }
{-# INLINE rConfigure #-}

-- | An array of custom recipe names to be run following a deploy event.
rDeploy :: Lens' Recipes ([Text])
rDeploy f x =
    f (_rDeploy x)
        <&> \y -> x { _rDeploy = y }
{-# INLINE rDeploy #-}

-- | An array of custom recipe names to be run following a undeploy event.
rUndeploy :: Lens' Recipes ([Text])
rUndeploy f x =
    f (_rUndeploy x)
        <&> \y -> x { _rUndeploy = y }
{-# INLINE rUndeploy #-}

-- | An array of custom recipe names to be run following a shutdown event.
rShutdown :: Lens' Recipes ([Text])
rShutdown f x =
    f (_rShutdown x)
        <&> \y -> x { _rShutdown = y }
{-# INLINE rShutdown #-}

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
supIamUserArn f x =
    f (_supIamUserArn x)
        <&> \y -> x { _supIamUserArn = y }
{-# INLINE supIamUserArn #-}

-- | The user's name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName f x =
    f (_supName x)
        <&> \y -> x { _supName = y }
{-# INLINE supName #-}

-- | The user's SSH user name.
supSshUsername :: Lens' SelfUserProfile (Maybe Text)
supSshUsername f x =
    f (_supSshUsername x)
        <&> \y -> x { _supSshUsername = y }
{-# INLINE supSshUsername #-}

-- | The user's SSH public key.
supSshPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSshPublicKey f x =
    f (_supSshPublicKey x)
        <&> \y -> x { _supSshPublicKey = y }
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
sssssssssssssssxServiceErrorId f x =
    f (_sssssssssssssssxServiceErrorId x)
        <&> \y -> x { _sssssssssssssssxServiceErrorId = y }
{-# INLINE sssssssssssssssxServiceErrorId #-}

-- | The stack ID.
sssssssssssssssxStackId :: Lens' ServiceError (Maybe Text)
sssssssssssssssxStackId f x =
    f (_sssssssssssssssxStackId x)
        <&> \y -> x { _sssssssssssssssxStackId = y }
{-# INLINE sssssssssssssssxStackId #-}

-- | The instance ID.
sssssssssssssssxInstanceId :: Lens' ServiceError (Maybe Text)
sssssssssssssssxInstanceId f x =
    f (_sssssssssssssssxInstanceId x)
        <&> \y -> x { _sssssssssssssssxInstanceId = y }
{-# INLINE sssssssssssssssxInstanceId #-}

-- | The error type.
sssssssssssssssxType :: Lens' ServiceError (Maybe Text)
sssssssssssssssxType f x =
    f (_sssssssssssssssxType x)
        <&> \y -> x { _sssssssssssssssxType = y }
{-# INLINE sssssssssssssssxType #-}

-- | A message that describes the error.
sssssssssssssssxMessage :: Lens' ServiceError (Maybe Text)
sssssssssssssssxMessage f x =
    f (_sssssssssssssssxMessage x)
        <&> \y -> x { _sssssssssssssssxMessage = y }
{-# INLINE sssssssssssssssxMessage #-}

-- | When the error occurred.
sssssssssssssssxCreatedAt :: Lens' ServiceError (Maybe Text)
sssssssssssssssxCreatedAt f x =
    f (_sssssssssssssssxCreatedAt x)
        <&> \y -> x { _sssssssssssssssxCreatedAt = y }
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
seType f x =
    f (_seType x)
        <&> \y -> x { _seType = y }
{-# INLINE seType #-}

-- | The source URL.
seUrl :: Lens' Source (Maybe Text)
seUrl f x =
    f (_seUrl x)
        <&> \y -> x { _seUrl = y }
{-# INLINE seUrl #-}

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Username to the appropriate IAM access key ID. For HTTP bundles, Git
-- repositories, and Subversion repositories, set Username to the user name.
seUsername :: Lens' Source (Maybe Text)
seUsername f x =
    f (_seUsername x)
        <&> \y -> x { _seUsername = y }
{-# INLINE seUsername #-}

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Password to the appropriate IAM secret access key. For HTTP bundles and
-- Subversion repositories, set Password to the password. For more information
-- on how to safely handle IAM credentials, see .
sePassword :: Lens' Source (Maybe Text)
sePassword f x =
    f (_sePassword x)
        <&> \y -> x { _sePassword = y }
{-# INLINE sePassword #-}

-- | The repository's SSH key.
seSshKey :: Lens' Source (Maybe Text)
seSshKey f x =
    f (_seSshKey x)
        <&> \y -> x { _seSshKey = y }
{-# INLINE seSshKey #-}

-- | The application's version. AWS OpsWorks enables you to easily deploy new
-- versions of an application. One of the simplest approaches is to have
-- branches or revisions in your repository that represent different versions
-- that can potentially be deployed.
seRevision :: Lens' Source (Maybe Text)
seRevision f x =
    f (_seRevision x)
        <&> \y -> x { _seRevision = y }
{-# INLINE seRevision #-}

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
scCertificate f x =
    f (_scCertificate x)
        <&> \y -> x { _scCertificate = y }
{-# INLINE scCertificate #-}

-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey :: Lens' SslConfiguration (Text)
scPrivateKey f x =
    f (_scPrivateKey x)
        <&> \y -> x { _scPrivateKey = y }
{-# INLINE scPrivateKey #-}

-- | Optional. Can be used to specify an intermediate certificate authority key
-- or client authentication.
scChain :: Lens' SslConfiguration (Maybe Text)
scChain f x =
    f (_scChain x)
        <&> \y -> x { _scChain = y }
{-# INLINE scChain #-}

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
ssssssssssssssssoStackId f x =
    f (_ssssssssssssssssoStackId x)
        <&> \y -> x { _ssssssssssssssssoStackId = y }
{-# INLINE ssssssssssssssssoStackId #-}

-- | The stack name.
ssssssssssssssssoName :: Lens' Stack (Maybe Text)
ssssssssssssssssoName f x =
    f (_ssssssssssssssssoName x)
        <&> \y -> x { _ssssssssssssssssoName = y }
{-# INLINE ssssssssssssssssoName #-}

-- | The stack's ARN.
ssssssssssssssssoArn :: Lens' Stack (Maybe Text)
ssssssssssssssssoArn f x =
    f (_ssssssssssssssssoArn x)
        <&> \y -> x { _ssssssssssssssssoArn = y }
{-# INLINE ssssssssssssssssoArn #-}

-- | The stack AWS region, such as "us-east-1". For more information about AWS
-- regions, see Regions and Endpoints.
ssssssssssssssssoRegion :: Lens' Stack (Maybe Text)
ssssssssssssssssoRegion f x =
    f (_ssssssssssssssssoRegion x)
        <&> \y -> x { _ssssssssssssssssoRegion = y }
{-# INLINE ssssssssssssssssoRegion #-}

-- | The VPC ID, if the stack is running in a VPC.
ssssssssssssssssoVpcId :: Lens' Stack (Maybe Text)
ssssssssssssssssoVpcId f x =
    f (_ssssssssssssssssoVpcId x)
        <&> \y -> x { _ssssssssssssssssoVpcId = y }
{-# INLINE ssssssssssssssssoVpcId #-}

-- | The stack's attributes.
ssssssssssssssssoAttributes :: Lens' Stack (Map StackAttributesKeys Text)
ssssssssssssssssoAttributes f x =
    f (_ssssssssssssssssoAttributes x)
        <&> \y -> x { _ssssssssssssssssoAttributes = y }
{-# INLINE ssssssssssssssssoAttributes #-}

-- | The stack AWS Identity and Access Management (IAM) role.
ssssssssssssssssoServiceRoleArn :: Lens' Stack (Maybe Text)
ssssssssssssssssoServiceRoleArn f x =
    f (_ssssssssssssssssoServiceRoleArn x)
        <&> \y -> x { _ssssssssssssssssoServiceRoleArn = y }
{-# INLINE ssssssssssssssssoServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
ssssssssssssssssoDefaultInstanceProfileArn :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultInstanceProfileArn f x =
    f (_ssssssssssssssssoDefaultInstanceProfileArn x)
        <&> \y -> x { _ssssssssssssssssoDefaultInstanceProfileArn = y }
{-# INLINE ssssssssssssssssoDefaultInstanceProfileArn #-}

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
ssssssssssssssssoDefaultOs :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultOs f x =
    f (_ssssssssssssssssoDefaultOs x)
        <&> \y -> x { _ssssssssssssssssoDefaultOs = y }
{-# INLINE ssssssssssssssssoDefaultOs #-}

-- | The stack host name theme, with spaces replaced by underscores.
ssssssssssssssssoHostnameTheme :: Lens' Stack (Maybe Text)
ssssssssssssssssoHostnameTheme f x =
    f (_ssssssssssssssssoHostnameTheme x)
        <&> \y -> x { _ssssssssssssssssoHostnameTheme = y }
{-# INLINE ssssssssssssssssoHostnameTheme #-}

-- | The stack's default Availability Zone. For more information, see Regions
-- and Endpoints.
ssssssssssssssssoDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultAvailabilityZone f x =
    f (_ssssssssssssssssoDefaultAvailabilityZone x)
        <&> \y -> x { _ssssssssssssssssoDefaultAvailabilityZone = y }
{-# INLINE ssssssssssssssssoDefaultAvailabilityZone #-}

-- | The default subnet ID, if the stack is running in a VPC.
ssssssssssssssssoDefaultSubnetId :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultSubnetId f x =
    f (_ssssssssssssssssoDefaultSubnetId x)
        <&> \y -> x { _ssssssssssssssssoDefaultSubnetId = y }
{-# INLINE ssssssssssssssssoDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
ssssssssssssssssoCustomJson :: Lens' Stack (Maybe Text)
ssssssssssssssssoCustomJson f x =
    f (_ssssssssssssssssoCustomJson x)
        <&> \y -> x { _ssssssssssssssssoCustomJson = y }
{-# INLINE ssssssssssssssssoCustomJson #-}

-- | The configuration manager.
ssssssssssssssssoConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
ssssssssssssssssoConfigurationManager f x =
    f (_ssssssssssssssssoConfigurationManager x)
        <&> \y -> x { _ssssssssssssssssoConfigurationManager = y }
{-# INLINE ssssssssssssssssoConfigurationManager #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version. For more information, see Create a New Stack.
ssssssssssssssssoChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
ssssssssssssssssoChefConfiguration f x =
    f (_ssssssssssssssssoChefConfiguration x)
        <&> \y -> x { _ssssssssssssssssoChefConfiguration = y }
{-# INLINE ssssssssssssssssoChefConfiguration #-}

-- | Whether the stack uses custom cookbooks.
ssssssssssssssssoUseCustomCookbooks :: Lens' Stack (Maybe Bool)
ssssssssssssssssoUseCustomCookbooks f x =
    f (_ssssssssssssssssoUseCustomCookbooks x)
        <&> \y -> x { _ssssssssssssssssoUseCustomCookbooks = y }
{-# INLINE ssssssssssssssssoUseCustomCookbooks #-}

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack's layers.
ssssssssssssssssoUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
ssssssssssssssssoUseOpsworksSecurityGroups f x =
    f (_ssssssssssssssssoUseOpsworksSecurityGroups x)
        <&> \y -> x { _ssssssssssssssssoUseOpsworksSecurityGroups = y }
{-# INLINE ssssssssssssssssoUseOpsworksSecurityGroups #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
ssssssssssssssssoCustomCookbooksSource :: Lens' Stack (Maybe Source)
ssssssssssssssssoCustomCookbooksSource f x =
    f (_ssssssssssssssssoCustomCookbooksSource x)
        <&> \y -> x { _ssssssssssssssssoCustomCookbooksSource = y }
{-# INLINE ssssssssssssssssoCustomCookbooksSource #-}

-- | A default SSH key for the stack's instances. You can override this value
-- when you create or update an instance.
ssssssssssssssssoDefaultSshKeyName :: Lens' Stack (Maybe Text)
ssssssssssssssssoDefaultSshKeyName f x =
    f (_ssssssssssssssssoDefaultSshKeyName x)
        <&> \y -> x { _ssssssssssssssssoDefaultSshKeyName = y }
{-# INLINE ssssssssssssssssoDefaultSshKeyName #-}

-- | Date when the stack was created.
ssssssssssssssssoCreatedAt :: Lens' Stack (Maybe Text)
ssssssssssssssssoCreatedAt f x =
    f (_ssssssssssssssssoCreatedAt x)
        <&> \y -> x { _ssssssssssssssssoCreatedAt = y }
{-# INLINE ssssssssssssssssoCreatedAt #-}

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
ssssssssssssssssoDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
ssssssssssssssssoDefaultRootDeviceType f x =
    f (_ssssssssssssssssoDefaultRootDeviceType x)
        <&> \y -> x { _ssssssssssssssssoDefaultRootDeviceType = y }
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
scmName f x =
    f (_scmName x)
        <&> \y -> x { _scmName = y }
{-# INLINE scmName #-}

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion f x =
    f (_scmVersion x)
        <&> \y -> x { _scmVersion = y }
{-# INLINE scmVersion #-}

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
ssssssssssssssssyStackId f x =
    f (_ssssssssssssssssyStackId x)
        <&> \y -> x { _ssssssssssssssssyStackId = y }
{-# INLINE ssssssssssssssssyStackId #-}

-- | The stack name.
ssssssssssssssssyName :: Lens' StackSummary (Maybe Text)
ssssssssssssssssyName f x =
    f (_ssssssssssssssssyName x)
        <&> \y -> x { _ssssssssssssssssyName = y }
{-# INLINE ssssssssssssssssyName #-}

-- | The stack's ARN.
ssssssssssssssssyArn :: Lens' StackSummary (Maybe Text)
ssssssssssssssssyArn f x =
    f (_ssssssssssssssssyArn x)
        <&> \y -> x { _ssssssssssssssssyArn = y }
{-# INLINE ssssssssssssssssyArn #-}

-- | The number of layers.
ssssssssssssssssyLayersCount :: Lens' StackSummary (Maybe Integer)
ssssssssssssssssyLayersCount f x =
    f (_ssssssssssssssssyLayersCount x)
        <&> \y -> x { _ssssssssssssssssyLayersCount = y }
{-# INLINE ssssssssssssssssyLayersCount #-}

-- | The number of apps.
ssssssssssssssssyAppsCount :: Lens' StackSummary (Maybe Integer)
ssssssssssssssssyAppsCount f x =
    f (_ssssssssssssssssyAppsCount x)
        <&> \y -> x { _ssssssssssssssssyAppsCount = y }
{-# INLINE ssssssssssssssssyAppsCount #-}

-- | An InstancesCount object with the number of instances in each status.
ssssssssssssssssyInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssssssssssssssssyInstancesCount f x =
    f (_ssssssssssssssssyInstancesCount x)
        <&> \y -> x { _ssssssssssssssssyInstancesCount = y }
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
tbasdInstanceId f x =
    f (_tbasdInstanceId x)
        <&> \y -> x { _tbasdInstanceId = y }
{-# INLINE tbasdInstanceId #-}

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
tbasdAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbasdAutoScalingSchedule f x =
    f (_tbasdAutoScalingSchedule x)
        <&> \y -> x { _tbasdAutoScalingSchedule = y }
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
uxIamUserArn f x =
    f (_uxIamUserArn x)
        <&> \y -> x { _uxIamUserArn = y }
{-# INLINE uxIamUserArn #-}

-- | The user's name.
uxName :: Lens' UserProfile (Maybe Text)
uxName f x =
    f (_uxName x)
        <&> \y -> x { _uxName = y }
{-# INLINE uxName #-}

-- | The user's SSH user name.
uxSshUsername :: Lens' UserProfile (Maybe Text)
uxSshUsername f x =
    f (_uxSshUsername x)
        <&> \y -> x { _uxSshUsername = y }
{-# INLINE uxSshUsername #-}

-- | The user's SSH public key.
uxSshPublicKey :: Lens' UserProfile (Maybe Text)
uxSshPublicKey f x =
    f (_uxSshPublicKey x)
        <&> \y -> x { _uxSshPublicKey = y }
{-# INLINE uxSshPublicKey #-}

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Managing User Permissions.
uxAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
uxAllowSelfManagement f x =
    f (_uxAllowSelfManagement x)
        <&> \y -> x { _uxAllowSelfManagement = y }
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
vgVolumeId f x =
    f (_vgVolumeId x)
        <&> \y -> x { _vgVolumeId = y }
{-# INLINE vgVolumeId #-}

-- | The Amazon EC2 volume ID.
vgEc2VolumeId :: Lens' Volume (Maybe Text)
vgEc2VolumeId f x =
    f (_vgEc2VolumeId x)
        <&> \y -> x { _vgEc2VolumeId = y }
{-# INLINE vgEc2VolumeId #-}

-- | The volume name.
vgName :: Lens' Volume (Maybe Text)
vgName f x =
    f (_vgName x)
        <&> \y -> x { _vgName = y }
{-# INLINE vgName #-}

-- | The RAID array ID.
vgRaidArrayId :: Lens' Volume (Maybe Text)
vgRaidArrayId f x =
    f (_vgRaidArrayId x)
        <&> \y -> x { _vgRaidArrayId = y }
{-# INLINE vgRaidArrayId #-}

-- | The instance ID.
vgInstanceId :: Lens' Volume (Maybe Text)
vgInstanceId f x =
    f (_vgInstanceId x)
        <&> \y -> x { _vgInstanceId = y }
{-# INLINE vgInstanceId #-}

-- | The value returned by DescribeVolumes.
vgStatus :: Lens' Volume (Maybe Text)
vgStatus f x =
    f (_vgStatus x)
        <&> \y -> x { _vgStatus = y }
{-# INLINE vgStatus #-}

-- | The volume size.
vgSize :: Lens' Volume (Maybe Integer)
vgSize f x =
    f (_vgSize x)
        <&> \y -> x { _vgSize = y }
{-# INLINE vgSize #-}

-- | The device name.
vgDevice :: Lens' Volume (Maybe Text)
vgDevice f x =
    f (_vgDevice x)
        <&> \y -> x { _vgDevice = y }
{-# INLINE vgDevice #-}

-- | The volume mount point. For example "/dev/sdh".
vgMountPoint :: Lens' Volume (Maybe Text)
vgMountPoint f x =
    f (_vgMountPoint x)
        <&> \y -> x { _vgMountPoint = y }
{-# INLINE vgMountPoint #-}

-- | The AWS region. For more information about AWS regions, see Regions and
-- Endpoints.
vgRegion :: Lens' Volume (Maybe Text)
vgRegion f x =
    f (_vgRegion x)
        <&> \y -> x { _vgRegion = y }
{-# INLINE vgRegion #-}

-- | The volume Availability Zone. For more information, see Regions and
-- Endpoints.
vgAvailabilityZone :: Lens' Volume (Maybe Text)
vgAvailabilityZone f x =
    f (_vgAvailabilityZone x)
        <&> \y -> x { _vgAvailabilityZone = y }
{-# INLINE vgAvailabilityZone #-}

-- | The volume type, standard or PIOPS.
vgVolumeType :: Lens' Volume (Maybe Text)
vgVolumeType f x =
    f (_vgVolumeType x)
        <&> \y -> x { _vgVolumeType = y }
{-# INLINE vgVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
vgIops :: Lens' Volume (Maybe Integer)
vgIops f x =
    f (_vgIops x)
        <&> \y -> x { _vgIops = y }
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
vdMountPoint f x =
    f (_vdMountPoint x)
        <&> \y -> x { _vdMountPoint = y }
{-# INLINE vdMountPoint #-}

-- | The volume RAID level.
vdRaidLevel :: Lens' VolumeConfiguration (Maybe Integer)
vdRaidLevel f x =
    f (_vdRaidLevel x)
        <&> \y -> x { _vdRaidLevel = y }
{-# INLINE vdRaidLevel #-}

-- | The number of disks in the volume.
vdNumberOfDisks :: Lens' VolumeConfiguration (Integer)
vdNumberOfDisks f x =
    f (_vdNumberOfDisks x)
        <&> \y -> x { _vdNumberOfDisks = y }
{-# INLINE vdNumberOfDisks #-}

-- | The volume size.
vdSize :: Lens' VolumeConfiguration (Integer)
vdSize f x =
    f (_vdSize x)
        <&> \y -> x { _vdSize = y }
{-# INLINE vdSize #-}

-- | The volume type, standard or PIOPS.
vdVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vdVolumeType f x =
    f (_vdVolumeType x)
        <&> \y -> x { _vdVolumeType = y }
{-# INLINE vdVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
vdIops :: Lens' VolumeConfiguration (Maybe Integer)
vdIops f x =
    f (_vdIops x)
        <&> \y -> x { _vdIops = y }
{-# INLINE vdIops #-}

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
wassMonday f x =
    f (_wassMonday x)
        <&> \y -> x { _wassMonday = y }
{-# INLINE wassMonday #-}

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassTuesday f x =
    f (_wassTuesday x)
        <&> \y -> x { _wassTuesday = y }
{-# INLINE wassTuesday #-}

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassWednesday f x =
    f (_wassWednesday x)
        <&> \y -> x { _wassWednesday = y }
{-# INLINE wassWednesday #-}

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassThursday f x =
    f (_wassThursday x)
        <&> \y -> x { _wassThursday = y }
{-# INLINE wassThursday #-}

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassFriday f x =
    f (_wassFriday x)
        <&> \y -> x { _wassFriday = y }
{-# INLINE wassFriday #-}

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassSaturday f x =
    f (_wassSaturday x)
        <&> \y -> x { _wassSaturday = y }
{-# INLINE wassSaturday #-}

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (Map Text Text)
wassSunday f x =
    f (_wassSunday x)
        <&> \y -> x { _wassSunday = y }
{-# INLINE wassSunday #-}

instance FromJSON WeeklyAutoScalingSchedule

instance ToJSON WeeklyAutoScalingSchedule
