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
apAppId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> App
    -> f App
apAppId f x =
    (\y -> x { _apAppId = y })
       <$> f (_apAppId x)
{-# INLINE apAppId #-}

-- | The app stack ID.
apStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> App
    -> f App
apStackId f x =
    (\y -> x { _apStackId = y })
       <$> f (_apStackId x)
{-# INLINE apStackId #-}

-- | The app's short name.
apShortname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> App
    -> f App
apShortname f x =
    (\y -> x { _apShortname = y })
       <$> f (_apShortname x)
{-# INLINE apShortname #-}

-- | The app name.
apName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> App
    -> f App
apName f x =
    (\y -> x { _apName = y })
       <$> f (_apName x)
{-# INLINE apName #-}

-- | A description of the app.
apDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> App
    -> f App
apDescription f x =
    (\y -> x { _apDescription = y })
       <$> f (_apDescription x)
{-# INLINE apDescription #-}

-- | The app's data sources.
apDataSources
    :: Functor f
    => ([DataSource]
    -> f ([DataSource]))
    -> App
    -> f App
apDataSources f x =
    (\y -> x { _apDataSources = y })
       <$> f (_apDataSources x)
{-# INLINE apDataSources #-}

-- | The app type.
apType
    :: Functor f
    => (Maybe AppType
    -> f (Maybe AppType))
    -> App
    -> f App
apType f x =
    (\y -> x { _apType = y })
       <$> f (_apType x)
{-# INLINE apType #-}

-- | A Source object that describes the app repository.
apAppSource
    :: Functor f
    => (Maybe Source
    -> f (Maybe Source))
    -> App
    -> f App
apAppSource f x =
    (\y -> x { _apAppSource = y })
       <$> f (_apAppSource x)
{-# INLINE apAppSource #-}

-- | The app vhost settings with multiple domains separated by commas. For
-- example: 'www.example.com, example.com'.
apDomains
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> App
    -> f App
apDomains f x =
    (\y -> x { _apDomains = y })
       <$> f (_apDomains x)
{-# INLINE apDomains #-}

-- | Whether to enable SSL for the app.
apEnableSsl
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> App
    -> f App
apEnableSsl f x =
    (\y -> x { _apEnableSsl = y })
       <$> f (_apEnableSsl x)
{-# INLINE apEnableSsl #-}

-- | An SslConfiguration object with the SSL configuration.
apSslConfiguration
    :: Functor f
    => (Maybe SslConfiguration
    -> f (Maybe SslConfiguration))
    -> App
    -> f App
apSslConfiguration f x =
    (\y -> x { _apSslConfiguration = y })
       <$> f (_apSslConfiguration x)
{-# INLINE apSslConfiguration #-}

-- | The stack attributes.
apAttributes
    :: Functor f
    => (Map AppAttributesKeys Text
    -> f (Map AppAttributesKeys Text))
    -> App
    -> f App
apAttributes f x =
    (\y -> x { _apAttributes = y })
       <$> f (_apAttributes x)
{-# INLINE apAttributes #-}

-- | When the app was created.
apCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> App
    -> f App
apCreatedAt f x =
    (\y -> x { _apCreatedAt = y })
       <$> f (_apCreatedAt x)
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
asvInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AutoScalingThresholds
    -> f AutoScalingThresholds
asvInstanceCount f x =
    (\y -> x { _asvInstanceCount = y })
       <$> f (_asvInstanceCount x)
{-# INLINE asvInstanceCount #-}

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
asvThresholdsWaitTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AutoScalingThresholds
    -> f AutoScalingThresholds
asvThresholdsWaitTime f x =
    (\y -> x { _asvThresholdsWaitTime = y })
       <$> f (_asvThresholdsWaitTime x)
{-# INLINE asvThresholdsWaitTime #-}

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks should ignore metrics and not raise any additional scaling events.
-- For example, AWS OpsWorks adds new instances following an upscaling event
-- but the instances won't start reducing the load until they have been booted
-- and configured. There is no point in raising additional scaling events
-- during that operation, which typically takes several minutes.
-- IgnoreMetricsTime allows you to direct AWS OpsWorks to not raise any
-- scaling events long enough to get the new instances online.
asvIgnoreMetricsTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AutoScalingThresholds
    -> f AutoScalingThresholds
asvIgnoreMetricsTime f x =
    (\y -> x { _asvIgnoreMetricsTime = y })
       <$> f (_asvIgnoreMetricsTime x)
{-# INLINE asvIgnoreMetricsTime #-}

-- | The CPU utilization threshold, as a percent of the available CPU.
asvCpuThreshold
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> AutoScalingThresholds
    -> f AutoScalingThresholds
asvCpuThreshold f x =
    (\y -> x { _asvCpuThreshold = y })
       <$> f (_asvCpuThreshold x)
{-# INLINE asvCpuThreshold #-}

-- | The memory utilization threshold, as a percent of the available memory.
asvMemoryThreshold
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> AutoScalingThresholds
    -> f AutoScalingThresholds
asvMemoryThreshold f x =
    (\y -> x { _asvMemoryThreshold = y })
       <$> f (_asvMemoryThreshold x)
{-# INLINE asvMemoryThreshold #-}

-- | The load threshold. For more information about how load is computed, see
-- Load (computing).
asvLoadThreshold
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> AutoScalingThresholds
    -> f AutoScalingThresholds
asvLoadThreshold f x =
    (\y -> x { _asvLoadThreshold = y })
       <$> f (_asvLoadThreshold x)
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
ccManageBerkshelf
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ChefConfiguration
    -> f ChefConfiguration
ccManageBerkshelf f x =
    (\y -> x { _ccManageBerkshelf = y })
       <$> f (_ccManageBerkshelf x)
{-# INLINE ccManageBerkshelf #-}

-- | The Berkshelf version.
ccBerkshelfVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChefConfiguration
    -> f ChefConfiguration
ccBerkshelfVersion f x =
    (\y -> x { _ccBerkshelfVersion = y })
       <$> f (_ccBerkshelfVersion x)
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
ceCommandId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceCommandId f x =
    (\y -> x { _ceCommandId = y })
       <$> f (_ceCommandId x)
{-# INLINE ceCommandId #-}

-- | The ID of the instance where the command was executed.
ceInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceInstanceId f x =
    (\y -> x { _ceInstanceId = y })
       <$> f (_ceInstanceId x)
{-# INLINE ceInstanceId #-}

-- | The command deployment ID.
ceDeploymentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceDeploymentId f x =
    (\y -> x { _ceDeploymentId = y })
       <$> f (_ceDeploymentId x)
{-# INLINE ceDeploymentId #-}

-- | Date and time when the command was run.
ceCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceCreatedAt f x =
    (\y -> x { _ceCreatedAt = y })
       <$> f (_ceCreatedAt x)
{-# INLINE ceCreatedAt #-}

-- | Date and time when the command was acknowledged.
ceAcknowledgedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceAcknowledgedAt f x =
    (\y -> x { _ceAcknowledgedAt = y })
       <$> f (_ceAcknowledgedAt x)
{-# INLINE ceAcknowledgedAt #-}

-- | Date when the command completed.
ceCompletedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceCompletedAt f x =
    (\y -> x { _ceCompletedAt = y })
       <$> f (_ceCompletedAt x)
{-# INLINE ceCompletedAt #-}

-- | The command status: failed successful skipped pending.
ceStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceStatus f x =
    (\y -> x { _ceStatus = y })
       <$> f (_ceStatus x)
{-# INLINE ceStatus #-}

-- | The command exit code.
ceExitCode
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Command
    -> f Command
ceExitCode f x =
    (\y -> x { _ceExitCode = y })
       <$> f (_ceExitCode x)
{-# INLINE ceExitCode #-}

-- | The URL of the command log.
ceLogUrl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceLogUrl f x =
    (\y -> x { _ceLogUrl = y })
       <$> f (_ceLogUrl x)
{-# INLINE ceLogUrl #-}

-- | The command type: deploy rollback start stop restart undeploy
-- update_dependencies install_dependencies update_custom_cookbooks
-- execute_recipes.
ceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
ceType f x =
    (\y -> x { _ceType = y })
       <$> f (_ceType x)
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
dtType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DataSource
    -> f DataSource
dtType f x =
    (\y -> x { _dtType = y })
       <$> f (_dtType x)
{-# INLINE dtType #-}

-- | The data source's ARN.
dtArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DataSource
    -> f DataSource
dtArn f x =
    (\y -> x { _dtArn = y })
       <$> f (_dtArn x)
{-# INLINE dtArn #-}

-- | The database name.
dtDatabaseName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DataSource
    -> f DataSource
dtDatabaseName f x =
    (\y -> x { _dtDatabaseName = y })
       <$> f (_dtDatabaseName x)
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
ddtDeploymentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtDeploymentId f x =
    (\y -> x { _ddtDeploymentId = y })
       <$> f (_ddtDeploymentId x)
{-# INLINE ddtDeploymentId #-}

-- | The stack ID.
ddtStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtStackId f x =
    (\y -> x { _ddtStackId = y })
       <$> f (_ddtStackId x)
{-# INLINE ddtStackId #-}

-- | The app ID.
ddtAppId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtAppId f x =
    (\y -> x { _ddtAppId = y })
       <$> f (_ddtAppId x)
{-# INLINE ddtAppId #-}

-- | Date when the deployment was created.
ddtCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtCreatedAt f x =
    (\y -> x { _ddtCreatedAt = y })
       <$> f (_ddtCreatedAt x)
{-# INLINE ddtCreatedAt #-}

-- | Date when the deployment completed.
ddtCompletedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtCompletedAt f x =
    (\y -> x { _ddtCompletedAt = y })
       <$> f (_ddtCompletedAt x)
{-# INLINE ddtCompletedAt #-}

-- | The deployment duration.
ddtDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Deployment
    -> f Deployment
ddtDuration f x =
    (\y -> x { _ddtDuration = y })
       <$> f (_ddtDuration x)
{-# INLINE ddtDuration #-}

-- | The user's IAM ARN.
ddtIamUserArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtIamUserArn f x =
    (\y -> x { _ddtIamUserArn = y })
       <$> f (_ddtIamUserArn x)
{-# INLINE ddtIamUserArn #-}

-- | A user-defined comment.
ddtComment
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtComment f x =
    (\y -> x { _ddtComment = y })
       <$> f (_ddtComment x)
{-# INLINE ddtComment #-}

-- | Used to specify a deployment operation.
ddtCommand
    :: Functor f
    => (Maybe DeploymentCommand
    -> f (Maybe DeploymentCommand))
    -> Deployment
    -> f Deployment
ddtCommand f x =
    (\y -> x { _ddtCommand = y })
       <$> f (_ddtCommand x)
{-# INLINE ddtCommand #-}

-- | The deployment status: running successful failed.
ddtStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtStatus f x =
    (\y -> x { _ddtStatus = y })
       <$> f (_ddtStatus x)
{-# INLINE ddtStatus #-}

-- | A string that contains user-defined custom JSON. It is used to override the
-- corresponding default stack configuration JSON values for stack. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
ddtCustomJson
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Deployment
    -> f Deployment
ddtCustomJson f x =
    (\y -> x { _ddtCustomJson = y })
       <$> f (_ddtCustomJson x)
{-# INLINE ddtCustomJson #-}

-- | The IDs of the target instances.
ddtInstanceIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Deployment
    -> f Deployment
ddtInstanceIds f x =
    (\y -> x { _ddtInstanceIds = y })
       <$> f (_ddtInstanceIds x)
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
dcName
    :: Functor f
    => (DeploymentCommandName
    -> f (DeploymentCommandName))
    -> DeploymentCommand
    -> f DeploymentCommand
dcName f x =
    (\y -> x { _dcName = y })
       <$> f (_dcName x)
{-# INLINE dcName #-}

-- | The arguments of those commands that take arguments. It should be set to a
-- JSON object with the following format: {"arg_name":["value1", "value2",
-- ...]}.
dcArgs
    :: Functor f
    => (Map Text [Text]
    -> f (Map Text [Text]))
    -> DeploymentCommand
    -> f DeploymentCommand
dcArgs f x =
    (\y -> x { _dcArgs = y })
       <$> f (_dcArgs x)
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
ejIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIp
    -> f ElasticIp
ejIp f x =
    (\y -> x { _ejIp = y })
       <$> f (_ejIp x)
{-# INLINE ejIp #-}

-- | The name.
ejName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIp
    -> f ElasticIp
ejName f x =
    (\y -> x { _ejName = y })
       <$> f (_ejName x)
{-# INLINE ejName #-}

-- | The domain.
ejDomain
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIp
    -> f ElasticIp
ejDomain f x =
    (\y -> x { _ejDomain = y })
       <$> f (_ejDomain x)
{-# INLINE ejDomain #-}

-- | The AWS region. For more information, see Regions and Endpoints.
ejRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIp
    -> f ElasticIp
ejRegion f x =
    (\y -> x { _ejRegion = y })
       <$> f (_ejRegion x)
{-# INLINE ejRegion #-}

-- | The ID of the instance that the address is attached to.
ejInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticIp
    -> f ElasticIp
ejInstanceId f x =
    (\y -> x { _ejInstanceId = y })
       <$> f (_ejInstanceId x)
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
elcElasticLoadBalancerName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcElasticLoadBalancerName f x =
    (\y -> x { _elcElasticLoadBalancerName = y })
       <$> f (_elcElasticLoadBalancerName x)
{-# INLINE elcElasticLoadBalancerName #-}

-- | The instance's AWS region.
elcRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcRegion f x =
    (\y -> x { _elcRegion = y })
       <$> f (_elcRegion x)
{-# INLINE elcRegion #-}

-- | The instance's public DNS name.
elcDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcDnsName f x =
    (\y -> x { _elcDnsName = y })
       <$> f (_elcDnsName x)
{-# INLINE elcDnsName #-}

-- | The ID of the stack that the instance is associated with.
elcStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcStackId f x =
    (\y -> x { _elcStackId = y })
       <$> f (_elcStackId x)
{-# INLINE elcStackId #-}

-- | The ID of the layer that the instance is attached to.
elcLayerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcLayerId f x =
    (\y -> x { _elcLayerId = y })
       <$> f (_elcLayerId x)
{-# INLINE elcLayerId #-}

-- | The VPC ID.
elcVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcVpcId f x =
    (\y -> x { _elcVpcId = y })
       <$> f (_elcVpcId x)
{-# INLINE elcVpcId #-}

-- | A list of Availability Zones.
elcAvailabilityZones
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcAvailabilityZones f x =
    (\y -> x { _elcAvailabilityZones = y })
       <$> f (_elcAvailabilityZones x)
{-# INLINE elcAvailabilityZones #-}

-- | A list of subnet IDs, if the stack is running in a VPC.
elcSubnetIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcSubnetIds f x =
    (\y -> x { _elcSubnetIds = y })
       <$> f (_elcSubnetIds x)
{-# INLINE elcSubnetIds #-}

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elcEc2InstanceIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ElasticLoadBalancer
    -> f ElasticLoadBalancer
elcEc2InstanceIds f x =
    (\y -> x { _elcEc2InstanceIds = y })
       <$> f (_elcEc2InstanceIds x)
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
ieInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieInstanceId f x =
    (\y -> x { _ieInstanceId = y })
       <$> f (_ieInstanceId x)
{-# INLINE ieInstanceId #-}

-- | The ID of the associated Amazon EC2 instance.
ieEc2InstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieEc2InstanceId f x =
    (\y -> x { _ieEc2InstanceId = y })
       <$> f (_ieEc2InstanceId x)
{-# INLINE ieEc2InstanceId #-}

-- | The instance's virtualization type, paravirtual or hvm.
ieVirtualizationType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieVirtualizationType f x =
    (\y -> x { _ieVirtualizationType = y })
       <$> f (_ieVirtualizationType x)
{-# INLINE ieVirtualizationType #-}

-- | The instance host name.
ieHostname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieHostname f x =
    (\y -> x { _ieHostname = y })
       <$> f (_ieHostname x)
{-# INLINE ieHostname #-}

-- | The stack ID.
ieStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieStackId f x =
    (\y -> x { _ieStackId = y })
       <$> f (_ieStackId x)
{-# INLINE ieStackId #-}

-- | An array containing the instance layer IDs.
ieLayerIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Instance
    -> f Instance
ieLayerIds f x =
    (\y -> x { _ieLayerIds = y })
       <$> f (_ieLayerIds x)
{-# INLINE ieLayerIds #-}

-- | An array containing the instance security group IDs.
ieSecurityGroupIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Instance
    -> f Instance
ieSecurityGroupIds f x =
    (\y -> x { _ieSecurityGroupIds = y })
       <$> f (_ieSecurityGroupIds x)
{-# INLINE ieSecurityGroupIds #-}

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that specify the various
-- types are in the API Name column of the Available Instance Types table.
ieInstanceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieInstanceType f x =
    (\y -> x { _ieInstanceType = y })
       <$> f (_ieInstanceType x)
{-# INLINE ieInstanceType #-}

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs,
-- see Using Identifiers.
ieInstanceProfileArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieInstanceProfileArn f x =
    (\y -> x { _ieInstanceProfileArn = y })
       <$> f (_ieInstanceProfileArn x)
{-# INLINE ieInstanceProfileArn #-}

-- | The instance status: requested booting running_setup online setup_failed
-- start_failed terminating terminated stopped connection_lost.
ieStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieStatus f x =
    (\y -> x { _ieStatus = y })
       <$> f (_ieStatus x)
{-# INLINE ieStatus #-}

-- | The instance operating system.
ieOs
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieOs f x =
    (\y -> x { _ieOs = y })
       <$> f (_ieOs x)
{-# INLINE ieOs #-}

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
-- For more information, see Instances.
ieAmiId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieAmiId f x =
    (\y -> x { _ieAmiId = y })
       <$> f (_ieAmiId x)
{-# INLINE ieAmiId #-}

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
ieAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieAvailabilityZone f x =
    (\y -> x { _ieAvailabilityZone = y })
       <$> f (_ieAvailabilityZone x)
{-# INLINE ieAvailabilityZone #-}

-- | The instance's subnet ID, if the stack is running in a VPC.
ieSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieSubnetId f x =
    (\y -> x { _ieSubnetId = y })
       <$> f (_ieSubnetId x)
{-# INLINE ieSubnetId #-}

-- | The instance public DNS name.
iePublicDns
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePublicDns f x =
    (\y -> x { _iePublicDns = y })
       <$> f (_iePublicDns x)
{-# INLINE iePublicDns #-}

-- | The instance private DNS name.
iePrivateDns
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePrivateDns f x =
    (\y -> x { _iePrivateDns = y })
       <$> f (_iePrivateDns x)
{-# INLINE iePrivateDns #-}

-- | The instance public IP address.
iePublicIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePublicIp f x =
    (\y -> x { _iePublicIp = y })
       <$> f (_iePublicIp x)
{-# INLINE iePublicIp #-}

-- | The instance private IP address.
iePrivateIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePrivateIp f x =
    (\y -> x { _iePrivateIp = y })
       <$> f (_iePrivateIp x)
{-# INLINE iePrivateIp #-}

-- | The instance Elastic IP address .
ieElasticIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieElasticIp f x =
    (\y -> x { _ieElasticIp = y })
       <$> f (_ieElasticIp x)
{-# INLINE ieElasticIp #-}

-- | The instance's auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. LoadBasedAutoScaling: A
-- load-based auto scaling instance, which is started and stopped based on
-- load metrics.
ieAutoScalingType
    :: Functor f
    => (Maybe AutoScalingType
    -> f (Maybe AutoScalingType))
    -> Instance
    -> f Instance
ieAutoScalingType f x =
    (\y -> x { _ieAutoScalingType = y })
       <$> f (_ieAutoScalingType x)
{-# INLINE ieAutoScalingType #-}

-- | The instance SSH key name.
ieSshKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieSshKeyName f x =
    (\y -> x { _ieSshKeyName = y })
       <$> f (_ieSshKeyName x)
{-# INLINE ieSshKeyName #-}

-- | The SSH key's RSA fingerprint.
ieSshHostRsaKeyFingerprint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieSshHostRsaKeyFingerprint f x =
    (\y -> x { _ieSshHostRsaKeyFingerprint = y })
       <$> f (_ieSshHostRsaKeyFingerprint x)
{-# INLINE ieSshHostRsaKeyFingerprint #-}

-- | The SSH key's DSA fingerprint.
ieSshHostDsaKeyFingerprint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieSshHostDsaKeyFingerprint f x =
    (\y -> x { _ieSshHostDsaKeyFingerprint = y })
       <$> f (_ieSshHostDsaKeyFingerprint x)
{-# INLINE ieSshHostDsaKeyFingerprint #-}

-- | The time that the instance was created.
ieCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieCreatedAt f x =
    (\y -> x { _ieCreatedAt = y })
       <$> f (_ieCreatedAt x)
{-# INLINE ieCreatedAt #-}

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
ieLastServiceErrorId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieLastServiceErrorId f x =
    (\y -> x { _ieLastServiceErrorId = y })
       <$> f (_ieLastServiceErrorId x)
{-# INLINE ieLastServiceErrorId #-}

-- | The instance architecture, "i386" or "x86_64".
ieArchitecture
    :: Functor f
    => (Maybe Architecture
    -> f (Maybe Architecture))
    -> Instance
    -> f Instance
ieArchitecture f x =
    (\y -> x { _ieArchitecture = y })
       <$> f (_ieArchitecture x)
{-# INLINE ieArchitecture #-}

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
ieRootDeviceType
    :: Functor f
    => (Maybe RootDeviceType
    -> f (Maybe RootDeviceType))
    -> Instance
    -> f Instance
ieRootDeviceType f x =
    (\y -> x { _ieRootDeviceType = y })
       <$> f (_ieRootDeviceType x)
{-# INLINE ieRootDeviceType #-}

-- | The root device volume ID.
ieRootDeviceVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieRootDeviceVolumeId f x =
    (\y -> x { _ieRootDeviceVolumeId = y })
       <$> f (_ieRootDeviceVolumeId x)
{-# INLINE ieRootDeviceVolumeId #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
ieInstallUpdatesOnBoot
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Instance
    -> f Instance
ieInstallUpdatesOnBoot f x =
    (\y -> x { _ieInstallUpdatesOnBoot = y })
       <$> f (_ieInstallUpdatesOnBoot x)
{-# INLINE ieInstallUpdatesOnBoot #-}

-- | Whether this is an Amazon EBS-optimized instance.
ieEbsOptimized
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Instance
    -> f Instance
ieEbsOptimized f x =
    (\y -> x { _ieEbsOptimized = y })
       <$> f (_ieEbsOptimized x)
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
icBooting
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icBooting f x =
    (\y -> x { _icBooting = y })
       <$> f (_icBooting x)
{-# INLINE icBooting #-}

-- | The number of instances with connection_lost status.
icConnectionLost
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icConnectionLost f x =
    (\y -> x { _icConnectionLost = y })
       <$> f (_icConnectionLost x)
{-# INLINE icConnectionLost #-}

-- | The number of instances with online status.
icOnline
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icOnline f x =
    (\y -> x { _icOnline = y })
       <$> f (_icOnline x)
{-# INLINE icOnline #-}

-- | The number of instances with pending status.
icPending
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icPending f x =
    (\y -> x { _icPending = y })
       <$> f (_icPending x)
{-# INLINE icPending #-}

-- | The number of instances with rebooting status.
icRebooting
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icRebooting f x =
    (\y -> x { _icRebooting = y })
       <$> f (_icRebooting x)
{-# INLINE icRebooting #-}

-- | The number of instances with requested status.
icRequested
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icRequested f x =
    (\y -> x { _icRequested = y })
       <$> f (_icRequested x)
{-# INLINE icRequested #-}

-- | The number of instances with running_setup status.
icRunningSetup
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icRunningSetup f x =
    (\y -> x { _icRunningSetup = y })
       <$> f (_icRunningSetup x)
{-# INLINE icRunningSetup #-}

-- | The number of instances with setup_failed status.
icSetupFailed
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icSetupFailed f x =
    (\y -> x { _icSetupFailed = y })
       <$> f (_icSetupFailed x)
{-# INLINE icSetupFailed #-}

-- | The number of instances with shutting_down status.
icShuttingDown
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icShuttingDown f x =
    (\y -> x { _icShuttingDown = y })
       <$> f (_icShuttingDown x)
{-# INLINE icShuttingDown #-}

-- | The number of instances with start_failed status.
icStartFailed
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icStartFailed f x =
    (\y -> x { _icStartFailed = y })
       <$> f (_icStartFailed x)
{-# INLINE icStartFailed #-}

-- | The number of instances with stopped status.
icStopped
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icStopped f x =
    (\y -> x { _icStopped = y })
       <$> f (_icStopped x)
{-# INLINE icStopped #-}

-- | The number of instances with stopping status.
icStopping
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icStopping f x =
    (\y -> x { _icStopping = y })
       <$> f (_icStopping x)
{-# INLINE icStopping #-}

-- | The number of instances with terminated status.
icTerminated
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icTerminated f x =
    (\y -> x { _icTerminated = y })
       <$> f (_icTerminated x)
{-# INLINE icTerminated #-}

-- | The number of instances with terminating status.
icTerminating
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstancesCount
    -> f InstancesCount
icTerminating f x =
    (\y -> x { _icTerminating = y })
       <$> f (_icTerminating x)
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
lrStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Layer
    -> f Layer
lrStackId f x =
    (\y -> x { _lrStackId = y })
       <$> f (_lrStackId x)
{-# INLINE lrStackId #-}

-- | The layer ID.
lrLayerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Layer
    -> f Layer
lrLayerId f x =
    (\y -> x { _lrLayerId = y })
       <$> f (_lrLayerId x)
{-# INLINE lrLayerId #-}

-- | The layer type, which must be one of the following: Custom
-- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster NodeJsAppServer
-- PhpAppServer RailsAppServer WebServer.
lrType
    :: Functor f
    => (Maybe LayerType
    -> f (Maybe LayerType))
    -> Layer
    -> f Layer
lrType f x =
    (\y -> x { _lrType = y })
       <$> f (_lrType x)
{-# INLINE lrType #-}

-- | The layer name.
lrName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Layer
    -> f Layer
lrName f x =
    (\y -> x { _lrName = y })
       <$> f (_lrName x)
{-# INLINE lrName #-}

-- | The layer short name.
lrShortname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Layer
    -> f Layer
lrShortname f x =
    (\y -> x { _lrShortname = y })
       <$> f (_lrShortname x)
{-# INLINE lrShortname #-}

-- | The layer attributes.
lrAttributes
    :: Functor f
    => (Map LayerAttributesKeys Text
    -> f (Map LayerAttributesKeys Text))
    -> Layer
    -> f Layer
lrAttributes f x =
    (\y -> x { _lrAttributes = y })
       <$> f (_lrAttributes x)
{-# INLINE lrAttributes #-}

-- | The ARN of the default IAM profile to be used for the layer's EC2
-- instances. For more information about IAM ARNs, see Using Identifiers.
lrCustomInstanceProfileArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Layer
    -> f Layer
lrCustomInstanceProfileArn f x =
    (\y -> x { _lrCustomInstanceProfileArn = y })
       <$> f (_lrCustomInstanceProfileArn x)
{-# INLINE lrCustomInstanceProfileArn #-}

-- | An array containing the layer's custom security group IDs.
lrCustomSecurityGroupIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Layer
    -> f Layer
lrCustomSecurityGroupIds f x =
    (\y -> x { _lrCustomSecurityGroupIds = y })
       <$> f (_lrCustomSecurityGroupIds x)
{-# INLINE lrCustomSecurityGroupIds #-}

-- | An array containing the layer's security group names.
lrDefaultSecurityGroupNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Layer
    -> f Layer
lrDefaultSecurityGroupNames f x =
    (\y -> x { _lrDefaultSecurityGroupNames = y })
       <$> f (_lrDefaultSecurityGroupNames x)
{-# INLINE lrDefaultSecurityGroupNames #-}

-- | An array of Package objects that describe the layer's packages.
lrPackages
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Layer
    -> f Layer
lrPackages f x =
    (\y -> x { _lrPackages = y })
       <$> f (_lrPackages x)
{-# INLINE lrPackages #-}

-- | A VolumeConfigurations object that describes the layer's Amazon EBS
-- volumes.
lrVolumeConfigurations
    :: Functor f
    => ([VolumeConfiguration]
    -> f ([VolumeConfiguration]))
    -> Layer
    -> f Layer
lrVolumeConfigurations f x =
    (\y -> x { _lrVolumeConfigurations = y })
       <$> f (_lrVolumeConfigurations x)
{-# INLINE lrVolumeConfigurations #-}

-- | Whether auto healing is disabled for the layer.
lrEnableAutoHealing
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Layer
    -> f Layer
lrEnableAutoHealing f x =
    (\y -> x { _lrEnableAutoHealing = y })
       <$> f (_lrEnableAutoHealing x)
{-# INLINE lrEnableAutoHealing #-}

-- | Whether to automatically assign an Elastic IP address to the layer's
-- instances. For more information, see How to Edit a Layer.
lrAutoAssignElasticIps
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Layer
    -> f Layer
lrAutoAssignElasticIps f x =
    (\y -> x { _lrAutoAssignElasticIps = y })
       <$> f (_lrAutoAssignElasticIps x)
{-# INLINE lrAutoAssignElasticIps #-}

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer's instances. For more information, see How
-- to Edit a Layer.
lrAutoAssignPublicIps
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Layer
    -> f Layer
lrAutoAssignPublicIps f x =
    (\y -> x { _lrAutoAssignPublicIps = y })
       <$> f (_lrAutoAssignPublicIps x)
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
lrDefaultRecipes
    :: Functor f
    => (Maybe Recipes
    -> f (Maybe Recipes))
    -> Layer
    -> f Layer
lrDefaultRecipes f x =
    (\y -> x { _lrDefaultRecipes = y })
       <$> f (_lrDefaultRecipes x)
{-# INLINE lrDefaultRecipes #-}

-- | A LayerCustomRecipes object that specifies the layer's custom recipes.
lrCustomRecipes
    :: Functor f
    => (Maybe Recipes
    -> f (Maybe Recipes))
    -> Layer
    -> f Layer
lrCustomRecipes f x =
    (\y -> x { _lrCustomRecipes = y })
       <$> f (_lrCustomRecipes x)
{-# INLINE lrCustomRecipes #-}

-- | Date when the layer was created.
lrCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Layer
    -> f Layer
lrCreatedAt f x =
    (\y -> x { _lrCreatedAt = y })
       <$> f (_lrCreatedAt x)
{-# INLINE lrCreatedAt #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. If this value is set to false, you must
-- then update your instances manually by using CreateDeployment to run the
-- update_dependencies stack command or manually running yum (Amazon Linux) or
-- apt-get (Ubuntu) on the instances. We strongly recommend using the default
-- value of true, to ensure that your instances have the latest security
-- updates.
lrInstallUpdatesOnBoot
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Layer
    -> f Layer
lrInstallUpdatesOnBoot f x =
    (\y -> x { _lrInstallUpdatesOnBoot = y })
       <$> f (_lrInstallUpdatesOnBoot x)
{-# INLINE lrInstallUpdatesOnBoot #-}

-- | Whether the layer uses Amazon EBS-optimized instances.
lrUseEbsOptimizedInstances
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Layer
    -> f Layer
lrUseEbsOptimizedInstances f x =
    (\y -> x { _lrUseEbsOptimizedInstances = y })
       <$> f (_lrUseEbsOptimizedInstances x)
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
lbasdLayerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LoadBasedAutoScalingConfiguration
    -> f LoadBasedAutoScalingConfiguration
lbasdLayerId f x =
    (\y -> x { _lbasdLayerId = y })
       <$> f (_lbasdLayerId x)
{-# INLINE lbasdLayerId #-}

-- | Whether load-based auto scaling is enabled for the layer.
lbasdEnable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LoadBasedAutoScalingConfiguration
    -> f LoadBasedAutoScalingConfiguration
lbasdEnable f x =
    (\y -> x { _lbasdEnable = y })
       <$> f (_lbasdEnable x)
{-# INLINE lbasdEnable #-}

-- | A LoadBasedAutoscalingInstruction object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the number
-- of instances.
lbasdUpScaling
    :: Functor f
    => (Maybe AutoScalingThresholds
    -> f (Maybe AutoScalingThresholds))
    -> LoadBasedAutoScalingConfiguration
    -> f LoadBasedAutoScalingConfiguration
lbasdUpScaling f x =
    (\y -> x { _lbasdUpScaling = y })
       <$> f (_lbasdUpScaling x)
{-# INLINE lbasdUpScaling #-}

-- | A LoadBasedAutoscalingInstruction object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the number
-- of instances.
lbasdDownScaling
    :: Functor f
    => (Maybe AutoScalingThresholds
    -> f (Maybe AutoScalingThresholds))
    -> LoadBasedAutoScalingConfiguration
    -> f LoadBasedAutoScalingConfiguration
lbasdDownScaling f x =
    (\y -> x { _lbasdDownScaling = y })
       <$> f (_lbasdDownScaling x)
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
pnStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Permission
    -> f Permission
pnStackId f x =
    (\y -> x { _pnStackId = y })
       <$> f (_pnStackId x)
{-# INLINE pnStackId #-}

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see Using Identifiers.
pnIamUserArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Permission
    -> f Permission
pnIamUserArn f x =
    (\y -> x { _pnIamUserArn = y })
       <$> f (_pnIamUserArn x)
{-# INLINE pnIamUserArn #-}

-- | Whether the user can use SSH.
pnAllowSsh
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Permission
    -> f Permission
pnAllowSsh f x =
    (\y -> x { _pnAllowSsh = y })
       <$> f (_pnAllowSsh x)
{-# INLINE pnAllowSsh #-}

-- | Whether the user can use sudo.
pnAllowSudo
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Permission
    -> f Permission
pnAllowSudo f x =
    (\y -> x { _pnAllowSudo = y })
       <$> f (_pnAllowSudo x)
{-# INLINE pnAllowSudo #-}

-- | The user's permission level, which must be the following: deny show deploy
-- manage iam_only For more information on the permissions associated with
-- these levels, see Managing User Permissions.
pnLevel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Permission
    -> f Permission
pnLevel f x =
    (\y -> x { _pnLevel = y })
       <$> f (_pnLevel x)
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
rbRaidArrayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbRaidArrayId f x =
    (\y -> x { _rbRaidArrayId = y })
       <$> f (_rbRaidArrayId x)
{-# INLINE rbRaidArrayId #-}

-- | The instance ID.
rbInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbInstanceId f x =
    (\y -> x { _rbInstanceId = y })
       <$> f (_rbInstanceId x)
{-# INLINE rbInstanceId #-}

-- | The array name.
rbName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbName f x =
    (\y -> x { _rbName = y })
       <$> f (_rbName x)
{-# INLINE rbName #-}

-- | The RAID level.
rbRaidLevel
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RaidArray
    -> f RaidArray
rbRaidLevel f x =
    (\y -> x { _rbRaidLevel = y })
       <$> f (_rbRaidLevel x)
{-# INLINE rbRaidLevel #-}

-- | The number of disks in the array.
rbNumberOfDisks
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RaidArray
    -> f RaidArray
rbNumberOfDisks f x =
    (\y -> x { _rbNumberOfDisks = y })
       <$> f (_rbNumberOfDisks x)
{-# INLINE rbNumberOfDisks #-}

-- | The array's size.
rbSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RaidArray
    -> f RaidArray
rbSize f x =
    (\y -> x { _rbSize = y })
       <$> f (_rbSize x)
{-# INLINE rbSize #-}

-- | The array's Linux device. For example /dev/mdadm0.
rbDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbDevice f x =
    (\y -> x { _rbDevice = y })
       <$> f (_rbDevice x)
{-# INLINE rbDevice #-}

-- | The array's mount point.
rbMountPoint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbMountPoint f x =
    (\y -> x { _rbMountPoint = y })
       <$> f (_rbMountPoint x)
{-# INLINE rbMountPoint #-}

-- | The array's Availability Zone. For more information, see Regions and
-- Endpoints.
rbAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbAvailabilityZone f x =
    (\y -> x { _rbAvailabilityZone = y })
       <$> f (_rbAvailabilityZone x)
{-# INLINE rbAvailabilityZone #-}

-- | When the RAID array was created.
rbCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbCreatedAt f x =
    (\y -> x { _rbCreatedAt = y })
       <$> f (_rbCreatedAt x)
{-# INLINE rbCreatedAt #-}

-- | The volume type, standard or PIOPS.
rbVolumeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RaidArray
    -> f RaidArray
rbVolumeType f x =
    (\y -> x { _rbVolumeType = y })
       <$> f (_rbVolumeType x)
{-# INLINE rbVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
rbIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> RaidArray
    -> f RaidArray
rbIops f x =
    (\y -> x { _rbIops = y })
       <$> f (_rbIops x)
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
rdjRdsDbInstanceArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjRdsDbInstanceArn f x =
    (\y -> x { _rdjRdsDbInstanceArn = y })
       <$> f (_rdjRdsDbInstanceArn x)
{-# INLINE rdjRdsDbInstanceArn #-}

-- | The DB instance identifier.
rdjDbInstanceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjDbInstanceIdentifier f x =
    (\y -> x { _rdjDbInstanceIdentifier = y })
       <$> f (_rdjDbInstanceIdentifier x)
{-# INLINE rdjDbInstanceIdentifier #-}

-- | The master user name.
rdjDbUser
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjDbUser f x =
    (\y -> x { _rdjDbUser = y })
       <$> f (_rdjDbUser x)
{-# INLINE rdjDbUser #-}

-- | The database password.
rdjDbPassword
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjDbPassword f x =
    (\y -> x { _rdjDbPassword = y })
       <$> f (_rdjDbPassword x)
{-# INLINE rdjDbPassword #-}

-- | The instance's AWS region.
rdjRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjRegion f x =
    (\y -> x { _rdjRegion = y })
       <$> f (_rdjRegion x)
{-# INLINE rdjRegion #-}

-- | The instance's address.
rdjAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjAddress f x =
    (\y -> x { _rdjAddress = y })
       <$> f (_rdjAddress x)
{-# INLINE rdjAddress #-}

-- | The instance's database engine.
rdjEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjEngine f x =
    (\y -> x { _rdjEngine = y })
       <$> f (_rdjEngine x)
{-# INLINE rdjEngine #-}

-- | The ID of the stack that the instance is registered with.
rdjStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjStackId f x =
    (\y -> x { _rdjStackId = y })
       <$> f (_rdjStackId x)
{-# INLINE rdjStackId #-}

-- | Set to true if AWS OpsWorks was unable to discover the Amazon RDS instance.
-- AWS OpsWorks attempts to discover the instance only once. If this value is
-- set to true, you must deregister the instance and then register it again.
rdjMissingOnRds
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> RdsDbInstance
    -> f RdsDbInstance
rdjMissingOnRds f x =
    (\y -> x { _rdjMissingOnRds = y })
       <$> f (_rdjMissingOnRds x)
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
rSetup
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Recipes
    -> f Recipes
rSetup f x =
    (\y -> x { _rSetup = y })
       <$> f (_rSetup x)
{-# INLINE rSetup #-}

-- | An array of custom recipe names to be run following a configure event.
rConfigure
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Recipes
    -> f Recipes
rConfigure f x =
    (\y -> x { _rConfigure = y })
       <$> f (_rConfigure x)
{-# INLINE rConfigure #-}

-- | An array of custom recipe names to be run following a deploy event.
rDeploy
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Recipes
    -> f Recipes
rDeploy f x =
    (\y -> x { _rDeploy = y })
       <$> f (_rDeploy x)
{-# INLINE rDeploy #-}

-- | An array of custom recipe names to be run following a undeploy event.
rUndeploy
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Recipes
    -> f Recipes
rUndeploy f x =
    (\y -> x { _rUndeploy = y })
       <$> f (_rUndeploy x)
{-# INLINE rUndeploy #-}

-- | An array of custom recipe names to be run following a shutdown event.
rShutdown
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Recipes
    -> f Recipes
rShutdown f x =
    (\y -> x { _rShutdown = y })
       <$> f (_rShutdown x)
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
supIamUserArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SelfUserProfile
    -> f SelfUserProfile
supIamUserArn f x =
    (\y -> x { _supIamUserArn = y })
       <$> f (_supIamUserArn x)
{-# INLINE supIamUserArn #-}

-- | The user's name.
supName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SelfUserProfile
    -> f SelfUserProfile
supName f x =
    (\y -> x { _supName = y })
       <$> f (_supName x)
{-# INLINE supName #-}

-- | The user's SSH user name.
supSshUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SelfUserProfile
    -> f SelfUserProfile
supSshUsername f x =
    (\y -> x { _supSshUsername = y })
       <$> f (_supSshUsername x)
{-# INLINE supSshUsername #-}

-- | The user's SSH public key.
supSshPublicKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SelfUserProfile
    -> f SelfUserProfile
supSshPublicKey f x =
    (\y -> x { _supSshPublicKey = y })
       <$> f (_supSshPublicKey x)
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
sssssssssssssssxServiceErrorId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceError
    -> f ServiceError
sssssssssssssssxServiceErrorId f x =
    (\y -> x { _sssssssssssssssxServiceErrorId = y })
       <$> f (_sssssssssssssssxServiceErrorId x)
{-# INLINE sssssssssssssssxServiceErrorId #-}

-- | The stack ID.
sssssssssssssssxStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceError
    -> f ServiceError
sssssssssssssssxStackId f x =
    (\y -> x { _sssssssssssssssxStackId = y })
       <$> f (_sssssssssssssssxStackId x)
{-# INLINE sssssssssssssssxStackId #-}

-- | The instance ID.
sssssssssssssssxInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceError
    -> f ServiceError
sssssssssssssssxInstanceId f x =
    (\y -> x { _sssssssssssssssxInstanceId = y })
       <$> f (_sssssssssssssssxInstanceId x)
{-# INLINE sssssssssssssssxInstanceId #-}

-- | The error type.
sssssssssssssssxType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceError
    -> f ServiceError
sssssssssssssssxType f x =
    (\y -> x { _sssssssssssssssxType = y })
       <$> f (_sssssssssssssssxType x)
{-# INLINE sssssssssssssssxType #-}

-- | A message that describes the error.
sssssssssssssssxMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceError
    -> f ServiceError
sssssssssssssssxMessage f x =
    (\y -> x { _sssssssssssssssxMessage = y })
       <$> f (_sssssssssssssssxMessage x)
{-# INLINE sssssssssssssssxMessage #-}

-- | When the error occurred.
sssssssssssssssxCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceError
    -> f ServiceError
sssssssssssssssxCreatedAt f x =
    (\y -> x { _sssssssssssssssxCreatedAt = y })
       <$> f (_sssssssssssssssxCreatedAt x)
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
seType
    :: Functor f
    => (Maybe SourceType
    -> f (Maybe SourceType))
    -> Source
    -> f Source
seType f x =
    (\y -> x { _seType = y })
       <$> f (_seType x)
{-# INLINE seType #-}

-- | The source URL.
seUrl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Source
    -> f Source
seUrl f x =
    (\y -> x { _seUrl = y })
       <$> f (_seUrl x)
{-# INLINE seUrl #-}

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Username to the appropriate IAM access key ID. For HTTP bundles, Git
-- repositories, and Subversion repositories, set Username to the user name.
seUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Source
    -> f Source
seUsername f x =
    (\y -> x { _seUsername = y })
       <$> f (_seUsername x)
{-# INLINE seUsername #-}

-- | This parameter depends on the repository type. For Amazon S3 bundles, set
-- Password to the appropriate IAM secret access key. For HTTP bundles and
-- Subversion repositories, set Password to the password. For more information
-- on how to safely handle IAM credentials, see .
sePassword
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Source
    -> f Source
sePassword f x =
    (\y -> x { _sePassword = y })
       <$> f (_sePassword x)
{-# INLINE sePassword #-}

-- | The repository's SSH key.
seSshKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Source
    -> f Source
seSshKey f x =
    (\y -> x { _seSshKey = y })
       <$> f (_seSshKey x)
{-# INLINE seSshKey #-}

-- | The application's version. AWS OpsWorks enables you to easily deploy new
-- versions of an application. One of the simplest approaches is to have
-- branches or revisions in your repository that represent different versions
-- that can potentially be deployed.
seRevision
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Source
    -> f Source
seRevision f x =
    (\y -> x { _seRevision = y })
       <$> f (_seRevision x)
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
scCertificate
    :: Functor f
    => (Text
    -> f (Text))
    -> SslConfiguration
    -> f SslConfiguration
scCertificate f x =
    (\y -> x { _scCertificate = y })
       <$> f (_scCertificate x)
{-# INLINE scCertificate #-}

-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey
    :: Functor f
    => (Text
    -> f (Text))
    -> SslConfiguration
    -> f SslConfiguration
scPrivateKey f x =
    (\y -> x { _scPrivateKey = y })
       <$> f (_scPrivateKey x)
{-# INLINE scPrivateKey #-}

-- | Optional. Can be used to specify an intermediate certificate authority key
-- or client authentication.
scChain
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SslConfiguration
    -> f SslConfiguration
scChain f x =
    (\y -> x { _scChain = y })
       <$> f (_scChain x)
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
ssssssssssssssssoStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoStackId f x =
    (\y -> x { _ssssssssssssssssoStackId = y })
       <$> f (_ssssssssssssssssoStackId x)
{-# INLINE ssssssssssssssssoStackId #-}

-- | The stack name.
ssssssssssssssssoName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoName f x =
    (\y -> x { _ssssssssssssssssoName = y })
       <$> f (_ssssssssssssssssoName x)
{-# INLINE ssssssssssssssssoName #-}

-- | The stack's ARN.
ssssssssssssssssoArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoArn f x =
    (\y -> x { _ssssssssssssssssoArn = y })
       <$> f (_ssssssssssssssssoArn x)
{-# INLINE ssssssssssssssssoArn #-}

-- | The stack AWS region, such as "us-east-1". For more information about AWS
-- regions, see Regions and Endpoints.
ssssssssssssssssoRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoRegion f x =
    (\y -> x { _ssssssssssssssssoRegion = y })
       <$> f (_ssssssssssssssssoRegion x)
{-# INLINE ssssssssssssssssoRegion #-}

-- | The VPC ID, if the stack is running in a VPC.
ssssssssssssssssoVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoVpcId f x =
    (\y -> x { _ssssssssssssssssoVpcId = y })
       <$> f (_ssssssssssssssssoVpcId x)
{-# INLINE ssssssssssssssssoVpcId #-}

-- | The stack's attributes.
ssssssssssssssssoAttributes
    :: Functor f
    => (Map StackAttributesKeys Text
    -> f (Map StackAttributesKeys Text))
    -> Stack
    -> f Stack
ssssssssssssssssoAttributes f x =
    (\y -> x { _ssssssssssssssssoAttributes = y })
       <$> f (_ssssssssssssssssoAttributes x)
{-# INLINE ssssssssssssssssoAttributes #-}

-- | The stack AWS Identity and Access Management (IAM) role.
ssssssssssssssssoServiceRoleArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoServiceRoleArn f x =
    (\y -> x { _ssssssssssssssssoServiceRoleArn = y })
       <$> f (_ssssssssssssssssoServiceRoleArn x)
{-# INLINE ssssssssssssssssoServiceRoleArn #-}

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack's EC2 instances. For more information about IAM ARNs, see Using
-- Identifiers.
ssssssssssssssssoDefaultInstanceProfileArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoDefaultInstanceProfileArn f x =
    (\y -> x { _ssssssssssssssssoDefaultInstanceProfileArn = y })
       <$> f (_ssssssssssssssssoDefaultInstanceProfileArn x)
{-# INLINE ssssssssssssssssoDefaultInstanceProfileArn #-}

-- | The stack's default operating system, which must be set to Amazon Linux or
-- Ubuntu 12.04 LTS. The default option is Amazon Linux.
ssssssssssssssssoDefaultOs
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoDefaultOs f x =
    (\y -> x { _ssssssssssssssssoDefaultOs = y })
       <$> f (_ssssssssssssssssoDefaultOs x)
{-# INLINE ssssssssssssssssoDefaultOs #-}

-- | The stack host name theme, with spaces replaced by underscores.
ssssssssssssssssoHostnameTheme
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoHostnameTheme f x =
    (\y -> x { _ssssssssssssssssoHostnameTheme = y })
       <$> f (_ssssssssssssssssoHostnameTheme x)
{-# INLINE ssssssssssssssssoHostnameTheme #-}

-- | The stack's default Availability Zone. For more information, see Regions
-- and Endpoints.
ssssssssssssssssoDefaultAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoDefaultAvailabilityZone f x =
    (\y -> x { _ssssssssssssssssoDefaultAvailabilityZone = y })
       <$> f (_ssssssssssssssssoDefaultAvailabilityZone x)
{-# INLINE ssssssssssssssssoDefaultAvailabilityZone #-}

-- | The default subnet ID, if the stack is running in a VPC.
ssssssssssssssssoDefaultSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoDefaultSubnetId f x =
    (\y -> x { _ssssssssssssssssoDefaultSubnetId = y })
       <$> f (_ssssssssssssssssoDefaultSubnetId x)
{-# INLINE ssssssssssssssssoDefaultSubnetId #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
ssssssssssssssssoCustomJson
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoCustomJson f x =
    (\y -> x { _ssssssssssssssssoCustomJson = y })
       <$> f (_ssssssssssssssssoCustomJson x)
{-# INLINE ssssssssssssssssoCustomJson #-}

-- | The configuration manager.
ssssssssssssssssoConfigurationManager
    :: Functor f
    => (Maybe StackConfigurationManager
    -> f (Maybe StackConfigurationManager))
    -> Stack
    -> f Stack
ssssssssssssssssoConfigurationManager f x =
    (\y -> x { _ssssssssssssssssoConfigurationManager = y })
       <$> f (_ssssssssssssssssoConfigurationManager x)
{-# INLINE ssssssssssssssssoConfigurationManager #-}

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version. For more information, see Create a New Stack.
ssssssssssssssssoChefConfiguration
    :: Functor f
    => (Maybe ChefConfiguration
    -> f (Maybe ChefConfiguration))
    -> Stack
    -> f Stack
ssssssssssssssssoChefConfiguration f x =
    (\y -> x { _ssssssssssssssssoChefConfiguration = y })
       <$> f (_ssssssssssssssssoChefConfiguration x)
{-# INLINE ssssssssssssssssoChefConfiguration #-}

-- | Whether the stack uses custom cookbooks.
ssssssssssssssssoUseCustomCookbooks
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Stack
    -> f Stack
ssssssssssssssssoUseCustomCookbooks f x =
    (\y -> x { _ssssssssssssssssoUseCustomCookbooks = y })
       <$> f (_ssssssssssssssssoUseCustomCookbooks x)
{-# INLINE ssssssssssssssssoUseCustomCookbooks #-}

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack's layers.
ssssssssssssssssoUseOpsworksSecurityGroups
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Stack
    -> f Stack
ssssssssssssssssoUseOpsworksSecurityGroups f x =
    (\y -> x { _ssssssssssssssssoUseOpsworksSecurityGroups = y })
       <$> f (_ssssssssssssssssoUseOpsworksSecurityGroups x)
{-# INLINE ssssssssssssssssoUseOpsworksSecurityGroups #-}

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
ssssssssssssssssoCustomCookbooksSource
    :: Functor f
    => (Maybe Source
    -> f (Maybe Source))
    -> Stack
    -> f Stack
ssssssssssssssssoCustomCookbooksSource f x =
    (\y -> x { _ssssssssssssssssoCustomCookbooksSource = y })
       <$> f (_ssssssssssssssssoCustomCookbooksSource x)
{-# INLINE ssssssssssssssssoCustomCookbooksSource #-}

-- | A default SSH key for the stack's instances. You can override this value
-- when you create or update an instance.
ssssssssssssssssoDefaultSshKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoDefaultSshKeyName f x =
    (\y -> x { _ssssssssssssssssoDefaultSshKeyName = y })
       <$> f (_ssssssssssssssssoDefaultSshKeyName x)
{-# INLINE ssssssssssssssssoDefaultSshKeyName #-}

-- | Date when the stack was created.
ssssssssssssssssoCreatedAt
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Stack
    -> f Stack
ssssssssssssssssoCreatedAt f x =
    (\y -> x { _ssssssssssssssssoCreatedAt = y })
       <$> f (_ssssssssssssssssoCreatedAt x)
{-# INLINE ssssssssssssssssoCreatedAt #-}

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.
ssssssssssssssssoDefaultRootDeviceType
    :: Functor f
    => (Maybe RootDeviceType
    -> f (Maybe RootDeviceType))
    -> Stack
    -> f Stack
ssssssssssssssssoDefaultRootDeviceType f x =
    (\y -> x { _ssssssssssssssssoDefaultRootDeviceType = y })
       <$> f (_ssssssssssssssssoDefaultRootDeviceType x)
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
scmName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackConfigurationManager
    -> f StackConfigurationManager
scmName f x =
    (\y -> x { _scmName = y })
       <$> f (_scmName x)
{-# INLINE scmName #-}

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackConfigurationManager
    -> f StackConfigurationManager
scmVersion f x =
    (\y -> x { _scmVersion = y })
       <$> f (_scmVersion x)
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
ssssssssssssssssyStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackSummary
    -> f StackSummary
ssssssssssssssssyStackId f x =
    (\y -> x { _ssssssssssssssssyStackId = y })
       <$> f (_ssssssssssssssssyStackId x)
{-# INLINE ssssssssssssssssyStackId #-}

-- | The stack name.
ssssssssssssssssyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackSummary
    -> f StackSummary
ssssssssssssssssyName f x =
    (\y -> x { _ssssssssssssssssyName = y })
       <$> f (_ssssssssssssssssyName x)
{-# INLINE ssssssssssssssssyName #-}

-- | The stack's ARN.
ssssssssssssssssyArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StackSummary
    -> f StackSummary
ssssssssssssssssyArn f x =
    (\y -> x { _ssssssssssssssssyArn = y })
       <$> f (_ssssssssssssssssyArn x)
{-# INLINE ssssssssssssssssyArn #-}

-- | The number of layers.
ssssssssssssssssyLayersCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> StackSummary
    -> f StackSummary
ssssssssssssssssyLayersCount f x =
    (\y -> x { _ssssssssssssssssyLayersCount = y })
       <$> f (_ssssssssssssssssyLayersCount x)
{-# INLINE ssssssssssssssssyLayersCount #-}

-- | The number of apps.
ssssssssssssssssyAppsCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> StackSummary
    -> f StackSummary
ssssssssssssssssyAppsCount f x =
    (\y -> x { _ssssssssssssssssyAppsCount = y })
       <$> f (_ssssssssssssssssyAppsCount x)
{-# INLINE ssssssssssssssssyAppsCount #-}

-- | An InstancesCount object with the number of instances in each status.
ssssssssssssssssyInstancesCount
    :: Functor f
    => (Maybe InstancesCount
    -> f (Maybe InstancesCount))
    -> StackSummary
    -> f StackSummary
ssssssssssssssssyInstancesCount f x =
    (\y -> x { _ssssssssssssssssyInstancesCount = y })
       <$> f (_ssssssssssssssssyInstancesCount x)
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
tbasdInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TimeBasedAutoScalingConfiguration
    -> f TimeBasedAutoScalingConfiguration
tbasdInstanceId f x =
    (\y -> x { _tbasdInstanceId = y })
       <$> f (_tbasdInstanceId x)
{-# INLINE tbasdInstanceId #-}

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
tbasdAutoScalingSchedule
    :: Functor f
    => (Maybe WeeklyAutoScalingSchedule
    -> f (Maybe WeeklyAutoScalingSchedule))
    -> TimeBasedAutoScalingConfiguration
    -> f TimeBasedAutoScalingConfiguration
tbasdAutoScalingSchedule f x =
    (\y -> x { _tbasdAutoScalingSchedule = y })
       <$> f (_tbasdAutoScalingSchedule x)
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
uxIamUserArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserProfile
    -> f UserProfile
uxIamUserArn f x =
    (\y -> x { _uxIamUserArn = y })
       <$> f (_uxIamUserArn x)
{-# INLINE uxIamUserArn #-}

-- | The user's name.
uxName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserProfile
    -> f UserProfile
uxName f x =
    (\y -> x { _uxName = y })
       <$> f (_uxName x)
{-# INLINE uxName #-}

-- | The user's SSH user name.
uxSshUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserProfile
    -> f UserProfile
uxSshUsername f x =
    (\y -> x { _uxSshUsername = y })
       <$> f (_uxSshUsername x)
{-# INLINE uxSshUsername #-}

-- | The user's SSH public key.
uxSshPublicKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserProfile
    -> f UserProfile
uxSshPublicKey f x =
    (\y -> x { _uxSshPublicKey = y })
       <$> f (_uxSshPublicKey x)
{-# INLINE uxSshPublicKey #-}

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Managing User Permissions.
uxAllowSelfManagement
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UserProfile
    -> f UserProfile
uxAllowSelfManagement f x =
    (\y -> x { _uxAllowSelfManagement = y })
       <$> f (_uxAllowSelfManagement x)
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
vgVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgVolumeId f x =
    (\y -> x { _vgVolumeId = y })
       <$> f (_vgVolumeId x)
{-# INLINE vgVolumeId #-}

-- | The Amazon EC2 volume ID.
vgEc2VolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgEc2VolumeId f x =
    (\y -> x { _vgEc2VolumeId = y })
       <$> f (_vgEc2VolumeId x)
{-# INLINE vgEc2VolumeId #-}

-- | The volume name.
vgName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgName f x =
    (\y -> x { _vgName = y })
       <$> f (_vgName x)
{-# INLINE vgName #-}

-- | The RAID array ID.
vgRaidArrayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgRaidArrayId f x =
    (\y -> x { _vgRaidArrayId = y })
       <$> f (_vgRaidArrayId x)
{-# INLINE vgRaidArrayId #-}

-- | The instance ID.
vgInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgInstanceId f x =
    (\y -> x { _vgInstanceId = y })
       <$> f (_vgInstanceId x)
{-# INLINE vgInstanceId #-}

-- | The value returned by DescribeVolumes.
vgStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgStatus f x =
    (\y -> x { _vgStatus = y })
       <$> f (_vgStatus x)
{-# INLINE vgStatus #-}

-- | The volume size.
vgSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Volume
    -> f Volume
vgSize f x =
    (\y -> x { _vgSize = y })
       <$> f (_vgSize x)
{-# INLINE vgSize #-}

-- | The device name.
vgDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgDevice f x =
    (\y -> x { _vgDevice = y })
       <$> f (_vgDevice x)
{-# INLINE vgDevice #-}

-- | The volume mount point. For example "/dev/sdh".
vgMountPoint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgMountPoint f x =
    (\y -> x { _vgMountPoint = y })
       <$> f (_vgMountPoint x)
{-# INLINE vgMountPoint #-}

-- | The AWS region. For more information about AWS regions, see Regions and
-- Endpoints.
vgRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgRegion f x =
    (\y -> x { _vgRegion = y })
       <$> f (_vgRegion x)
{-# INLINE vgRegion #-}

-- | The volume Availability Zone. For more information, see Regions and
-- Endpoints.
vgAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgAvailabilityZone f x =
    (\y -> x { _vgAvailabilityZone = y })
       <$> f (_vgAvailabilityZone x)
{-# INLINE vgAvailabilityZone #-}

-- | The volume type, standard or PIOPS.
vgVolumeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vgVolumeType f x =
    (\y -> x { _vgVolumeType = y })
       <$> f (_vgVolumeType x)
{-# INLINE vgVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
vgIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Volume
    -> f Volume
vgIops f x =
    (\y -> x { _vgIops = y })
       <$> f (_vgIops x)
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
vdMountPoint
    :: Functor f
    => (Text
    -> f (Text))
    -> VolumeConfiguration
    -> f VolumeConfiguration
vdMountPoint f x =
    (\y -> x { _vdMountPoint = y })
       <$> f (_vdMountPoint x)
{-# INLINE vdMountPoint #-}

-- | The volume RAID level.
vdRaidLevel
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VolumeConfiguration
    -> f VolumeConfiguration
vdRaidLevel f x =
    (\y -> x { _vdRaidLevel = y })
       <$> f (_vdRaidLevel x)
{-# INLINE vdRaidLevel #-}

-- | The number of disks in the volume.
vdNumberOfDisks
    :: Functor f
    => (Integer
    -> f (Integer))
    -> VolumeConfiguration
    -> f VolumeConfiguration
vdNumberOfDisks f x =
    (\y -> x { _vdNumberOfDisks = y })
       <$> f (_vdNumberOfDisks x)
{-# INLINE vdNumberOfDisks #-}

-- | The volume size.
vdSize
    :: Functor f
    => (Integer
    -> f (Integer))
    -> VolumeConfiguration
    -> f VolumeConfiguration
vdSize f x =
    (\y -> x { _vdSize = y })
       <$> f (_vdSize x)
{-# INLINE vdSize #-}

-- | The volume type, standard or PIOPS.
vdVolumeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeConfiguration
    -> f VolumeConfiguration
vdVolumeType f x =
    (\y -> x { _vdVolumeType = y })
       <$> f (_vdVolumeType x)
{-# INLINE vdVolumeType #-}

-- | For PIOPS volumes, the IOPS per disk.
vdIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VolumeConfiguration
    -> f VolumeConfiguration
vdIops f x =
    (\y -> x { _vdIops = y })
       <$> f (_vdIops x)
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
wassMonday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassMonday f x =
    (\y -> x { _wassMonday = y })
       <$> f (_wassMonday x)
{-# INLINE wassMonday #-}

-- | The schedule for Tuesday.
wassTuesday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassTuesday f x =
    (\y -> x { _wassTuesday = y })
       <$> f (_wassTuesday x)
{-# INLINE wassTuesday #-}

-- | The schedule for Wednesday.
wassWednesday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassWednesday f x =
    (\y -> x { _wassWednesday = y })
       <$> f (_wassWednesday x)
{-# INLINE wassWednesday #-}

-- | The schedule for Thursday.
wassThursday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassThursday f x =
    (\y -> x { _wassThursday = y })
       <$> f (_wassThursday x)
{-# INLINE wassThursday #-}

-- | The schedule for Friday.
wassFriday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassFriday f x =
    (\y -> x { _wassFriday = y })
       <$> f (_wassFriday x)
{-# INLINE wassFriday #-}

-- | The schedule for Saturday.
wassSaturday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassSaturday f x =
    (\y -> x { _wassSaturday = y })
       <$> f (_wassSaturday x)
{-# INLINE wassSaturday #-}

-- | The schedule for Sunday.
wassSunday
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> WeeklyAutoScalingSchedule
    -> f WeeklyAutoScalingSchedule
wassSunday f x =
    (\y -> x { _wassSunday = y })
       <$> f (_wassSunday x)
{-# INLINE wassSunday #-}

instance FromJSON WeeklyAutoScalingSchedule

instance ToJSON WeeklyAutoScalingSchedule
