{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.OpsWorks.Types
    (
    -- * Service
      OpsWorks

    -- * Errors
    , _ValidationException
    , _ResourceNotFoundException

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

    -- * VirtualizationType
    , VirtualizationType (..)

    -- * VolumeType
    , VolumeType (..)

    -- * App
    , App
    , app
    , appSSLConfiguration
    , appShortname
    , appEnableSSL
    , appCreatedAt
    , appEnvironment
    , appDataSources
    , appAppId
    , appAppSource
    , appName
    , appAttributes
    , appType
    , appDomains
    , appStackId
    , appDescription

    -- * AutoScalingThresholds
    , AutoScalingThresholds
    , autoScalingThresholds
    , astInstanceCount
    , astIgnoreMetricsTime
    , astLoadThreshold
    , astThresholdsWaitTime
    , astAlarms
    , astMemoryThreshold
    , astCPUThreshold

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- * ChefConfiguration
    , ChefConfiguration
    , chefConfiguration
    , ccBerkshelfVersion
    , ccManageBerkshelf

    -- * Command
    , Command
    , command
    , comInstanceId
    , comDeploymentId
    , comStatus
    , comLogURL
    , comCreatedAt
    , comCommandId
    , comExitCode
    , comType
    , comCompletedAt
    , comAcknowledgedAt

    -- * DataSource
    , DataSource
    , dataSource
    , dsARN
    , dsDatabaseName
    , dsType

    -- * Deployment
    , Deployment
    , deployment
    , depDeploymentId
    , depStatus
    , depCommand
    , depCreatedAt
    , depCustomJSON
    , depIAMUserARN
    , depAppId
    , depInstanceIds
    , depCompletedAt
    , depStackId
    , depComment
    , depDuration

    -- * DeploymentCommand
    , DeploymentCommand
    , deploymentCommand
    , dcArgs
    , dcName

    -- * EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdVolumeSize
    , ebdIOPS
    , ebdVolumeType
    , ebdSnapshotId

    -- * ElasticIP
    , ElasticIP
    , elasticIP
    , eiInstanceId
    , eiDomain
    , eiIP
    , eiName
    , eiRegion

    -- * ElasticLoadBalancer
    , ElasticLoadBalancer
    , elasticLoadBalancer
    , elbSubnetIds
    , elbVPCId
    , elbAvailabilityZones
    , elbRegion
    , elbElasticLoadBalancerName
    , elbEC2InstanceIds
    , elbStackId
    , elbLayerId
    , elbDNSName

    -- * EnvironmentVariable
    , EnvironmentVariable
    , environmentVariable
    , evSecure
    , evKey
    , evValue

    -- * Instance
    , Instance
    , instance'
    , insInstanceId
    , insPrivateIP
    , insInstallUpdatesOnBoot
    , insReportedAgentVersion
    , insStatus
    , insPrivateDNS
    , insVirtualizationType
    , insSecurityGroupIds
    , insSSHHostRsaKeyFingerprint
    , insInstanceProfileARN
    , insHostname
    , insCreatedAt
    , insSSHKeyName
    , insEC2InstanceId
    , insRootDeviceVolumeId
    , insSubnetId
    , insInstanceType
    , insInfrastructureClass
    , insEBSOptimized
    , insSSHHostDsaKeyFingerprint
    , insElasticIP
    , insOS
    , insAvailabilityZone
    , insLastServiceErrorId
    , insAutoScalingType
    , insLayerIds
    , insArchitecture
    , insPublicDNS
    , insPublicIP
    , insAMIId
    , insReportedOS
    , insStackId
    , insRegisteredBy
    , insBlockDeviceMappings
    , insRootDeviceType

    -- * InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiSignature
    , iiDocument

    -- * InstancesCount
    , InstancesCount
    , instancesCount
    , icTerminating
    , icPending
    , icOnline
    , icUnassigning
    , icRequested
    , icRunningSetup
    , icDeregistering
    , icBooting
    , icStopped
    , icRebooting
    , icAssigning
    , icShuttingDown
    , icSetupFailed
    , icConnectionLost
    , icTerminated
    , icStopping
    , icRegistered
    , icStartFailed
    , icRegistering

    -- * Layer
    , Layer
    , layer
    , layCustomInstanceProfileARN
    , layInstallUpdatesOnBoot
    , layCustomSecurityGroupIds
    , layLifecycleEventConfiguration
    , layShortname
    , layCreatedAt
    , layDefaultRecipes
    , layCustomRecipes
    , layVolumeConfigurations
    , layEnableAutoHealing
    , layPackages
    , layName
    , layAttributes
    , layAutoAssignPublicIPs
    , layUseEBSOptimizedInstances
    , layType
    , layStackId
    , layLayerId
    , layDefaultSecurityGroupNames
    , layAutoAssignElasticIPs

    -- * LifecycleEventConfiguration
    , LifecycleEventConfiguration
    , lifecycleEventConfiguration
    , lecShutdown

    -- * LoadBasedAutoScalingConfiguration
    , LoadBasedAutoScalingConfiguration
    , loadBasedAutoScalingConfiguration
    , lbascUpScaling
    , lbascEnable
    , lbascDownScaling
    , lbascLayerId

    -- * Permission
    , Permission
    , permission
    , perIAMUserARN
    , perAllowSudo
    , perStackId
    , perLevel
    , perAllowSSH

    -- * RAIDArray
    , RAIDArray
    , raidArray
    , raInstanceId
    , raSize
    , raCreatedAt
    , raIOPS
    , raRAIDLevel
    , raDevice
    , raNumberOfDisks
    , raName
    , raAvailabilityZone
    , raRAIDArrayId
    , raVolumeType
    , raStackId
    , raMountPoint

    -- * RDSDBInstance
    , RDSDBInstance
    , rdsDBInstance
    , rdiDBUser
    , rdiRDSDBInstanceARN
    , rdiMissingOnRDS
    , rdiEngine
    , rdiAddress
    , rdiDBInstanceIdentifier
    , rdiRegion
    , rdiStackId
    , rdiDBPassword

    -- * Recipes
    , Recipes
    , recipes
    , recSetup
    , recUndeploy
    , recShutdown
    , recConfigure
    , recDeploy

    -- * ReportedOS
    , ReportedOS
    , reportedOS
    , roFamily
    , roName
    , roVersion

    -- * SSLConfiguration
    , SSLConfiguration
    , sslConfiguration
    , scChain
    , scCertificate
    , scPrivateKey

    -- * SelfUserProfile
    , SelfUserProfile
    , selfUserProfile
    , supSSHUsername
    , supSSHPublicKey
    , supIAMUserARN
    , supName

    -- * ServiceError'
    , ServiceError'
    , serviceError'
    , seInstanceId
    , seCreatedAt
    , seServiceErrorId
    , seType
    , seMessage
    , seStackId

    -- * ShutdownEventConfiguration
    , ShutdownEventConfiguration
    , shutdownEventConfiguration
    , secExecutionTimeout
    , secDelayUntilElbConnectionsDrained

    -- * Source
    , Source
    , source
    , souURL
    , souUsername
    , souSSHKey
    , souPassword
    , souType
    , souRevision

    -- * Stack
    , Stack
    , stack
    , staDefaultInstanceProfileARN
    , staServiceRoleARN
    , staARN
    , staDefaultRootDeviceType
    , staCreatedAt
    , staChefConfiguration
    , staVPCId
    , staDefaultSSHKeyName
    , staCustomJSON
    , staCustomCookbooksSource
    , staDefaultAvailabilityZone
    , staName
    , staUseOpsworksSecurityGroups
    , staDefaultOS
    , staAttributes
    , staUseCustomCookbooks
    , staDefaultSubnetId
    , staRegion
    , staConfigurationManager
    , staStackId
    , staHostnameTheme

    -- * StackConfigurationManager
    , StackConfigurationManager
    , stackConfigurationManager
    , scmName
    , scmVersion

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssARN
    , ssAppsCount
    , ssName
    , ssStackId
    , ssLayersCount
    , ssInstancesCount

    -- * TemporaryCredential
    , TemporaryCredential
    , temporaryCredential
    , tcInstanceId
    , tcUsername
    , tcPassword
    , tcValidForInMinutes

    -- * TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration
    , timeBasedAutoScalingConfiguration
    , tbascInstanceId
    , tbascAutoScalingSchedule

    -- * UserProfile
    , UserProfile
    , userProfile
    , upSSHUsername
    , upSSHPublicKey
    , upAllowSelfManagement
    , upIAMUserARN
    , upName

    -- * Volume
    , Volume
    , volume
    , volInstanceId
    , volStatus
    , volSize
    , volIOPS
    , volDevice
    , volName
    , volAvailabilityZone
    , volRAIDArrayId
    , volVolumeId
    , volRegion
    , volVolumeType
    , volEC2VolumeId
    , volMountPoint

    -- * VolumeConfiguration
    , VolumeConfiguration
    , volumeConfiguration
    , vcIOPS
    , vcRAIDLevel
    , vcVolumeType
    , vcMountPoint
    , vcNumberOfDisks
    , vcSize

    -- * WeeklyAutoScalingSchedule
    , WeeklyAutoScalingSchedule
    , weeklyAutoScalingSchedule
    , wassThursday
    , wassWednesday
    , wassSaturday
    , wassMonday
    , wassFriday
    , wassSunday
    , wassTuesday
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-02-18@ of the Amazon OpsWorks SDK.
data OpsWorks

instance AWSService OpsWorks where
    type Sg OpsWorks = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "OpsWorks"
            , _svcPrefix = "opsworks"
            , _svcVersion = "2013-02-18"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Indicates that a request was invalid.
_ValidationException :: AWSError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | Indicates that a resource was not found.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

data AppAttributesKeys
    = DocumentRoot
    | RailsEnv
    | AutoBundleOnDeploy
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText AppAttributesKeys where
    parser = takeLowerText >>= \case
        "AutoBundleOnDeploy" -> pure AutoBundleOnDeploy
        "DocumentRoot" -> pure DocumentRoot
        "RailsEnv" -> pure RailsEnv
        e -> fail ("Failure parsing AppAttributesKeys from " ++ show e)

instance ToText AppAttributesKeys where
    toText = \case
        AutoBundleOnDeploy -> "AutoBundleOnDeploy"
        DocumentRoot -> "DocumentRoot"
        RailsEnv -> "RailsEnv"

instance Hashable AppAttributesKeys
instance ToQuery AppAttributesKeys
instance ToHeader AppAttributesKeys

instance ToJSON AppAttributesKeys where
    toJSON = toJSONText

instance FromJSON AppAttributesKeys where
    parseJSON = parseJSONText "AppAttributesKeys"

data AppType
    = Java
    | Other
    | Rails
    | Static
    | PHP
    | Nodejs
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText AppType where
    parser = takeLowerText >>= \case
        "java" -> pure Java
        "nodejs" -> pure Nodejs
        "other" -> pure Other
        "php" -> pure PHP
        "rails" -> pure Rails
        "static" -> pure Static
        e -> fail ("Failure parsing AppType from " ++ show e)

instance ToText AppType where
    toText = \case
        Java -> "java"
        Nodejs -> "nodejs"
        Other -> "other"
        PHP -> "php"
        Rails -> "rails"
        Static -> "static"

instance Hashable AppType
instance ToQuery AppType
instance ToHeader AppType

instance ToJSON AppType where
    toJSON = toJSONText

instance FromJSON AppType where
    parseJSON = parseJSONText "AppType"

data Architecture
    = I386
    | X8664
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText Architecture where
    parser = takeLowerText >>= \case
        "i386" -> pure I386
        "x86_64" -> pure X8664
        e -> fail ("Failure parsing Architecture from " ++ show e)

instance ToText Architecture where
    toText = \case
        I386 -> "i386"
        X8664 -> "x86_64"

instance Hashable Architecture
instance ToQuery Architecture
instance ToHeader Architecture

instance ToJSON Architecture where
    toJSON = toJSONText

instance FromJSON Architecture where
    parseJSON = parseJSONText "Architecture"

data AutoScalingType
    = Timer
    | Load
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText AutoScalingType where
    parser = takeLowerText >>= \case
        "load" -> pure Load
        "timer" -> pure Timer
        e -> fail ("Failure parsing AutoScalingType from " ++ show e)

instance ToText AutoScalingType where
    toText = \case
        Load -> "load"
        Timer -> "timer"

instance Hashable AutoScalingType
instance ToQuery AutoScalingType
instance ToHeader AutoScalingType

instance ToJSON AutoScalingType where
    toJSON = toJSONText

instance FromJSON AutoScalingType where
    parseJSON = parseJSONText "AutoScalingType"

data DeploymentCommandName
    = ExecuteRecipes
    | Start
    | UpdateCustomCookbooks
    | InstallDependencies
    | Undeploy
    | Rollback
    | Restart
    | Stop
    | UpdateDependencies
    | Deploy
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DeploymentCommandName where
    parser = takeLowerText >>= \case
        "deploy" -> pure Deploy
        "execute_recipes" -> pure ExecuteRecipes
        "install_dependencies" -> pure InstallDependencies
        "restart" -> pure Restart
        "rollback" -> pure Rollback
        "start" -> pure Start
        "stop" -> pure Stop
        "undeploy" -> pure Undeploy
        "update_custom_cookbooks" -> pure UpdateCustomCookbooks
        "update_dependencies" -> pure UpdateDependencies
        e -> fail ("Failure parsing DeploymentCommandName from " ++ show e)

instance ToText DeploymentCommandName where
    toText = \case
        Deploy -> "deploy"
        ExecuteRecipes -> "execute_recipes"
        InstallDependencies -> "install_dependencies"
        Restart -> "restart"
        Rollback -> "rollback"
        Start -> "start"
        Stop -> "stop"
        Undeploy -> "undeploy"
        UpdateCustomCookbooks -> "update_custom_cookbooks"
        UpdateDependencies -> "update_dependencies"

instance Hashable DeploymentCommandName
instance ToQuery DeploymentCommandName
instance ToHeader DeploymentCommandName

instance ToJSON DeploymentCommandName where
    toJSON = toJSONText

instance FromJSON DeploymentCommandName where
    parseJSON = parseJSONText "DeploymentCommandName"

data LayerAttributesKeys
    = HaproxyHealthCheckURL
    | MemcachedMemory
    | GangliaPassword
    | JavaAppServerVersion
    | GangliaURL
    | HaproxyHealthCheckMethod
    | PassengerVersion
    | JVMVersion
    | MysqlRootPassword
    | HaproxyStatsPassword
    | RubyVersion
    | JVMOptions
    | JVM
    | BundlerVersion
    | HaproxyStatsURL
    | ManageBundler
    | RubygemsVersion
    | GangliaUser
    | EnableHaproxyStats
    | MysqlRootPasswordUbiquitous
    | HaproxyStatsUser
    | JavaAppServer
    | NodejsVersion
    | RailsStack
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText LayerAttributesKeys where
    parser = takeLowerText >>= \case
        "BundlerVersion" -> pure BundlerVersion
        "EnableHaproxyStats" -> pure EnableHaproxyStats
        "GangliaPassword" -> pure GangliaPassword
        "GangliaUrl" -> pure GangliaURL
        "GangliaUser" -> pure GangliaUser
        "HaproxyHealthCheckMethod" -> pure HaproxyHealthCheckMethod
        "HaproxyHealthCheckUrl" -> pure HaproxyHealthCheckURL
        "HaproxyStatsPassword" -> pure HaproxyStatsPassword
        "HaproxyStatsUrl" -> pure HaproxyStatsURL
        "HaproxyStatsUser" -> pure HaproxyStatsUser
        "Jvm" -> pure JVM
        "JvmOptions" -> pure JVMOptions
        "JvmVersion" -> pure JVMVersion
        "JavaAppServer" -> pure JavaAppServer
        "JavaAppServerVersion" -> pure JavaAppServerVersion
        "ManageBundler" -> pure ManageBundler
        "MemcachedMemory" -> pure MemcachedMemory
        "MysqlRootPassword" -> pure MysqlRootPassword
        "MysqlRootPasswordUbiquitous" -> pure MysqlRootPasswordUbiquitous
        "NodejsVersion" -> pure NodejsVersion
        "PassengerVersion" -> pure PassengerVersion
        "RailsStack" -> pure RailsStack
        "RubyVersion" -> pure RubyVersion
        "RubygemsVersion" -> pure RubygemsVersion
        e -> fail ("Failure parsing LayerAttributesKeys from " ++ show e)

instance ToText LayerAttributesKeys where
    toText = \case
        BundlerVersion -> "BundlerVersion"
        EnableHaproxyStats -> "EnableHaproxyStats"
        GangliaPassword -> "GangliaPassword"
        GangliaURL -> "GangliaUrl"
        GangliaUser -> "GangliaUser"
        HaproxyHealthCheckMethod -> "HaproxyHealthCheckMethod"
        HaproxyHealthCheckURL -> "HaproxyHealthCheckUrl"
        HaproxyStatsPassword -> "HaproxyStatsPassword"
        HaproxyStatsURL -> "HaproxyStatsUrl"
        HaproxyStatsUser -> "HaproxyStatsUser"
        JVM -> "Jvm"
        JVMOptions -> "JvmOptions"
        JVMVersion -> "JvmVersion"
        JavaAppServer -> "JavaAppServer"
        JavaAppServerVersion -> "JavaAppServerVersion"
        ManageBundler -> "ManageBundler"
        MemcachedMemory -> "MemcachedMemory"
        MysqlRootPassword -> "MysqlRootPassword"
        MysqlRootPasswordUbiquitous -> "MysqlRootPasswordUbiquitous"
        NodejsVersion -> "NodejsVersion"
        PassengerVersion -> "PassengerVersion"
        RailsStack -> "RailsStack"
        RubyVersion -> "RubyVersion"
        RubygemsVersion -> "RubygemsVersion"

instance Hashable LayerAttributesKeys
instance ToQuery LayerAttributesKeys
instance ToHeader LayerAttributesKeys

instance ToJSON LayerAttributesKeys where
    toJSON = toJSONText

instance FromJSON LayerAttributesKeys where
    parseJSON = parseJSONText "LayerAttributesKeys"

data LayerType
    = Memcached
    | JavaApp
    | MonitoringMaster
    | NodejsApp
    | Custom
    | LB
    | RailsApp
    | DBMaster
    | Web
    | PHPApp
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText LayerType where
    parser = takeLowerText >>= \case
        "custom" -> pure Custom
        "db-master" -> pure DBMaster
        "java-app" -> pure JavaApp
        "lb" -> pure LB
        "memcached" -> pure Memcached
        "monitoring-master" -> pure MonitoringMaster
        "nodejs-app" -> pure NodejsApp
        "php-app" -> pure PHPApp
        "rails-app" -> pure RailsApp
        "web" -> pure Web
        e -> fail ("Failure parsing LayerType from " ++ show e)

instance ToText LayerType where
    toText = \case
        Custom -> "custom"
        DBMaster -> "db-master"
        JavaApp -> "java-app"
        LB -> "lb"
        Memcached -> "memcached"
        MonitoringMaster -> "monitoring-master"
        NodejsApp -> "nodejs-app"
        PHPApp -> "php-app"
        RailsApp -> "rails-app"
        Web -> "web"

instance Hashable LayerType
instance ToQuery LayerType
instance ToHeader LayerType

instance ToJSON LayerType where
    toJSON = toJSONText

instance FromJSON LayerType where
    parseJSON = parseJSONText "LayerType"

data RootDeviceType
    = InstanceStore
    | EBS
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RootDeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fail ("Failure parsing RootDeviceType from " ++ show e)

instance ToText RootDeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance Hashable RootDeviceType
instance ToQuery RootDeviceType
instance ToHeader RootDeviceType

instance ToJSON RootDeviceType where
    toJSON = toJSONText

instance FromJSON RootDeviceType where
    parseJSON = parseJSONText "RootDeviceType"

data SourceType
    = SVN
    | Git
    | Archive
    | S3
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "archive" -> pure Archive
        "git" -> pure Git
        "s3" -> pure S3
        "svn" -> pure SVN
        e -> fail ("Failure parsing SourceType from " ++ show e)

instance ToText SourceType where
    toText = \case
        Archive -> "archive"
        Git -> "git"
        S3 -> "s3"
        SVN -> "svn"

instance Hashable SourceType
instance ToQuery SourceType
instance ToHeader SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"

data StackAttributesKeys =
    Color
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText StackAttributesKeys where
    parser = takeLowerText >>= \case
        "Color" -> pure Color
        e -> fail ("Failure parsing StackAttributesKeys from " ++ show e)

instance ToText StackAttributesKeys where
    toText = \case
        Color -> "Color"

instance Hashable StackAttributesKeys
instance ToQuery StackAttributesKeys
instance ToHeader StackAttributesKeys

instance ToJSON StackAttributesKeys where
    toJSON = toJSONText

instance FromJSON StackAttributesKeys where
    parseJSON = parseJSONText "StackAttributesKeys"

data VirtualizationType
    = Paravirtual
    | HVM
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fail ("Failure parsing VirtualizationType from " ++ show e)

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance Hashable VirtualizationType
instance ToQuery VirtualizationType
instance ToHeader VirtualizationType

instance FromJSON VirtualizationType where
    parseJSON = parseJSONText "VirtualizationType"

data VolumeType
    = Standard
    | IO1
    | GP2
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "standard" -> pure Standard
        e -> fail ("Failure parsing VolumeType from " ++ show e)

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        Standard -> "standard"

instance Hashable VolumeType
instance ToQuery VolumeType
instance ToHeader VolumeType

instance ToJSON VolumeType where
    toJSON = toJSONText

instance FromJSON VolumeType where
    parseJSON = parseJSONText "VolumeType"

-- | A description of the app.
--
-- /See:/ 'app' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'appSSLConfiguration'
--
-- * 'appShortname'
--
-- * 'appEnableSSL'
--
-- * 'appCreatedAt'
--
-- * 'appEnvironment'
--
-- * 'appDataSources'
--
-- * 'appAppId'
--
-- * 'appAppSource'
--
-- * 'appName'
--
-- * 'appAttributes'
--
-- * 'appType'
--
-- * 'appDomains'
--
-- * 'appStackId'
--
-- * 'appDescription'
data App = App'
    { _appSSLConfiguration :: Maybe SSLConfiguration
    , _appShortname        :: Maybe Text
    , _appEnableSSL        :: Maybe Bool
    , _appCreatedAt        :: Maybe Text
    , _appEnvironment      :: Maybe [EnvironmentVariable]
    , _appDataSources      :: Maybe [DataSource]
    , _appAppId            :: Maybe Text
    , _appAppSource        :: Maybe Source
    , _appName             :: Maybe Text
    , _appAttributes       :: Maybe (Map AppAttributesKeys Text)
    , _appType             :: Maybe AppType
    , _appDomains          :: Maybe [Text]
    , _appStackId          :: Maybe Text
    , _appDescription      :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'App' smart constructor.
app :: App
app =
    App'
    { _appSSLConfiguration = Nothing
    , _appShortname = Nothing
    , _appEnableSSL = Nothing
    , _appCreatedAt = Nothing
    , _appEnvironment = Nothing
    , _appDataSources = Nothing
    , _appAppId = Nothing
    , _appAppSource = Nothing
    , _appName = Nothing
    , _appAttributes = Nothing
    , _appType = Nothing
    , _appDomains = Nothing
    , _appStackId = Nothing
    , _appDescription = Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
appSSLConfiguration :: Lens' App (Maybe SSLConfiguration)
appSSLConfiguration = lens _appSSLConfiguration (\ s a -> s{_appSSLConfiguration = a});

-- | The app\'s short name.
appShortname :: Lens' App (Maybe Text)
appShortname = lens _appShortname (\ s a -> s{_appShortname = a});

-- | Whether to enable SSL for the app.
appEnableSSL :: Lens' App (Maybe Bool)
appEnableSSL = lens _appEnableSSL (\ s a -> s{_appEnableSSL = a});

-- | When the app was created.
appCreatedAt :: Lens' App (Maybe Text)
appCreatedAt = lens _appCreatedAt (\ s a -> s{_appCreatedAt = a});

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances. For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 10
-- KB (10240 Bytes). This limit should accommodate most if not all use
-- cases, but if you do exceed it, you will cause an exception (API) with
-- an \"Environment: is too large (maximum is 10KB)\" message.
appEnvironment :: Lens' App [EnvironmentVariable]
appEnvironment = lens _appEnvironment (\ s a -> s{_appEnvironment = a}) . _Default;

-- | The app\'s data sources.
appDataSources :: Lens' App [DataSource]
appDataSources = lens _appDataSources (\ s a -> s{_appDataSources = a}) . _Default;

-- | The app ID.
appAppId :: Lens' App (Maybe Text)
appAppId = lens _appAppId (\ s a -> s{_appAppId = a});

-- | A @Source@ object that describes the app repository.
appAppSource :: Lens' App (Maybe Source)
appAppSource = lens _appAppSource (\ s a -> s{_appAppSource = a});

-- | The app name.
appName :: Lens' App (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a});

-- | The stack attributes.
appAttributes :: Lens' App (HashMap AppAttributesKeys Text)
appAttributes = lens _appAttributes (\ s a -> s{_appAttributes = a}) . _Default . _Map;

-- | The app type.
appType :: Lens' App (Maybe AppType)
appType = lens _appType (\ s a -> s{_appType = a});

-- | The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
appDomains :: Lens' App [Text]
appDomains = lens _appDomains (\ s a -> s{_appDomains = a}) . _Default;

-- | The app stack ID.
appStackId :: Lens' App (Maybe Text)
appStackId = lens _appStackId (\ s a -> s{_appStackId = a});

-- | A description of the app.
appDescription :: Lens' App (Maybe Text)
appDescription = lens _appDescription (\ s a -> s{_appDescription = a});

instance FromJSON App where
        parseJSON
          = withObject "App"
              (\ x ->
                 App' <$>
                   (x .:? "SslConfiguration") <*> (x .:? "Shortname")
                     <*> (x .:? "EnableSsl")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Environment" .!= mempty)
                     <*> (x .:? "DataSources" .!= mempty)
                     <*> (x .:? "AppId")
                     <*> (x .:? "AppSource")
                     <*> (x .:? "Name")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "Type")
                     <*> (x .:? "Domains" .!= mempty)
                     <*> (x .:? "StackId")
                     <*> (x .:? "Description"))

-- | Describes a load-based auto scaling upscaling or downscaling threshold
-- configuration, which specifies when AWS OpsWorks starts or stops
-- load-based instances.
--
-- /See:/ 'autoScalingThresholds' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'astInstanceCount'
--
-- * 'astIgnoreMetricsTime'
--
-- * 'astLoadThreshold'
--
-- * 'astThresholdsWaitTime'
--
-- * 'astAlarms'
--
-- * 'astMemoryThreshold'
--
-- * 'astCPUThreshold'
data AutoScalingThresholds = AutoScalingThresholds'
    { _astInstanceCount      :: Maybe Int
    , _astIgnoreMetricsTime  :: Maybe Nat
    , _astLoadThreshold      :: Maybe Double
    , _astThresholdsWaitTime :: Maybe Nat
    , _astAlarms             :: Maybe [Text]
    , _astMemoryThreshold    :: Maybe Double
    , _astCPUThreshold       :: Maybe Double
    } deriving (Eq,Read,Show)

-- | 'AutoScalingThresholds' smart constructor.
autoScalingThresholds :: AutoScalingThresholds
autoScalingThresholds =
    AutoScalingThresholds'
    { _astInstanceCount = Nothing
    , _astIgnoreMetricsTime = Nothing
    , _astLoadThreshold = Nothing
    , _astThresholdsWaitTime = Nothing
    , _astAlarms = Nothing
    , _astMemoryThreshold = Nothing
    , _astCPUThreshold = Nothing
    }

-- | The number of instances to add or remove when the load exceeds a
-- threshold.
astInstanceCount :: Lens' AutoScalingThresholds (Maybe Int)
astInstanceCount = lens _astInstanceCount (\ s a -> s{_astInstanceCount = a});

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks should ignore metrics and suppress additional scaling events.
-- For example, AWS OpsWorks adds new instances following an upscaling
-- event but the instances won\'t start reducing the load until they have
-- been booted and configured. There is no point in raising additional
-- scaling events during that operation, which typically takes several
-- minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks to
-- suppress scaling events long enough to get the new instances online.
astIgnoreMetricsTime :: Lens' AutoScalingThresholds (Maybe Natural)
astIgnoreMetricsTime = lens _astIgnoreMetricsTime (\ s a -> s{_astIgnoreMetricsTime = a}) . mapping _Nat;

-- | The load threshold. For more information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
astLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astLoadThreshold = lens _astLoadThreshold (\ s a -> s{_astLoadThreshold = a});

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
astThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Natural)
astThresholdsWaitTime = lens _astThresholdsWaitTime (\ s a -> s{_astThresholdsWaitTime = a}) . mapping _Nat;

-- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This
-- parameter takes a list of up to five alarm names, which are case
-- sensitive and must be in the same region as the stack.
--
-- To use custom alarms, you must update your service role to allow
-- @cloudwatch:DescribeAlarms@. You can either have AWS OpsWorks update the
-- role for you when you first use this feature or you can edit the role
-- manually. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-servicerole.html Allowing AWS OpsWorks to Act on Your Behalf>.
astAlarms :: Lens' AutoScalingThresholds [Text]
astAlarms = lens _astAlarms (\ s a -> s{_astAlarms = a}) . _Default;

-- | The memory utilization threshold, as a percent of the available memory.
astMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astMemoryThreshold = lens _astMemoryThreshold (\ s a -> s{_astMemoryThreshold = a});

-- | The CPU utilization threshold, as a percent of the available CPU.
astCPUThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astCPUThreshold = lens _astCPUThreshold (\ s a -> s{_astCPUThreshold = a});

instance FromJSON AutoScalingThresholds where
        parseJSON
          = withObject "AutoScalingThresholds"
              (\ x ->
                 AutoScalingThresholds' <$>
                   (x .:? "InstanceCount") <*>
                     (x .:? "IgnoreMetricsTime")
                     <*> (x .:? "LoadThreshold")
                     <*> (x .:? "ThresholdsWaitTime")
                     <*> (x .:? "Alarms" .!= mempty)
                     <*> (x .:? "MemoryThreshold")
                     <*> (x .:? "CpuThreshold"))

instance ToJSON AutoScalingThresholds where
        toJSON AutoScalingThresholds'{..}
          = object
              ["InstanceCount" .= _astInstanceCount,
               "IgnoreMetricsTime" .= _astIgnoreMetricsTime,
               "LoadThreshold" .= _astLoadThreshold,
               "ThresholdsWaitTime" .= _astThresholdsWaitTime,
               "Alarms" .= _astAlarms,
               "MemoryThreshold" .= _astMemoryThreshold,
               "CpuThreshold" .= _astCPUThreshold]

-- | Describes a block device mapping. This data type maps directly to the
-- Amazon EC2
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>
-- data type.
--
-- /See:/ 'blockDeviceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmVirtualName'
--
-- * 'bdmNoDevice'
--
-- * 'bdmEBS'
--
-- * 'bdmDeviceName'
data BlockDeviceMapping = BlockDeviceMapping'
    { _bdmVirtualName :: Maybe Text
    , _bdmNoDevice    :: Maybe Text
    , _bdmEBS         :: Maybe EBSBlockDevice
    , _bdmDeviceName  :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'BlockDeviceMapping' smart constructor.
blockDeviceMapping :: BlockDeviceMapping
blockDeviceMapping =
    BlockDeviceMapping'
    { _bdmVirtualName = Nothing
    , _bdmNoDevice = Nothing
    , _bdmEBS = Nothing
    , _bdmDeviceName = Nothing
    }

-- | The virtual device name. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a});

-- | Suppresses the specified device included in the AMI\'s block device
-- mapping.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a});

-- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume
-- when the instance is launched.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBSBlockDevice)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a});

-- | The device name that is exposed to the instance, such as @\/dev\/sdh@.
-- For the root device, you can use the explicit device name or you can set
-- this parameter to @ROOT_DEVICE@ and AWS OpsWorks will provide the
-- correct device name.
bdmDeviceName :: Lens' BlockDeviceMapping (Maybe Text)
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a});

instance FromJSON BlockDeviceMapping where
        parseJSON
          = withObject "BlockDeviceMapping"
              (\ x ->
                 BlockDeviceMapping' <$>
                   (x .:? "VirtualName") <*> (x .:? "NoDevice") <*>
                     (x .:? "Ebs")
                     <*> (x .:? "DeviceName"))

instance ToJSON BlockDeviceMapping where
        toJSON BlockDeviceMapping'{..}
          = object
              ["VirtualName" .= _bdmVirtualName,
               "NoDevice" .= _bdmNoDevice, "Ebs" .= _bdmEBS,
               "DeviceName" .= _bdmDeviceName]

-- | Describes the Chef configuration.
--
-- /See:/ 'chefConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccBerkshelfVersion'
--
-- * 'ccManageBerkshelf'
data ChefConfiguration = ChefConfiguration'
    { _ccBerkshelfVersion :: Maybe Text
    , _ccManageBerkshelf  :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'ChefConfiguration' smart constructor.
chefConfiguration :: ChefConfiguration
chefConfiguration =
    ChefConfiguration'
    { _ccBerkshelfVersion = Nothing
    , _ccManageBerkshelf = Nothing
    }

-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion = lens _ccBerkshelfVersion (\ s a -> s{_ccBerkshelfVersion = a});

-- | Whether to enable Berkshelf.
ccManageBerkshelf :: Lens' ChefConfiguration (Maybe Bool)
ccManageBerkshelf = lens _ccManageBerkshelf (\ s a -> s{_ccManageBerkshelf = a});

instance FromJSON ChefConfiguration where
        parseJSON
          = withObject "ChefConfiguration"
              (\ x ->
                 ChefConfiguration' <$>
                   (x .:? "BerkshelfVersion") <*>
                     (x .:? "ManageBerkshelf"))

instance ToJSON ChefConfiguration where
        toJSON ChefConfiguration'{..}
          = object
              ["BerkshelfVersion" .= _ccBerkshelfVersion,
               "ManageBerkshelf" .= _ccManageBerkshelf]

-- | Describes a command.
--
-- /See:/ 'command' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'comInstanceId'
--
-- * 'comDeploymentId'
--
-- * 'comStatus'
--
-- * 'comLogURL'
--
-- * 'comCreatedAt'
--
-- * 'comCommandId'
--
-- * 'comExitCode'
--
-- * 'comType'
--
-- * 'comCompletedAt'
--
-- * 'comAcknowledgedAt'
data Command = Command'
    { _comInstanceId     :: Maybe Text
    , _comDeploymentId   :: Maybe Text
    , _comStatus         :: Maybe Text
    , _comLogURL         :: Maybe Text
    , _comCreatedAt      :: Maybe Text
    , _comCommandId      :: Maybe Text
    , _comExitCode       :: Maybe Int
    , _comType           :: Maybe Text
    , _comCompletedAt    :: Maybe Text
    , _comAcknowledgedAt :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Command' smart constructor.
command :: Command
command =
    Command'
    { _comInstanceId = Nothing
    , _comDeploymentId = Nothing
    , _comStatus = Nothing
    , _comLogURL = Nothing
    , _comCreatedAt = Nothing
    , _comCommandId = Nothing
    , _comExitCode = Nothing
    , _comType = Nothing
    , _comCompletedAt = Nothing
    , _comAcknowledgedAt = Nothing
    }

-- | The ID of the instance where the command was executed.
comInstanceId :: Lens' Command (Maybe Text)
comInstanceId = lens _comInstanceId (\ s a -> s{_comInstanceId = a});

-- | The command deployment ID.
comDeploymentId :: Lens' Command (Maybe Text)
comDeploymentId = lens _comDeploymentId (\ s a -> s{_comDeploymentId = a});

-- | The command status:
--
-- -   failed
-- -   successful
-- -   skipped
-- -   pending
comStatus :: Lens' Command (Maybe Text)
comStatus = lens _comStatus (\ s a -> s{_comStatus = a});

-- | The URL of the command log.
comLogURL :: Lens' Command (Maybe Text)
comLogURL = lens _comLogURL (\ s a -> s{_comLogURL = a});

-- | Date and time when the command was run.
comCreatedAt :: Lens' Command (Maybe Text)
comCreatedAt = lens _comCreatedAt (\ s a -> s{_comCreatedAt = a});

-- | The command ID.
comCommandId :: Lens' Command (Maybe Text)
comCommandId = lens _comCommandId (\ s a -> s{_comCommandId = a});

-- | The command exit code.
comExitCode :: Lens' Command (Maybe Int)
comExitCode = lens _comExitCode (\ s a -> s{_comExitCode = a});

-- | The command type:
--
-- -   @deploy@
-- -   @rollback@
-- -   @start@
-- -   @stop@
-- -   @restart@
-- -   @undeploy@
-- -   @update_dependencies@
-- -   @install_dependencies@
-- -   @update_custom_cookbooks@
-- -   @execute_recipes@
comType :: Lens' Command (Maybe Text)
comType = lens _comType (\ s a -> s{_comType = a});

-- | Date when the command completed.
comCompletedAt :: Lens' Command (Maybe Text)
comCompletedAt = lens _comCompletedAt (\ s a -> s{_comCompletedAt = a});

-- | Date and time when the command was acknowledged.
comAcknowledgedAt :: Lens' Command (Maybe Text)
comAcknowledgedAt = lens _comAcknowledgedAt (\ s a -> s{_comAcknowledgedAt = a});

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "InstanceId") <*> (x .:? "DeploymentId") <*>
                     (x .:? "Status")
                     <*> (x .:? "LogUrl")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "ExitCode")
                     <*> (x .:? "Type")
                     <*> (x .:? "CompletedAt")
                     <*> (x .:? "AcknowledgedAt"))

-- | Describes an app\'s data source.
--
-- /See:/ 'dataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsARN'
--
-- * 'dsDatabaseName'
--
-- * 'dsType'
data DataSource = DataSource'
    { _dsARN          :: Maybe Text
    , _dsDatabaseName :: Maybe Text
    , _dsType         :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DataSource' smart constructor.
dataSource :: DataSource
dataSource =
    DataSource'
    { _dsARN = Nothing
    , _dsDatabaseName = Nothing
    , _dsType = Nothing
    }

-- | The data source\'s ARN.
dsARN :: Lens' DataSource (Maybe Text)
dsARN = lens _dsARN (\ s a -> s{_dsARN = a});

-- | The database name.
dsDatabaseName :: Lens' DataSource (Maybe Text)
dsDatabaseName = lens _dsDatabaseName (\ s a -> s{_dsDatabaseName = a});

-- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, or @RdsDbInstance@.
dsType :: Lens' DataSource (Maybe Text)
dsType = lens _dsType (\ s a -> s{_dsType = a});

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x ->
                 DataSource' <$>
                   (x .:? "Arn") <*> (x .:? "DatabaseName") <*>
                     (x .:? "Type"))

instance ToJSON DataSource where
        toJSON DataSource'{..}
          = object
              ["Arn" .= _dsARN, "DatabaseName" .= _dsDatabaseName,
               "Type" .= _dsType]

-- | Describes a deployment of a stack or app.
--
-- /See:/ 'deployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depDeploymentId'
--
-- * 'depStatus'
--
-- * 'depCommand'
--
-- * 'depCreatedAt'
--
-- * 'depCustomJSON'
--
-- * 'depIAMUserARN'
--
-- * 'depAppId'
--
-- * 'depInstanceIds'
--
-- * 'depCompletedAt'
--
-- * 'depStackId'
--
-- * 'depComment'
--
-- * 'depDuration'
data Deployment = Deployment'
    { _depDeploymentId :: Maybe Text
    , _depStatus       :: Maybe Text
    , _depCommand      :: Maybe DeploymentCommand
    , _depCreatedAt    :: Maybe Text
    , _depCustomJSON   :: Maybe Text
    , _depIAMUserARN   :: Maybe Text
    , _depAppId        :: Maybe Text
    , _depInstanceIds  :: Maybe [Text]
    , _depCompletedAt  :: Maybe Text
    , _depStackId      :: Maybe Text
    , _depComment      :: Maybe Text
    , _depDuration     :: Maybe Int
    } deriving (Eq,Read,Show)

-- | 'Deployment' smart constructor.
deployment :: Deployment
deployment =
    Deployment'
    { _depDeploymentId = Nothing
    , _depStatus = Nothing
    , _depCommand = Nothing
    , _depCreatedAt = Nothing
    , _depCustomJSON = Nothing
    , _depIAMUserARN = Nothing
    , _depAppId = Nothing
    , _depInstanceIds = Nothing
    , _depCompletedAt = Nothing
    , _depStackId = Nothing
    , _depComment = Nothing
    , _depDuration = Nothing
    }

-- | The deployment ID.
depDeploymentId :: Lens' Deployment (Maybe Text)
depDeploymentId = lens _depDeploymentId (\ s a -> s{_depDeploymentId = a});

-- | The deployment status:
--
-- -   running
-- -   successful
-- -   failed
depStatus :: Lens' Deployment (Maybe Text)
depStatus = lens _depStatus (\ s a -> s{_depStatus = a});

-- | FIXME: Undocumented member.
depCommand :: Lens' Deployment (Maybe DeploymentCommand)
depCommand = lens _depCommand (\ s a -> s{_depCommand = a});

-- | Date when the deployment was created.
depCreatedAt :: Lens' Deployment (Maybe Text)
depCreatedAt = lens _depCreatedAt (\ s a -> s{_depCreatedAt = a});

-- | A string that contains user-defined custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- for stack or to pass data to recipes. The string should be in the
-- following format and must escape characters such as \'\"\'.:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
depCustomJSON :: Lens' Deployment (Maybe Text)
depCustomJSON = lens _depCustomJSON (\ s a -> s{_depCustomJSON = a});

-- | The user\'s IAM ARN.
depIAMUserARN :: Lens' Deployment (Maybe Text)
depIAMUserARN = lens _depIAMUserARN (\ s a -> s{_depIAMUserARN = a});

-- | The app ID.
depAppId :: Lens' Deployment (Maybe Text)
depAppId = lens _depAppId (\ s a -> s{_depAppId = a});

-- | The IDs of the target instances.
depInstanceIds :: Lens' Deployment [Text]
depInstanceIds = lens _depInstanceIds (\ s a -> s{_depInstanceIds = a}) . _Default;

-- | Date when the deployment completed.
depCompletedAt :: Lens' Deployment (Maybe Text)
depCompletedAt = lens _depCompletedAt (\ s a -> s{_depCompletedAt = a});

-- | The stack ID.
depStackId :: Lens' Deployment (Maybe Text)
depStackId = lens _depStackId (\ s a -> s{_depStackId = a});

-- | A user-defined comment.
depComment :: Lens' Deployment (Maybe Text)
depComment = lens _depComment (\ s a -> s{_depComment = a});

-- | The deployment duration.
depDuration :: Lens' Deployment (Maybe Int)
depDuration = lens _depDuration (\ s a -> s{_depDuration = a});

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "DeploymentId") <*> (x .:? "Status") <*>
                     (x .:? "Command")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CustomJson")
                     <*> (x .:? "IamUserArn")
                     <*> (x .:? "AppId")
                     <*> (x .:? "InstanceIds" .!= mempty)
                     <*> (x .:? "CompletedAt")
                     <*> (x .:? "StackId")
                     <*> (x .:? "Comment")
                     <*> (x .:? "Duration"))

-- | Used to specify a stack or deployment command.
--
-- /See:/ 'deploymentCommand' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcArgs'
--
-- * 'dcName'
data DeploymentCommand = DeploymentCommand'
    { _dcArgs :: Maybe (Map Text [Text])
    , _dcName :: DeploymentCommandName
    } deriving (Eq,Read,Show)

-- | 'DeploymentCommand' smart constructor.
deploymentCommand :: DeploymentCommandName -> DeploymentCommand
deploymentCommand pName =
    DeploymentCommand'
    { _dcArgs = Nothing
    , _dcName = pName
    }

-- | The arguments of those commands that take arguments. It should be set to
-- a JSON object with the following format:
--
-- @{\"arg_name1\" : [\"value1\", \"value2\", ...], \"arg_name2\" : [\"value1\", \"value2\", ...], ...}@
--
-- The @update_dependencies@ command takes two arguments:
--
-- -   @upgrade_os_to@ - Specifies the desired Amazon Linux version for
--     instances whose OS you want to upgrade, such as
--     @Amazon Linux 2014.09@. You must also set the @allow_reboot@
--     argument to true.
-- -   @allow_reboot@ - Specifies whether to allow AWS OpsWorks to reboot
--     the instances if necessary, after installing the updates. This
--     argument can be set to either @true@ or @false@. The default value
--     is @false@.
--
-- For example, to upgrade an instance to Amazon Linux 2014.09, set @Args@
-- to the following.
--
-- @ { \"upgrade_os_to\":[\"Amazon Linux 2014.09\"], \"allow_reboot\":[\"true\"] } @
dcArgs :: Lens' DeploymentCommand (HashMap Text [Text])
dcArgs = lens _dcArgs (\ s a -> s{_dcArgs = a}) . _Default . _Map;

-- | Specifies the operation. You can specify only one command.
--
-- For stacks, the following commands are available:
--
-- -   @execute_recipes@: Execute one or more recipes. To specify the
--     recipes, set an @Args@ parameter named @recipes@ to the list of
--     recipes to be executed. For example, to execute @phpapp::appsetup@,
--     set @Args@ to @{\"recipes\":[\"phpapp::appsetup\"]}@.
-- -   @install_dependencies@: Install the stack\'s dependencies.
-- -   @update_custom_cookbooks@: Update the stack\'s custom cookbooks.
-- -   @update_dependencies@: Update the stack\'s dependencies.
--
-- The update_dependencies and install_dependencies commands are supported
-- only for Linux instances. You can run the commands successfully on
-- Windows instances, but they do nothing.
--
-- For apps, the following commands are available:
--
-- -   @deploy@: Deploy an app. Ruby on Rails apps have an optional @Args@
--     parameter named @migrate@. Set @Args@ to {\"migrate\":[\"true\"]} to
--     migrate the database. The default setting is
--     {\"migrate\":[\"false\"]}.
-- -   @rollback@ Roll the app back to the previous version. When you
--     update an app, AWS OpsWorks stores the previous version, up to a
--     maximum of five versions. You can use this command to roll an app
--     back as many as four versions.
-- -   @start@: Start the app\'s web or application server.
-- -   @stop@: Stop the app\'s web or application server.
-- -   @restart@: Restart the app\'s web or application server.
-- -   @undeploy@: Undeploy the app.
dcName :: Lens' DeploymentCommand DeploymentCommandName
dcName = lens _dcName (\ s a -> s{_dcName = a});

instance FromJSON DeploymentCommand where
        parseJSON
          = withObject "DeploymentCommand"
              (\ x ->
                 DeploymentCommand' <$>
                   (x .:? "Args" .!= mempty) <*> (x .: "Name"))

instance ToJSON DeploymentCommand where
        toJSON DeploymentCommand'{..}
          = object ["Args" .= _dcArgs, "Name" .= _dcName]

-- | Describes an Amazon EBS volume. This data type maps directly to the
-- Amazon EC2
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>
-- data type.
--
-- /See:/ 'ebsBlockDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebdDeleteOnTermination'
--
-- * 'ebdVolumeSize'
--
-- * 'ebdIOPS'
--
-- * 'ebdVolumeType'
--
-- * 'ebdSnapshotId'
data EBSBlockDevice = EBSBlockDevice'
    { _ebdDeleteOnTermination :: Maybe Bool
    , _ebdVolumeSize          :: Maybe Int
    , _ebdIOPS                :: Maybe Int
    , _ebdVolumeType          :: Maybe VolumeType
    , _ebdSnapshotId          :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'EBSBlockDevice' smart constructor.
ebsBlockDevice :: EBSBlockDevice
ebsBlockDevice =
    EBSBlockDevice'
    { _ebdDeleteOnTermination = Nothing
    , _ebdVolumeSize = Nothing
    , _ebdIOPS = Nothing
    , _ebdVolumeType = Nothing
    , _ebdSnapshotId = Nothing
    }

-- | Whether the volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EBSBlockDevice (Maybe Bool)
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\ s a -> s{_ebdDeleteOnTermination = a});

-- | The volume size, in GiB. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebdVolumeSize :: Lens' EBSBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\ s a -> s{_ebdVolumeSize = a});

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebdIOPS :: Lens' EBSBlockDevice (Maybe Int)
ebdIOPS = lens _ebdIOPS (\ s a -> s{_ebdIOPS = a});

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, and @standard@ for Magnetic volumes.
ebdVolumeType :: Lens' EBSBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\ s a -> s{_ebdVolumeType = a});

-- | The snapshot ID.
ebdSnapshotId :: Lens' EBSBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\ s a -> s{_ebdSnapshotId = a});

instance FromJSON EBSBlockDevice where
        parseJSON
          = withObject "EBSBlockDevice"
              (\ x ->
                 EBSBlockDevice' <$>
                   (x .:? "DeleteOnTermination") <*>
                     (x .:? "VolumeSize")
                     <*> (x .:? "Iops")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "SnapshotId"))

instance ToJSON EBSBlockDevice where
        toJSON EBSBlockDevice'{..}
          = object
              ["DeleteOnTermination" .= _ebdDeleteOnTermination,
               "VolumeSize" .= _ebdVolumeSize, "Iops" .= _ebdIOPS,
               "VolumeType" .= _ebdVolumeType,
               "SnapshotId" .= _ebdSnapshotId]

-- | Describes an Elastic IP address.
--
-- /See:/ 'elasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiInstanceId'
--
-- * 'eiDomain'
--
-- * 'eiIP'
--
-- * 'eiName'
--
-- * 'eiRegion'
data ElasticIP = ElasticIP'
    { _eiInstanceId :: Maybe Text
    , _eiDomain     :: Maybe Text
    , _eiIP         :: Maybe Text
    , _eiName       :: Maybe Text
    , _eiRegion     :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ElasticIP' smart constructor.
elasticIP :: ElasticIP
elasticIP =
    ElasticIP'
    { _eiInstanceId = Nothing
    , _eiDomain = Nothing
    , _eiIP = Nothing
    , _eiName = Nothing
    , _eiRegion = Nothing
    }

-- | The ID of the instance that the address is attached to.
eiInstanceId :: Lens' ElasticIP (Maybe Text)
eiInstanceId = lens _eiInstanceId (\ s a -> s{_eiInstanceId = a});

-- | The domain.
eiDomain :: Lens' ElasticIP (Maybe Text)
eiDomain = lens _eiDomain (\ s a -> s{_eiDomain = a});

-- | The IP address.
eiIP :: Lens' ElasticIP (Maybe Text)
eiIP = lens _eiIP (\ s a -> s{_eiIP = a});

-- | The name.
eiName :: Lens' ElasticIP (Maybe Text)
eiName = lens _eiName (\ s a -> s{_eiName = a});

-- | The AWS region. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
eiRegion :: Lens' ElasticIP (Maybe Text)
eiRegion = lens _eiRegion (\ s a -> s{_eiRegion = a});

instance FromJSON ElasticIP where
        parseJSON
          = withObject "ElasticIP"
              (\ x ->
                 ElasticIP' <$>
                   (x .:? "InstanceId") <*> (x .:? "Domain") <*>
                     (x .:? "Ip")
                     <*> (x .:? "Name")
                     <*> (x .:? "Region"))

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'elasticLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elbSubnetIds'
--
-- * 'elbVPCId'
--
-- * 'elbAvailabilityZones'
--
-- * 'elbRegion'
--
-- * 'elbElasticLoadBalancerName'
--
-- * 'elbEC2InstanceIds'
--
-- * 'elbStackId'
--
-- * 'elbLayerId'
--
-- * 'elbDNSName'
data ElasticLoadBalancer = ElasticLoadBalancer'
    { _elbSubnetIds               :: Maybe [Text]
    , _elbVPCId                   :: Maybe Text
    , _elbAvailabilityZones       :: Maybe [Text]
    , _elbRegion                  :: Maybe Text
    , _elbElasticLoadBalancerName :: Maybe Text
    , _elbEC2InstanceIds          :: Maybe [Text]
    , _elbStackId                 :: Maybe Text
    , _elbLayerId                 :: Maybe Text
    , _elbDNSName                 :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ElasticLoadBalancer' smart constructor.
elasticLoadBalancer :: ElasticLoadBalancer
elasticLoadBalancer =
    ElasticLoadBalancer'
    { _elbSubnetIds = Nothing
    , _elbVPCId = Nothing
    , _elbAvailabilityZones = Nothing
    , _elbRegion = Nothing
    , _elbElasticLoadBalancerName = Nothing
    , _elbEC2InstanceIds = Nothing
    , _elbStackId = Nothing
    , _elbLayerId = Nothing
    , _elbDNSName = Nothing
    }

-- | A list of subnet IDs, if the stack is running in a VPC.
elbSubnetIds :: Lens' ElasticLoadBalancer [Text]
elbSubnetIds = lens _elbSubnetIds (\ s a -> s{_elbSubnetIds = a}) . _Default;

-- | The VPC ID.
elbVPCId :: Lens' ElasticLoadBalancer (Maybe Text)
elbVPCId = lens _elbVPCId (\ s a -> s{_elbVPCId = a});

-- | A list of Availability Zones.
elbAvailabilityZones :: Lens' ElasticLoadBalancer [Text]
elbAvailabilityZones = lens _elbAvailabilityZones (\ s a -> s{_elbAvailabilityZones = a}) . _Default;

-- | The instance\'s AWS region.
elbRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elbRegion = lens _elbRegion (\ s a -> s{_elbRegion = a});

-- | The Elastic Load Balancing instance\'s name.
elbElasticLoadBalancerName :: Lens' ElasticLoadBalancer (Maybe Text)
elbElasticLoadBalancerName = lens _elbElasticLoadBalancerName (\ s a -> s{_elbElasticLoadBalancerName = a});

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elbEC2InstanceIds :: Lens' ElasticLoadBalancer [Text]
elbEC2InstanceIds = lens _elbEC2InstanceIds (\ s a -> s{_elbEC2InstanceIds = a}) . _Default;

-- | The ID of the stack that the instance is associated with.
elbStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elbStackId = lens _elbStackId (\ s a -> s{_elbStackId = a});

-- | The ID of the layer that the instance is attached to.
elbLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elbLayerId = lens _elbLayerId (\ s a -> s{_elbLayerId = a});

-- | The instance\'s public DNS name.
elbDNSName :: Lens' ElasticLoadBalancer (Maybe Text)
elbDNSName = lens _elbDNSName (\ s a -> s{_elbDNSName = a});

instance FromJSON ElasticLoadBalancer where
        parseJSON
          = withObject "ElasticLoadBalancer"
              (\ x ->
                 ElasticLoadBalancer' <$>
                   (x .:? "SubnetIds" .!= mempty) <*> (x .:? "VpcId")
                     <*> (x .:? "AvailabilityZones" .!= mempty)
                     <*> (x .:? "Region")
                     <*> (x .:? "ElasticLoadBalancerName")
                     <*> (x .:? "Ec2InstanceIds" .!= mempty)
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayerId")
                     <*> (x .:? "DnsName"))

-- | Represents an app\'s environment variable.
--
-- /See:/ 'environmentVariable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evSecure'
--
-- * 'evKey'
--
-- * 'evValue'
data EnvironmentVariable = EnvironmentVariable'
    { _evSecure :: Maybe Bool
    , _evKey    :: Text
    , _evValue  :: Text
    } deriving (Eq,Read,Show)

-- | 'EnvironmentVariable' smart constructor.
environmentVariable :: Text -> Text -> EnvironmentVariable
environmentVariable pKey pValue =
    EnvironmentVariable'
    { _evSecure = Nothing
    , _evKey = pKey
    , _evValue = pValue
    }

-- | (Optional) Whether the variable\'s value will be returned by the
-- DescribeApps action. To conceal an environment variable\'s value, set
-- @Secure@ to @true@. @DescribeApps@ then returns @*****FILTERED*****@
-- instead of the actual value. The default value for @Secure@ is @false@.
evSecure :: Lens' EnvironmentVariable (Maybe Bool)
evSecure = lens _evSecure (\ s a -> s{_evSecure = a});

-- | (Required) The environment variable\'s name, which can consist of up to
-- 64 characters and must be specified. The name can contain upper- and
-- lowercase letters, numbers, and underscores (_), but it must start with
-- a letter or underscore.
evKey :: Lens' EnvironmentVariable Text
evKey = lens _evKey (\ s a -> s{_evKey = a});

-- | (Optional) The environment variable\'s value, which can be left empty.
-- If you specify a value, it can contain up to 256 characters, which must
-- all be printable.
evValue :: Lens' EnvironmentVariable Text
evValue = lens _evValue (\ s a -> s{_evValue = a});

instance FromJSON EnvironmentVariable where
        parseJSON
          = withObject "EnvironmentVariable"
              (\ x ->
                 EnvironmentVariable' <$>
                   (x .:? "Secure") <*> (x .: "Key") <*> (x .: "Value"))

instance ToJSON EnvironmentVariable where
        toJSON EnvironmentVariable'{..}
          = object
              ["Secure" .= _evSecure, "Key" .= _evKey,
               "Value" .= _evValue]

-- | Describes an instance.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'insInstanceId'
--
-- * 'insPrivateIP'
--
-- * 'insInstallUpdatesOnBoot'
--
-- * 'insReportedAgentVersion'
--
-- * 'insStatus'
--
-- * 'insPrivateDNS'
--
-- * 'insVirtualizationType'
--
-- * 'insSecurityGroupIds'
--
-- * 'insSSHHostRsaKeyFingerprint'
--
-- * 'insInstanceProfileARN'
--
-- * 'insHostname'
--
-- * 'insCreatedAt'
--
-- * 'insSSHKeyName'
--
-- * 'insEC2InstanceId'
--
-- * 'insRootDeviceVolumeId'
--
-- * 'insSubnetId'
--
-- * 'insInstanceType'
--
-- * 'insInfrastructureClass'
--
-- * 'insEBSOptimized'
--
-- * 'insSSHHostDsaKeyFingerprint'
--
-- * 'insElasticIP'
--
-- * 'insOS'
--
-- * 'insAvailabilityZone'
--
-- * 'insLastServiceErrorId'
--
-- * 'insAutoScalingType'
--
-- * 'insLayerIds'
--
-- * 'insArchitecture'
--
-- * 'insPublicDNS'
--
-- * 'insPublicIP'
--
-- * 'insAMIId'
--
-- * 'insReportedOS'
--
-- * 'insStackId'
--
-- * 'insRegisteredBy'
--
-- * 'insBlockDeviceMappings'
--
-- * 'insRootDeviceType'
data Instance = Instance'
    { _insInstanceId               :: Maybe Text
    , _insPrivateIP                :: Maybe Text
    , _insInstallUpdatesOnBoot     :: Maybe Bool
    , _insReportedAgentVersion     :: Maybe Text
    , _insStatus                   :: Maybe Text
    , _insPrivateDNS               :: Maybe Text
    , _insVirtualizationType       :: Maybe VirtualizationType
    , _insSecurityGroupIds         :: Maybe [Text]
    , _insSSHHostRsaKeyFingerprint :: Maybe Text
    , _insInstanceProfileARN       :: Maybe Text
    , _insHostname                 :: Maybe Text
    , _insCreatedAt                :: Maybe Text
    , _insSSHKeyName               :: Maybe Text
    , _insEC2InstanceId            :: Maybe Text
    , _insRootDeviceVolumeId       :: Maybe Text
    , _insSubnetId                 :: Maybe Text
    , _insInstanceType             :: Maybe Text
    , _insInfrastructureClass      :: Maybe Text
    , _insEBSOptimized             :: Maybe Bool
    , _insSSHHostDsaKeyFingerprint :: Maybe Text
    , _insElasticIP                :: Maybe Text
    , _insOS                       :: Maybe Text
    , _insAvailabilityZone         :: Maybe Text
    , _insLastServiceErrorId       :: Maybe Text
    , _insAutoScalingType          :: Maybe AutoScalingType
    , _insLayerIds                 :: Maybe [Text]
    , _insArchitecture             :: Maybe Architecture
    , _insPublicDNS                :: Maybe Text
    , _insPublicIP                 :: Maybe Text
    , _insAMIId                    :: Maybe Text
    , _insReportedOS               :: Maybe ReportedOS
    , _insStackId                  :: Maybe Text
    , _insRegisteredBy             :: Maybe Text
    , _insBlockDeviceMappings      :: Maybe [BlockDeviceMapping]
    , _insRootDeviceType           :: Maybe RootDeviceType
    } deriving (Eq,Read,Show)

-- | 'Instance' smart constructor.
instance' :: Instance
instance' =
    Instance'
    { _insInstanceId = Nothing
    , _insPrivateIP = Nothing
    , _insInstallUpdatesOnBoot = Nothing
    , _insReportedAgentVersion = Nothing
    , _insStatus = Nothing
    , _insPrivateDNS = Nothing
    , _insVirtualizationType = Nothing
    , _insSecurityGroupIds = Nothing
    , _insSSHHostRsaKeyFingerprint = Nothing
    , _insInstanceProfileARN = Nothing
    , _insHostname = Nothing
    , _insCreatedAt = Nothing
    , _insSSHKeyName = Nothing
    , _insEC2InstanceId = Nothing
    , _insRootDeviceVolumeId = Nothing
    , _insSubnetId = Nothing
    , _insInstanceType = Nothing
    , _insInfrastructureClass = Nothing
    , _insEBSOptimized = Nothing
    , _insSSHHostDsaKeyFingerprint = Nothing
    , _insElasticIP = Nothing
    , _insOS = Nothing
    , _insAvailabilityZone = Nothing
    , _insLastServiceErrorId = Nothing
    , _insAutoScalingType = Nothing
    , _insLayerIds = Nothing
    , _insArchitecture = Nothing
    , _insPublicDNS = Nothing
    , _insPublicIP = Nothing
    , _insAMIId = Nothing
    , _insReportedOS = Nothing
    , _insStackId = Nothing
    , _insRegisteredBy = Nothing
    , _insBlockDeviceMappings = Nothing
    , _insRootDeviceType = Nothing
    }

-- | The instance ID.
insInstanceId :: Lens' Instance (Maybe Text)
insInstanceId = lens _insInstanceId (\ s a -> s{_insInstanceId = a});

-- | The instance private IP address.
insPrivateIP :: Lens' Instance (Maybe Text)
insPrivateIP = lens _insPrivateIP (\ s a -> s{_insPrivateIP = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
insInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
insInstallUpdatesOnBoot = lens _insInstallUpdatesOnBoot (\ s a -> s{_insInstallUpdatesOnBoot = a});

-- | The instance\'s reported AWS OpsWorks agent version.
insReportedAgentVersion :: Lens' Instance (Maybe Text)
insReportedAgentVersion = lens _insReportedAgentVersion (\ s a -> s{_insReportedAgentVersion = a});

-- | The instance status:
--
-- -   @booting@
-- -   @connection_lost@
-- -   @online@
-- -   @pending@
-- -   @rebooting@
-- -   @requested@
-- -   @running_setup@
-- -   @setup_failed@
-- -   @shutting_down@
-- -   @start_failed@
-- -   @stopped@
-- -   @stopping@
-- -   @terminated@
-- -   @terminating@
insStatus :: Lens' Instance (Maybe Text)
insStatus = lens _insStatus (\ s a -> s{_insStatus = a});

-- | The instance private DNS name.
insPrivateDNS :: Lens' Instance (Maybe Text)
insPrivateDNS = lens _insPrivateDNS (\ s a -> s{_insPrivateDNS = a});

-- | The instance\'s virtualization type, @paravirtual@ or @hvm@.
insVirtualizationType :: Lens' Instance (Maybe VirtualizationType)
insVirtualizationType = lens _insVirtualizationType (\ s a -> s{_insVirtualizationType = a});

-- | An array containing the instance security group IDs.
insSecurityGroupIds :: Lens' Instance [Text]
insSecurityGroupIds = lens _insSecurityGroupIds (\ s a -> s{_insSecurityGroupIds = a}) . _Default;

-- | The SSH key\'s RSA fingerprint.
insSSHHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
insSSHHostRsaKeyFingerprint = lens _insSSHHostRsaKeyFingerprint (\ s a -> s{_insSSHHostRsaKeyFingerprint = a});

-- | The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
insInstanceProfileARN :: Lens' Instance (Maybe Text)
insInstanceProfileARN = lens _insInstanceProfileARN (\ s a -> s{_insInstanceProfileARN = a});

-- | The instance host name.
insHostname :: Lens' Instance (Maybe Text)
insHostname = lens _insHostname (\ s a -> s{_insHostname = a});

-- | The time that the instance was created.
insCreatedAt :: Lens' Instance (Maybe Text)
insCreatedAt = lens _insCreatedAt (\ s a -> s{_insCreatedAt = a});

-- | The instance\'s Amazon EC2 key pair name.
insSSHKeyName :: Lens' Instance (Maybe Text)
insSSHKeyName = lens _insSSHKeyName (\ s a -> s{_insSSHKeyName = a});

-- | The ID of the associated Amazon EC2 instance.
insEC2InstanceId :: Lens' Instance (Maybe Text)
insEC2InstanceId = lens _insEC2InstanceId (\ s a -> s{_insEC2InstanceId = a});

-- | The root device volume ID.
insRootDeviceVolumeId :: Lens' Instance (Maybe Text)
insRootDeviceVolumeId = lens _insRootDeviceVolumeId (\ s a -> s{_insRootDeviceVolumeId = a});

-- | The instance\'s subnet ID, if the stack is running in a VPC.
insSubnetId :: Lens' Instance (Maybe Text)
insSubnetId = lens _insSubnetId (\ s a -> s{_insSubnetId = a});

-- | The instance type. AWS OpsWorks supports all instance types except
-- Cluster Compute, Cluster GPU, and High Memory Cluster. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that specify the various types are in the API Name
-- column of the Available Instance Types table.
insInstanceType :: Lens' Instance (Maybe Text)
insInstanceType = lens _insInstanceType (\ s a -> s{_insInstanceType = a});

-- | For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@
insInfrastructureClass :: Lens' Instance (Maybe Text)
insInfrastructureClass = lens _insInfrastructureClass (\ s a -> s{_insInfrastructureClass = a});

-- | Whether this is an Amazon EBS-optimized instance.
insEBSOptimized :: Lens' Instance (Maybe Bool)
insEBSOptimized = lens _insEBSOptimized (\ s a -> s{_insEBSOptimized = a});

-- | The SSH key\'s DSA fingerprint.
insSSHHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
insSSHHostDsaKeyFingerprint = lens _insSSHHostDsaKeyFingerprint (\ s a -> s{_insSSHHostDsaKeyFingerprint = a});

-- | The instance
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>.
insElasticIP :: Lens' Instance (Maybe Text)
insElasticIP = lens _insElasticIP (\ s a -> s{_insElasticIP = a});

-- | The instance\'s operating system.
insOS :: Lens' Instance (Maybe Text)
insOS = lens _insOS (\ s a -> s{_insOS = a});

-- | The instance Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
insAvailabilityZone :: Lens' Instance (Maybe Text)
insAvailabilityZone = lens _insAvailabilityZone (\ s a -> s{_insAvailabilityZone = a});

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
insLastServiceErrorId :: Lens' Instance (Maybe Text)
insLastServiceErrorId = lens _insLastServiceErrorId (\ s a -> s{_insLastServiceErrorId = a});

-- | For load-based or time-based instances, the type.
insAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
insAutoScalingType = lens _insAutoScalingType (\ s a -> s{_insAutoScalingType = a});

-- | An array containing the instance layer IDs.
insLayerIds :: Lens' Instance [Text]
insLayerIds = lens _insLayerIds (\ s a -> s{_insLayerIds = a}) . _Default;

-- | The instance architecture, \"i386\" or \"x86_64\".
insArchitecture :: Lens' Instance (Maybe Architecture)
insArchitecture = lens _insArchitecture (\ s a -> s{_insArchitecture = a});

-- | The instance public DNS name.
insPublicDNS :: Lens' Instance (Maybe Text)
insPublicDNS = lens _insPublicDNS (\ s a -> s{_insPublicDNS = a});

-- | The instance public IP address.
insPublicIP :: Lens' Instance (Maybe Text)
insPublicIP = lens _insPublicIP (\ s a -> s{_insPublicIP = a});

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the standard AWS OpsWorks APIs: Amazon Linux, Ubuntu
-- 12.04 LTS, or Ubuntu 14.04 LTS. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances.html Instances>
insAMIId :: Lens' Instance (Maybe Text)
insAMIId = lens _insAMIId (\ s a -> s{_insAMIId = a});

-- | For registered instances, the reported operating system.
insReportedOS :: Lens' Instance (Maybe ReportedOS)
insReportedOS = lens _insReportedOS (\ s a -> s{_insReportedOS = a});

-- | The stack ID.
insStackId :: Lens' Instance (Maybe Text)
insStackId = lens _insStackId (\ s a -> s{_insStackId = a});

-- | For registered instances, who performed the registration.
insRegisteredBy :: Lens' Instance (Maybe Text)
insRegisteredBy = lens _insRegisteredBy (\ s a -> s{_insRegisteredBy = a});

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
insBlockDeviceMappings :: Lens' Instance [BlockDeviceMapping]
insBlockDeviceMappings = lens _insBlockDeviceMappings (\ s a -> s{_insBlockDeviceMappings = a}) . _Default;

-- | The instance\'s root device type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
insRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
insRootDeviceType = lens _insRootDeviceType (\ s a -> s{_insRootDeviceType = a});

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "InstanceId") <*> (x .:? "PrivateIp") <*>
                     (x .:? "InstallUpdatesOnBoot")
                     <*> (x .:? "ReportedAgentVersion")
                     <*> (x .:? "Status")
                     <*> (x .:? "PrivateDns")
                     <*> (x .:? "VirtualizationType")
                     <*> (x .:? "SecurityGroupIds" .!= mempty)
                     <*> (x .:? "SshHostRsaKeyFingerprint")
                     <*> (x .:? "InstanceProfileArn")
                     <*> (x .:? "Hostname")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "SshKeyName")
                     <*> (x .:? "Ec2InstanceId")
                     <*> (x .:? "RootDeviceVolumeId")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "InfrastructureClass")
                     <*> (x .:? "EbsOptimized")
                     <*> (x .:? "SshHostDsaKeyFingerprint")
                     <*> (x .:? "ElasticIp")
                     <*> (x .:? "Os")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "LastServiceErrorId")
                     <*> (x .:? "AutoScalingType")
                     <*> (x .:? "LayerIds" .!= mempty)
                     <*> (x .:? "Architecture")
                     <*> (x .:? "PublicDns")
                     <*> (x .:? "PublicIp")
                     <*> (x .:? "AmiId")
                     <*> (x .:? "ReportedOs")
                     <*> (x .:? "StackId")
                     <*> (x .:? "RegisteredBy")
                     <*> (x .:? "BlockDeviceMappings" .!= mempty)
                     <*> (x .:? "RootDeviceType"))

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2
-- metadata service. For more information, see
-- <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data>.
--
-- /See:/ 'instanceIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iiSignature'
--
-- * 'iiDocument'
data InstanceIdentity = InstanceIdentity'
    { _iiSignature :: Maybe Text
    , _iiDocument  :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'InstanceIdentity' smart constructor.
instanceIdentity :: InstanceIdentity
instanceIdentity =
    InstanceIdentity'
    { _iiSignature = Nothing
    , _iiDocument = Nothing
    }

-- | A signature that can be used to verify the document\'s accuracy and
-- authenticity.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\ s a -> s{_iiSignature = a});

-- | A JSON document that contains the metadata.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\ s a -> s{_iiDocument = a});

instance ToJSON InstanceIdentity where
        toJSON InstanceIdentity'{..}
          = object
              ["Signature" .= _iiSignature,
               "Document" .= _iiDocument]

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'instancesCount' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icTerminating'
--
-- * 'icPending'
--
-- * 'icOnline'
--
-- * 'icUnassigning'
--
-- * 'icRequested'
--
-- * 'icRunningSetup'
--
-- * 'icDeregistering'
--
-- * 'icBooting'
--
-- * 'icStopped'
--
-- * 'icRebooting'
--
-- * 'icAssigning'
--
-- * 'icShuttingDown'
--
-- * 'icSetupFailed'
--
-- * 'icConnectionLost'
--
-- * 'icTerminated'
--
-- * 'icStopping'
--
-- * 'icRegistered'
--
-- * 'icStartFailed'
--
-- * 'icRegistering'
data InstancesCount = InstancesCount'
    { _icTerminating    :: Maybe Int
    , _icPending        :: Maybe Int
    , _icOnline         :: Maybe Int
    , _icUnassigning    :: Maybe Int
    , _icRequested      :: Maybe Int
    , _icRunningSetup   :: Maybe Int
    , _icDeregistering  :: Maybe Int
    , _icBooting        :: Maybe Int
    , _icStopped        :: Maybe Int
    , _icRebooting      :: Maybe Int
    , _icAssigning      :: Maybe Int
    , _icShuttingDown   :: Maybe Int
    , _icSetupFailed    :: Maybe Int
    , _icConnectionLost :: Maybe Int
    , _icTerminated     :: Maybe Int
    , _icStopping       :: Maybe Int
    , _icRegistered     :: Maybe Int
    , _icStartFailed    :: Maybe Int
    , _icRegistering    :: Maybe Int
    } deriving (Eq,Read,Show)

-- | 'InstancesCount' smart constructor.
instancesCount :: InstancesCount
instancesCount =
    InstancesCount'
    { _icTerminating = Nothing
    , _icPending = Nothing
    , _icOnline = Nothing
    , _icUnassigning = Nothing
    , _icRequested = Nothing
    , _icRunningSetup = Nothing
    , _icDeregistering = Nothing
    , _icBooting = Nothing
    , _icStopped = Nothing
    , _icRebooting = Nothing
    , _icAssigning = Nothing
    , _icShuttingDown = Nothing
    , _icSetupFailed = Nothing
    , _icConnectionLost = Nothing
    , _icTerminated = Nothing
    , _icStopping = Nothing
    , _icRegistered = Nothing
    , _icStartFailed = Nothing
    , _icRegistering = Nothing
    }

-- | The number of instances with @terminating@ status.
icTerminating :: Lens' InstancesCount (Maybe Int)
icTerminating = lens _icTerminating (\ s a -> s{_icTerminating = a});

-- | The number of instances with @pending@ status.
icPending :: Lens' InstancesCount (Maybe Int)
icPending = lens _icPending (\ s a -> s{_icPending = a});

-- | The number of instances with @online@ status.
icOnline :: Lens' InstancesCount (Maybe Int)
icOnline = lens _icOnline (\ s a -> s{_icOnline = a});

-- | The number of instances in the Unassigning state.
icUnassigning :: Lens' InstancesCount (Maybe Int)
icUnassigning = lens _icUnassigning (\ s a -> s{_icUnassigning = a});

-- | The number of instances with @requested@ status.
icRequested :: Lens' InstancesCount (Maybe Int)
icRequested = lens _icRequested (\ s a -> s{_icRequested = a});

-- | The number of instances with @running_setup@ status.
icRunningSetup :: Lens' InstancesCount (Maybe Int)
icRunningSetup = lens _icRunningSetup (\ s a -> s{_icRunningSetup = a});

-- | The number of instances in the Deregistering state.
icDeregistering :: Lens' InstancesCount (Maybe Int)
icDeregistering = lens _icDeregistering (\ s a -> s{_icDeregistering = a});

-- | The number of instances with @booting@ status.
icBooting :: Lens' InstancesCount (Maybe Int)
icBooting = lens _icBooting (\ s a -> s{_icBooting = a});

-- | The number of instances with @stopped@ status.
icStopped :: Lens' InstancesCount (Maybe Int)
icStopped = lens _icStopped (\ s a -> s{_icStopped = a});

-- | The number of instances with @rebooting@ status.
icRebooting :: Lens' InstancesCount (Maybe Int)
icRebooting = lens _icRebooting (\ s a -> s{_icRebooting = a});

-- | The number of instances in the Assigning state.
icAssigning :: Lens' InstancesCount (Maybe Int)
icAssigning = lens _icAssigning (\ s a -> s{_icAssigning = a});

-- | The number of instances with @shutting_down@ status.
icShuttingDown :: Lens' InstancesCount (Maybe Int)
icShuttingDown = lens _icShuttingDown (\ s a -> s{_icShuttingDown = a});

-- | The number of instances with @setup_failed@ status.
icSetupFailed :: Lens' InstancesCount (Maybe Int)
icSetupFailed = lens _icSetupFailed (\ s a -> s{_icSetupFailed = a});

-- | The number of instances with @connection_lost@ status.
icConnectionLost :: Lens' InstancesCount (Maybe Int)
icConnectionLost = lens _icConnectionLost (\ s a -> s{_icConnectionLost = a});

-- | The number of instances with @terminated@ status.
icTerminated :: Lens' InstancesCount (Maybe Int)
icTerminated = lens _icTerminated (\ s a -> s{_icTerminated = a});

-- | The number of instances with @stopping@ status.
icStopping :: Lens' InstancesCount (Maybe Int)
icStopping = lens _icStopping (\ s a -> s{_icStopping = a});

-- | The number of instances in the Registered state.
icRegistered :: Lens' InstancesCount (Maybe Int)
icRegistered = lens _icRegistered (\ s a -> s{_icRegistered = a});

-- | The number of instances with @start_failed@ status.
icStartFailed :: Lens' InstancesCount (Maybe Int)
icStartFailed = lens _icStartFailed (\ s a -> s{_icStartFailed = a});

-- | The number of instances in the Registering state.
icRegistering :: Lens' InstancesCount (Maybe Int)
icRegistering = lens _icRegistering (\ s a -> s{_icRegistering = a});

instance FromJSON InstancesCount where
        parseJSON
          = withObject "InstancesCount"
              (\ x ->
                 InstancesCount' <$>
                   (x .:? "Terminating") <*> (x .:? "Pending") <*>
                     (x .:? "Online")
                     <*> (x .:? "Unassigning")
                     <*> (x .:? "Requested")
                     <*> (x .:? "RunningSetup")
                     <*> (x .:? "Deregistering")
                     <*> (x .:? "Booting")
                     <*> (x .:? "Stopped")
                     <*> (x .:? "Rebooting")
                     <*> (x .:? "Assigning")
                     <*> (x .:? "ShuttingDown")
                     <*> (x .:? "SetupFailed")
                     <*> (x .:? "ConnectionLost")
                     <*> (x .:? "Terminated")
                     <*> (x .:? "Stopping")
                     <*> (x .:? "Registered")
                     <*> (x .:? "StartFailed")
                     <*> (x .:? "Registering"))

-- | Describes a layer.
--
-- /See:/ 'layer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'layCustomInstanceProfileARN'
--
-- * 'layInstallUpdatesOnBoot'
--
-- * 'layCustomSecurityGroupIds'
--
-- * 'layLifecycleEventConfiguration'
--
-- * 'layShortname'
--
-- * 'layCreatedAt'
--
-- * 'layDefaultRecipes'
--
-- * 'layCustomRecipes'
--
-- * 'layVolumeConfigurations'
--
-- * 'layEnableAutoHealing'
--
-- * 'layPackages'
--
-- * 'layName'
--
-- * 'layAttributes'
--
-- * 'layAutoAssignPublicIPs'
--
-- * 'layUseEBSOptimizedInstances'
--
-- * 'layType'
--
-- * 'layStackId'
--
-- * 'layLayerId'
--
-- * 'layDefaultSecurityGroupNames'
--
-- * 'layAutoAssignElasticIPs'
data Layer = Layer'
    { _layCustomInstanceProfileARN    :: Maybe Text
    , _layInstallUpdatesOnBoot        :: Maybe Bool
    , _layCustomSecurityGroupIds      :: Maybe [Text]
    , _layLifecycleEventConfiguration :: Maybe LifecycleEventConfiguration
    , _layShortname                   :: Maybe Text
    , _layCreatedAt                   :: Maybe Text
    , _layDefaultRecipes              :: Maybe Recipes
    , _layCustomRecipes               :: Maybe Recipes
    , _layVolumeConfigurations        :: Maybe [VolumeConfiguration]
    , _layEnableAutoHealing           :: Maybe Bool
    , _layPackages                    :: Maybe [Text]
    , _layName                        :: Maybe Text
    , _layAttributes                  :: Maybe (Map LayerAttributesKeys Text)
    , _layAutoAssignPublicIPs         :: Maybe Bool
    , _layUseEBSOptimizedInstances    :: Maybe Bool
    , _layType                        :: Maybe LayerType
    , _layStackId                     :: Maybe Text
    , _layLayerId                     :: Maybe Text
    , _layDefaultSecurityGroupNames   :: Maybe [Text]
    , _layAutoAssignElasticIPs        :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'Layer' smart constructor.
layer :: Layer
layer =
    Layer'
    { _layCustomInstanceProfileARN = Nothing
    , _layInstallUpdatesOnBoot = Nothing
    , _layCustomSecurityGroupIds = Nothing
    , _layLifecycleEventConfiguration = Nothing
    , _layShortname = Nothing
    , _layCreatedAt = Nothing
    , _layDefaultRecipes = Nothing
    , _layCustomRecipes = Nothing
    , _layVolumeConfigurations = Nothing
    , _layEnableAutoHealing = Nothing
    , _layPackages = Nothing
    , _layName = Nothing
    , _layAttributes = Nothing
    , _layAutoAssignPublicIPs = Nothing
    , _layUseEBSOptimizedInstances = Nothing
    , _layType = Nothing
    , _layStackId = Nothing
    , _layLayerId = Nothing
    , _layDefaultSecurityGroupNames = Nothing
    , _layAutoAssignElasticIPs = Nothing
    }

-- | The ARN of the default IAM profile to be used for the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
layCustomInstanceProfileARN :: Lens' Layer (Maybe Text)
layCustomInstanceProfileARN = lens _layCustomInstanceProfileARN (\ s a -> s{_layCustomInstanceProfileARN = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
layInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
layInstallUpdatesOnBoot = lens _layInstallUpdatesOnBoot (\ s a -> s{_layInstallUpdatesOnBoot = a});

-- | An array containing the layer\'s custom security group IDs.
layCustomSecurityGroupIds :: Lens' Layer [Text]
layCustomSecurityGroupIds = lens _layCustomSecurityGroupIds (\ s a -> s{_layCustomSecurityGroupIds = a}) . _Default;

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event
-- configuration.
layLifecycleEventConfiguration :: Lens' Layer (Maybe LifecycleEventConfiguration)
layLifecycleEventConfiguration = lens _layLifecycleEventConfiguration (\ s a -> s{_layLifecycleEventConfiguration = a});

-- | The layer short name.
layShortname :: Lens' Layer (Maybe Text)
layShortname = lens _layShortname (\ s a -> s{_layShortname = a});

-- | Date when the layer was created.
layCreatedAt :: Lens' Layer (Maybe Text)
layCreatedAt = lens _layCreatedAt (\ s a -> s{_layCreatedAt = a});

-- | FIXME: Undocumented member.
layDefaultRecipes :: Lens' Layer (Maybe Recipes)
layDefaultRecipes = lens _layDefaultRecipes (\ s a -> s{_layDefaultRecipes = a});

-- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
layCustomRecipes :: Lens' Layer (Maybe Recipes)
layCustomRecipes = lens _layCustomRecipes (\ s a -> s{_layCustomRecipes = a});

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
layVolumeConfigurations :: Lens' Layer [VolumeConfiguration]
layVolumeConfigurations = lens _layVolumeConfigurations (\ s a -> s{_layVolumeConfigurations = a}) . _Default;

-- | Whether auto healing is disabled for the layer.
layEnableAutoHealing :: Lens' Layer (Maybe Bool)
layEnableAutoHealing = lens _layEnableAutoHealing (\ s a -> s{_layEnableAutoHealing = a});

-- | An array of @Package@ objects that describe the layer\'s packages.
layPackages :: Lens' Layer [Text]
layPackages = lens _layPackages (\ s a -> s{_layPackages = a}) . _Default;

-- | The layer name.
layName :: Lens' Layer (Maybe Text)
layName = lens _layName (\ s a -> s{_layName = a});

-- | The layer attributes.
--
-- For the @HaproxyStatsPassword@, @MysqlRootPassword@, and
-- @GangliaPassword@ attributes, AWS OpsWorks returns @*****FILTERED*****@
-- instead of the actual value
layAttributes :: Lens' Layer (HashMap LayerAttributesKeys Text)
layAttributes = lens _layAttributes (\ s a -> s{_layAttributes = a}) . _Default . _Map;

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
layAutoAssignPublicIPs :: Lens' Layer (Maybe Bool)
layAutoAssignPublicIPs = lens _layAutoAssignPublicIPs (\ s a -> s{_layAutoAssignPublicIPs = a});

-- | Whether the layer uses Amazon EBS-optimized instances.
layUseEBSOptimizedInstances :: Lens' Layer (Maybe Bool)
layUseEBSOptimizedInstances = lens _layUseEBSOptimizedInstances (\ s a -> s{_layUseEBSOptimizedInstances = a});

-- | The layer type.
layType :: Lens' Layer (Maybe LayerType)
layType = lens _layType (\ s a -> s{_layType = a});

-- | The layer stack ID.
layStackId :: Lens' Layer (Maybe Text)
layStackId = lens _layStackId (\ s a -> s{_layStackId = a});

-- | The layer ID.
layLayerId :: Lens' Layer (Maybe Text)
layLayerId = lens _layLayerId (\ s a -> s{_layLayerId = a});

-- | An array containing the layer\'s security group names.
layDefaultSecurityGroupNames :: Lens' Layer [Text]
layDefaultSecurityGroupNames = lens _layDefaultSecurityGroupNames (\ s a -> s{_layDefaultSecurityGroupNames = a}) . _Default;

-- | Whether to automatically assign an
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
layAutoAssignElasticIPs :: Lens' Layer (Maybe Bool)
layAutoAssignElasticIPs = lens _layAutoAssignElasticIPs (\ s a -> s{_layAutoAssignElasticIPs = a});

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$>
                   (x .:? "CustomInstanceProfileArn") <*>
                     (x .:? "InstallUpdatesOnBoot")
                     <*> (x .:? "CustomSecurityGroupIds" .!= mempty)
                     <*> (x .:? "LifecycleEventConfiguration")
                     <*> (x .:? "Shortname")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "DefaultRecipes")
                     <*> (x .:? "CustomRecipes")
                     <*> (x .:? "VolumeConfigurations" .!= mempty)
                     <*> (x .:? "EnableAutoHealing")
                     <*> (x .:? "Packages" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "AutoAssignPublicIps")
                     <*> (x .:? "UseEbsOptimizedInstances")
                     <*> (x .:? "Type")
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayerId")
                     <*> (x .:? "DefaultSecurityGroupNames" .!= mempty)
                     <*> (x .:? "AutoAssignElasticIps"))

-- | Specifies the lifecycle event configuration
--
-- /See:/ 'lifecycleEventConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lecShutdown'
newtype LifecycleEventConfiguration = LifecycleEventConfiguration'
    { _lecShutdown :: Maybe ShutdownEventConfiguration
    } deriving (Eq,Read,Show)

-- | 'LifecycleEventConfiguration' smart constructor.
lifecycleEventConfiguration :: LifecycleEventConfiguration
lifecycleEventConfiguration =
    LifecycleEventConfiguration'
    { _lecShutdown = Nothing
    }

-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event
-- configuration.
lecShutdown :: Lens' LifecycleEventConfiguration (Maybe ShutdownEventConfiguration)
lecShutdown = lens _lecShutdown (\ s a -> s{_lecShutdown = a});

instance FromJSON LifecycleEventConfiguration where
        parseJSON
          = withObject "LifecycleEventConfiguration"
              (\ x ->
                 LifecycleEventConfiguration' <$> (x .:? "Shutdown"))

instance ToJSON LifecycleEventConfiguration where
        toJSON LifecycleEventConfiguration'{..}
          = object ["Shutdown" .= _lecShutdown]

-- | Describes a layer\'s load-based auto scaling configuration.
--
-- /See:/ 'loadBasedAutoScalingConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbascUpScaling'
--
-- * 'lbascEnable'
--
-- * 'lbascDownScaling'
--
-- * 'lbascLayerId'
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
    { _lbascUpScaling   :: Maybe AutoScalingThresholds
    , _lbascEnable      :: Maybe Bool
    , _lbascDownScaling :: Maybe AutoScalingThresholds
    , _lbascLayerId     :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'LoadBasedAutoScalingConfiguration' smart constructor.
loadBasedAutoScalingConfiguration :: LoadBasedAutoScalingConfiguration
loadBasedAutoScalingConfiguration =
    LoadBasedAutoScalingConfiguration'
    { _lbascUpScaling = Nothing
    , _lbascEnable = Nothing
    , _lbascDownScaling = Nothing
    , _lbascLayerId = Nothing
    }

-- | An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the
-- number of instances.
lbascUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascUpScaling = lens _lbascUpScaling (\ s a -> s{_lbascUpScaling = a});

-- | Whether load-based auto scaling is enabled for the layer.
lbascEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbascEnable = lens _lbascEnable (\ s a -> s{_lbascEnable = a});

-- | An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the
-- number of instances.
lbascDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascDownScaling = lens _lbascDownScaling (\ s a -> s{_lbascDownScaling = a});

-- | The layer ID.
lbascLayerId :: Lens' LoadBasedAutoScalingConfiguration (Maybe Text)
lbascLayerId = lens _lbascLayerId (\ s a -> s{_lbascLayerId = a});

instance FromJSON LoadBasedAutoScalingConfiguration
         where
        parseJSON
          = withObject "LoadBasedAutoScalingConfiguration"
              (\ x ->
                 LoadBasedAutoScalingConfiguration' <$>
                   (x .:? "UpScaling") <*> (x .:? "Enable") <*>
                     (x .:? "DownScaling")
                     <*> (x .:? "LayerId"))

-- | Describes stack or user permissions.
--
-- /See:/ 'permission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'perIAMUserARN'
--
-- * 'perAllowSudo'
--
-- * 'perStackId'
--
-- * 'perLevel'
--
-- * 'perAllowSSH'
data Permission = Permission'
    { _perIAMUserARN :: Maybe Text
    , _perAllowSudo  :: Maybe Bool
    , _perStackId    :: Maybe Text
    , _perLevel      :: Maybe Text
    , _perAllowSSH   :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'Permission' smart constructor.
permission :: Permission
permission =
    Permission'
    { _perIAMUserARN = Nothing
    , _perAllowSudo = Nothing
    , _perStackId = Nothing
    , _perLevel = Nothing
    , _perAllowSSH = Nothing
    }

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
perIAMUserARN :: Lens' Permission (Maybe Text)
perIAMUserARN = lens _perIAMUserARN (\ s a -> s{_perIAMUserARN = a});

-- | Whether the user can use __sudo__.
perAllowSudo :: Lens' Permission (Maybe Bool)
perAllowSudo = lens _perAllowSudo (\ s a -> s{_perAllowSudo = a});

-- | A stack ID.
perStackId :: Lens' Permission (Maybe Text)
perStackId = lens _perStackId (\ s a -> s{_perStackId = a});

-- | The user\'s permission level, which must be the following:
--
-- -   @deny@
-- -   @show@
-- -   @deploy@
-- -   @manage@
-- -   @iam_only@
--
-- For more information on the permissions associated with these levels,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
perLevel :: Lens' Permission (Maybe Text)
perLevel = lens _perLevel (\ s a -> s{_perLevel = a});

-- | Whether the user can use SSH.
perAllowSSH :: Lens' Permission (Maybe Bool)
perAllowSSH = lens _perAllowSSH (\ s a -> s{_perAllowSSH = a});

instance FromJSON Permission where
        parseJSON
          = withObject "Permission"
              (\ x ->
                 Permission' <$>
                   (x .:? "IamUserArn") <*> (x .:? "AllowSudo") <*>
                     (x .:? "StackId")
                     <*> (x .:? "Level")
                     <*> (x .:? "AllowSsh"))

-- | Describes an instance\'s RAID array.
--
-- /See:/ 'raidArray' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raInstanceId'
--
-- * 'raSize'
--
-- * 'raCreatedAt'
--
-- * 'raIOPS'
--
-- * 'raRAIDLevel'
--
-- * 'raDevice'
--
-- * 'raNumberOfDisks'
--
-- * 'raName'
--
-- * 'raAvailabilityZone'
--
-- * 'raRAIDArrayId'
--
-- * 'raVolumeType'
--
-- * 'raStackId'
--
-- * 'raMountPoint'
data RAIDArray = RAIDArray'
    { _raInstanceId       :: Maybe Text
    , _raSize             :: Maybe Int
    , _raCreatedAt        :: Maybe Text
    , _raIOPS             :: Maybe Int
    , _raRAIDLevel        :: Maybe Int
    , _raDevice           :: Maybe Text
    , _raNumberOfDisks    :: Maybe Int
    , _raName             :: Maybe Text
    , _raAvailabilityZone :: Maybe Text
    , _raRAIDArrayId      :: Maybe Text
    , _raVolumeType       :: Maybe Text
    , _raStackId          :: Maybe Text
    , _raMountPoint       :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'RAIDArray' smart constructor.
raidArray :: RAIDArray
raidArray =
    RAIDArray'
    { _raInstanceId = Nothing
    , _raSize = Nothing
    , _raCreatedAt = Nothing
    , _raIOPS = Nothing
    , _raRAIDLevel = Nothing
    , _raDevice = Nothing
    , _raNumberOfDisks = Nothing
    , _raName = Nothing
    , _raAvailabilityZone = Nothing
    , _raRAIDArrayId = Nothing
    , _raVolumeType = Nothing
    , _raStackId = Nothing
    , _raMountPoint = Nothing
    }

-- | The instance ID.
raInstanceId :: Lens' RAIDArray (Maybe Text)
raInstanceId = lens _raInstanceId (\ s a -> s{_raInstanceId = a});

-- | The array\'s size.
raSize :: Lens' RAIDArray (Maybe Int)
raSize = lens _raSize (\ s a -> s{_raSize = a});

-- | When the RAID array was created.
raCreatedAt :: Lens' RAIDArray (Maybe Text)
raCreatedAt = lens _raCreatedAt (\ s a -> s{_raCreatedAt = a});

-- | For PIOPS volumes, the IOPS per disk.
raIOPS :: Lens' RAIDArray (Maybe Int)
raIOPS = lens _raIOPS (\ s a -> s{_raIOPS = a});

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
raRAIDLevel :: Lens' RAIDArray (Maybe Int)
raRAIDLevel = lens _raRAIDLevel (\ s a -> s{_raRAIDLevel = a});

-- | The array\'s Linux device. For example \/dev\/mdadm0.
raDevice :: Lens' RAIDArray (Maybe Text)
raDevice = lens _raDevice (\ s a -> s{_raDevice = a});

-- | The number of disks in the array.
raNumberOfDisks :: Lens' RAIDArray (Maybe Int)
raNumberOfDisks = lens _raNumberOfDisks (\ s a -> s{_raNumberOfDisks = a});

-- | The array name.
raName :: Lens' RAIDArray (Maybe Text)
raName = lens _raName (\ s a -> s{_raName = a});

-- | The array\'s Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
raAvailabilityZone :: Lens' RAIDArray (Maybe Text)
raAvailabilityZone = lens _raAvailabilityZone (\ s a -> s{_raAvailabilityZone = a});

-- | The array ID.
raRAIDArrayId :: Lens' RAIDArray (Maybe Text)
raRAIDArrayId = lens _raRAIDArrayId (\ s a -> s{_raRAIDArrayId = a});

-- | The volume type, standard or PIOPS.
raVolumeType :: Lens' RAIDArray (Maybe Text)
raVolumeType = lens _raVolumeType (\ s a -> s{_raVolumeType = a});

-- | The stack ID.
raStackId :: Lens' RAIDArray (Maybe Text)
raStackId = lens _raStackId (\ s a -> s{_raStackId = a});

-- | The array\'s mount point.
raMountPoint :: Lens' RAIDArray (Maybe Text)
raMountPoint = lens _raMountPoint (\ s a -> s{_raMountPoint = a});

instance FromJSON RAIDArray where
        parseJSON
          = withObject "RAIDArray"
              (\ x ->
                 RAIDArray' <$>
                   (x .:? "InstanceId") <*> (x .:? "Size") <*>
                     (x .:? "CreatedAt")
                     <*> (x .:? "Iops")
                     <*> (x .:? "RaidLevel")
                     <*> (x .:? "Device")
                     <*> (x .:? "NumberOfDisks")
                     <*> (x .:? "Name")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "RaidArrayId")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "StackId")
                     <*> (x .:? "MountPoint"))

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'rdsDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdiDBUser'
--
-- * 'rdiRDSDBInstanceARN'
--
-- * 'rdiMissingOnRDS'
--
-- * 'rdiEngine'
--
-- * 'rdiAddress'
--
-- * 'rdiDBInstanceIdentifier'
--
-- * 'rdiRegion'
--
-- * 'rdiStackId'
--
-- * 'rdiDBPassword'
data RDSDBInstance = RDSDBInstance'
    { _rdiDBUser               :: Maybe Text
    , _rdiRDSDBInstanceARN     :: Maybe Text
    , _rdiMissingOnRDS         :: Maybe Bool
    , _rdiEngine               :: Maybe Text
    , _rdiAddress              :: Maybe Text
    , _rdiDBInstanceIdentifier :: Maybe Text
    , _rdiRegion               :: Maybe Text
    , _rdiStackId              :: Maybe Text
    , _rdiDBPassword           :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'RDSDBInstance' smart constructor.
rdsDBInstance :: RDSDBInstance
rdsDBInstance =
    RDSDBInstance'
    { _rdiDBUser = Nothing
    , _rdiRDSDBInstanceARN = Nothing
    , _rdiMissingOnRDS = Nothing
    , _rdiEngine = Nothing
    , _rdiAddress = Nothing
    , _rdiDBInstanceIdentifier = Nothing
    , _rdiRegion = Nothing
    , _rdiStackId = Nothing
    , _rdiDBPassword = Nothing
    }

-- | The master user name.
rdiDBUser :: Lens' RDSDBInstance (Maybe Text)
rdiDBUser = lens _rdiDBUser (\ s a -> s{_rdiDBUser = a});

-- | The instance\'s ARN.
rdiRDSDBInstanceARN :: Lens' RDSDBInstance (Maybe Text)
rdiRDSDBInstanceARN = lens _rdiRDSDBInstanceARN (\ s a -> s{_rdiRDSDBInstanceARN = a});

-- | Set to @true@ if AWS OpsWorks was unable to discover the Amazon RDS
-- instance. AWS OpsWorks attempts to discover the instance only once. If
-- this value is set to @true@, you must deregister the instance and then
-- register it again.
rdiMissingOnRDS :: Lens' RDSDBInstance (Maybe Bool)
rdiMissingOnRDS = lens _rdiMissingOnRDS (\ s a -> s{_rdiMissingOnRDS = a});

-- | The instance\'s database engine.
rdiEngine :: Lens' RDSDBInstance (Maybe Text)
rdiEngine = lens _rdiEngine (\ s a -> s{_rdiEngine = a});

-- | The instance\'s address.
rdiAddress :: Lens' RDSDBInstance (Maybe Text)
rdiAddress = lens _rdiAddress (\ s a -> s{_rdiAddress = a});

-- | The DB instance identifier.
rdiDBInstanceIdentifier :: Lens' RDSDBInstance (Maybe Text)
rdiDBInstanceIdentifier = lens _rdiDBInstanceIdentifier (\ s a -> s{_rdiDBInstanceIdentifier = a});

-- | The instance\'s AWS region.
rdiRegion :: Lens' RDSDBInstance (Maybe Text)
rdiRegion = lens _rdiRegion (\ s a -> s{_rdiRegion = a});

-- | The ID of the stack that the instance is registered with.
rdiStackId :: Lens' RDSDBInstance (Maybe Text)
rdiStackId = lens _rdiStackId (\ s a -> s{_rdiStackId = a});

-- | AWS OpsWorks returns @*****FILTERED*****@ instead of the actual value.
rdiDBPassword :: Lens' RDSDBInstance (Maybe Text)
rdiDBPassword = lens _rdiDBPassword (\ s a -> s{_rdiDBPassword = a});

instance FromJSON RDSDBInstance where
        parseJSON
          = withObject "RDSDBInstance"
              (\ x ->
                 RDSDBInstance' <$>
                   (x .:? "DbUser") <*> (x .:? "RdsDbInstanceArn") <*>
                     (x .:? "MissingOnRds")
                     <*> (x .:? "Engine")
                     <*> (x .:? "Address")
                     <*> (x .:? "DbInstanceIdentifier")
                     <*> (x .:? "Region")
                     <*> (x .:? "StackId")
                     <*> (x .:? "DbPassword"))

-- | AWS OpsWorks supports five lifecycle events, __setup__,
-- __configuration__, __deploy__, __undeploy__, and __shutdown__. For each
-- layer, AWS OpsWorks runs a set of standard recipes for each event. In
-- addition, you can provide custom recipes for any or all layers and
-- events. AWS OpsWorks runs custom event recipes after the standard
-- recipes. @LayerCustomRecipes@ specifies the custom recipes for a
-- particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook\'s directory name in the
-- repository followed by two colons and the recipe name, which is the
-- recipe\'s file name without the .rb extension. For example:
-- phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository\'s
-- phpapp2 folder.
--
-- /See:/ 'recipes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'recSetup'
--
-- * 'recUndeploy'
--
-- * 'recShutdown'
--
-- * 'recConfigure'
--
-- * 'recDeploy'
data Recipes = Recipes'
    { _recSetup     :: Maybe [Text]
    , _recUndeploy  :: Maybe [Text]
    , _recShutdown  :: Maybe [Text]
    , _recConfigure :: Maybe [Text]
    , _recDeploy    :: Maybe [Text]
    } deriving (Eq,Read,Show)

-- | 'Recipes' smart constructor.
recipes :: Recipes
recipes =
    Recipes'
    { _recSetup = Nothing
    , _recUndeploy = Nothing
    , _recShutdown = Nothing
    , _recConfigure = Nothing
    , _recDeploy = Nothing
    }

-- | An array of custom recipe names to be run following a @setup@ event.
recSetup :: Lens' Recipes [Text]
recSetup = lens _recSetup (\ s a -> s{_recSetup = a}) . _Default;

-- | An array of custom recipe names to be run following a @undeploy@ event.
recUndeploy :: Lens' Recipes [Text]
recUndeploy = lens _recUndeploy (\ s a -> s{_recUndeploy = a}) . _Default;

-- | An array of custom recipe names to be run following a @shutdown@ event.
recShutdown :: Lens' Recipes [Text]
recShutdown = lens _recShutdown (\ s a -> s{_recShutdown = a}) . _Default;

-- | An array of custom recipe names to be run following a @configure@ event.
recConfigure :: Lens' Recipes [Text]
recConfigure = lens _recConfigure (\ s a -> s{_recConfigure = a}) . _Default;

-- | An array of custom recipe names to be run following a @deploy@ event.
recDeploy :: Lens' Recipes [Text]
recDeploy = lens _recDeploy (\ s a -> s{_recDeploy = a}) . _Default;

instance FromJSON Recipes where
        parseJSON
          = withObject "Recipes"
              (\ x ->
                 Recipes' <$>
                   (x .:? "Setup" .!= mempty) <*>
                     (x .:? "Undeploy" .!= mempty)
                     <*> (x .:? "Shutdown" .!= mempty)
                     <*> (x .:? "Configure" .!= mempty)
                     <*> (x .:? "Deploy" .!= mempty))

instance ToJSON Recipes where
        toJSON Recipes'{..}
          = object
              ["Setup" .= _recSetup, "Undeploy" .= _recUndeploy,
               "Shutdown" .= _recShutdown,
               "Configure" .= _recConfigure, "Deploy" .= _recDeploy]

-- | A registered instance\'s reported operating system.
--
-- /See:/ 'reportedOS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'roFamily'
--
-- * 'roName'
--
-- * 'roVersion'
data ReportedOS = ReportedOS'
    { _roFamily  :: Maybe Text
    , _roName    :: Maybe Text
    , _roVersion :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ReportedOS' smart constructor.
reportedOS :: ReportedOS
reportedOS =
    ReportedOS'
    { _roFamily = Nothing
    , _roName = Nothing
    , _roVersion = Nothing
    }

-- | The operating system family.
roFamily :: Lens' ReportedOS (Maybe Text)
roFamily = lens _roFamily (\ s a -> s{_roFamily = a});

-- | The operating system name.
roName :: Lens' ReportedOS (Maybe Text)
roName = lens _roName (\ s a -> s{_roName = a});

-- | The operating system version.
roVersion :: Lens' ReportedOS (Maybe Text)
roVersion = lens _roVersion (\ s a -> s{_roVersion = a});

instance FromJSON ReportedOS where
        parseJSON
          = withObject "ReportedOS"
              (\ x ->
                 ReportedOS' <$>
                   (x .:? "Family") <*> (x .:? "Name") <*>
                     (x .:? "Version"))

-- | Describes an app\'s SSL configuration.
--
-- /See:/ 'sslConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scChain'
--
-- * 'scCertificate'
--
-- * 'scPrivateKey'
data SSLConfiguration = SSLConfiguration'
    { _scChain       :: Maybe Text
    , _scCertificate :: Text
    , _scPrivateKey  :: Text
    } deriving (Eq,Read,Show)

-- | 'SSLConfiguration' smart constructor.
sslConfiguration :: Text -> Text -> SSLConfiguration
sslConfiguration pCertificate pPrivateKey =
    SSLConfiguration'
    { _scChain = Nothing
    , _scCertificate = pCertificate
    , _scPrivateKey = pPrivateKey
    }

-- | Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
scChain :: Lens' SSLConfiguration (Maybe Text)
scChain = lens _scChain (\ s a -> s{_scChain = a});

-- | The contents of the certificate\'s domain.crt file.
scCertificate :: Lens' SSLConfiguration Text
scCertificate = lens _scCertificate (\ s a -> s{_scCertificate = a});

-- | The private key; the contents of the certificate\'s domain.kex file.
scPrivateKey :: Lens' SSLConfiguration Text
scPrivateKey = lens _scPrivateKey (\ s a -> s{_scPrivateKey = a});

instance FromJSON SSLConfiguration where
        parseJSON
          = withObject "SSLConfiguration"
              (\ x ->
                 SSLConfiguration' <$>
                   (x .:? "Chain") <*> (x .: "Certificate") <*>
                     (x .: "PrivateKey"))

instance ToJSON SSLConfiguration where
        toJSON SSLConfiguration'{..}
          = object
              ["Chain" .= _scChain,
               "Certificate" .= _scCertificate,
               "PrivateKey" .= _scPrivateKey]

-- | Describes a user\'s SSH information.
--
-- /See:/ 'selfUserProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'supSSHUsername'
--
-- * 'supSSHPublicKey'
--
-- * 'supIAMUserARN'
--
-- * 'supName'
data SelfUserProfile = SelfUserProfile'
    { _supSSHUsername  :: Maybe Text
    , _supSSHPublicKey :: Maybe Text
    , _supIAMUserARN   :: Maybe Text
    , _supName         :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'SelfUserProfile' smart constructor.
selfUserProfile :: SelfUserProfile
selfUserProfile =
    SelfUserProfile'
    { _supSSHUsername = Nothing
    , _supSSHPublicKey = Nothing
    , _supIAMUserARN = Nothing
    , _supName = Nothing
    }

-- | The user\'s SSH user name.
supSSHUsername :: Lens' SelfUserProfile (Maybe Text)
supSSHUsername = lens _supSSHUsername (\ s a -> s{_supSSHUsername = a});

-- | The user\'s SSH public key.
supSSHPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSSHPublicKey = lens _supSSHPublicKey (\ s a -> s{_supSSHPublicKey = a});

-- | The user\'s IAM ARN.
supIAMUserARN :: Lens' SelfUserProfile (Maybe Text)
supIAMUserARN = lens _supIAMUserARN (\ s a -> s{_supIAMUserARN = a});

-- | The user\'s name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName = lens _supName (\ s a -> s{_supName = a});

instance FromJSON SelfUserProfile where
        parseJSON
          = withObject "SelfUserProfile"
              (\ x ->
                 SelfUserProfile' <$>
                   (x .:? "SshUsername") <*> (x .:? "SshPublicKey") <*>
                     (x .:? "IamUserArn")
                     <*> (x .:? "Name"))

-- | Describes an AWS OpsWorks service error.
--
-- /See:/ 'serviceError'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seInstanceId'
--
-- * 'seCreatedAt'
--
-- * 'seServiceErrorId'
--
-- * 'seType'
--
-- * 'seMessage'
--
-- * 'seStackId'
data ServiceError' = ServiceError''
    { _seInstanceId     :: Maybe Text
    , _seCreatedAt      :: Maybe Text
    , _seServiceErrorId :: Maybe Text
    , _seType           :: Maybe Text
    , _seMessage        :: Maybe Text
    , _seStackId        :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ServiceError'' smart constructor.
serviceError' :: ServiceError'
serviceError' =
    ServiceError''
    { _seInstanceId = Nothing
    , _seCreatedAt = Nothing
    , _seServiceErrorId = Nothing
    , _seType = Nothing
    , _seMessage = Nothing
    , _seStackId = Nothing
    }

-- | The instance ID.
seInstanceId :: Lens' ServiceError' (Maybe Text)
seInstanceId = lens _seInstanceId (\ s a -> s{_seInstanceId = a});

-- | When the error occurred.
seCreatedAt :: Lens' ServiceError' (Maybe Text)
seCreatedAt = lens _seCreatedAt (\ s a -> s{_seCreatedAt = a});

-- | The error ID.
seServiceErrorId :: Lens' ServiceError' (Maybe Text)
seServiceErrorId = lens _seServiceErrorId (\ s a -> s{_seServiceErrorId = a});

-- | The error type.
seType :: Lens' ServiceError' (Maybe Text)
seType = lens _seType (\ s a -> s{_seType = a});

-- | A message that describes the error.
seMessage :: Lens' ServiceError' (Maybe Text)
seMessage = lens _seMessage (\ s a -> s{_seMessage = a});

-- | The stack ID.
seStackId :: Lens' ServiceError' (Maybe Text)
seStackId = lens _seStackId (\ s a -> s{_seStackId = a});

instance FromJSON ServiceError' where
        parseJSON
          = withObject "ServiceError'"
              (\ x ->
                 ServiceError'' <$>
                   (x .:? "InstanceId") <*> (x .:? "CreatedAt") <*>
                     (x .:? "ServiceErrorId")
                     <*> (x .:? "Type")
                     <*> (x .:? "Message")
                     <*> (x .:? "StackId"))

-- | The Shutdown event configuration.
--
-- /See:/ 'shutdownEventConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'secExecutionTimeout'
--
-- * 'secDelayUntilElbConnectionsDrained'
data ShutdownEventConfiguration = ShutdownEventConfiguration'
    { _secExecutionTimeout                :: Maybe Int
    , _secDelayUntilElbConnectionsDrained :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'ShutdownEventConfiguration' smart constructor.
shutdownEventConfiguration :: ShutdownEventConfiguration
shutdownEventConfiguration =
    ShutdownEventConfiguration'
    { _secExecutionTimeout = Nothing
    , _secDelayUntilElbConnectionsDrained = Nothing
    }

-- | The time, in seconds, that AWS OpsWorks will wait after triggering a
-- Shutdown event before shutting down an instance.
secExecutionTimeout :: Lens' ShutdownEventConfiguration (Maybe Int)
secExecutionTimeout = lens _secExecutionTimeout (\ s a -> s{_secExecutionTimeout = a});

-- | Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
secDelayUntilElbConnectionsDrained :: Lens' ShutdownEventConfiguration (Maybe Bool)
secDelayUntilElbConnectionsDrained = lens _secDelayUntilElbConnectionsDrained (\ s a -> s{_secDelayUntilElbConnectionsDrained = a});

instance FromJSON ShutdownEventConfiguration where
        parseJSON
          = withObject "ShutdownEventConfiguration"
              (\ x ->
                 ShutdownEventConfiguration' <$>
                   (x .:? "ExecutionTimeout") <*>
                     (x .:? "DelayUntilElbConnectionsDrained"))

instance ToJSON ShutdownEventConfiguration where
        toJSON ShutdownEventConfiguration'{..}
          = object
              ["ExecutionTimeout" .= _secExecutionTimeout,
               "DelayUntilElbConnectionsDrained" .=
                 _secDelayUntilElbConnectionsDrained]

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps>
-- or
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Custom Recipes and Cookbooks>.
--
-- /See:/ 'source' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'souURL'
--
-- * 'souUsername'
--
-- * 'souSSHKey'
--
-- * 'souPassword'
--
-- * 'souType'
--
-- * 'souRevision'
data Source = Source'
    { _souURL      :: Maybe Text
    , _souUsername :: Maybe Text
    , _souSSHKey   :: Maybe Text
    , _souPassword :: Maybe Text
    , _souType     :: Maybe SourceType
    , _souRevision :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Source' smart constructor.
source :: Source
source =
    Source'
    { _souURL = Nothing
    , _souUsername = Nothing
    , _souSSHKey = Nothing
    , _souPassword = Nothing
    , _souType = Nothing
    , _souRevision = Nothing
    }

-- | The source URL.
souURL :: Lens' Source (Maybe Text)
souURL = lens _souURL (\ s a -> s{_souURL = a});

-- | This parameter depends on the repository type.
--
-- -   For Amazon S3 bundles, set @Username@ to the appropriate IAM access
--     key ID.
-- -   For HTTP bundles, Git repositories, and Subversion repositories, set
--     @Username@ to the user name.
souUsername :: Lens' Source (Maybe Text)
souUsername = lens _souUsername (\ s a -> s{_souUsername = a});

-- | In requests, the repository\'s SSH key.
--
-- In responses, AWS OpsWorks returns @*****FILTERED*****@ instead of the
-- actual value.
souSSHKey :: Lens' Source (Maybe Text)
souSSHKey = lens _souSSHKey (\ s a -> s{_souSSHKey = a});

-- | When included in a request, the parameter depends on the repository
-- type.
--
-- -   For Amazon S3 bundles, set @Password@ to the appropriate IAM secret
--     access key.
-- -   For HTTP bundles and Subversion repositories, set @Password@ to the
--     password.
--
-- For more information on how to safely handle IAM credentials, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html >.
--
-- In responses, AWS OpsWorks returns @*****FILTERED*****@ instead of the
-- actual value.
souPassword :: Lens' Source (Maybe Text)
souPassword = lens _souPassword (\ s a -> s{_souPassword = a});

-- | The repository type.
souType :: Lens' Source (Maybe SourceType)
souType = lens _souType (\ s a -> s{_souType = a});

-- | The application\'s version. AWS OpsWorks enables you to easily deploy
-- new versions of an application. One of the simplest approaches is to
-- have branches or revisions in your repository that represent different
-- versions that can potentially be deployed.
souRevision :: Lens' Source (Maybe Text)
souRevision = lens _souRevision (\ s a -> s{_souRevision = a});

instance FromJSON Source where
        parseJSON
          = withObject "Source"
              (\ x ->
                 Source' <$>
                   (x .:? "Url") <*> (x .:? "Username") <*>
                     (x .:? "SshKey")
                     <*> (x .:? "Password")
                     <*> (x .:? "Type")
                     <*> (x .:? "Revision"))

instance ToJSON Source where
        toJSON Source'{..}
          = object
              ["Url" .= _souURL, "Username" .= _souUsername,
               "SshKey" .= _souSSHKey, "Password" .= _souPassword,
               "Type" .= _souType, "Revision" .= _souRevision]

-- | Describes a stack.
--
-- /See:/ 'stack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staDefaultInstanceProfileARN'
--
-- * 'staServiceRoleARN'
--
-- * 'staARN'
--
-- * 'staDefaultRootDeviceType'
--
-- * 'staCreatedAt'
--
-- * 'staChefConfiguration'
--
-- * 'staVPCId'
--
-- * 'staDefaultSSHKeyName'
--
-- * 'staCustomJSON'
--
-- * 'staCustomCookbooksSource'
--
-- * 'staDefaultAvailabilityZone'
--
-- * 'staName'
--
-- * 'staUseOpsworksSecurityGroups'
--
-- * 'staDefaultOS'
--
-- * 'staAttributes'
--
-- * 'staUseCustomCookbooks'
--
-- * 'staDefaultSubnetId'
--
-- * 'staRegion'
--
-- * 'staConfigurationManager'
--
-- * 'staStackId'
--
-- * 'staHostnameTheme'
data Stack = Stack'
    { _staDefaultInstanceProfileARN :: Maybe Text
    , _staServiceRoleARN            :: Maybe Text
    , _staARN                       :: Maybe Text
    , _staDefaultRootDeviceType     :: Maybe RootDeviceType
    , _staCreatedAt                 :: Maybe Text
    , _staChefConfiguration         :: Maybe ChefConfiguration
    , _staVPCId                     :: Maybe Text
    , _staDefaultSSHKeyName         :: Maybe Text
    , _staCustomJSON                :: Maybe Text
    , _staCustomCookbooksSource     :: Maybe Source
    , _staDefaultAvailabilityZone   :: Maybe Text
    , _staName                      :: Maybe Text
    , _staUseOpsworksSecurityGroups :: Maybe Bool
    , _staDefaultOS                 :: Maybe Text
    , _staAttributes                :: Maybe (Map StackAttributesKeys Text)
    , _staUseCustomCookbooks        :: Maybe Bool
    , _staDefaultSubnetId           :: Maybe Text
    , _staRegion                    :: Maybe Text
    , _staConfigurationManager      :: Maybe StackConfigurationManager
    , _staStackId                   :: Maybe Text
    , _staHostnameTheme             :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Stack' smart constructor.
stack :: Stack
stack =
    Stack'
    { _staDefaultInstanceProfileARN = Nothing
    , _staServiceRoleARN = Nothing
    , _staARN = Nothing
    , _staDefaultRootDeviceType = Nothing
    , _staCreatedAt = Nothing
    , _staChefConfiguration = Nothing
    , _staVPCId = Nothing
    , _staDefaultSSHKeyName = Nothing
    , _staCustomJSON = Nothing
    , _staCustomCookbooksSource = Nothing
    , _staDefaultAvailabilityZone = Nothing
    , _staName = Nothing
    , _staUseOpsworksSecurityGroups = Nothing
    , _staDefaultOS = Nothing
    , _staAttributes = Nothing
    , _staUseCustomCookbooks = Nothing
    , _staDefaultSubnetId = Nothing
    , _staRegion = Nothing
    , _staConfigurationManager = Nothing
    , _staStackId = Nothing
    , _staHostnameTheme = Nothing
    }

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
staDefaultInstanceProfileARN :: Lens' Stack (Maybe Text)
staDefaultInstanceProfileARN = lens _staDefaultInstanceProfileARN (\ s a -> s{_staDefaultInstanceProfileARN = a});

-- | The stack AWS Identity and Access Management (IAM) role.
staServiceRoleARN :: Lens' Stack (Maybe Text)
staServiceRoleARN = lens _staServiceRoleARN (\ s a -> s{_staServiceRoleARN = a});

-- | The stack\'s ARN.
staARN :: Lens' Stack (Maybe Text)
staARN = lens _staARN (\ s a -> s{_staARN = a});

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
staDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
staDefaultRootDeviceType = lens _staDefaultRootDeviceType (\ s a -> s{_staDefaultRootDeviceType = a});

-- | Date when the stack was created.
staCreatedAt :: Lens' Stack (Maybe Text)
staCreatedAt = lens _staCreatedAt (\ s a -> s{_staCreatedAt = a});

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
staChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
staChefConfiguration = lens _staChefConfiguration (\ s a -> s{_staChefConfiguration = a});

-- | The VPC ID, if the stack is running in a VPC.
staVPCId :: Lens' Stack (Maybe Text)
staVPCId = lens _staVPCId (\ s a -> s{_staVPCId = a});

-- | A default Amazon EC2 key pair for the stack\'s instances. You can
-- override this value when you create or update an instance.
staDefaultSSHKeyName :: Lens' Stack (Maybe Text)
staDefaultSSHKeyName = lens _staDefaultSSHKeyName (\ s a -> s{_staDefaultSSHKeyName = a});

-- | A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration JSON values or to
-- pass data to recipes. The string should be in the following format and
-- must escape characters such as \'\"\'.:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
staCustomJSON :: Lens' Stack (Maybe Text)
staCustomJSON = lens _staCustomJSON (\ s a -> s{_staCustomJSON = a});

-- | FIXME: Undocumented member.
staCustomCookbooksSource :: Lens' Stack (Maybe Source)
staCustomCookbooksSource = lens _staCustomCookbooksSource (\ s a -> s{_staCustomCookbooksSource = a});

-- | The stack\'s default Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
staDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
staDefaultAvailabilityZone = lens _staDefaultAvailabilityZone (\ s a -> s{_staDefaultAvailabilityZone = a});

-- | The stack name.
staName :: Lens' Stack (Maybe Text)
staName = lens _staName (\ s a -> s{_staName = a});

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack\'s layers.
staUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
staUseOpsworksSecurityGroups = lens _staUseOpsworksSecurityGroups (\ s a -> s{_staUseOpsworksSecurityGroups = a});

-- | The stack\'s default operating system.
staDefaultOS :: Lens' Stack (Maybe Text)
staDefaultOS = lens _staDefaultOS (\ s a -> s{_staDefaultOS = a});

-- | The stack\'s attributes.
staAttributes :: Lens' Stack (HashMap StackAttributesKeys Text)
staAttributes = lens _staAttributes (\ s a -> s{_staAttributes = a}) . _Default . _Map;

-- | Whether the stack uses custom cookbooks.
staUseCustomCookbooks :: Lens' Stack (Maybe Bool)
staUseCustomCookbooks = lens _staUseCustomCookbooks (\ s a -> s{_staUseCustomCookbooks = a});

-- | The default subnet ID, if the stack is running in a VPC.
staDefaultSubnetId :: Lens' Stack (Maybe Text)
staDefaultSubnetId = lens _staDefaultSubnetId (\ s a -> s{_staDefaultSubnetId = a});

-- | The stack AWS region, such as \"us-east-1\". For more information about
-- AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
staRegion :: Lens' Stack (Maybe Text)
staRegion = lens _staRegion (\ s a -> s{_staRegion = a});

-- | The configuration manager.
staConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
staConfigurationManager = lens _staConfigurationManager (\ s a -> s{_staConfigurationManager = a});

-- | The stack ID.
staStackId :: Lens' Stack (Maybe Text)
staStackId = lens _staStackId (\ s a -> s{_staStackId = a});

-- | The stack host name theme, with spaces replaced by underscores.
staHostnameTheme :: Lens' Stack (Maybe Text)
staHostnameTheme = lens _staHostnameTheme (\ s a -> s{_staHostnameTheme = a});

instance FromJSON Stack where
        parseJSON
          = withObject "Stack"
              (\ x ->
                 Stack' <$>
                   (x .:? "DefaultInstanceProfileArn") <*>
                     (x .:? "ServiceRoleArn")
                     <*> (x .:? "Arn")
                     <*> (x .:? "DefaultRootDeviceType")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "ChefConfiguration")
                     <*> (x .:? "VpcId")
                     <*> (x .:? "DefaultSshKeyName")
                     <*> (x .:? "CustomJson")
                     <*> (x .:? "CustomCookbooksSource")
                     <*> (x .:? "DefaultAvailabilityZone")
                     <*> (x .:? "Name")
                     <*> (x .:? "UseOpsworksSecurityGroups")
                     <*> (x .:? "DefaultOs")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "UseCustomCookbooks")
                     <*> (x .:? "DefaultSubnetId")
                     <*> (x .:? "Region")
                     <*> (x .:? "ConfigurationManager")
                     <*> (x .:? "StackId")
                     <*> (x .:? "HostnameTheme"))

-- | Describes the configuration manager.
--
-- /See:/ 'stackConfigurationManager' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scmName'
--
-- * 'scmVersion'
data StackConfigurationManager = StackConfigurationManager'
    { _scmName    :: Maybe Text
    , _scmVersion :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'StackConfigurationManager' smart constructor.
stackConfigurationManager :: StackConfigurationManager
stackConfigurationManager =
    StackConfigurationManager'
    { _scmName = Nothing
    , _scmVersion = Nothing
    }

-- | The name. This parameter must be set to \"Chef\".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\ s a -> s{_scmName = a});

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\ s a -> s{_scmVersion = a});

instance FromJSON StackConfigurationManager where
        parseJSON
          = withObject "StackConfigurationManager"
              (\ x ->
                 StackConfigurationManager' <$>
                   (x .:? "Name") <*> (x .:? "Version"))

instance ToJSON StackConfigurationManager where
        toJSON StackConfigurationManager'{..}
          = object
              ["Name" .= _scmName, "Version" .= _scmVersion]

-- | Summarizes the number of layers, instances, and apps in a stack.
--
-- /See:/ 'stackSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssARN'
--
-- * 'ssAppsCount'
--
-- * 'ssName'
--
-- * 'ssStackId'
--
-- * 'ssLayersCount'
--
-- * 'ssInstancesCount'
data StackSummary = StackSummary'
    { _ssARN            :: Maybe Text
    , _ssAppsCount      :: Maybe Int
    , _ssName           :: Maybe Text
    , _ssStackId        :: Maybe Text
    , _ssLayersCount    :: Maybe Int
    , _ssInstancesCount :: Maybe InstancesCount
    } deriving (Eq,Read,Show)

-- | 'StackSummary' smart constructor.
stackSummary :: StackSummary
stackSummary =
    StackSummary'
    { _ssARN = Nothing
    , _ssAppsCount = Nothing
    , _ssName = Nothing
    , _ssStackId = Nothing
    , _ssLayersCount = Nothing
    , _ssInstancesCount = Nothing
    }

-- | The stack\'s ARN.
ssARN :: Lens' StackSummary (Maybe Text)
ssARN = lens _ssARN (\ s a -> s{_ssARN = a});

-- | The number of apps.
ssAppsCount :: Lens' StackSummary (Maybe Int)
ssAppsCount = lens _ssAppsCount (\ s a -> s{_ssAppsCount = a});

-- | The stack name.
ssName :: Lens' StackSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a});

-- | The stack ID.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\ s a -> s{_ssStackId = a});

-- | The number of layers.
ssLayersCount :: Lens' StackSummary (Maybe Int)
ssLayersCount = lens _ssLayersCount (\ s a -> s{_ssLayersCount = a});

-- | An @InstancesCount@ object with the number of instances in each status.
ssInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssInstancesCount = lens _ssInstancesCount (\ s a -> s{_ssInstancesCount = a});

instance FromJSON StackSummary where
        parseJSON
          = withObject "StackSummary"
              (\ x ->
                 StackSummary' <$>
                   (x .:? "Arn") <*> (x .:? "AppsCount") <*>
                     (x .:? "Name")
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayersCount")
                     <*> (x .:? "InstancesCount"))

-- | Contains the data needed by RDP clients such as the Microsoft Remote
-- Desktop Connection to log in to the instance.
--
-- /See:/ 'temporaryCredential' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tcInstanceId'
--
-- * 'tcUsername'
--
-- * 'tcPassword'
--
-- * 'tcValidForInMinutes'
data TemporaryCredential = TemporaryCredential'
    { _tcInstanceId        :: Maybe Text
    , _tcUsername          :: Maybe Text
    , _tcPassword          :: Maybe Text
    , _tcValidForInMinutes :: Maybe Int
    } deriving (Eq,Read,Show)

-- | 'TemporaryCredential' smart constructor.
temporaryCredential :: TemporaryCredential
temporaryCredential =
    TemporaryCredential'
    { _tcInstanceId = Nothing
    , _tcUsername = Nothing
    , _tcPassword = Nothing
    , _tcValidForInMinutes = Nothing
    }

-- | The instance\'s AWS OpsWorks ID.
tcInstanceId :: Lens' TemporaryCredential (Maybe Text)
tcInstanceId = lens _tcInstanceId (\ s a -> s{_tcInstanceId = a});

-- | The user name.
tcUsername :: Lens' TemporaryCredential (Maybe Text)
tcUsername = lens _tcUsername (\ s a -> s{_tcUsername = a});

-- | The password.
tcPassword :: Lens' TemporaryCredential (Maybe Text)
tcPassword = lens _tcPassword (\ s a -> s{_tcPassword = a});

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires, at the end of this period, the user will no longer be able to
-- use the credentials to log in. If they are logged in at the time, they
-- will be automatically logged out.
tcValidForInMinutes :: Lens' TemporaryCredential (Maybe Int)
tcValidForInMinutes = lens _tcValidForInMinutes (\ s a -> s{_tcValidForInMinutes = a});

instance FromJSON TemporaryCredential where
        parseJSON
          = withObject "TemporaryCredential"
              (\ x ->
                 TemporaryCredential' <$>
                   (x .:? "InstanceId") <*> (x .:? "Username") <*>
                     (x .:? "Password")
                     <*> (x .:? "ValidForInMinutes"))

-- | Describes an instance\'s time-based auto scaling configuration.
--
-- /See:/ 'timeBasedAutoScalingConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tbascInstanceId'
--
-- * 'tbascAutoScalingSchedule'
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
    { _tbascInstanceId          :: Maybe Text
    , _tbascAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
    } deriving (Eq,Read,Show)

-- | 'TimeBasedAutoScalingConfiguration' smart constructor.
timeBasedAutoScalingConfiguration :: TimeBasedAutoScalingConfiguration
timeBasedAutoScalingConfiguration =
    TimeBasedAutoScalingConfiguration'
    { _tbascInstanceId = Nothing
    , _tbascAutoScalingSchedule = Nothing
    }

-- | The instance ID.
tbascInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbascInstanceId = lens _tbascInstanceId (\ s a -> s{_tbascInstanceId = a});

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
tbascAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule = lens _tbascAutoScalingSchedule (\ s a -> s{_tbascAutoScalingSchedule = a});

instance FromJSON TimeBasedAutoScalingConfiguration
         where
        parseJSON
          = withObject "TimeBasedAutoScalingConfiguration"
              (\ x ->
                 TimeBasedAutoScalingConfiguration' <$>
                   (x .:? "InstanceId") <*>
                     (x .:? "AutoScalingSchedule"))

-- | Describes a user\'s SSH information.
--
-- /See:/ 'userProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upSSHUsername'
--
-- * 'upSSHPublicKey'
--
-- * 'upAllowSelfManagement'
--
-- * 'upIAMUserARN'
--
-- * 'upName'
data UserProfile = UserProfile'
    { _upSSHUsername         :: Maybe Text
    , _upSSHPublicKey        :: Maybe Text
    , _upAllowSelfManagement :: Maybe Bool
    , _upIAMUserARN          :: Maybe Text
    , _upName                :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'UserProfile' smart constructor.
userProfile :: UserProfile
userProfile =
    UserProfile'
    { _upSSHUsername = Nothing
    , _upSSHPublicKey = Nothing
    , _upAllowSelfManagement = Nothing
    , _upIAMUserARN = Nothing
    , _upName = Nothing
    }

-- | The user\'s SSH user name.
upSSHUsername :: Lens' UserProfile (Maybe Text)
upSSHUsername = lens _upSSHUsername (\ s a -> s{_upSSHUsername = a});

-- | The user\'s SSH public key.
upSSHPublicKey :: Lens' UserProfile (Maybe Text)
upSSHPublicKey = lens _upSSHPublicKey (\ s a -> s{_upSSHPublicKey = a});

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
upAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
upAllowSelfManagement = lens _upAllowSelfManagement (\ s a -> s{_upAllowSelfManagement = a});

-- | The user\'s IAM ARN.
upIAMUserARN :: Lens' UserProfile (Maybe Text)
upIAMUserARN = lens _upIAMUserARN (\ s a -> s{_upIAMUserARN = a});

-- | The user\'s name.
upName :: Lens' UserProfile (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a});

instance FromJSON UserProfile where
        parseJSON
          = withObject "UserProfile"
              (\ x ->
                 UserProfile' <$>
                   (x .:? "SshUsername") <*> (x .:? "SshPublicKey") <*>
                     (x .:? "AllowSelfManagement")
                     <*> (x .:? "IamUserArn")
                     <*> (x .:? "Name"))

-- | Describes an instance\'s Amazon EBS volume.
--
-- /See:/ 'volume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'volInstanceId'
--
-- * 'volStatus'
--
-- * 'volSize'
--
-- * 'volIOPS'
--
-- * 'volDevice'
--
-- * 'volName'
--
-- * 'volAvailabilityZone'
--
-- * 'volRAIDArrayId'
--
-- * 'volVolumeId'
--
-- * 'volRegion'
--
-- * 'volVolumeType'
--
-- * 'volEC2VolumeId'
--
-- * 'volMountPoint'
data Volume = Volume'
    { _volInstanceId       :: Maybe Text
    , _volStatus           :: Maybe Text
    , _volSize             :: Maybe Int
    , _volIOPS             :: Maybe Int
    , _volDevice           :: Maybe Text
    , _volName             :: Maybe Text
    , _volAvailabilityZone :: Maybe Text
    , _volRAIDArrayId      :: Maybe Text
    , _volVolumeId         :: Maybe Text
    , _volRegion           :: Maybe Text
    , _volVolumeType       :: Maybe Text
    , _volEC2VolumeId      :: Maybe Text
    , _volMountPoint       :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Volume' smart constructor.
volume :: Volume
volume =
    Volume'
    { _volInstanceId = Nothing
    , _volStatus = Nothing
    , _volSize = Nothing
    , _volIOPS = Nothing
    , _volDevice = Nothing
    , _volName = Nothing
    , _volAvailabilityZone = Nothing
    , _volRAIDArrayId = Nothing
    , _volVolumeId = Nothing
    , _volRegion = Nothing
    , _volVolumeType = Nothing
    , _volEC2VolumeId = Nothing
    , _volMountPoint = Nothing
    }

-- | The instance ID.
volInstanceId :: Lens' Volume (Maybe Text)
volInstanceId = lens _volInstanceId (\ s a -> s{_volInstanceId = a});

-- | The value returned by
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
volStatus :: Lens' Volume (Maybe Text)
volStatus = lens _volStatus (\ s a -> s{_volStatus = a});

-- | The volume size.
volSize :: Lens' Volume (Maybe Int)
volSize = lens _volSize (\ s a -> s{_volSize = a});

-- | For PIOPS volumes, the IOPS per disk.
volIOPS :: Lens' Volume (Maybe Int)
volIOPS = lens _volIOPS (\ s a -> s{_volIOPS = a});

-- | The device name.
volDevice :: Lens' Volume (Maybe Text)
volDevice = lens _volDevice (\ s a -> s{_volDevice = a});

-- | The volume name.
volName :: Lens' Volume (Maybe Text)
volName = lens _volName (\ s a -> s{_volName = a});

-- | The volume Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
volAvailabilityZone :: Lens' Volume (Maybe Text)
volAvailabilityZone = lens _volAvailabilityZone (\ s a -> s{_volAvailabilityZone = a});

-- | The RAID array ID.
volRAIDArrayId :: Lens' Volume (Maybe Text)
volRAIDArrayId = lens _volRAIDArrayId (\ s a -> s{_volRAIDArrayId = a});

-- | The volume ID.
volVolumeId :: Lens' Volume (Maybe Text)
volVolumeId = lens _volVolumeId (\ s a -> s{_volVolumeId = a});

-- | The AWS region. For more information about AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
volRegion :: Lens' Volume (Maybe Text)
volRegion = lens _volRegion (\ s a -> s{_volRegion = a});

-- | The volume type, standard or PIOPS.
volVolumeType :: Lens' Volume (Maybe Text)
volVolumeType = lens _volVolumeType (\ s a -> s{_volVolumeType = a});

-- | The Amazon EC2 volume ID.
volEC2VolumeId :: Lens' Volume (Maybe Text)
volEC2VolumeId = lens _volEC2VolumeId (\ s a -> s{_volEC2VolumeId = a});

-- | The volume mount point. For example \"\/dev\/sdh\".
volMountPoint :: Lens' Volume (Maybe Text)
volMountPoint = lens _volMountPoint (\ s a -> s{_volMountPoint = a});

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "Size")
                     <*> (x .:? "Iops")
                     <*> (x .:? "Device")
                     <*> (x .:? "Name")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "RaidArrayId")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "Region")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "Ec2VolumeId")
                     <*> (x .:? "MountPoint"))

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'volumeConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcIOPS'
--
-- * 'vcRAIDLevel'
--
-- * 'vcVolumeType'
--
-- * 'vcMountPoint'
--
-- * 'vcNumberOfDisks'
--
-- * 'vcSize'
data VolumeConfiguration = VolumeConfiguration'
    { _vcIOPS          :: Maybe Int
    , _vcRAIDLevel     :: Maybe Int
    , _vcVolumeType    :: Maybe Text
    , _vcMountPoint    :: Text
    , _vcNumberOfDisks :: !Int
    , _vcSize          :: !Int
    } deriving (Eq,Read,Show)

-- | 'VolumeConfiguration' smart constructor.
volumeConfiguration :: Text -> Int -> Int -> VolumeConfiguration
volumeConfiguration pMountPoint pNumberOfDisks pSize =
    VolumeConfiguration'
    { _vcIOPS = Nothing
    , _vcRAIDLevel = Nothing
    , _vcVolumeType = Nothing
    , _vcMountPoint = pMountPoint
    , _vcNumberOfDisks = pNumberOfDisks
    , _vcSize = pSize
    }

-- | For PIOPS volumes, the IOPS per disk.
vcIOPS :: Lens' VolumeConfiguration (Maybe Int)
vcIOPS = lens _vcIOPS (\ s a -> s{_vcIOPS = a});

-- | The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
vcRAIDLevel :: Lens' VolumeConfiguration (Maybe Int)
vcRAIDLevel = lens _vcRAIDLevel (\ s a -> s{_vcRAIDLevel = a});

-- | The volume type:
--
-- -   @standard@ - Magnetic
-- -   @io1@ - Provisioned IOPS (SSD)
-- -   @gp2@ - General Purpose (SSD)
vcVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vcVolumeType = lens _vcVolumeType (\ s a -> s{_vcVolumeType = a});

-- | The volume mount point. For example \"\/dev\/sdh\".
vcMountPoint :: Lens' VolumeConfiguration Text
vcMountPoint = lens _vcMountPoint (\ s a -> s{_vcMountPoint = a});

-- | The number of disks in the volume.
vcNumberOfDisks :: Lens' VolumeConfiguration Int
vcNumberOfDisks = lens _vcNumberOfDisks (\ s a -> s{_vcNumberOfDisks = a});

-- | The volume size.
vcSize :: Lens' VolumeConfiguration Int
vcSize = lens _vcSize (\ s a -> s{_vcSize = a});

instance FromJSON VolumeConfiguration where
        parseJSON
          = withObject "VolumeConfiguration"
              (\ x ->
                 VolumeConfiguration' <$>
                   (x .:? "Iops") <*> (x .:? "RaidLevel") <*>
                     (x .:? "VolumeType")
                     <*> (x .: "MountPoint")
                     <*> (x .: "NumberOfDisks")
                     <*> (x .: "Size"))

instance ToJSON VolumeConfiguration where
        toJSON VolumeConfiguration'{..}
          = object
              ["Iops" .= _vcIOPS, "RaidLevel" .= _vcRAIDLevel,
               "VolumeType" .= _vcVolumeType,
               "MountPoint" .= _vcMountPoint,
               "NumberOfDisks" .= _vcNumberOfDisks,
               "Size" .= _vcSize]

-- | Describes a time-based instance\'s auto scaling schedule. The schedule
-- consists of a set of key-value pairs.
--
-- -   The key is the time period (a UTC hour) and must be an integer from
--     0 - 23.
-- -   The value indicates whether the instance should be online or offline
--     for the specified period, and must be set to \"on\" or \"off\"
--
-- The default setting for all time periods is off, so you use the
-- following parameters primarily to specify the online periods. You don\'t
-- have to explicitly specify offline periods unless you want to change an
-- online period to an offline period.
--
-- The following example specifies that the instance should be online for
-- four hours, from UTC 1200 - 1600. It will be off for the remainder of
-- the day.
--
-- @ { \"12\":\"on\", \"13\":\"on\", \"14\":\"on\", \"15\":\"on\" } @
--
-- /See:/ 'weeklyAutoScalingSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wassThursday'
--
-- * 'wassWednesday'
--
-- * 'wassSaturday'
--
-- * 'wassMonday'
--
-- * 'wassFriday'
--
-- * 'wassSunday'
--
-- * 'wassTuesday'
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
    { _wassThursday  :: Maybe (Map Text Text)
    , _wassWednesday :: Maybe (Map Text Text)
    , _wassSaturday  :: Maybe (Map Text Text)
    , _wassMonday    :: Maybe (Map Text Text)
    , _wassFriday    :: Maybe (Map Text Text)
    , _wassSunday    :: Maybe (Map Text Text)
    , _wassTuesday   :: Maybe (Map Text Text)
    } deriving (Eq,Read,Show)

-- | 'WeeklyAutoScalingSchedule' smart constructor.
weeklyAutoScalingSchedule :: WeeklyAutoScalingSchedule
weeklyAutoScalingSchedule =
    WeeklyAutoScalingSchedule'
    { _wassThursday = Nothing
    , _wassWednesday = Nothing
    , _wassSaturday = Nothing
    , _wassMonday = Nothing
    , _wassFriday = Nothing
    , _wassSunday = Nothing
    , _wassTuesday = Nothing
    }

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassThursday = lens _wassThursday (\ s a -> s{_wassThursday = a}) . _Default . _Map;

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassWednesday = lens _wassWednesday (\ s a -> s{_wassWednesday = a}) . _Default . _Map;

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSaturday = lens _wassSaturday (\ s a -> s{_wassSaturday = a}) . _Default . _Map;

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassMonday = lens _wassMonday (\ s a -> s{_wassMonday = a}) . _Default . _Map;

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassFriday = lens _wassFriday (\ s a -> s{_wassFriday = a}) . _Default . _Map;

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSunday = lens _wassSunday (\ s a -> s{_wassSunday = a}) . _Default . _Map;

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassTuesday = lens _wassTuesday (\ s a -> s{_wassTuesday = a}) . _Default . _Map;

instance FromJSON WeeklyAutoScalingSchedule where
        parseJSON
          = withObject "WeeklyAutoScalingSchedule"
              (\ x ->
                 WeeklyAutoScalingSchedule' <$>
                   (x .:? "Thursday" .!= mempty) <*>
                     (x .:? "Wednesday" .!= mempty)
                     <*> (x .:? "Saturday" .!= mempty)
                     <*> (x .:? "Monday" .!= mempty)
                     <*> (x .:? "Friday" .!= mempty)
                     <*> (x .:? "Sunday" .!= mempty)
                     <*> (x .:? "Tuesday" .!= mempty))

instance ToJSON WeeklyAutoScalingSchedule where
        toJSON WeeklyAutoScalingSchedule'{..}
          = object
              ["Thursday" .= _wassThursday,
               "Wednesday" .= _wassWednesday,
               "Saturday" .= _wassSaturday, "Monday" .= _wassMonday,
               "Friday" .= _wassFriday, "Sunday" .= _wassSunday,
               "Tuesday" .= _wassTuesday]
