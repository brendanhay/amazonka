{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.Types where

import Control.Lens.TH (makeLenses)
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

instance ToByteString AppAttributesKeys

instance FromJSON AppAttributesKeys

instance ToJSON AppAttributesKeys

-- | The app type.
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

instance ToByteString AppType

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

instance ToByteString Architecture

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

instance ToByteString AutoScalingType

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

instance ToByteString DeploymentCommandName

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

instance ToByteString LayerAttributesKeys

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

instance ToByteString LayerType

instance FromJSON LayerType

instance ToJSON LayerType

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
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

instance ToByteString RootDeviceType

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

instance ToByteString SourceType

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

instance ToByteString StackAttributesKeys

instance FromJSON StackAttributesKeys

instance ToJSON StackAttributesKeys

-- | A description of the app.
data App = App
    { _apSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , _apEnableSsl :: Maybe Bool
      -- ^ Whether to enable SSL for the app.
    , _apCreatedAt :: Maybe Text
      -- ^ When the app was created.
    , _apShortname :: Maybe Text
      -- ^ The app's short name.
    , _apDataSources :: [DataSource]
      -- ^ The app's data sources.
    , _apAppSource :: Maybe Source
      -- ^ A Source object that describes the app repository.
    , _apAppId :: Maybe Text
      -- ^ The app ID.
    , _apAttributes :: Map AppAttributesKeys Text
      -- ^ The stack attributes.
    , _apName :: Maybe Text
      -- ^ The app name.
    , _apType :: Maybe AppType
      -- ^ The app type.
    , _apStackId :: Maybe Text
      -- ^ The app stack ID.
    , _apDomains :: [Text]
      -- ^ The app vhost settings with multiple domains separated by commas.
      -- For example: 'www.example.com, example.com'.
    , _apDescription :: Maybe Text
      -- ^ A description of the app.
    } deriving (Show, Generic)

instance FromJSON App

-- | An AutoScalingThresholds object with the upscaling threshold configuration.
-- If the load exceeds these thresholds for a specified amount of time, AWS
-- OpsWorks starts a specified number of instances.
data AutoScalingThresholds = AutoScalingThresholds
    { _astInstanceCount :: Maybe Integer
      -- ^ The number of instances to add or remove when the load exceeds a
      -- threshold.
    , _astIgnoreMetricsTime :: Maybe Integer
      -- ^ The amount of time (in minutes) after a scaling event occurs that
      -- AWS OpsWorks should ignore metrics and not raise any additional
      -- scaling events. For example, AWS OpsWorks adds new instances
      -- following an upscaling event but the instances won't start
      -- reducing the load until they have been booted and configured.
      -- There is no point in raising additional scaling events during
      -- that operation, which typically takes several minutes.
      -- IgnoreMetricsTime allows you to direct AWS OpsWorks to not raise
      -- any scaling events long enough to get the new instances online.
    , _astLoadThreshold :: Maybe Double
      -- ^ The load threshold. For more information about how load is
      -- computed, see Load (computing).
    , _astThresholdsWaitTime :: Maybe Integer
      -- ^ The amount of time, in minutes, that the load must exceed a
      -- threshold before more instances are added or removed.
    , _astMemoryThreshold :: Maybe Double
      -- ^ The memory utilization threshold, as a percent of the available
      -- memory.
    , _astCpuThreshold :: Maybe Double
      -- ^ The CPU utilization threshold, as a percent of the available CPU.
    } deriving (Show, Generic)

instance FromJSON AutoScalingThresholds

instance ToJSON AutoScalingThresholds

-- | A ChefConfiguration object that specifies whether to enable Berkshelf and
-- the Berkshelf version on Chef 11.10 stacks. For more information, see
-- Create a New Stack.
data ChefConfiguration = ChefConfiguration
    { _ccBerkshelfVersion :: Maybe Text
      -- ^ The Berkshelf version.
    , _ccManageBerkshelf :: Maybe Bool
      -- ^ Whether to enable Berkshelf.
    } deriving (Show, Generic)

instance FromJSON ChefConfiguration

instance ToJSON ChefConfiguration

-- | Describes a command.
data Command = Command
    { _cgDeploymentId :: Maybe Text
      -- ^ The command deployment ID.
    , _cgInstanceId :: Maybe Text
      -- ^ The ID of the instance where the command was executed.
    , _cgStatus :: Maybe Text
      -- ^ The command status: failed successful skipped pending.
    , _cgLogUrl :: Maybe Text
      -- ^ The URL of the command log.
    , _cgCreatedAt :: Maybe Text
      -- ^ Date and time when the command was run.
    , _cgCommandId :: Maybe Text
      -- ^ The command ID.
    , _cgExitCode :: Maybe Integer
      -- ^ The command exit code.
    , _cgType :: Maybe Text
      -- ^ The command type: deploy rollback start stop restart undeploy
      -- update_dependencies install_dependencies update_custom_cookbooks
      -- execute_recipes.
    , _cgCompletedAt :: Maybe Text
      -- ^ Date when the command completed.
    , _cgAcknowledgedAt :: Maybe Text
      -- ^ Date and time when the command was acknowledged.
    } deriving (Show, Generic)

instance FromJSON Command

-- | Describes an app's data source.
data DataSource = DataSource
    { _dvArn :: Maybe Text
      -- ^ The data source's ARN.
    , _dvDatabaseName :: Maybe Text
      -- ^ The database name.
    , _dvType :: Maybe Text
      -- ^ The data source's type, AutoSelectOpsworksMysqlInstance,
      -- OpsworksMysqlInstance, or RdsDbInstance.
    } deriving (Show, Generic)

instance FromJSON DataSource

instance ToJSON DataSource

-- | Describes a deployment of a stack or app.
data Deployment = Deployment
    { _ddtDeploymentId :: Maybe Text
      -- ^ The deployment ID.
    , _ddtStatus :: Maybe Text
      -- ^ The deployment status: running successful failed.
    , _ddtCommand :: Maybe DeploymentCommand
      -- ^ Used to specify a deployment operation.
    , _ddtCreatedAt :: Maybe Text
      -- ^ Date when the deployment was created.
    , _ddtCustomJson :: Maybe Text
      -- ^ A string that contains user-defined custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values for stack. The string should be in the following format
      -- and must escape characters such as '"'.: "{\"key1\": \"value1\",
      -- \"key2\": \"value2\",...}" For more information on custom JSON,
      -- see Use Custom JSON to Modify the Stack Configuration JSON.
    , _ddtIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , _ddtAppId :: Maybe Text
      -- ^ The app ID.
    , _ddtInstanceIds :: [Text]
      -- ^ The IDs of the target instances.
    , _ddtCompletedAt :: Maybe Text
      -- ^ Date when the deployment completed.
    , _ddtStackId :: Maybe Text
      -- ^ The stack ID.
    , _ddtComment :: Maybe Text
      -- ^ A user-defined comment.
    , _ddtDuration :: Maybe Integer
      -- ^ The deployment duration.
    } deriving (Show, Generic)

instance FromJSON Deployment

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
data DeploymentCommand = DeploymentCommand
    { _dcArgs :: Map Text [Text]
      -- ^ The arguments of those commands that take arguments. It should be
      -- set to a JSON object with the following format:
      -- {"arg_name":["value1", "value2", ...]}.
    , _dcName :: DeploymentCommandName
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
    } deriving (Show, Generic)

instance FromJSON DeploymentCommand

instance ToJSON DeploymentCommand

-- | Describes an Elastic IP address.
data ElasticIp = ElasticIp
    { _ejInstanceId :: Maybe Text
      -- ^ The ID of the instance that the address is attached to.
    , _ejDomain :: Maybe Text
      -- ^ The domain.
    , _ejIp :: Maybe Text
      -- ^ The IP address.
    , _ejName :: Maybe Text
      -- ^ The name.
    , _ejRegion :: Maybe Text
      -- ^ The AWS region. For more information, see Regions and Endpoints.
    } deriving (Show, Generic)

instance FromJSON ElasticIp

-- | Describes an Elastic Load Balancing instance.
data ElasticLoadBalancer = ElasticLoadBalancer
    { _elcSubnetIds :: [Text]
      -- ^ A list of subnet IDs, if the stack is running in a VPC.
    , _elcVpcId :: Maybe Text
      -- ^ The VPC ID.
    , _elcAvailabilityZones :: [Text]
      -- ^ A list of Availability Zones.
    , _elcRegion :: Maybe Text
      -- ^ The instance's AWS region.
    , _elcElasticLoadBalancerName :: Maybe Text
      -- ^ The Elastic Load Balancing instance's name.
    , _elcStackId :: Maybe Text
      -- ^ The ID of the stack that the instance is associated with.
    , _elcEc2InstanceIds :: [Text]
      -- ^ A list of the EC2 instances that the Elastic Load Balancing
      -- instance is managing traffic for.
    , _elcLayerId :: Maybe Text
      -- ^ The ID of the layer that the instance is attached to.
    , _elcDnsName :: Maybe Text
      -- ^ The instance's public DNS name.
    } deriving (Show, Generic)

instance FromJSON ElasticLoadBalancer

-- | Describes an instance.
data Instance = Instance
    { _iePrivateDns :: Maybe Text
      -- ^ The instance private DNS name.
    , _ieInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _ieStatus :: Maybe Text
      -- ^ The instance status: requested booting running_setup online
      -- setup_failed start_failed terminating terminated stopped
      -- connection_lost.
    , _iePrivateIp :: Maybe Text
      -- ^ The instance private IP address.
    , _ieInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. If this value is set
      -- to false, you must then update your instances manually by using
      -- CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the
      -- instances. We strongly recommend using the default value of true,
      -- to ensure that your instances have the latest security updates.
    , _ieVirtualizationType :: Maybe Text
      -- ^ The instance's virtualization type, paravirtual or hvm.
    , _ieInstanceProfileArn :: Maybe Text
      -- ^ The ARN of the instance's IAM profile. For more information about
      -- IAM ARNs, see Using Identifiers.
    , _ieHostname :: Maybe Text
      -- ^ The instance host name.
    , _ieSshHostRsaKeyFingerprint :: Maybe Text
      -- ^ The SSH key's RSA fingerprint.
    , _ieSecurityGroupIds :: [Text]
      -- ^ An array containing the instance security group IDs.
    , _ieCreatedAt :: Maybe Text
      -- ^ The time that the instance was created.
    , _ieEc2InstanceId :: Maybe Text
      -- ^ The ID of the associated Amazon EC2 instance.
    , _ieSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    , _ieRootDeviceVolumeId :: Maybe Text
      -- ^ The root device volume ID.
    , _ieSubnetId :: Maybe Text
      -- ^ The instance's subnet ID, if the stack is running in a VPC.
    , _ieSshHostDsaKeyFingerprint :: Maybe Text
      -- ^ The SSH key's DSA fingerprint.
    , _ieInstanceType :: Maybe Text
      -- ^ The instance type. AWS OpsWorks supports all instance types
      -- except Cluster Compute, Cluster GPU, and High Memory Cluster. For
      -- more information, see Instance Families and Types. The parameter
      -- values that specify the various types are in the API Name column
      -- of the Available Instance Types table.
    , _ieEbsOptimized :: Maybe Bool
      -- ^ Whether this is an Amazon EBS-optimized instance.
    , _ieElasticIp :: Maybe Text
      -- ^ The instance Elastic IP address .
    , _ieOs :: Maybe Text
      -- ^ The instance operating system.
    , _ieAvailabilityZone :: Maybe Text
      -- ^ The instance Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _ieLastServiceErrorId :: Maybe Text
      -- ^ The ID of the last service error. For more information, call
      -- DescribeServiceErrors.
    , _ieAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance's auto scaling type, which has three possible
      -- values: AlwaysRunning: A 24/7 instance, which is not affected by
      -- auto scaling. TimeBasedAutoScaling: A time-based auto scaling
      -- instance, which is started and stopped based on a specified
      -- schedule. LoadBasedAutoScaling: A load-based auto scaling
      -- instance, which is started and stopped based on load metrics.
    , _ieLayerIds :: [Text]
      -- ^ An array containing the instance layer IDs.
    , _ieArchitecture :: Maybe Architecture
      -- ^ The instance architecture, "i386" or "x86_64".
    , _iePublicDns :: Maybe Text
      -- ^ The instance public DNS name.
    , _ieAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should
      -- be based on one of the standard AWS OpsWorks APIs: Amazon Linux
      -- or Ubuntu 12.04 LTS. For more information, see Instances.
    , _iePublicIp :: Maybe Text
      -- ^ The instance public IP address.
    , _ieStackId :: Maybe Text
      -- ^ The stack ID.
    , _ieRootDeviceType :: Maybe RootDeviceType
      -- ^ The instance root device type. For more information, see Storage
      -- for the Root Device.
    } deriving (Show, Generic)

instance FromJSON Instance

-- | An InstancesCount object with the number of instances in each status.
data InstancesCount = InstancesCount
    { _icTerminating :: Maybe Integer
      -- ^ The number of instances with terminating status.
    , _icPending :: Maybe Integer
      -- ^ The number of instances with pending status.
    , _icOnline :: Maybe Integer
      -- ^ The number of instances with online status.
    , _icRunningSetup :: Maybe Integer
      -- ^ The number of instances with running_setup status.
    , _icRequested :: Maybe Integer
      -- ^ The number of instances with requested status.
    , _icBooting :: Maybe Integer
      -- ^ The number of instances with booting status.
    , _icStopped :: Maybe Integer
      -- ^ The number of instances with stopped status.
    , _icRebooting :: Maybe Integer
      -- ^ The number of instances with rebooting status.
    , _icShuttingDown :: Maybe Integer
      -- ^ The number of instances with shutting_down status.
    , _icSetupFailed :: Maybe Integer
      -- ^ The number of instances with setup_failed status.
    , _icConnectionLost :: Maybe Integer
      -- ^ The number of instances with connection_lost status.
    , _icTerminated :: Maybe Integer
      -- ^ The number of instances with terminated status.
    , _icStopping :: Maybe Integer
      -- ^ The number of instances with stopping status.
    , _icStartFailed :: Maybe Integer
      -- ^ The number of instances with start_failed status.
    } deriving (Show, Generic)

instance FromJSON InstancesCount

-- | Describes a layer.
data Layer = Layer
    { _lrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of the default IAM profile to be used for the layer's EC2
      -- instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , _lrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer's custom security group IDs.
    , _lrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. If this value is set
      -- to false, you must then update your instances manually by using
      -- CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the
      -- instances. We strongly recommend using the default value of true,
      -- to ensure that your instances have the latest security updates.
    , _lrCreatedAt :: Maybe Text
      -- ^ Date when the layer was created.
    , _lrShortname :: Maybe Text
      -- ^ The layer short name.
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
    , _lrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon
      -- EBS volumes.
    , _lrEnableAutoHealing :: Maybe Bool
      -- ^ Whether auto healing is disabled for the layer.
    , _lrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer's packages.
    , _lrAttributes :: Map LayerAttributesKeys Text
      -- ^ The layer attributes.
    , _lrName :: Maybe Text
      -- ^ The layer name.
    , _lrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically
      -- assign a public IP address to the layer's instances. For more
      -- information, see How to Edit a Layer.
    , _lrType :: Maybe LayerType
      -- ^ The layer type, which must be one of the following: Custom
      -- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster
      -- NodeJsAppServer PhpAppServer RailsAppServer WebServer.
    , _lrUseEbsOptimizedInstances :: Maybe Bool
      -- ^ Whether the layer uses Amazon EBS-optimized instances.
    , _lrStackId :: Maybe Text
      -- ^ The layer stack ID.
    , _lrLayerId :: Maybe Text
      -- ^ The layer ID.
    , _lrDefaultSecurityGroupNames :: [Text]
      -- ^ An array containing the layer's security group names.
    , _lrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the
      -- layer's instances. For more information, see How to Edit a Layer.
    } deriving (Show, Generic)

instance FromJSON Layer

-- | Describes a layer's load-based auto scaling configuration.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { _lbasdUpScaling :: Maybe AutoScalingThresholds
      -- ^ A LoadBasedAutoscalingInstruction object that describes the
      -- upscaling configuration, which defines how and when AWS OpsWorks
      -- increases the number of instances.
    , _lbasdEnable :: Maybe Bool
      -- ^ Whether load-based auto scaling is enabled for the layer.
    , _lbasdDownScaling :: Maybe AutoScalingThresholds
      -- ^ A LoadBasedAutoscalingInstruction object that describes the
      -- downscaling configuration, which defines how and when AWS
      -- OpsWorks reduces the number of instances.
    , _lbasdLayerId :: Maybe Text
      -- ^ The layer ID.
    } deriving (Show, Generic)

instance FromJSON LoadBasedAutoScalingConfiguration

-- | Describes stack or user permissions.
data Permission = Permission
    { _pnIamUserArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for an AWS Identity and Access
      -- Management (IAM) role. For more information about IAM ARNs, see
      -- Using Identifiers.
    , _pnAllowSudo :: Maybe Bool
      -- ^ Whether the user can use sudo.
    , _pnStackId :: Maybe Text
      -- ^ A stack ID.
    , _pnLevel :: Maybe Text
      -- ^ The user's permission level, which must be the following: deny
      -- show deploy manage iam_only For more information on the
      -- permissions associated with these levels, see Managing User
      -- Permissions.
    , _pnAllowSsh :: Maybe Bool
      -- ^ Whether the user can use SSH.
    } deriving (Show, Generic)

instance FromJSON Permission

-- | Describes an instance's RAID array.
data RaidArray = RaidArray
    { _rbInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _rbSize :: Maybe Integer
      -- ^ The array's size.
    , _rbIops :: Maybe Integer
      -- ^ For PIOPS volumes, the IOPS per disk.
    , _rbCreatedAt :: Maybe Text
      -- ^ When the RAID array was created.
    , _rbRaidLevel :: Maybe Integer
      -- ^ The RAID level.
    , _rbDevice :: Maybe Text
      -- ^ The array's Linux device. For example /dev/mdadm0.
    , _rbNumberOfDisks :: Maybe Integer
      -- ^ The number of disks in the array.
    , _rbAvailabilityZone :: Maybe Text
      -- ^ The array's Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _rbName :: Maybe Text
      -- ^ The array name.
    , _rbRaidArrayId :: Maybe Text
      -- ^ The array ID.
    , _rbVolumeType :: Maybe Text
      -- ^ The volume type, standard or PIOPS.
    , _rbMountPoint :: Maybe Text
      -- ^ The array's mount point.
    } deriving (Show, Generic)

instance FromJSON RaidArray

-- | Describes an Amazon RDS instance.
data RdsDbInstance = RdsDbInstance
    { _rdjRdsDbInstanceArn :: Maybe Text
      -- ^ The instance's ARN.
    , _rdjDbUser :: Maybe Text
      -- ^ The master user name.
    , _rdjMissingOnRds :: Maybe Bool
      -- ^ Set to true if AWS OpsWorks was unable to discover the Amazon RDS
      -- instance. AWS OpsWorks attempts to discover the instance only
      -- once. If this value is set to true, you must deregister the
      -- instance and then register it again.
    , _rdjEngine :: Maybe Text
      -- ^ The instance's database engine.
    , _rdjAddress :: Maybe Text
      -- ^ The instance's address.
    , _rdjDbInstanceIdentifier :: Maybe Text
      -- ^ The DB instance identifier.
    , _rdjRegion :: Maybe Text
      -- ^ The instance's AWS region.
    , _rdjStackId :: Maybe Text
      -- ^ The ID of the stack that the instance is registered with.
    , _rdjDbPassword :: Maybe Text
      -- ^ The database password.
    } deriving (Show, Generic)

instance FromJSON RdsDbInstance

-- | A LayerCustomRecipes object that specifies the layer custom recipes.
data Recipes = Recipes
    { _rSetup :: [Text]
      -- ^ An array of custom recipe names to be run following a setup
      -- event.
    , _rShutdown :: [Text]
      -- ^ An array of custom recipe names to be run following a shutdown
      -- event.
    , _rUndeploy :: [Text]
      -- ^ An array of custom recipe names to be run following a undeploy
      -- event.
    , _rConfigure :: [Text]
      -- ^ An array of custom recipe names to be run following a configure
      -- event.
    , _rDeploy :: [Text]
      -- ^ An array of custom recipe names to be run following a deploy
      -- event.
    } deriving (Show, Generic)

instance FromJSON Recipes

instance ToJSON Recipes

-- | A UserProfile object that describes the user's SSH information.
data SelfUserProfile = SelfUserProfile
    { _supSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    , _supSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    , _supIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , _supName :: Maybe Text
      -- ^ The user's name.
    } deriving (Show, Generic)

instance FromJSON SelfUserProfile

-- | Describes an AWS OpsWorks service error.
data ServiceError = ServiceError
    { _ssssssssssssssssssssssrInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _ssssssssssssssssssssssrCreatedAt :: Maybe Text
      -- ^ When the error occurred.
    , _ssssssssssssssssssssssrServiceErrorId :: Maybe Text
      -- ^ The error ID.
    , _ssssssssssssssssssssssrType :: Maybe Text
      -- ^ The error type.
    , _ssssssssssssssssssssssrStackId :: Maybe Text
      -- ^ The stack ID.
    , _ssssssssssssssssssssssrMessage :: Maybe Text
      -- ^ A message that describes the error.
    } deriving (Show, Generic)

instance FromJSON ServiceError

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
data Source = Source
    { _seUrl :: Maybe Text
      -- ^ The source URL.
    , _seUsername :: Maybe Text
      -- ^ This parameter depends on the repository type. For Amazon S3
      -- bundles, set Username to the appropriate IAM access key ID. For
      -- HTTP bundles, Git repositories, and Subversion repositories, set
      -- Username to the user name.
    , _seSshKey :: Maybe Text
      -- ^ The repository's SSH key.
    , _sePassword :: Maybe Text
      -- ^ This parameter depends on the repository type. For Amazon S3
      -- bundles, set Password to the appropriate IAM secret access key.
      -- For HTTP bundles and Subversion repositories, set Password to the
      -- password. For more information on how to safely handle IAM
      -- credentials, see .
    , _seType :: Maybe SourceType
      -- ^ The repository type.
    , _seRevision :: Maybe Text
      -- ^ The application's version. AWS OpsWorks enables you to easily
      -- deploy new versions of an application. One of the simplest
      -- approaches is to have branches or revisions in your repository
      -- that represent different versions that can potentially be
      -- deployed.
    } deriving (Show, Generic)

instance FromJSON Source

instance ToJSON Source

-- | An SslConfiguration object with the SSL configuration.
data SslConfiguration = SslConfiguration
    { _scPrivateKey :: Text
      -- ^ The private key; the contents of the certificate's domain.kex
      -- file.
    , _scCertificate :: Text
      -- ^ The contents of the certificate's domain.crt file.
    , _scChain :: Maybe Text
      -- ^ Optional. Can be used to specify an intermediate certificate
      -- authority key or client authentication.
    } deriving (Show, Generic)

instance FromJSON SslConfiguration

instance ToJSON SslConfiguration

-- | Describes a stack.
data Stack = Stack
    { _ssssssqDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of
      -- the stack's EC2 instances. For more information about IAM ARNs,
      -- see Using Identifiers.
    , _ssssssqServiceRoleArn :: Maybe Text
      -- ^ The stack AWS Identity and Access Management (IAM) role.
    , _ssssssqDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for
      -- all instances in the stack, but you can override it when you
      -- create an instance. For more information, see Storage for the
      -- Root Device.
    , _ssssssqArn :: Maybe Text
      -- ^ The stack's ARN.
    , _ssssssqCreatedAt :: Maybe Text
      -- ^ Date when the stack was created.
    , _ssssssqVpcId :: Maybe Text
      -- ^ The VPC ID, if the stack is running in a VPC.
    , _ssssssqChefConfiguration :: Maybe ChefConfiguration
      -- ^ A ChefConfiguration object that specifies whether to enable
      -- Berkshelf and the Berkshelf version. For more information, see
      -- Create a New Stack.
    , _ssssssqDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack's instances. You can override
      -- this value when you create or update an instance.
    , _ssssssqCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _ssssssqCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook
      -- from a repository. For more information, see Creating Apps or
      -- Custom Recipes and Cookbooks.
    , _ssssssqDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone. For more information, see
      -- Regions and Endpoints.
    , _ssssssqAttributes :: Map StackAttributesKeys Text
      -- ^ The stack's attributes.
    , _ssssssqName :: Maybe Text
      -- ^ The stack name.
    , _ssssssqDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon
      -- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , _ssssssqUseOpsworksSecurityGroups :: Maybe Bool
      -- ^ Whether the stack automatically associates the AWS OpsWorks
      -- built-in security groups with the stack's layers.
    , _ssssssqUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , _ssssssqDefaultSubnetId :: Maybe Text
      -- ^ The default subnet ID, if the stack is running in a VPC.
    , _ssssssqRegion :: Maybe Text
      -- ^ The stack AWS region, such as "us-east-1". For more information
      -- about AWS regions, see Regions and Endpoints.
    , _ssssssqConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager.
    , _ssssssqStackId :: Maybe Text
      -- ^ The stack ID.
    , _ssssssqHostnameTheme :: Maybe Text
      -- ^ The stack host name theme, with spaces replaced by underscores.
    } deriving (Show, Generic)

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

instance FromJSON StackConfigurationManager

instance ToJSON StackConfigurationManager

-- | A StackSummary object that contains the results.
data StackSummary = StackSummary
    { _sssssssssssssyArn :: Maybe Text
      -- ^ The stack's ARN.
    , _sssssssssssssyAppsCount :: Maybe Integer
      -- ^ The number of apps.
    , _sssssssssssssyName :: Maybe Text
      -- ^ The stack name.
    , _sssssssssssssyStackId :: Maybe Text
      -- ^ The stack ID.
    , _sssssssssssssyLayersCount :: Maybe Integer
      -- ^ The number of layers.
    , _sssssssssssssyInstancesCount :: Maybe InstancesCount
      -- ^ An InstancesCount object with the number of instances in each
      -- status.
    } deriving (Show, Generic)

instance FromJSON StackSummary

-- | Describes an instance's time-based auto scaling configuration.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { _tbasdInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _tbasdAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
      -- ^ A WeeklyAutoScalingSchedule object with the instance schedule.
    } deriving (Show, Generic)

instance FromJSON TimeBasedAutoScalingConfiguration

-- | Describes a user's SSH information.
data UserProfile = UserProfile
    { _uueAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My
      -- Settings page. For more information, see Managing User
      -- Permissions.
    , _uueSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    , _uueSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    , _uueIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , _uueName :: Maybe Text
      -- ^ The user's name.
    } deriving (Show, Generic)

instance FromJSON UserProfile

-- | Describes an instance's Amazon EBS volume.
data Volume = Volume
    { _vgInstanceId :: Maybe Text
      -- ^ The instance ID.
    , _vgStatus :: Maybe Text
      -- ^ The value returned by DescribeVolumes.
    , _vgSize :: Maybe Integer
      -- ^ The volume size.
    , _vgIops :: Maybe Integer
      -- ^ For PIOPS volumes, the IOPS per disk.
    , _vgDevice :: Maybe Text
      -- ^ The device name.
    , _vgAvailabilityZone :: Maybe Text
      -- ^ The volume Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _vgName :: Maybe Text
      -- ^ The volume name.
    , _vgRaidArrayId :: Maybe Text
      -- ^ The RAID array ID.
    , _vgVolumeId :: Maybe Text
      -- ^ The volume ID.
    , _vgRegion :: Maybe Text
      -- ^ The AWS region. For more information about AWS regions, see
      -- Regions and Endpoints.
    , _vgVolumeType :: Maybe Text
      -- ^ The volume type, standard or PIOPS.
    , _vgEc2VolumeId :: Maybe Text
      -- ^ The Amazon EC2 volume ID.
    , _vgMountPoint :: Maybe Text
      -- ^ The volume mount point. For example "/dev/sdh".
    } deriving (Show, Generic)

instance FromJSON Volume

-- | Describes an Amazon EBS volume configuration.
data VolumeConfiguration = VolumeConfiguration
    { _vdSize :: Integer
      -- ^ The volume size.
    , _vdIops :: Maybe Integer
      -- ^ For PIOPS volumes, the IOPS per disk.
    , _vdRaidLevel :: Maybe Integer
      -- ^ The volume RAID level.
    , _vdNumberOfDisks :: Integer
      -- ^ The number of disks in the volume.
    , _vdVolumeType :: Maybe Text
      -- ^ The volume type, standard or PIOPS.
    , _vdMountPoint :: Text
      -- ^ The volume mount point. For example "/dev/sdh".
    } deriving (Show, Generic)

instance FromJSON VolumeConfiguration

instance ToJSON VolumeConfiguration

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { _wassThursday :: Map Text Text
      -- ^ The schedule for Thursday.
    , _wassWednesday :: Map Text Text
      -- ^ The schedule for Wednesday.
    , _wassSaturday :: Map Text Text
      -- ^ The schedule for Saturday.
    , _wassMonday :: Map Text Text
      -- ^ The schedule for Monday.
    , _wassFriday :: Map Text Text
      -- ^ The schedule for Friday.
    , _wassSunday :: Map Text Text
      -- ^ The schedule for Sunday.
    , _wassTuesday :: Map Text Text
      -- ^ The schedule for Tuesday.
    } deriving (Show, Generic)

instance FromJSON WeeklyAutoScalingSchedule

instance ToJSON WeeklyAutoScalingSchedule

-- Newtypes

-- Products
makeLenses ''App
makeLenses ''AutoScalingThresholds
makeLenses ''ChefConfiguration
makeLenses ''Command
makeLenses ''DataSource
makeLenses ''Deployment
makeLenses ''DeploymentCommand
makeLenses ''ElasticIp
makeLenses ''ElasticLoadBalancer
makeLenses ''Instance
makeLenses ''InstancesCount
makeLenses ''Layer
makeLenses ''LoadBasedAutoScalingConfiguration
makeLenses ''Permission
makeLenses ''RaidArray
makeLenses ''RdsDbInstance
makeLenses ''Recipes
makeLenses ''SelfUserProfile
makeLenses ''ServiceError
makeLenses ''Source
makeLenses ''SslConfiguration
makeLenses ''Stack
makeLenses ''StackConfigurationManager
makeLenses ''StackSummary
makeLenses ''TimeBasedAutoScalingConfiguration
makeLenses ''UserProfile
makeLenses ''Volume
makeLenses ''VolumeConfiguration
makeLenses ''WeeklyAutoScalingSchedule
