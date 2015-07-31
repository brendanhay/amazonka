{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Sum where

import           Network.AWS.Prelude

data AppAttributesKeys
    = DocumentRoot
    | RailsEnv
    | AWSFlowRubySettings
    | AutoBundleOnDeploy
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AppAttributesKeys where
    parser = takeLowerText >>= \case
        "awsflowrubysettings" -> pure AWSFlowRubySettings
        "autobundleondeploy" -> pure AutoBundleOnDeploy
        "documentroot" -> pure DocumentRoot
        "railsenv" -> pure RailsEnv
        e -> fromTextError $ "Failure parsing AppAttributesKeys from value: '" <> e
           <> "'. Accepted values: awsflowrubysettings, autobundleondeploy, documentroot, railsenv"

instance ToText AppAttributesKeys where
    toText = \case
        AWSFlowRubySettings -> "awsflowrubysettings"
        AutoBundleOnDeploy -> "autobundleondeploy"
        DocumentRoot -> "documentroot"
        RailsEnv -> "railsenv"

instance Hashable     AppAttributesKeys
instance ToByteString AppAttributesKeys
instance ToQuery      AppAttributesKeys
instance ToHeader     AppAttributesKeys

instance ToJSON AppAttributesKeys where
    toJSON = toJSONText

instance FromJSON AppAttributesKeys where
    parseJSON = parseJSONText "AppAttributesKeys"

data AppType
    = ATPHP
    | ATStatic
    | ATAWSFlowRuby
    | ATJava
    | ATNodejs
    | ATRails
    | ATOther
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AppType where
    parser = takeLowerText >>= \case
        "aws-flow-ruby" -> pure ATAWSFlowRuby
        "java" -> pure ATJava
        "nodejs" -> pure ATNodejs
        "other" -> pure ATOther
        "php" -> pure ATPHP
        "rails" -> pure ATRails
        "static" -> pure ATStatic
        e -> fromTextError $ "Failure parsing AppType from value: '" <> e
           <> "'. Accepted values: aws-flow-ruby, java, nodejs, other, php, rails, static"

instance ToText AppType where
    toText = \case
        ATAWSFlowRuby -> "aws-flow-ruby"
        ATJava -> "java"
        ATNodejs -> "nodejs"
        ATOther -> "other"
        ATPHP -> "php"
        ATRails -> "rails"
        ATStatic -> "static"

instance Hashable     AppType
instance ToByteString AppType
instance ToQuery      AppType
instance ToHeader     AppType

instance ToJSON AppType where
    toJSON = toJSONText

instance FromJSON AppType where
    parseJSON = parseJSONText "AppType"

data Architecture
    = I386
    | X86_64
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Architecture where
    parser = takeLowerText >>= \case
        "i386" -> pure I386
        "x86_64" -> pure X86_64
        e -> fromTextError $ "Failure parsing Architecture from value: '" <> e
           <> "'. Accepted values: i386, x86_64"

instance ToText Architecture where
    toText = \case
        I386 -> "i386"
        X86_64 -> "x86_64"

instance Hashable     Architecture
instance ToByteString Architecture
instance ToQuery      Architecture
instance ToHeader     Architecture

instance ToJSON Architecture where
    toJSON = toJSONText

instance FromJSON Architecture where
    parseJSON = parseJSONText "Architecture"

data AutoScalingType
    = Timer
    | Load
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AutoScalingType where
    parser = takeLowerText >>= \case
        "load" -> pure Load
        "timer" -> pure Timer
        e -> fromTextError $ "Failure parsing AutoScalingType from value: '" <> e
           <> "'. Accepted values: load, timer"

instance ToText AutoScalingType where
    toText = \case
        Load -> "load"
        Timer -> "timer"

instance Hashable     AutoScalingType
instance ToByteString AutoScalingType
instance ToQuery      AutoScalingType
instance ToHeader     AutoScalingType

instance ToJSON AutoScalingType where
    toJSON = toJSONText

instance FromJSON AutoScalingType where
    parseJSON = parseJSONText "AutoScalingType"

data DeploymentCommandName
    = ExecuteRecipes
    | Setup
    | Start
    | UpdateCustomCookbooks
    | InstallDependencies
    | Undeploy
    | Rollback
    | Restart
    | Stop
    | UpdateDependencies
    | Configure
    | Deploy
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeploymentCommandName where
    parser = takeLowerText >>= \case
        "configure" -> pure Configure
        "deploy" -> pure Deploy
        "execute_recipes" -> pure ExecuteRecipes
        "install_dependencies" -> pure InstallDependencies
        "restart" -> pure Restart
        "rollback" -> pure Rollback
        "setup" -> pure Setup
        "start" -> pure Start
        "stop" -> pure Stop
        "undeploy" -> pure Undeploy
        "update_custom_cookbooks" -> pure UpdateCustomCookbooks
        "update_dependencies" -> pure UpdateDependencies
        e -> fromTextError $ "Failure parsing DeploymentCommandName from value: '" <> e
           <> "'. Accepted values: configure, deploy, execute_recipes, install_dependencies, restart, rollback, setup, start, stop, undeploy, update_custom_cookbooks, update_dependencies"

instance ToText DeploymentCommandName where
    toText = \case
        Configure -> "configure"
        Deploy -> "deploy"
        ExecuteRecipes -> "execute_recipes"
        InstallDependencies -> "install_dependencies"
        Restart -> "restart"
        Rollback -> "rollback"
        Setup -> "setup"
        Start -> "start"
        Stop -> "stop"
        Undeploy -> "undeploy"
        UpdateCustomCookbooks -> "update_custom_cookbooks"
        UpdateDependencies -> "update_dependencies"

instance Hashable     DeploymentCommandName
instance ToByteString DeploymentCommandName
instance ToQuery      DeploymentCommandName
instance ToHeader     DeploymentCommandName

instance ToJSON DeploymentCommandName where
    toJSON = toJSONText

instance FromJSON DeploymentCommandName where
    parseJSON = parseJSONText "DeploymentCommandName"

data LayerAttributesKeys
    = HaproxyHealthCheckURL
    | MemcachedMemory
    | GangliaPassword
    | EcsClusterARN
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText LayerAttributesKeys where
    parser = takeLowerText >>= \case
        "bundlerversion" -> pure BundlerVersion
        "ecsclusterarn" -> pure EcsClusterARN
        "enablehaproxystats" -> pure EnableHaproxyStats
        "gangliapassword" -> pure GangliaPassword
        "gangliaurl" -> pure GangliaURL
        "gangliauser" -> pure GangliaUser
        "haproxyhealthcheckmethod" -> pure HaproxyHealthCheckMethod
        "haproxyhealthcheckurl" -> pure HaproxyHealthCheckURL
        "haproxystatspassword" -> pure HaproxyStatsPassword
        "haproxystatsurl" -> pure HaproxyStatsURL
        "haproxystatsuser" -> pure HaproxyStatsUser
        "jvm" -> pure JVM
        "jvmoptions" -> pure JVMOptions
        "jvmversion" -> pure JVMVersion
        "javaappserver" -> pure JavaAppServer
        "javaappserverversion" -> pure JavaAppServerVersion
        "managebundler" -> pure ManageBundler
        "memcachedmemory" -> pure MemcachedMemory
        "mysqlrootpassword" -> pure MysqlRootPassword
        "mysqlrootpasswordubiquitous" -> pure MysqlRootPasswordUbiquitous
        "nodejsversion" -> pure NodejsVersion
        "passengerversion" -> pure PassengerVersion
        "railsstack" -> pure RailsStack
        "rubyversion" -> pure RubyVersion
        "rubygemsversion" -> pure RubygemsVersion
        e -> fromTextError $ "Failure parsing LayerAttributesKeys from value: '" <> e
           <> "'. Accepted values: bundlerversion, ecsclusterarn, enablehaproxystats, gangliapassword, gangliaurl, gangliauser, haproxyhealthcheckmethod, haproxyhealthcheckurl, haproxystatspassword, haproxystatsurl, haproxystatsuser, jvm, jvmoptions, jvmversion, javaappserver, javaappserverversion, managebundler, memcachedmemory, mysqlrootpassword, mysqlrootpasswordubiquitous, nodejsversion, passengerversion, railsstack, rubyversion, rubygemsversion"

instance ToText LayerAttributesKeys where
    toText = \case
        BundlerVersion -> "bundlerversion"
        EcsClusterARN -> "ecsclusterarn"
        EnableHaproxyStats -> "enablehaproxystats"
        GangliaPassword -> "gangliapassword"
        GangliaURL -> "gangliaurl"
        GangliaUser -> "gangliauser"
        HaproxyHealthCheckMethod -> "haproxyhealthcheckmethod"
        HaproxyHealthCheckURL -> "haproxyhealthcheckurl"
        HaproxyStatsPassword -> "haproxystatspassword"
        HaproxyStatsURL -> "haproxystatsurl"
        HaproxyStatsUser -> "haproxystatsuser"
        JVM -> "jvm"
        JVMOptions -> "jvmoptions"
        JVMVersion -> "jvmversion"
        JavaAppServer -> "javaappserver"
        JavaAppServerVersion -> "javaappserverversion"
        ManageBundler -> "managebundler"
        MemcachedMemory -> "memcachedmemory"
        MysqlRootPassword -> "mysqlrootpassword"
        MysqlRootPasswordUbiquitous -> "mysqlrootpasswordubiquitous"
        NodejsVersion -> "nodejsversion"
        PassengerVersion -> "passengerversion"
        RailsStack -> "railsstack"
        RubyVersion -> "rubyversion"
        RubygemsVersion -> "rubygemsversion"

instance Hashable     LayerAttributesKeys
instance ToByteString LayerAttributesKeys
instance ToQuery      LayerAttributesKeys
instance ToHeader     LayerAttributesKeys

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
    | AWSFlowRuby
    | LB
    | RailsApp
    | EcsCluster
    | DBMaster
    | Web
    | PHPApp
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText LayerType where
    parser = takeLowerText >>= \case
        "aws-flow-ruby" -> pure AWSFlowRuby
        "custom" -> pure Custom
        "db-master" -> pure DBMaster
        "ecs-cluster" -> pure EcsCluster
        "java-app" -> pure JavaApp
        "lb" -> pure LB
        "memcached" -> pure Memcached
        "monitoring-master" -> pure MonitoringMaster
        "nodejs-app" -> pure NodejsApp
        "php-app" -> pure PHPApp
        "rails-app" -> pure RailsApp
        "web" -> pure Web
        e -> fromTextError $ "Failure parsing LayerType from value: '" <> e
           <> "'. Accepted values: aws-flow-ruby, custom, db-master, ecs-cluster, java-app, lb, memcached, monitoring-master, nodejs-app, php-app, rails-app, web"

instance ToText LayerType where
    toText = \case
        AWSFlowRuby -> "aws-flow-ruby"
        Custom -> "custom"
        DBMaster -> "db-master"
        EcsCluster -> "ecs-cluster"
        JavaApp -> "java-app"
        LB -> "lb"
        Memcached -> "memcached"
        MonitoringMaster -> "monitoring-master"
        NodejsApp -> "nodejs-app"
        PHPApp -> "php-app"
        RailsApp -> "rails-app"
        Web -> "web"

instance Hashable     LayerType
instance ToByteString LayerType
instance ToQuery      LayerType
instance ToHeader     LayerType

instance ToJSON LayerType where
    toJSON = toJSONText

instance FromJSON LayerType where
    parseJSON = parseJSONText "LayerType"

data RootDeviceType
    = InstanceStore
    | EBS
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RootDeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fromTextError $ "Failure parsing RootDeviceType from value: '" <> e
           <> "'. Accepted values: ebs, instance-store"

instance ToText RootDeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance Hashable     RootDeviceType
instance ToByteString RootDeviceType
instance ToQuery      RootDeviceType
instance ToHeader     RootDeviceType

instance ToJSON RootDeviceType where
    toJSON = toJSONText

instance FromJSON RootDeviceType where
    parseJSON = parseJSONText "RootDeviceType"

data SourceType
    = SVN
    | Git
    | Archive
    | S3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "archive" -> pure Archive
        "git" -> pure Git
        "s3" -> pure S3
        "svn" -> pure SVN
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: archive, git, s3, svn"

instance ToText SourceType where
    toText = \case
        Archive -> "archive"
        Git -> "git"
        S3 -> "s3"
        SVN -> "svn"

instance Hashable     SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"

data StackAttributesKeys =
    Color
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StackAttributesKeys where
    parser = takeLowerText >>= \case
        "color" -> pure Color
        e -> fromTextError $ "Failure parsing StackAttributesKeys from value: '" <> e
           <> "'. Accepted values: color"

instance ToText StackAttributesKeys where
    toText = \case
        Color -> "color"

instance Hashable     StackAttributesKeys
instance ToByteString StackAttributesKeys
instance ToQuery      StackAttributesKeys
instance ToHeader     StackAttributesKeys

instance ToJSON StackAttributesKeys where
    toJSON = toJSONText

instance FromJSON StackAttributesKeys where
    parseJSON = parseJSONText "StackAttributesKeys"

data VirtualizationType
    = Paravirtual
    | HVM
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fromTextError $ "Failure parsing VirtualizationType from value: '" <> e
           <> "'. Accepted values: hvm, paravirtual"

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance Hashable     VirtualizationType
instance ToByteString VirtualizationType
instance ToQuery      VirtualizationType
instance ToHeader     VirtualizationType

instance FromJSON VirtualizationType where
    parseJSON = parseJSONText "VirtualizationType"

data VolumeType
    = Standard
    | IO1
    | GP2
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing VolumeType from value: '" <> e
           <> "'. Accepted values: gp2, io1, standard"

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        Standard -> "standard"

instance Hashable     VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance ToJSON VolumeType where
    toJSON = toJSONText

instance FromJSON VolumeType where
    parseJSON = parseJSONText "VolumeType"
