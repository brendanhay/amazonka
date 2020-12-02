{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerAttributesKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LayerAttributesKeys where

import Network.AWS.Prelude

data LayerAttributesKeys
  = BundlerVersion
  | EcsClusterARN
  | EnableHaproxyStats
  | GangliaPassword
  | GangliaURL
  | GangliaUser
  | HaproxyHealthCheckMethod
  | HaproxyHealthCheckURL
  | HaproxyStatsPassword
  | HaproxyStatsURL
  | HaproxyStatsUser
  | JVM
  | JVMOptions
  | JVMVersion
  | JavaAppServer
  | JavaAppServerVersion
  | ManageBundler
  | MemcachedMemory
  | MysqlRootPassword
  | MysqlRootPasswordUbiquitous
  | NodejsVersion
  | PassengerVersion
  | RailsStack
  | RubyVersion
  | RubygemsVersion
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText LayerAttributesKeys where
  parser =
    takeLowerText >>= \case
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
      e ->
        fromTextError $
          "Failure parsing LayerAttributesKeys from value: '" <> e
            <> "'. Accepted values: bundlerversion, ecsclusterarn, enablehaproxystats, gangliapassword, gangliaurl, gangliauser, haproxyhealthcheckmethod, haproxyhealthcheckurl, haproxystatspassword, haproxystatsurl, haproxystatsuser, jvm, jvmoptions, jvmversion, javaappserver, javaappserverversion, managebundler, memcachedmemory, mysqlrootpassword, mysqlrootpasswordubiquitous, nodejsversion, passengerversion, railsstack, rubyversion, rubygemsversion"

instance ToText LayerAttributesKeys where
  toText = \case
    BundlerVersion -> "BundlerVersion"
    EcsClusterARN -> "EcsClusterArn"
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

instance NFData LayerAttributesKeys

instance ToByteString LayerAttributesKeys

instance ToQuery LayerAttributesKeys

instance ToHeader LayerAttributesKeys

instance ToJSON LayerAttributesKeys where
  toJSON = toJSONText

instance FromJSON LayerAttributesKeys where
  parseJSON = parseJSONText "LayerAttributesKeys"
