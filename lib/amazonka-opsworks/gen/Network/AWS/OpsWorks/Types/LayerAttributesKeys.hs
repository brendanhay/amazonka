{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerAttributesKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LayerAttributesKeys
  ( LayerAttributesKeys
      ( LayerAttributesKeys',
        EcsClusterARN,
        EnableHaproxyStats,
        HaproxyStatsURL,
        HaproxyStatsUser,
        HaproxyStatsPassword,
        HaproxyHealthCheckURL,
        HaproxyHealthCheckMethod,
        MysqlRootPassword,
        MysqlRootPasswordUbiquitous,
        GangliaURL,
        GangliaUser,
        GangliaPassword,
        MemcachedMemory,
        NodejsVersion,
        RubyVersion,
        RubygemsVersion,
        ManageBundler,
        BundlerVersion,
        RailsStack,
        PassengerVersion,
        JVM,
        JVMVersion,
        JVMOptions,
        JavaAppServer,
        JavaAppServerVersion
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LayerAttributesKeys = LayerAttributesKeys' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern EcsClusterARN :: LayerAttributesKeys
pattern EcsClusterARN = LayerAttributesKeys' "EcsClusterArn"

pattern EnableHaproxyStats :: LayerAttributesKeys
pattern EnableHaproxyStats = LayerAttributesKeys' "EnableHaproxyStats"

pattern HaproxyStatsURL :: LayerAttributesKeys
pattern HaproxyStatsURL = LayerAttributesKeys' "HaproxyStatsUrl"

pattern HaproxyStatsUser :: LayerAttributesKeys
pattern HaproxyStatsUser = LayerAttributesKeys' "HaproxyStatsUser"

pattern HaproxyStatsPassword :: LayerAttributesKeys
pattern HaproxyStatsPassword = LayerAttributesKeys' "HaproxyStatsPassword"

pattern HaproxyHealthCheckURL :: LayerAttributesKeys
pattern HaproxyHealthCheckURL = LayerAttributesKeys' "HaproxyHealthCheckUrl"

pattern HaproxyHealthCheckMethod :: LayerAttributesKeys
pattern HaproxyHealthCheckMethod = LayerAttributesKeys' "HaproxyHealthCheckMethod"

pattern MysqlRootPassword :: LayerAttributesKeys
pattern MysqlRootPassword = LayerAttributesKeys' "MysqlRootPassword"

pattern MysqlRootPasswordUbiquitous :: LayerAttributesKeys
pattern MysqlRootPasswordUbiquitous = LayerAttributesKeys' "MysqlRootPasswordUbiquitous"

pattern GangliaURL :: LayerAttributesKeys
pattern GangliaURL = LayerAttributesKeys' "GangliaUrl"

pattern GangliaUser :: LayerAttributesKeys
pattern GangliaUser = LayerAttributesKeys' "GangliaUser"

pattern GangliaPassword :: LayerAttributesKeys
pattern GangliaPassword = LayerAttributesKeys' "GangliaPassword"

pattern MemcachedMemory :: LayerAttributesKeys
pattern MemcachedMemory = LayerAttributesKeys' "MemcachedMemory"

pattern NodejsVersion :: LayerAttributesKeys
pattern NodejsVersion = LayerAttributesKeys' "NodejsVersion"

pattern RubyVersion :: LayerAttributesKeys
pattern RubyVersion = LayerAttributesKeys' "RubyVersion"

pattern RubygemsVersion :: LayerAttributesKeys
pattern RubygemsVersion = LayerAttributesKeys' "RubygemsVersion"

pattern ManageBundler :: LayerAttributesKeys
pattern ManageBundler = LayerAttributesKeys' "ManageBundler"

pattern BundlerVersion :: LayerAttributesKeys
pattern BundlerVersion = LayerAttributesKeys' "BundlerVersion"

pattern RailsStack :: LayerAttributesKeys
pattern RailsStack = LayerAttributesKeys' "RailsStack"

pattern PassengerVersion :: LayerAttributesKeys
pattern PassengerVersion = LayerAttributesKeys' "PassengerVersion"

pattern JVM :: LayerAttributesKeys
pattern JVM = LayerAttributesKeys' "Jvm"

pattern JVMVersion :: LayerAttributesKeys
pattern JVMVersion = LayerAttributesKeys' "JvmVersion"

pattern JVMOptions :: LayerAttributesKeys
pattern JVMOptions = LayerAttributesKeys' "JvmOptions"

pattern JavaAppServer :: LayerAttributesKeys
pattern JavaAppServer = LayerAttributesKeys' "JavaAppServer"

pattern JavaAppServerVersion :: LayerAttributesKeys
pattern JavaAppServerVersion = LayerAttributesKeys' "JavaAppServerVersion"

{-# COMPLETE
  EcsClusterARN,
  EnableHaproxyStats,
  HaproxyStatsURL,
  HaproxyStatsUser,
  HaproxyStatsPassword,
  HaproxyHealthCheckURL,
  HaproxyHealthCheckMethod,
  MysqlRootPassword,
  MysqlRootPasswordUbiquitous,
  GangliaURL,
  GangliaUser,
  GangliaPassword,
  MemcachedMemory,
  NodejsVersion,
  RubyVersion,
  RubygemsVersion,
  ManageBundler,
  BundlerVersion,
  RailsStack,
  PassengerVersion,
  JVM,
  JVMVersion,
  JVMOptions,
  JavaAppServer,
  JavaAppServerVersion,
  LayerAttributesKeys'
  #-}
