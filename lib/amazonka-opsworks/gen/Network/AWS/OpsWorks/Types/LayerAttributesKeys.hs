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
        BundlerVersion,
        EcsClusterARN,
        EnableHaproxyStats,
        GangliaPassword,
        GangliaURL,
        GangliaUser,
        HaproxyHealthCheckMethod,
        HaproxyHealthCheckURL,
        HaproxyStatsPassword,
        HaproxyStatsURL,
        HaproxyStatsUser,
        JVM,
        JVMOptions,
        JVMVersion,
        JavaAppServer,
        JavaAppServerVersion,
        ManageBundler,
        MemcachedMemory,
        MysqlRootPassword,
        MysqlRootPasswordUbiquitous,
        NodejsVersion,
        PassengerVersion,
        RailsStack,
        RubyVersion,
        RubygemsVersion
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

pattern BundlerVersion :: LayerAttributesKeys
pattern BundlerVersion = LayerAttributesKeys' "BundlerVersion"

pattern EcsClusterARN :: LayerAttributesKeys
pattern EcsClusterARN = LayerAttributesKeys' "EcsClusterArn"

pattern EnableHaproxyStats :: LayerAttributesKeys
pattern EnableHaproxyStats = LayerAttributesKeys' "EnableHaproxyStats"

pattern GangliaPassword :: LayerAttributesKeys
pattern GangliaPassword = LayerAttributesKeys' "GangliaPassword"

pattern GangliaURL :: LayerAttributesKeys
pattern GangliaURL = LayerAttributesKeys' "GangliaUrl"

pattern GangliaUser :: LayerAttributesKeys
pattern GangliaUser = LayerAttributesKeys' "GangliaUser"

pattern HaproxyHealthCheckMethod :: LayerAttributesKeys
pattern HaproxyHealthCheckMethod = LayerAttributesKeys' "HaproxyHealthCheckMethod"

pattern HaproxyHealthCheckURL :: LayerAttributesKeys
pattern HaproxyHealthCheckURL = LayerAttributesKeys' "HaproxyHealthCheckUrl"

pattern HaproxyStatsPassword :: LayerAttributesKeys
pattern HaproxyStatsPassword = LayerAttributesKeys' "HaproxyStatsPassword"

pattern HaproxyStatsURL :: LayerAttributesKeys
pattern HaproxyStatsURL = LayerAttributesKeys' "HaproxyStatsUrl"

pattern HaproxyStatsUser :: LayerAttributesKeys
pattern HaproxyStatsUser = LayerAttributesKeys' "HaproxyStatsUser"

pattern JVM :: LayerAttributesKeys
pattern JVM = LayerAttributesKeys' "Jvm"

pattern JVMOptions :: LayerAttributesKeys
pattern JVMOptions = LayerAttributesKeys' "JvmOptions"

pattern JVMVersion :: LayerAttributesKeys
pattern JVMVersion = LayerAttributesKeys' "JvmVersion"

pattern JavaAppServer :: LayerAttributesKeys
pattern JavaAppServer = LayerAttributesKeys' "JavaAppServer"

pattern JavaAppServerVersion :: LayerAttributesKeys
pattern JavaAppServerVersion = LayerAttributesKeys' "JavaAppServerVersion"

pattern ManageBundler :: LayerAttributesKeys
pattern ManageBundler = LayerAttributesKeys' "ManageBundler"

pattern MemcachedMemory :: LayerAttributesKeys
pattern MemcachedMemory = LayerAttributesKeys' "MemcachedMemory"

pattern MysqlRootPassword :: LayerAttributesKeys
pattern MysqlRootPassword = LayerAttributesKeys' "MysqlRootPassword"

pattern MysqlRootPasswordUbiquitous :: LayerAttributesKeys
pattern MysqlRootPasswordUbiquitous = LayerAttributesKeys' "MysqlRootPasswordUbiquitous"

pattern NodejsVersion :: LayerAttributesKeys
pattern NodejsVersion = LayerAttributesKeys' "NodejsVersion"

pattern PassengerVersion :: LayerAttributesKeys
pattern PassengerVersion = LayerAttributesKeys' "PassengerVersion"

pattern RailsStack :: LayerAttributesKeys
pattern RailsStack = LayerAttributesKeys' "RailsStack"

pattern RubyVersion :: LayerAttributesKeys
pattern RubyVersion = LayerAttributesKeys' "RubyVersion"

pattern RubygemsVersion :: LayerAttributesKeys
pattern RubygemsVersion = LayerAttributesKeys' "RubygemsVersion"

{-# COMPLETE
  BundlerVersion,
  EcsClusterARN,
  EnableHaproxyStats,
  GangliaPassword,
  GangliaURL,
  GangliaUser,
  HaproxyHealthCheckMethod,
  HaproxyHealthCheckURL,
  HaproxyStatsPassword,
  HaproxyStatsURL,
  HaproxyStatsUser,
  JVM,
  JVMOptions,
  JVMVersion,
  JavaAppServer,
  JavaAppServerVersion,
  ManageBundler,
  MemcachedMemory,
  MysqlRootPassword,
  MysqlRootPasswordUbiquitous,
  NodejsVersion,
  PassengerVersion,
  RailsStack,
  RubyVersion,
  RubygemsVersion,
  LayerAttributesKeys'
  #-}
