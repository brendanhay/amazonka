{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerAttributesKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.LayerAttributesKeys
  ( LayerAttributesKeys
    ( LayerAttributesKeys'
    , LayerAttributesKeysEcsClusterArn
    , LayerAttributesKeysEnableHaproxyStats
    , LayerAttributesKeysHaproxyStatsUrl
    , LayerAttributesKeysHaproxyStatsUser
    , LayerAttributesKeysHaproxyStatsPassword
    , LayerAttributesKeysHaproxyHealthCheckUrl
    , LayerAttributesKeysHaproxyHealthCheckMethod
    , LayerAttributesKeysMysqlRootPassword
    , LayerAttributesKeysMysqlRootPasswordUbiquitous
    , LayerAttributesKeysGangliaUrl
    , LayerAttributesKeysGangliaUser
    , LayerAttributesKeysGangliaPassword
    , LayerAttributesKeysMemcachedMemory
    , LayerAttributesKeysNodejsVersion
    , LayerAttributesKeysRubyVersion
    , LayerAttributesKeysRubygemsVersion
    , LayerAttributesKeysManageBundler
    , LayerAttributesKeysBundlerVersion
    , LayerAttributesKeysRailsStack
    , LayerAttributesKeysPassengerVersion
    , LayerAttributesKeysJvm
    , LayerAttributesKeysJvmVersion
    , LayerAttributesKeysJvmOptions
    , LayerAttributesKeysJavaAppServer
    , LayerAttributesKeysJavaAppServerVersion
    , fromLayerAttributesKeys
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LayerAttributesKeys = LayerAttributesKeys'{fromLayerAttributesKeys
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern LayerAttributesKeysEcsClusterArn :: LayerAttributesKeys
pattern LayerAttributesKeysEcsClusterArn = LayerAttributesKeys' "EcsClusterArn"

pattern LayerAttributesKeysEnableHaproxyStats :: LayerAttributesKeys
pattern LayerAttributesKeysEnableHaproxyStats = LayerAttributesKeys' "EnableHaproxyStats"

pattern LayerAttributesKeysHaproxyStatsUrl :: LayerAttributesKeys
pattern LayerAttributesKeysHaproxyStatsUrl = LayerAttributesKeys' "HaproxyStatsUrl"

pattern LayerAttributesKeysHaproxyStatsUser :: LayerAttributesKeys
pattern LayerAttributesKeysHaproxyStatsUser = LayerAttributesKeys' "HaproxyStatsUser"

pattern LayerAttributesKeysHaproxyStatsPassword :: LayerAttributesKeys
pattern LayerAttributesKeysHaproxyStatsPassword = LayerAttributesKeys' "HaproxyStatsPassword"

pattern LayerAttributesKeysHaproxyHealthCheckUrl :: LayerAttributesKeys
pattern LayerAttributesKeysHaproxyHealthCheckUrl = LayerAttributesKeys' "HaproxyHealthCheckUrl"

pattern LayerAttributesKeysHaproxyHealthCheckMethod :: LayerAttributesKeys
pattern LayerAttributesKeysHaproxyHealthCheckMethod = LayerAttributesKeys' "HaproxyHealthCheckMethod"

pattern LayerAttributesKeysMysqlRootPassword :: LayerAttributesKeys
pattern LayerAttributesKeysMysqlRootPassword = LayerAttributesKeys' "MysqlRootPassword"

pattern LayerAttributesKeysMysqlRootPasswordUbiquitous :: LayerAttributesKeys
pattern LayerAttributesKeysMysqlRootPasswordUbiquitous = LayerAttributesKeys' "MysqlRootPasswordUbiquitous"

pattern LayerAttributesKeysGangliaUrl :: LayerAttributesKeys
pattern LayerAttributesKeysGangliaUrl = LayerAttributesKeys' "GangliaUrl"

pattern LayerAttributesKeysGangliaUser :: LayerAttributesKeys
pattern LayerAttributesKeysGangliaUser = LayerAttributesKeys' "GangliaUser"

pattern LayerAttributesKeysGangliaPassword :: LayerAttributesKeys
pattern LayerAttributesKeysGangliaPassword = LayerAttributesKeys' "GangliaPassword"

pattern LayerAttributesKeysMemcachedMemory :: LayerAttributesKeys
pattern LayerAttributesKeysMemcachedMemory = LayerAttributesKeys' "MemcachedMemory"

pattern LayerAttributesKeysNodejsVersion :: LayerAttributesKeys
pattern LayerAttributesKeysNodejsVersion = LayerAttributesKeys' "NodejsVersion"

pattern LayerAttributesKeysRubyVersion :: LayerAttributesKeys
pattern LayerAttributesKeysRubyVersion = LayerAttributesKeys' "RubyVersion"

pattern LayerAttributesKeysRubygemsVersion :: LayerAttributesKeys
pattern LayerAttributesKeysRubygemsVersion = LayerAttributesKeys' "RubygemsVersion"

pattern LayerAttributesKeysManageBundler :: LayerAttributesKeys
pattern LayerAttributesKeysManageBundler = LayerAttributesKeys' "ManageBundler"

pattern LayerAttributesKeysBundlerVersion :: LayerAttributesKeys
pattern LayerAttributesKeysBundlerVersion = LayerAttributesKeys' "BundlerVersion"

pattern LayerAttributesKeysRailsStack :: LayerAttributesKeys
pattern LayerAttributesKeysRailsStack = LayerAttributesKeys' "RailsStack"

pattern LayerAttributesKeysPassengerVersion :: LayerAttributesKeys
pattern LayerAttributesKeysPassengerVersion = LayerAttributesKeys' "PassengerVersion"

pattern LayerAttributesKeysJvm :: LayerAttributesKeys
pattern LayerAttributesKeysJvm = LayerAttributesKeys' "Jvm"

pattern LayerAttributesKeysJvmVersion :: LayerAttributesKeys
pattern LayerAttributesKeysJvmVersion = LayerAttributesKeys' "JvmVersion"

pattern LayerAttributesKeysJvmOptions :: LayerAttributesKeys
pattern LayerAttributesKeysJvmOptions = LayerAttributesKeys' "JvmOptions"

pattern LayerAttributesKeysJavaAppServer :: LayerAttributesKeys
pattern LayerAttributesKeysJavaAppServer = LayerAttributesKeys' "JavaAppServer"

pattern LayerAttributesKeysJavaAppServerVersion :: LayerAttributesKeys
pattern LayerAttributesKeysJavaAppServerVersion = LayerAttributesKeys' "JavaAppServerVersion"

{-# COMPLETE 
  LayerAttributesKeysEcsClusterArn,

  LayerAttributesKeysEnableHaproxyStats,

  LayerAttributesKeysHaproxyStatsUrl,

  LayerAttributesKeysHaproxyStatsUser,

  LayerAttributesKeysHaproxyStatsPassword,

  LayerAttributesKeysHaproxyHealthCheckUrl,

  LayerAttributesKeysHaproxyHealthCheckMethod,

  LayerAttributesKeysMysqlRootPassword,

  LayerAttributesKeysMysqlRootPasswordUbiquitous,

  LayerAttributesKeysGangliaUrl,

  LayerAttributesKeysGangliaUser,

  LayerAttributesKeysGangliaPassword,

  LayerAttributesKeysMemcachedMemory,

  LayerAttributesKeysNodejsVersion,

  LayerAttributesKeysRubyVersion,

  LayerAttributesKeysRubygemsVersion,

  LayerAttributesKeysManageBundler,

  LayerAttributesKeysBundlerVersion,

  LayerAttributesKeysRailsStack,

  LayerAttributesKeysPassengerVersion,

  LayerAttributesKeysJvm,

  LayerAttributesKeysJvmVersion,

  LayerAttributesKeysJvmOptions,

  LayerAttributesKeysJavaAppServer,

  LayerAttributesKeysJavaAppServerVersion,
  LayerAttributesKeys'
  #-}
