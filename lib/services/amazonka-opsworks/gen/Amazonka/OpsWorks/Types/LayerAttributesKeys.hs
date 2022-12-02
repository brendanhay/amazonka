{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Types.LayerAttributesKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.LayerAttributesKeys
  ( LayerAttributesKeys
      ( ..,
        LayerAttributesKeys_BundlerVersion,
        LayerAttributesKeys_EcsClusterArn,
        LayerAttributesKeys_EnableHaproxyStats,
        LayerAttributesKeys_GangliaPassword,
        LayerAttributesKeys_GangliaUrl,
        LayerAttributesKeys_GangliaUser,
        LayerAttributesKeys_HaproxyHealthCheckMethod,
        LayerAttributesKeys_HaproxyHealthCheckUrl,
        LayerAttributesKeys_HaproxyStatsPassword,
        LayerAttributesKeys_HaproxyStatsUrl,
        LayerAttributesKeys_HaproxyStatsUser,
        LayerAttributesKeys_JavaAppServer,
        LayerAttributesKeys_JavaAppServerVersion,
        LayerAttributesKeys_Jvm,
        LayerAttributesKeys_JvmOptions,
        LayerAttributesKeys_JvmVersion,
        LayerAttributesKeys_ManageBundler,
        LayerAttributesKeys_MemcachedMemory,
        LayerAttributesKeys_MysqlRootPassword,
        LayerAttributesKeys_MysqlRootPasswordUbiquitous,
        LayerAttributesKeys_NodejsVersion,
        LayerAttributesKeys_PassengerVersion,
        LayerAttributesKeys_RailsStack,
        LayerAttributesKeys_RubyVersion,
        LayerAttributesKeys_RubygemsVersion
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LayerAttributesKeys = LayerAttributesKeys'
  { fromLayerAttributesKeys ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern LayerAttributesKeys_BundlerVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_BundlerVersion = LayerAttributesKeys' "BundlerVersion"

pattern LayerAttributesKeys_EcsClusterArn :: LayerAttributesKeys
pattern LayerAttributesKeys_EcsClusterArn = LayerAttributesKeys' "EcsClusterArn"

pattern LayerAttributesKeys_EnableHaproxyStats :: LayerAttributesKeys
pattern LayerAttributesKeys_EnableHaproxyStats = LayerAttributesKeys' "EnableHaproxyStats"

pattern LayerAttributesKeys_GangliaPassword :: LayerAttributesKeys
pattern LayerAttributesKeys_GangliaPassword = LayerAttributesKeys' "GangliaPassword"

pattern LayerAttributesKeys_GangliaUrl :: LayerAttributesKeys
pattern LayerAttributesKeys_GangliaUrl = LayerAttributesKeys' "GangliaUrl"

pattern LayerAttributesKeys_GangliaUser :: LayerAttributesKeys
pattern LayerAttributesKeys_GangliaUser = LayerAttributesKeys' "GangliaUser"

pattern LayerAttributesKeys_HaproxyHealthCheckMethod :: LayerAttributesKeys
pattern LayerAttributesKeys_HaproxyHealthCheckMethod = LayerAttributesKeys' "HaproxyHealthCheckMethod"

pattern LayerAttributesKeys_HaproxyHealthCheckUrl :: LayerAttributesKeys
pattern LayerAttributesKeys_HaproxyHealthCheckUrl = LayerAttributesKeys' "HaproxyHealthCheckUrl"

pattern LayerAttributesKeys_HaproxyStatsPassword :: LayerAttributesKeys
pattern LayerAttributesKeys_HaproxyStatsPassword = LayerAttributesKeys' "HaproxyStatsPassword"

pattern LayerAttributesKeys_HaproxyStatsUrl :: LayerAttributesKeys
pattern LayerAttributesKeys_HaproxyStatsUrl = LayerAttributesKeys' "HaproxyStatsUrl"

pattern LayerAttributesKeys_HaproxyStatsUser :: LayerAttributesKeys
pattern LayerAttributesKeys_HaproxyStatsUser = LayerAttributesKeys' "HaproxyStatsUser"

pattern LayerAttributesKeys_JavaAppServer :: LayerAttributesKeys
pattern LayerAttributesKeys_JavaAppServer = LayerAttributesKeys' "JavaAppServer"

pattern LayerAttributesKeys_JavaAppServerVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_JavaAppServerVersion = LayerAttributesKeys' "JavaAppServerVersion"

pattern LayerAttributesKeys_Jvm :: LayerAttributesKeys
pattern LayerAttributesKeys_Jvm = LayerAttributesKeys' "Jvm"

pattern LayerAttributesKeys_JvmOptions :: LayerAttributesKeys
pattern LayerAttributesKeys_JvmOptions = LayerAttributesKeys' "JvmOptions"

pattern LayerAttributesKeys_JvmVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_JvmVersion = LayerAttributesKeys' "JvmVersion"

pattern LayerAttributesKeys_ManageBundler :: LayerAttributesKeys
pattern LayerAttributesKeys_ManageBundler = LayerAttributesKeys' "ManageBundler"

pattern LayerAttributesKeys_MemcachedMemory :: LayerAttributesKeys
pattern LayerAttributesKeys_MemcachedMemory = LayerAttributesKeys' "MemcachedMemory"

pattern LayerAttributesKeys_MysqlRootPassword :: LayerAttributesKeys
pattern LayerAttributesKeys_MysqlRootPassword = LayerAttributesKeys' "MysqlRootPassword"

pattern LayerAttributesKeys_MysqlRootPasswordUbiquitous :: LayerAttributesKeys
pattern LayerAttributesKeys_MysqlRootPasswordUbiquitous = LayerAttributesKeys' "MysqlRootPasswordUbiquitous"

pattern LayerAttributesKeys_NodejsVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_NodejsVersion = LayerAttributesKeys' "NodejsVersion"

pattern LayerAttributesKeys_PassengerVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_PassengerVersion = LayerAttributesKeys' "PassengerVersion"

pattern LayerAttributesKeys_RailsStack :: LayerAttributesKeys
pattern LayerAttributesKeys_RailsStack = LayerAttributesKeys' "RailsStack"

pattern LayerAttributesKeys_RubyVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_RubyVersion = LayerAttributesKeys' "RubyVersion"

pattern LayerAttributesKeys_RubygemsVersion :: LayerAttributesKeys
pattern LayerAttributesKeys_RubygemsVersion = LayerAttributesKeys' "RubygemsVersion"

{-# COMPLETE
  LayerAttributesKeys_BundlerVersion,
  LayerAttributesKeys_EcsClusterArn,
  LayerAttributesKeys_EnableHaproxyStats,
  LayerAttributesKeys_GangliaPassword,
  LayerAttributesKeys_GangliaUrl,
  LayerAttributesKeys_GangliaUser,
  LayerAttributesKeys_HaproxyHealthCheckMethod,
  LayerAttributesKeys_HaproxyHealthCheckUrl,
  LayerAttributesKeys_HaproxyStatsPassword,
  LayerAttributesKeys_HaproxyStatsUrl,
  LayerAttributesKeys_HaproxyStatsUser,
  LayerAttributesKeys_JavaAppServer,
  LayerAttributesKeys_JavaAppServerVersion,
  LayerAttributesKeys_Jvm,
  LayerAttributesKeys_JvmOptions,
  LayerAttributesKeys_JvmVersion,
  LayerAttributesKeys_ManageBundler,
  LayerAttributesKeys_MemcachedMemory,
  LayerAttributesKeys_MysqlRootPassword,
  LayerAttributesKeys_MysqlRootPasswordUbiquitous,
  LayerAttributesKeys_NodejsVersion,
  LayerAttributesKeys_PassengerVersion,
  LayerAttributesKeys_RailsStack,
  LayerAttributesKeys_RubyVersion,
  LayerAttributesKeys_RubygemsVersion,
  LayerAttributesKeys'
  #-}
