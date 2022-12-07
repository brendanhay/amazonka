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
-- Module      : Amazonka.ApplicationInsights.Types.Tier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.Tier
  ( Tier
      ( ..,
        Tier_ACTIVE_DIRECTORY,
        Tier_CUSTOM,
        Tier_DEFAULT,
        Tier_DOT_NET_CORE,
        Tier_DOT_NET_WEB,
        Tier_DOT_NET_WEB_TIER,
        Tier_DOT_NET_WORKER,
        Tier_JAVA_JMX,
        Tier_MYSQL,
        Tier_ORACLE,
        Tier_POSTGRESQL,
        Tier_SAP_HANA_HIGH_AVAILABILITY,
        Tier_SAP_HANA_MULTI_NODE,
        Tier_SAP_HANA_SINGLE_NODE,
        Tier_SHAREPOINT,
        Tier_SQL_SERVER,
        Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP,
        Tier_SQL_SERVER_FAILOVER_CLUSTER_INSTANCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Tier = Tier' {fromTier :: Data.Text}
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

pattern Tier_ACTIVE_DIRECTORY :: Tier
pattern Tier_ACTIVE_DIRECTORY = Tier' "ACTIVE_DIRECTORY"

pattern Tier_CUSTOM :: Tier
pattern Tier_CUSTOM = Tier' "CUSTOM"

pattern Tier_DEFAULT :: Tier
pattern Tier_DEFAULT = Tier' "DEFAULT"

pattern Tier_DOT_NET_CORE :: Tier
pattern Tier_DOT_NET_CORE = Tier' "DOT_NET_CORE"

pattern Tier_DOT_NET_WEB :: Tier
pattern Tier_DOT_NET_WEB = Tier' "DOT_NET_WEB"

pattern Tier_DOT_NET_WEB_TIER :: Tier
pattern Tier_DOT_NET_WEB_TIER = Tier' "DOT_NET_WEB_TIER"

pattern Tier_DOT_NET_WORKER :: Tier
pattern Tier_DOT_NET_WORKER = Tier' "DOT_NET_WORKER"

pattern Tier_JAVA_JMX :: Tier
pattern Tier_JAVA_JMX = Tier' "JAVA_JMX"

pattern Tier_MYSQL :: Tier
pattern Tier_MYSQL = Tier' "MYSQL"

pattern Tier_ORACLE :: Tier
pattern Tier_ORACLE = Tier' "ORACLE"

pattern Tier_POSTGRESQL :: Tier
pattern Tier_POSTGRESQL = Tier' "POSTGRESQL"

pattern Tier_SAP_HANA_HIGH_AVAILABILITY :: Tier
pattern Tier_SAP_HANA_HIGH_AVAILABILITY = Tier' "SAP_HANA_HIGH_AVAILABILITY"

pattern Tier_SAP_HANA_MULTI_NODE :: Tier
pattern Tier_SAP_HANA_MULTI_NODE = Tier' "SAP_HANA_MULTI_NODE"

pattern Tier_SAP_HANA_SINGLE_NODE :: Tier
pattern Tier_SAP_HANA_SINGLE_NODE = Tier' "SAP_HANA_SINGLE_NODE"

pattern Tier_SHAREPOINT :: Tier
pattern Tier_SHAREPOINT = Tier' "SHAREPOINT"

pattern Tier_SQL_SERVER :: Tier
pattern Tier_SQL_SERVER = Tier' "SQL_SERVER"

pattern Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP :: Tier
pattern Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP = Tier' "SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP"

pattern Tier_SQL_SERVER_FAILOVER_CLUSTER_INSTANCE :: Tier
pattern Tier_SQL_SERVER_FAILOVER_CLUSTER_INSTANCE = Tier' "SQL_SERVER_FAILOVER_CLUSTER_INSTANCE"

{-# COMPLETE
  Tier_ACTIVE_DIRECTORY,
  Tier_CUSTOM,
  Tier_DEFAULT,
  Tier_DOT_NET_CORE,
  Tier_DOT_NET_WEB,
  Tier_DOT_NET_WEB_TIER,
  Tier_DOT_NET_WORKER,
  Tier_JAVA_JMX,
  Tier_MYSQL,
  Tier_ORACLE,
  Tier_POSTGRESQL,
  Tier_SAP_HANA_HIGH_AVAILABILITY,
  Tier_SAP_HANA_MULTI_NODE,
  Tier_SAP_HANA_SINGLE_NODE,
  Tier_SHAREPOINT,
  Tier_SQL_SERVER,
  Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP,
  Tier_SQL_SERVER_FAILOVER_CLUSTER_INSTANCE,
  Tier'
  #-}
