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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.Tier
  ( Tier
      ( ..,
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
        Tier_SQL_SERVER,
        Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Tier = Tier' {fromTier :: Core.Text}
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

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

pattern Tier_SQL_SERVER :: Tier
pattern Tier_SQL_SERVER = Tier' "SQL_SERVER"

pattern Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP :: Tier
pattern Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP = Tier' "SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP"

{-# COMPLETE
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
  Tier_SQL_SERVER,
  Tier_SQL_SERVER_ALWAYSON_AVAILABILITY_GROUP,
  Tier'
  #-}
