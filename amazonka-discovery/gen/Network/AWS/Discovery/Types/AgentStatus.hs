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
-- Module      : Network.AWS.Discovery.Types.AgentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentStatus
  ( AgentStatus
      ( ..,
        AgentStatus_BLACKLISTED,
        AgentStatus_HEALTHY,
        AgentStatus_RUNNING,
        AgentStatus_SHUTDOWN,
        AgentStatus_UNHEALTHY,
        AgentStatus_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AgentStatus = AgentStatus'
  { fromAgentStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern AgentStatus_BLACKLISTED :: AgentStatus
pattern AgentStatus_BLACKLISTED = AgentStatus' "BLACKLISTED"

pattern AgentStatus_HEALTHY :: AgentStatus
pattern AgentStatus_HEALTHY = AgentStatus' "HEALTHY"

pattern AgentStatus_RUNNING :: AgentStatus
pattern AgentStatus_RUNNING = AgentStatus' "RUNNING"

pattern AgentStatus_SHUTDOWN :: AgentStatus
pattern AgentStatus_SHUTDOWN = AgentStatus' "SHUTDOWN"

pattern AgentStatus_UNHEALTHY :: AgentStatus
pattern AgentStatus_UNHEALTHY = AgentStatus' "UNHEALTHY"

pattern AgentStatus_UNKNOWN :: AgentStatus
pattern AgentStatus_UNKNOWN = AgentStatus' "UNKNOWN"

{-# COMPLETE
  AgentStatus_BLACKLISTED,
  AgentStatus_HEALTHY,
  AgentStatus_RUNNING,
  AgentStatus_SHUTDOWN,
  AgentStatus_UNHEALTHY,
  AgentStatus_UNKNOWN,
  AgentStatus'
  #-}
