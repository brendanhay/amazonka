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
-- Module      : Amazonka.Discovery.Types.AgentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.AgentStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AgentStatus = AgentStatus'
  { fromAgentStatus ::
      Core.Text
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
