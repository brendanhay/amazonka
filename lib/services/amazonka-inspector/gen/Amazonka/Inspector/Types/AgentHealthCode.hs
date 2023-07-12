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
-- Module      : Amazonka.Inspector.Types.AgentHealthCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AgentHealthCode
  ( AgentHealthCode
      ( ..,
        AgentHealthCode_IDLE,
        AgentHealthCode_RUNNING,
        AgentHealthCode_SHUTDOWN,
        AgentHealthCode_THROTTLED,
        AgentHealthCode_UNHEALTHY,
        AgentHealthCode_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AgentHealthCode = AgentHealthCode'
  { fromAgentHealthCode ::
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

pattern AgentHealthCode_IDLE :: AgentHealthCode
pattern AgentHealthCode_IDLE = AgentHealthCode' "IDLE"

pattern AgentHealthCode_RUNNING :: AgentHealthCode
pattern AgentHealthCode_RUNNING = AgentHealthCode' "RUNNING"

pattern AgentHealthCode_SHUTDOWN :: AgentHealthCode
pattern AgentHealthCode_SHUTDOWN = AgentHealthCode' "SHUTDOWN"

pattern AgentHealthCode_THROTTLED :: AgentHealthCode
pattern AgentHealthCode_THROTTLED = AgentHealthCode' "THROTTLED"

pattern AgentHealthCode_UNHEALTHY :: AgentHealthCode
pattern AgentHealthCode_UNHEALTHY = AgentHealthCode' "UNHEALTHY"

pattern AgentHealthCode_UNKNOWN :: AgentHealthCode
pattern AgentHealthCode_UNKNOWN = AgentHealthCode' "UNKNOWN"

{-# COMPLETE
  AgentHealthCode_IDLE,
  AgentHealthCode_RUNNING,
  AgentHealthCode_SHUTDOWN,
  AgentHealthCode_THROTTLED,
  AgentHealthCode_UNHEALTHY,
  AgentHealthCode_UNKNOWN,
  AgentHealthCode'
  #-}
