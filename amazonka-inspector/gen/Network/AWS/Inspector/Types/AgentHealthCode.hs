{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentHealthCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentHealthCode
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

import qualified Network.AWS.Prelude as Prelude

newtype AgentHealthCode = AgentHealthCode'
  { fromAgentHealthCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
