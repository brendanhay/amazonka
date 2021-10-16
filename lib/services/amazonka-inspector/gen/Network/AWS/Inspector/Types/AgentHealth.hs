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
-- Module      : Network.AWS.Inspector.Types.AgentHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentHealth
  ( AgentHealth
      ( ..,
        AgentHealth_HEALTHY,
        AgentHealth_UNHEALTHY,
        AgentHealth_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AgentHealth = AgentHealth'
  { fromAgentHealth ::
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

pattern AgentHealth_HEALTHY :: AgentHealth
pattern AgentHealth_HEALTHY = AgentHealth' "HEALTHY"

pattern AgentHealth_UNHEALTHY :: AgentHealth
pattern AgentHealth_UNHEALTHY = AgentHealth' "UNHEALTHY"

pattern AgentHealth_UNKNOWN :: AgentHealth
pattern AgentHealth_UNKNOWN = AgentHealth' "UNKNOWN"

{-# COMPLETE
  AgentHealth_HEALTHY,
  AgentHealth_UNHEALTHY,
  AgentHealth_UNKNOWN,
  AgentHealth'
  #-}
