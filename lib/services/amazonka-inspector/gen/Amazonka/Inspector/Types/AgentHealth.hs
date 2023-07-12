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
-- Module      : Amazonka.Inspector.Types.AgentHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AgentHealth
  ( AgentHealth
      ( ..,
        AgentHealth_HEALTHY,
        AgentHealth_UNHEALTHY,
        AgentHealth_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AgentHealth = AgentHealth'
  { fromAgentHealth ::
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
