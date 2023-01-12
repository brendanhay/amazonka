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
-- Module      : Amazonka.OpsWorksCM.Types.ServerStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types.ServerStatus
  ( ServerStatus
      ( ..,
        ServerStatus_BACKING_UP,
        ServerStatus_CONNECTION_LOST,
        ServerStatus_CREATING,
        ServerStatus_DELETING,
        ServerStatus_FAILED,
        ServerStatus_HEALTHY,
        ServerStatus_MODIFYING,
        ServerStatus_RESTORING,
        ServerStatus_RUNNING,
        ServerStatus_SETUP,
        ServerStatus_TERMINATED,
        ServerStatus_UNDER_MAINTENANCE,
        ServerStatus_UNHEALTHY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServerStatus = ServerStatus'
  { fromServerStatus ::
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

pattern ServerStatus_BACKING_UP :: ServerStatus
pattern ServerStatus_BACKING_UP = ServerStatus' "BACKING_UP"

pattern ServerStatus_CONNECTION_LOST :: ServerStatus
pattern ServerStatus_CONNECTION_LOST = ServerStatus' "CONNECTION_LOST"

pattern ServerStatus_CREATING :: ServerStatus
pattern ServerStatus_CREATING = ServerStatus' "CREATING"

pattern ServerStatus_DELETING :: ServerStatus
pattern ServerStatus_DELETING = ServerStatus' "DELETING"

pattern ServerStatus_FAILED :: ServerStatus
pattern ServerStatus_FAILED = ServerStatus' "FAILED"

pattern ServerStatus_HEALTHY :: ServerStatus
pattern ServerStatus_HEALTHY = ServerStatus' "HEALTHY"

pattern ServerStatus_MODIFYING :: ServerStatus
pattern ServerStatus_MODIFYING = ServerStatus' "MODIFYING"

pattern ServerStatus_RESTORING :: ServerStatus
pattern ServerStatus_RESTORING = ServerStatus' "RESTORING"

pattern ServerStatus_RUNNING :: ServerStatus
pattern ServerStatus_RUNNING = ServerStatus' "RUNNING"

pattern ServerStatus_SETUP :: ServerStatus
pattern ServerStatus_SETUP = ServerStatus' "SETUP"

pattern ServerStatus_TERMINATED :: ServerStatus
pattern ServerStatus_TERMINATED = ServerStatus' "TERMINATED"

pattern ServerStatus_UNDER_MAINTENANCE :: ServerStatus
pattern ServerStatus_UNDER_MAINTENANCE = ServerStatus' "UNDER_MAINTENANCE"

pattern ServerStatus_UNHEALTHY :: ServerStatus
pattern ServerStatus_UNHEALTHY = ServerStatus' "UNHEALTHY"

{-# COMPLETE
  ServerStatus_BACKING_UP,
  ServerStatus_CONNECTION_LOST,
  ServerStatus_CREATING,
  ServerStatus_DELETING,
  ServerStatus_FAILED,
  ServerStatus_HEALTHY,
  ServerStatus_MODIFYING,
  ServerStatus_RESTORING,
  ServerStatus_RUNNING,
  ServerStatus_SETUP,
  ServerStatus_TERMINATED,
  ServerStatus_UNDER_MAINTENANCE,
  ServerStatus_UNHEALTHY,
  ServerStatus'
  #-}
