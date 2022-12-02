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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceState
  ( WorkspaceState
      ( ..,
        WorkspaceState_ADMIN_MAINTENANCE,
        WorkspaceState_AVAILABLE,
        WorkspaceState_ERROR,
        WorkspaceState_IMPAIRED,
        WorkspaceState_MAINTENANCE,
        WorkspaceState_PENDING,
        WorkspaceState_REBOOTING,
        WorkspaceState_REBUILDING,
        WorkspaceState_RESTORING,
        WorkspaceState_STARTING,
        WorkspaceState_STOPPED,
        WorkspaceState_STOPPING,
        WorkspaceState_SUSPENDED,
        WorkspaceState_TERMINATED,
        WorkspaceState_TERMINATING,
        WorkspaceState_UNHEALTHY,
        WorkspaceState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkspaceState = WorkspaceState'
  { fromWorkspaceState ::
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

pattern WorkspaceState_ADMIN_MAINTENANCE :: WorkspaceState
pattern WorkspaceState_ADMIN_MAINTENANCE = WorkspaceState' "ADMIN_MAINTENANCE"

pattern WorkspaceState_AVAILABLE :: WorkspaceState
pattern WorkspaceState_AVAILABLE = WorkspaceState' "AVAILABLE"

pattern WorkspaceState_ERROR :: WorkspaceState
pattern WorkspaceState_ERROR = WorkspaceState' "ERROR"

pattern WorkspaceState_IMPAIRED :: WorkspaceState
pattern WorkspaceState_IMPAIRED = WorkspaceState' "IMPAIRED"

pattern WorkspaceState_MAINTENANCE :: WorkspaceState
pattern WorkspaceState_MAINTENANCE = WorkspaceState' "MAINTENANCE"

pattern WorkspaceState_PENDING :: WorkspaceState
pattern WorkspaceState_PENDING = WorkspaceState' "PENDING"

pattern WorkspaceState_REBOOTING :: WorkspaceState
pattern WorkspaceState_REBOOTING = WorkspaceState' "REBOOTING"

pattern WorkspaceState_REBUILDING :: WorkspaceState
pattern WorkspaceState_REBUILDING = WorkspaceState' "REBUILDING"

pattern WorkspaceState_RESTORING :: WorkspaceState
pattern WorkspaceState_RESTORING = WorkspaceState' "RESTORING"

pattern WorkspaceState_STARTING :: WorkspaceState
pattern WorkspaceState_STARTING = WorkspaceState' "STARTING"

pattern WorkspaceState_STOPPED :: WorkspaceState
pattern WorkspaceState_STOPPED = WorkspaceState' "STOPPED"

pattern WorkspaceState_STOPPING :: WorkspaceState
pattern WorkspaceState_STOPPING = WorkspaceState' "STOPPING"

pattern WorkspaceState_SUSPENDED :: WorkspaceState
pattern WorkspaceState_SUSPENDED = WorkspaceState' "SUSPENDED"

pattern WorkspaceState_TERMINATED :: WorkspaceState
pattern WorkspaceState_TERMINATED = WorkspaceState' "TERMINATED"

pattern WorkspaceState_TERMINATING :: WorkspaceState
pattern WorkspaceState_TERMINATING = WorkspaceState' "TERMINATING"

pattern WorkspaceState_UNHEALTHY :: WorkspaceState
pattern WorkspaceState_UNHEALTHY = WorkspaceState' "UNHEALTHY"

pattern WorkspaceState_UPDATING :: WorkspaceState
pattern WorkspaceState_UPDATING = WorkspaceState' "UPDATING"

{-# COMPLETE
  WorkspaceState_ADMIN_MAINTENANCE,
  WorkspaceState_AVAILABLE,
  WorkspaceState_ERROR,
  WorkspaceState_IMPAIRED,
  WorkspaceState_MAINTENANCE,
  WorkspaceState_PENDING,
  WorkspaceState_REBOOTING,
  WorkspaceState_REBUILDING,
  WorkspaceState_RESTORING,
  WorkspaceState_STARTING,
  WorkspaceState_STOPPED,
  WorkspaceState_STOPPING,
  WorkspaceState_SUSPENDED,
  WorkspaceState_TERMINATED,
  WorkspaceState_TERMINATING,
  WorkspaceState_UNHEALTHY,
  WorkspaceState_UPDATING,
  WorkspaceState'
  #-}
