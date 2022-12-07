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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationState
  ( RecoveryInstanceDataReplicationState
      ( ..,
        RecoveryInstanceDataReplicationState_BACKLOG,
        RecoveryInstanceDataReplicationState_CONTINUOUS,
        RecoveryInstanceDataReplicationState_CREATING_SNAPSHOT,
        RecoveryInstanceDataReplicationState_DISCONNECTED,
        RecoveryInstanceDataReplicationState_INITIAL_SYNC,
        RecoveryInstanceDataReplicationState_INITIATING,
        RecoveryInstanceDataReplicationState_PAUSED,
        RecoveryInstanceDataReplicationState_RESCAN,
        RecoveryInstanceDataReplicationState_STALLED,
        RecoveryInstanceDataReplicationState_STOPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecoveryInstanceDataReplicationState = RecoveryInstanceDataReplicationState'
  { fromRecoveryInstanceDataReplicationState ::
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

pattern RecoveryInstanceDataReplicationState_BACKLOG :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_BACKLOG = RecoveryInstanceDataReplicationState' "BACKLOG"

pattern RecoveryInstanceDataReplicationState_CONTINUOUS :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_CONTINUOUS = RecoveryInstanceDataReplicationState' "CONTINUOUS"

pattern RecoveryInstanceDataReplicationState_CREATING_SNAPSHOT :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_CREATING_SNAPSHOT = RecoveryInstanceDataReplicationState' "CREATING_SNAPSHOT"

pattern RecoveryInstanceDataReplicationState_DISCONNECTED :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_DISCONNECTED = RecoveryInstanceDataReplicationState' "DISCONNECTED"

pattern RecoveryInstanceDataReplicationState_INITIAL_SYNC :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_INITIAL_SYNC = RecoveryInstanceDataReplicationState' "INITIAL_SYNC"

pattern RecoveryInstanceDataReplicationState_INITIATING :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_INITIATING = RecoveryInstanceDataReplicationState' "INITIATING"

pattern RecoveryInstanceDataReplicationState_PAUSED :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_PAUSED = RecoveryInstanceDataReplicationState' "PAUSED"

pattern RecoveryInstanceDataReplicationState_RESCAN :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_RESCAN = RecoveryInstanceDataReplicationState' "RESCAN"

pattern RecoveryInstanceDataReplicationState_STALLED :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_STALLED = RecoveryInstanceDataReplicationState' "STALLED"

pattern RecoveryInstanceDataReplicationState_STOPPED :: RecoveryInstanceDataReplicationState
pattern RecoveryInstanceDataReplicationState_STOPPED = RecoveryInstanceDataReplicationState' "STOPPED"

{-# COMPLETE
  RecoveryInstanceDataReplicationState_BACKLOG,
  RecoveryInstanceDataReplicationState_CONTINUOUS,
  RecoveryInstanceDataReplicationState_CREATING_SNAPSHOT,
  RecoveryInstanceDataReplicationState_DISCONNECTED,
  RecoveryInstanceDataReplicationState_INITIAL_SYNC,
  RecoveryInstanceDataReplicationState_INITIATING,
  RecoveryInstanceDataReplicationState_PAUSED,
  RecoveryInstanceDataReplicationState_RESCAN,
  RecoveryInstanceDataReplicationState_STALLED,
  RecoveryInstanceDataReplicationState_STOPPED,
  RecoveryInstanceDataReplicationState'
  #-}
