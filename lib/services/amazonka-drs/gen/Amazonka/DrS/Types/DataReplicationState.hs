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
-- Module      : Amazonka.DrS.Types.DataReplicationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DataReplicationState
  ( DataReplicationState
      ( ..,
        DataReplicationState_BACKLOG,
        DataReplicationState_CONTINUOUS,
        DataReplicationState_CREATING_SNAPSHOT,
        DataReplicationState_DISCONNECTED,
        DataReplicationState_INITIAL_SYNC,
        DataReplicationState_INITIATING,
        DataReplicationState_PAUSED,
        DataReplicationState_RESCAN,
        DataReplicationState_STALLED,
        DataReplicationState_STOPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataReplicationState = DataReplicationState'
  { fromDataReplicationState ::
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

pattern DataReplicationState_BACKLOG :: DataReplicationState
pattern DataReplicationState_BACKLOG = DataReplicationState' "BACKLOG"

pattern DataReplicationState_CONTINUOUS :: DataReplicationState
pattern DataReplicationState_CONTINUOUS = DataReplicationState' "CONTINUOUS"

pattern DataReplicationState_CREATING_SNAPSHOT :: DataReplicationState
pattern DataReplicationState_CREATING_SNAPSHOT = DataReplicationState' "CREATING_SNAPSHOT"

pattern DataReplicationState_DISCONNECTED :: DataReplicationState
pattern DataReplicationState_DISCONNECTED = DataReplicationState' "DISCONNECTED"

pattern DataReplicationState_INITIAL_SYNC :: DataReplicationState
pattern DataReplicationState_INITIAL_SYNC = DataReplicationState' "INITIAL_SYNC"

pattern DataReplicationState_INITIATING :: DataReplicationState
pattern DataReplicationState_INITIATING = DataReplicationState' "INITIATING"

pattern DataReplicationState_PAUSED :: DataReplicationState
pattern DataReplicationState_PAUSED = DataReplicationState' "PAUSED"

pattern DataReplicationState_RESCAN :: DataReplicationState
pattern DataReplicationState_RESCAN = DataReplicationState' "RESCAN"

pattern DataReplicationState_STALLED :: DataReplicationState
pattern DataReplicationState_STALLED = DataReplicationState' "STALLED"

pattern DataReplicationState_STOPPED :: DataReplicationState
pattern DataReplicationState_STOPPED = DataReplicationState' "STOPPED"

{-# COMPLETE
  DataReplicationState_BACKLOG,
  DataReplicationState_CONTINUOUS,
  DataReplicationState_CREATING_SNAPSHOT,
  DataReplicationState_DISCONNECTED,
  DataReplicationState_INITIAL_SYNC,
  DataReplicationState_INITIATING,
  DataReplicationState_PAUSED,
  DataReplicationState_RESCAN,
  DataReplicationState_STALLED,
  DataReplicationState_STOPPED,
  DataReplicationState'
  #-}
