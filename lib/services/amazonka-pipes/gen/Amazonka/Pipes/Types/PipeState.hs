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
-- Module      : Amazonka.Pipes.Types.PipeState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeState
  ( PipeState
      ( ..,
        PipeState_CREATE_FAILED,
        PipeState_CREATING,
        PipeState_DELETING,
        PipeState_RUNNING,
        PipeState_STARTING,
        PipeState_START_FAILED,
        PipeState_STOPPED,
        PipeState_STOPPING,
        PipeState_STOP_FAILED,
        PipeState_UPDATE_FAILED,
        PipeState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PipeState = PipeState'
  { fromPipeState ::
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

pattern PipeState_CREATE_FAILED :: PipeState
pattern PipeState_CREATE_FAILED = PipeState' "CREATE_FAILED"

pattern PipeState_CREATING :: PipeState
pattern PipeState_CREATING = PipeState' "CREATING"

pattern PipeState_DELETING :: PipeState
pattern PipeState_DELETING = PipeState' "DELETING"

pattern PipeState_RUNNING :: PipeState
pattern PipeState_RUNNING = PipeState' "RUNNING"

pattern PipeState_STARTING :: PipeState
pattern PipeState_STARTING = PipeState' "STARTING"

pattern PipeState_START_FAILED :: PipeState
pattern PipeState_START_FAILED = PipeState' "START_FAILED"

pattern PipeState_STOPPED :: PipeState
pattern PipeState_STOPPED = PipeState' "STOPPED"

pattern PipeState_STOPPING :: PipeState
pattern PipeState_STOPPING = PipeState' "STOPPING"

pattern PipeState_STOP_FAILED :: PipeState
pattern PipeState_STOP_FAILED = PipeState' "STOP_FAILED"

pattern PipeState_UPDATE_FAILED :: PipeState
pattern PipeState_UPDATE_FAILED = PipeState' "UPDATE_FAILED"

pattern PipeState_UPDATING :: PipeState
pattern PipeState_UPDATING = PipeState' "UPDATING"

{-# COMPLETE
  PipeState_CREATE_FAILED,
  PipeState_CREATING,
  PipeState_DELETING,
  PipeState_RUNNING,
  PipeState_STARTING,
  PipeState_START_FAILED,
  PipeState_STOPPED,
  PipeState_STOPPING,
  PipeState_STOP_FAILED,
  PipeState_UPDATE_FAILED,
  PipeState_UPDATING,
  PipeState'
  #-}
