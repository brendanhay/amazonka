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
-- Module      : Amazonka.MediaLive.Types.MultiplexState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexState
  ( MultiplexState
      ( ..,
        MultiplexState_CREATE_FAILED,
        MultiplexState_CREATING,
        MultiplexState_DELETED,
        MultiplexState_DELETING,
        MultiplexState_IDLE,
        MultiplexState_RECOVERING,
        MultiplexState_RUNNING,
        MultiplexState_STARTING,
        MultiplexState_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current state of the multiplex.
newtype MultiplexState = MultiplexState'
  { fromMultiplexState ::
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

pattern MultiplexState_CREATE_FAILED :: MultiplexState
pattern MultiplexState_CREATE_FAILED = MultiplexState' "CREATE_FAILED"

pattern MultiplexState_CREATING :: MultiplexState
pattern MultiplexState_CREATING = MultiplexState' "CREATING"

pattern MultiplexState_DELETED :: MultiplexState
pattern MultiplexState_DELETED = MultiplexState' "DELETED"

pattern MultiplexState_DELETING :: MultiplexState
pattern MultiplexState_DELETING = MultiplexState' "DELETING"

pattern MultiplexState_IDLE :: MultiplexState
pattern MultiplexState_IDLE = MultiplexState' "IDLE"

pattern MultiplexState_RECOVERING :: MultiplexState
pattern MultiplexState_RECOVERING = MultiplexState' "RECOVERING"

pattern MultiplexState_RUNNING :: MultiplexState
pattern MultiplexState_RUNNING = MultiplexState' "RUNNING"

pattern MultiplexState_STARTING :: MultiplexState
pattern MultiplexState_STARTING = MultiplexState' "STARTING"

pattern MultiplexState_STOPPING :: MultiplexState
pattern MultiplexState_STOPPING = MultiplexState' "STOPPING"

{-# COMPLETE
  MultiplexState_CREATE_FAILED,
  MultiplexState_CREATING,
  MultiplexState_DELETED,
  MultiplexState_DELETING,
  MultiplexState_IDLE,
  MultiplexState_RECOVERING,
  MultiplexState_RUNNING,
  MultiplexState_STARTING,
  MultiplexState_STOPPING,
  MultiplexState'
  #-}
